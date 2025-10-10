use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::sink;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

use swc_common::{
    errors::{EmitterWriter, Handler, HANDLER},
    sync::Lrc,
    FileName, SourceMap, Span, GLOBALS,
};
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::{
    ArrayLit, Callee, Expr, ExprOrSpread, Ident, KeyValueProp, Lit, MemberExpr, MemberProp, Module,
    ModuleItem, ObjectLit, Prop, PropName, PropOrSpread,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Any,
    Number,
    String,
    Boolean,
    Null,
    Array(Box<TypeInfo>),
    Object(HashMap<String, TypeInfo>),
}

impl TypeInfo {
    pub fn describe(&self) -> String {
        match self {
            TypeInfo::Any => "any".to_string(),
            TypeInfo::Number => "number".to_string(),
            TypeInfo::String => "string".to_string(),
            TypeInfo::Boolean => "boolean".to_string(),
            TypeInfo::Null => "null".to_string(),
            TypeInfo::Array(inner) => format!("{}[]", inner.describe()),
            TypeInfo::Object(props) => {
                if props.is_empty() {
                    "Record<string, any>".to_string()
                } else {
                    let fields = props
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, ty.describe()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{{ {} }}", fields)
                }
            }
        }
    }

    pub fn element_type(&self) -> TypeInfo {
        match self {
            TypeInfo::Array(inner) => (*inner.clone()).clone(),
            _ => TypeInfo::Any,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StateVariable {
    pub name: String,
    pub ty: TypeInfo,
}

#[derive(Debug, Clone)]
pub struct LoopBinding {
    pub collection: String,
    pub collection_type: TypeInfo,
    pub item: Option<StateVariable>,
    pub index: Option<StateVariable>,
}

#[derive(Debug, Clone)]
pub struct ManifoldState {
    pub properties: HashMap<String, TypeInfo>,
}

impl ManifoldState {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
        }
    }

    pub fn set_property(&mut self, name: String, ty: TypeInfo) {
        self.properties.insert(name, ty);
    }

    pub fn property_type(&self, name: &str) -> Option<&TypeInfo> {
        self.properties.get(name)
    }
}

pub type ManifoldStates = HashMap<String, ManifoldState>;

#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    pub uri: Url,
    pub line: u32,
    pub character: u32,
    pub length: u32,
}

pub type DefinitionIndex = HashMap<(String, String), DefinitionLocation>;

#[derive(Clone)]
struct FileHandle {
    path: PathBuf,
    content: Arc<String>,
    line_index: Arc<crate::lineindex::LineIndex>,
}

impl FileHandle {
    fn new(path: PathBuf, content: String) -> Self {
        let content_arc = Arc::new(content);
        let line_index = Arc::new(crate::lineindex::LineIndex::new(content_arc.as_str()));
        Self {
            path,
            content: content_arc,
            line_index,
        }
    }
}

struct CachedFile {
    handle: FileHandle,
    module: Option<Arc<Module>>,
}

impl CachedFile {
    fn new(path: PathBuf, content: String) -> Self {
        Self {
            handle: FileHandle::new(path, content),
            module: None,
        }
    }
}

#[derive(Default)]
struct FileCache {
    files: HashMap<PathBuf, CachedFile>,
}

impl FileCache {
    fn get_handle(&self, path: &Path) -> Option<FileHandle> {
        self.files.get(path).map(|entry| entry.handle.clone())
    }

    fn resolve_script(&mut self, specifier: &str, base: &Path) -> Option<FileHandle> {
        if !(specifier.starts_with('.') || specifier.starts_with('/')) {
            return None;
        }

        let joined = if specifier.starts_with('/') {
            PathBuf::from(specifier)
        } else {
            base.join(specifier)
        };

        let candidates = vec![
            joined.clone(),
            joined.with_extension("ts"),
            joined.with_extension("js"),
            joined.with_extension("mjs"),
            joined.with_extension("cjs"),
            joined.join("index.ts"),
            joined.join("index.js"),
            joined.join("index.mjs"),
            joined.join("index.cjs"),
        ];

        for candidate in candidates {
            let canonical = canonical_or(candidate.clone());
            if let Some(handle) = self.get_handle(&canonical) {
                return Some(handle);
            }
            if let Ok(content) = fs::read_to_string(&candidate) {
                let canonical = canonical_or(candidate);
                let cached = CachedFile::new(canonical.clone(), content);
                let handle = cached.handle.clone();
                self.files.insert(canonical.clone(), cached);
                return Some(handle);
            }
        }

        None
    }

    fn ensure_module(&mut self, path: &Path) -> Option<Arc<Module>> {
        if let Some(entry) = self.files.get_mut(path) {
            if let Some(module) = &entry.module {
                return Some(module.clone());
            }
            if let Some(parsed) = parse_script_module(&entry.handle.content) {
                let arc = Arc::new(parsed);
                entry.module = Some(arc.clone());
                return Some(arc);
            }
        }
        None
    }
}

pub fn parse_states_from_scripts(text: &str) -> ManifoldStates {
    // Backward-compatible entry that ignores unresolved includes.
    let (states, _unresolved) = parse_states_from_document(text, None);
    states
}

pub fn parse_states_from_document(
    text: &str,
    base_dir: Option<&Path>,
) -> (ManifoldStates, Vec<String>) {
    let mut states: ManifoldStates = HashMap::new();
    let (inline_scripts, srcs) = extract_script_blocks_and_srcs(text);

    let default_base = std::env::current_dir().ok();
    let base = base_dir.or(default_base.as_deref());
    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Parse inline scripts, follow imports
    for script in inline_scripts {
        if let Some(module) = parse_script_module(&script) {
            process_module_for_states(&module, &mut states);
            if let Some(base) = base {
                follow_imports(
                    &module,
                    base,
                    &mut states,
                    &mut visited,
                    &mut unresolved,
                    0,
                    &mut file_cache,
                );
            }
        }
    }

    // Parse classic/extern scripts referenced via src
    for src in srcs {
        if let Some(base) = base {
            if let Some(handle) = file_cache.resolve_script(&src, base) {
                if visited.insert(handle.path.clone()) {
                    if let Some(module) = file_cache.ensure_module(&handle.path) {
                        process_module_for_states(module.as_ref(), &mut states);
                        let next_base = handle.path.parent().unwrap_or(base);
                        follow_imports(
                            module.as_ref(),
                            next_base,
                            &mut states,
                            &mut visited,
                            &mut unresolved,
                            0,
                            &mut file_cache,
                        );
                    }
                }
            } else {
                unresolved.push(src);
            }
        } else {
            // No base directory; record as unresolved so we can inform the user
            unresolved.push(src);
        }
    }

    (states, unresolved)
}

pub fn build_definition_index(
    html_text: &str,
    html_path: Option<&Path>,
) -> (DefinitionIndex, Vec<String>) {
    let mut index: DefinitionIndex = HashMap::new();
    let (inline_scripts, srcs, starts) = extract_script_blocks_srcs_with_starts(html_text);
    let html_uri = html_path.and_then(|p| Url::from_file_path(p).ok());
    let html_li = crate::lineindex::LineIndex::new(html_text);

    let default_base = html_path
        .map(|p| p.parent().map(|pp| pp.to_path_buf()))
        .flatten()
        .or_else(|| std::env::current_dir().ok())
        .unwrap_or_else(|| PathBuf::from("."));

    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Inline scripts: parse and harvest definitions; positions map to HTML URI
    for (script, start) in inline_scripts.iter().zip(starts.iter()) {
        if let Some(module) = parse_script_module(script) {
            // Collect property names per state
            let mut defs = Vec::<(String, PropertyDefinition)>::new();
            for item in &module.body {
                if let Some(definition) = analyze_module_item(item) {
                    let state_name = definition.name.unwrap_or_else(|| "default".to_string());
                    for prop in definition.properties {
                        defs.push((state_name.clone(), prop));
                    }
                }
            }
            // Simple text search to locate property occurrences
            for (state_name, prop) in defs {
                let mut recorded = false;
                if let Some(span) = prop.span {
                    if let Some((start_rel, end_rel)) = literal_body_range(span, script) {
                        if let Some(uri) = html_uri.clone() {
                            let absolute = *start + start_rel;
                            let p = html_li.position_at(absolute);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(script, start_rel, end_rel),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(method_pos) = script
                    .find(&format!("add(\"{}\"", prop.name))
                    .or_else(|| script.find(&format!(".add(\"{}\"", prop.name)))
                    .or_else(|| script.find(&format!("add('{}'", prop.name)))
                    .or_else(|| script.find(&format!(".add('{}'", prop.name)))
                {
                    let quote_offset = script[method_pos..]
                        .find('"')
                        .or_else(|| script[method_pos..].find('\''));
                    if let Some(quote_rel) = quote_offset {
                        let prop_start_rel = method_pos + quote_rel + 1;
                        let absolute = *start + prop_start_rel;
                        let p = html_li.position_at(absolute);
                        if let Some(uri) = html_uri.clone() {
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: prop.name.len() as u32,
                                },
                            );
                        }
                    }
                }
            }

            // Follow ESM imports from this inline module to discover external definitions
            collect_defs_from_imports(
                &module,
                &default_base,
                &mut index,
                &mut visited,
                &mut unresolved,
                0,
                &mut file_cache,
            );
        }
    }

    // External scripts via src
    for src in srcs {
        if let Some(handle) = file_cache.resolve_script(&src, &default_base) {
            if visited.insert(handle.path.clone()) {
                if let Some(module) = file_cache.ensure_module(&handle.path) {
                    harvest_definitions_from_module(
                        module.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        &mut index,
                    );
                    collect_defs_from_imports(
                        module.as_ref(),
                        handle.path.parent().unwrap_or(&default_base),
                        &mut index,
                        &mut visited,
                        &mut unresolved,
                        0,
                        &mut file_cache,
                    );
                }
            }
        } else {
            unresolved.push(src);
        }
    }

    (index, unresolved)
}

pub fn build_state_name_index(
    html_text: &str,
    html_path: Option<&Path>,
) -> (HashMap<String, DefinitionLocation>, Vec<String>) {
    let mut index: HashMap<String, DefinitionLocation> = HashMap::new();
    let (inline_scripts, srcs, starts) = extract_script_blocks_srcs_with_starts(html_text);
    let html_uri = html_path.and_then(|p| Url::from_file_path(p).ok());
    let html_li = crate::lineindex::LineIndex::new(html_text);

    let default_base = html_path
        .and_then(|p| p.parent().map(|pp| pp.to_path_buf()))
        .or_else(|| std::env::current_dir().ok())
        .unwrap_or_else(|| PathBuf::from("."));

    let mut unresolved: Vec<String> = Vec::new();
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut file_cache = FileCache::default();

    // Inline scripts
    for (script, start) in inline_scripts.iter().zip(starts.iter()) {
        if let Some(module) = parse_script_module(script) {
            // Collect state names
            let mut names: Vec<(String, Option<Span>)> = Vec::new();
            for item in &module.body {
                if let Some(definition) = analyze_module_item(item) {
                    if let Some(name) = definition.name {
                        names.push((name, definition.name_span));
                    }
                }
            }
            for (name, span) in names {
                let mut recorded = false;
                if let Some(span) = span {
                    if let Some((start_rel, end_rel)) = literal_body_range(span, script) {
                        if let Some(uri) = html_uri.clone() {
                            let absolute = *start + start_rel;
                            let p = html_li.position_at(absolute);
                            index.insert(
                                name.clone(),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(script, start_rel, end_rel),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(pos) = find_create_call(script, &name) {
                    if let Some(uri) = html_uri.clone() {
                        let absolute = *start + pos;
                        let p = html_li.position_at(absolute);
                        index.insert(
                            name.clone(),
                            DefinitionLocation {
                                uri,
                                line: p.line,
                                character: p.character,
                                length: name.len() as u32,
                            },
                        );
                    }
                }
            }

            collect_state_names_from_imports(
                &module,
                &default_base,
                &mut index,
                &mut visited,
                &mut unresolved,
                0,
                &mut file_cache,
            );
        }
    }

    // External scripts via src
    for src in srcs {
        if let Some(handle) = file_cache.resolve_script(&src, &default_base) {
            if visited.insert(handle.path.clone()) {
                if let Some(module) = file_cache.ensure_module(&handle.path) {
                    harvest_state_names_from_module(
                        module.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        &mut index,
                    );
                    collect_state_names_from_imports(
                        module.as_ref(),
                        handle.path.parent().unwrap_or(&default_base),
                        &mut index,
                        &mut visited,
                        &mut unresolved,
                        0,
                        &mut file_cache,
                    );
                }
            }
        } else {
            unresolved.push(src);
        }
    }

    (index, unresolved)
}

fn find_create_call(content: &str, name: &str) -> Option<usize> {
    [
        format!("create(\"{}\")", name),
        format!(".create(\"{}\")", name),
        format!("create('{}')", name),
        format!(".create('{}')", name),
    ]
    .into_iter()
    .find_map(|p| content.find(&p))
}

pub fn resolve_identifier_type(
    identifier: &str,
    state: Option<&ManifoldState>,
    locals: &HashMap<String, TypeInfo>,
) -> Option<TypeInfo> {
    let mut segments = identifier
        .split('.')
        .map(|segment| segment.trim())
        .filter(|s| !s.is_empty());
    let first = segments.next()?;

    let mut current = if let Some(local) = locals.get(first) {
        local.clone()
    } else {
        state?.property_type(first)?.clone()
    };

    for segment in segments {
        current = match current {
            TypeInfo::Object(ref props) => props.get(segment).cloned().unwrap_or(TypeInfo::Any),
            TypeInfo::Array(ref inner) => {
                if segment == "length" {
                    TypeInfo::Number
                } else {
                    (*inner.clone()).clone()
                }
            }
            _ => TypeInfo::Any,
        };
    }

    Some(current)
}

fn extract_script_blocks_and_srcs(text: &str) -> (Vec<String>, Vec<String>) {
    let mut blocks: Vec<String> = Vec::new();
    let mut srcs: Vec<String> = Vec::new();
    let lower = text.to_lowercase();
    let mut offset = 0;

    while let Some(start_rel) = lower[offset..].find("<script") {
        let start = offset + start_rel;
        let tag_close_rel = match lower[start..].find('>') {
            Some(index) => index,
            None => break,
        };
        // Extract within the tag to search for src attributes
        let tag_open = &text[start..start + tag_close_rel + 1];
        if let Some(src_val) = find_attribute_value(tag_open, "src") {
            srcs.push(src_val);
        }

        let content_start = start + tag_close_rel + 1;
        let end_rel = match lower[content_start..].find("</script") {
            Some(index) => index,
            None => break,
        };
        let content_end = content_start + end_rel;
        blocks.push(text[content_start..content_end].to_string());

        let after_end = match lower[content_end..].find('>') {
            Some(index) => content_end + index + 1,
            None => content_end,
        };
        offset = after_end;
    }

    (blocks, srcs)
}

fn extract_script_blocks_srcs_with_starts(text: &str) -> (Vec<String>, Vec<String>, Vec<usize>) {
    let mut blocks: Vec<String> = Vec::new();
    let mut srcs: Vec<String> = Vec::new();
    let mut starts: Vec<usize> = Vec::new();
    let lower = text.to_lowercase();
    let mut offset = 0;

    while let Some(start_rel) = lower[offset..].find("<script") {
        let start = offset + start_rel;
        let tag_close_rel = match lower[start..].find('>') {
            Some(index) => index,
            None => break,
        };
        let tag_open = &text[start..start + tag_close_rel + 1];
        if let Some(src_val) = find_attribute_value(tag_open, "src") {
            srcs.push(src_val);
        }
        let content_start = start + tag_close_rel + 1;
        let end_rel = match lower[content_start..].find("</script") {
            Some(index) => index,
            None => break,
        };
        let content_end = content_start + end_rel;
        starts.push(content_start);
        blocks.push(text[content_start..content_end].to_string());
        let after_end = match lower[content_end..].find('>') {
            Some(index) => content_end + index + 1,
            None => content_end,
        };
        offset = after_end;
    }

    (blocks, srcs, starts)
}

fn find_attribute_value(tag_open: &str, attr: &str) -> Option<String> {
    // naive attribute extractor for src="..." or src='...'
    let lower = tag_open.to_ascii_lowercase();
    let needle = format!("{}=", attr);
    let mut i = 0usize;
    while let Some(pos) = lower[i..].find(&needle) {
        let idx = i + pos + needle.len();
        let bytes = tag_open.as_bytes();
        if idx >= bytes.len() {
            break;
        }
        let quote = bytes[idx];
        if quote == b'"' || quote == b'\'' {
            let rest = &tag_open[idx + 1..];
            if let Some(end) = rest.find(quote as char) {
                return Some(rest[..end].to_string());
            }
        }
        i = idx + 1;
    }
    None
}

fn process_module_for_states(module: &Module, states: &mut ManifoldStates) {
    for item in &module.body {
        if let Some(definition) = analyze_module_item(item) {
            let name = definition.name.unwrap_or_else(|| "default".to_string());
            let mut state = states.remove(&name).unwrap_or_else(ManifoldState::new);
            for prop in definition.properties {
                state.set_property(prop.name, prop.ty);
            }
            states.insert(name, state);
        }
    }
}

fn follow_imports(
    module: &Module,
    base: &Path,
    states: &mut ManifoldStates,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return; // reasonable safety cap
    }
    let imports = collect_import_paths(module);
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    process_module_for_states(child.as_ref(), states);
                    let next_base = handle.path.parent().unwrap_or(base);
                    follow_imports(
                        child.as_ref(),
                        next_base,
                        states,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn collect_import_paths(module: &Module) -> Vec<String> {
    let mut paths = Vec::new();
    for item in &module.body {
        if let ModuleItem::ModuleDecl(decl) = item {
            match decl {
                swc_ecma_ast::ModuleDecl::Import(import) => {
                    paths.push(import.src.value.to_string());
                }
                swc_ecma_ast::ModuleDecl::ExportNamed(named) => {
                    if let Some(src) = &named.src {
                        paths.push(src.value.to_string());
                    }
                }
                swc_ecma_ast::ModuleDecl::ExportAll(all) => {
                    paths.push(all.src.value.to_string());
                }
                _ => {}
            }
        }
    }
    paths
}

fn canonical_or(path: PathBuf) -> PathBuf {
    fs::canonicalize(&path).unwrap_or(path)
}

fn harvest_definitions_from_module(
    module: &Module,
    content: &str,
    file_path: &Path,
    li: &crate::lineindex::LineIndex,
    index: &mut DefinitionIndex,
) {
    for item in &module.body {
        if let Some(definition) = analyze_module_item(item) {
            let state_name = definition.name.unwrap_or_else(|| "default".to_string());
            for prop in definition.properties {
                let mut recorded = false;
                if let Some(span) = prop.span {
                    if let Some((start, end)) = literal_body_range(span, content) {
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(start);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(content, start, end),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(method_pos) = content
                    .find(&format!("add(\"{}\"", prop.name))
                    .or_else(|| content.find(&format!(".add(\"{}\"", prop.name)))
                    .or_else(|| content.find(&format!("add('{}'", prop.name)))
                    .or_else(|| content.find(&format!(".add('{}'", prop.name)))
                {
                    let quote_offset = content[method_pos..]
                        .find('"')
                        .or_else(|| content[method_pos..].find('\''));
                    if let Some(quote_rel) = quote_offset {
                        let prop_start = method_pos + quote_rel + 1;
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(prop_start);
                            index.insert(
                                (state_name.clone(), prop.name.clone()),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: prop.name.len() as u32,
                                },
                            );
                        }
                    }
                }
            }
        }
    }
}

fn collect_defs_from_imports(
    module: &Module,
    base: &Path,
    index: &mut DefinitionIndex,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return;
    }
    let imports = collect_import_paths(module);
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    harvest_definitions_from_module(
                        child.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        index,
                    );
                    let next_base = handle.path.parent().unwrap_or(base);
                    collect_defs_from_imports(
                        child.as_ref(),
                        next_base,
                        index,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn harvest_state_names_from_module(
    module: &Module,
    content: &str,
    file_path: &Path,
    li: &crate::lineindex::LineIndex,
    index: &mut HashMap<String, DefinitionLocation>,
) {
    for item in &module.body {
        if let Some(definition) = analyze_module_item(item) {
            if let Some(name) = definition.name {
                let mut recorded = false;
                if let Some(span) = definition.name_span {
                    if let Some((start, end)) = literal_body_range(span, content) {
                        if let Ok(uri) = Url::from_file_path(file_path) {
                            let p = li.position_at(start);
                            index.insert(
                                name.clone(),
                                DefinitionLocation {
                                    uri,
                                    line: p.line,
                                    character: p.character,
                                    length: slice_char_length(content, start, end),
                                },
                            );
                            recorded = true;
                        }
                    }
                }

                if recorded {
                    continue;
                }

                if let Some(pos) = find_create_call(content, &name) {
                    if let Ok(uri) = Url::from_file_path(file_path) {
                        let p = li.position_at(pos);
                        index.insert(
                            name.clone(),
                            DefinitionLocation {
                                uri,
                                line: p.line,
                                character: p.character,
                                length: name.len() as u32,
                            },
                        );
                    }
                }
            }
        }
    }
}

fn collect_state_names_from_imports(
    module: &Module,
    base: &Path,
    index: &mut HashMap<String, DefinitionLocation>,
    visited: &mut HashSet<PathBuf>,
    unresolved: &mut Vec<String>,
    depth: usize,
    cache: &mut FileCache,
) {
    if depth > 16 {
        return;
    }
    let imports = collect_import_paths(module);
    for import in imports {
        if let Some(handle) = cache.resolve_script(&import, base) {
            if visited.insert(handle.path.clone()) {
                if let Some(child) = cache.ensure_module(&handle.path) {
                    harvest_state_names_from_module(
                        child.as_ref(),
                        &handle.content,
                        &handle.path,
                        handle.line_index.as_ref(),
                        index,
                    );
                    let next_base = handle.path.parent().unwrap_or(base);
                    collect_state_names_from_imports(
                        child.as_ref(),
                        next_base,
                        index,
                        visited,
                        unresolved,
                        depth + 1,
                        cache,
                    );
                }
            }
        } else {
            unresolved.push(import);
        }
    }
}

fn span_to_byte_range(span: Span) -> Option<(usize, usize)> {
    let lo = span.lo.0;
    let hi = span.hi.0;
    if hi < lo {
        return None;
    }
    let start = lo.checked_sub(1)? as usize;
    let len = (hi - lo) as usize;
    let end = start + len;
    Some((start, end))
}

fn literal_body_range(span: Span, text: &str) -> Option<(usize, usize)> {
    let (start, end) = span_to_byte_range(span)?;
    if end > text.len() {
        return None;
    }
    if end <= start {
        return None;
    }
    let bytes = text.as_bytes();
    if end - start >= 2 {
        let first = bytes.get(start)?;
        let last = bytes.get(end - 1)?;
        if (*first == b'"' && *last == b'"') || (*first == b'\'' && *last == b'\'') {
            return Some((start + 1, end - 1));
        }
        if *first == b'`' && *last == b'`' {
            return Some((start + 1, end - 1));
        }
    }
    Some((start, end))
}

fn slice_char_length(text: &str, start: usize, end: usize) -> u32 {
    text[start..end].chars().count() as u32
}

fn parse_script_module(script: &str) -> Option<Module> {
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_emitter(
        false,
        false,
        Box::new(EmitterWriter::new(Box::new(sink()), None, false, false)),
    );

    GLOBALS.set(&Default::default(), || {
        HANDLER.set(&handler, || {
            let fm = cm.new_source_file(FileName::Anon, script.to_string());
            let lexer = Lexer::new(
                Syntax::Typescript(TsConfig {
                    tsx: false,
                    dts: false,
                    ..Default::default()
                }),
                EsVersion::Es2022,
                StringInput::from(&*fm),
                None,
            );
            let mut parser = Parser::new_from(lexer);
            match parser.parse_module() {
                Ok(module) => Some(module),
                Err(err) => {
                    err.into_diagnostic(&handler).emit();
                    None
                }
            }
        })
    })
}

#[derive(Clone)]
struct PropertyDefinition {
    name: String,
    ty: TypeInfo,
    span: Option<Span>,
}

#[derive(Default)]
struct StateDefinition {
    name: Option<String>,
    name_span: Option<Span>,
    properties: Vec<PropertyDefinition>,
}

fn analyze_module_item(item: &ModuleItem) -> Option<StateDefinition> {
    match item {
        ModuleItem::Stmt(stmt) => match stmt {
            swc_ecma_ast::Stmt::Decl(decl) => match decl {
                swc_ecma_ast::Decl::Var(var_decl) => {
                    for declarator in &var_decl.decls {
                        if let Some(init) = declarator.init.as_ref() {
                            if let Some(def) = analyze_expression(init.as_ref()) {
                                return Some(def);
                            }
                        }
                    }
                    None
                }
                _ => None,
            },
            _ => None,
        },
        ModuleItem::ModuleDecl(module_decl) => match module_decl {
            swc_ecma_ast::ModuleDecl::ExportDecl(export_decl) => match &export_decl.decl {
                swc_ecma_ast::Decl::Var(var_decl) => {
                    for declarator in &var_decl.decls {
                        if let Some(init) = declarator.init.as_ref() {
                            if let Some(def) = analyze_expression(init.as_ref()) {
                                return Some(def);
                            }
                        }
                    }
                    None
                }
                _ => None,
            },
            swc_ecma_ast::ModuleDecl::ExportDefaultExpr(default_expr) => {
                analyze_expression(&default_expr.expr)
            }
            _ => None,
        },
    }
}

fn analyze_expression(expr: &Expr) -> Option<StateDefinition> {
    if let Expr::Call(call) = expr {
        if let Callee::Expr(callee) = &call.callee {
            if let Expr::Member(member) = &**callee {
                if property_name(member)? == "build" {
                    let mut definition = StateDefinition::default();
                    if let Some(arg) = call.args.get(0) {
                        if let Some((name, span)) = string_from_expr(&arg.expr) {
                            definition.name = Some(name);
                            definition.name_span = span;
                        }
                    }
                    collect_chain(&member.obj, &mut definition)?;
                    return Some(definition);
                }
            }
        }
    }
    None
}

fn collect_chain(expr: &Expr, definition: &mut StateDefinition) -> Option<()> {
    match expr {
        Expr::Call(call) => {
            if let Callee::Expr(callee) = &call.callee {
                if let Expr::Member(member) = &**callee {
                    let method = property_name(member)?;
                    match method.as_str() {
                        "add" => {
                            if let Some(ExprOrSpread {
                                expr: prop_expr, ..
                            }) = call.args.get(0)
                            {
                                if let Some((prop_name, span)) = string_from_expr(prop_expr) {
                                    let ty = call
                                        .args
                                        .get(1)
                                        .and_then(|arg| infer_type_from_expr(&arg.expr))
                                        .unwrap_or(TypeInfo::Any);
                                    definition.properties.push(PropertyDefinition {
                                        name: prop_name,
                                        ty,
                                        span,
                                    });
                                }
                            }
                            collect_chain(&member.obj, definition)
                        }
                        "create" => {
                            if definition.name.is_none() {
                                if let Some(ExprOrSpread {
                                    expr: name_expr, ..
                                }) = call.args.get(0)
                                {
                                    if let Some((name, span)) = string_from_expr(name_expr) {
                                        definition.name = Some(name);
                                        definition.name_span = span;
                                    }
                                }
                            }
                            Some(())
                        }
                        _ => collect_chain(&member.obj, definition),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expr::Member(member) => collect_chain(&member.obj, definition),
        Expr::Ident(ident) => {
            if ident.sym == *"Manifold" {
                Some(())
            } else {
                Some(())
            }
        }
        _ => Some(()),
    }
}

fn property_name(member: &MemberExpr) -> Option<String> {
    match &member.prop {
        MemberProp::Ident(ident) => Some(ident.sym.to_string()),
        _ => None,
    }
}

fn string_from_expr(expr: &Expr) -> Option<(String, Option<Span>)> {
    match expr {
        Expr::Lit(Lit::Str(str_lit)) => Some((str_lit.value.to_string(), Some(str_lit.span))),
        Expr::Lit(Lit::Bool(b)) => Some((b.value.to_string(), Some(b.span))),
        Expr::Ident(Ident { sym, span, .. }) => Some((sym.to_string(), Some(*span))),
        _ => None,
    }
}

fn infer_type_from_expr(expr: &Expr) -> Option<TypeInfo> {
    let ty = match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(_) => TypeInfo::String,
            Lit::Num(_) => TypeInfo::Number,
            Lit::Bool(_) => TypeInfo::Boolean,
            Lit::Null(_) => TypeInfo::Null,
            Lit::BigInt(_) => TypeInfo::Number,
            Lit::Regex(_) => TypeInfo::String,
            _ => TypeInfo::Any,
        },
        Expr::Array(array) => infer_array_type(array),
        Expr::Object(object) => infer_object_type(object),
        _ => TypeInfo::Any,
    };
    Some(ty)
}

fn infer_array_type(array: &ArrayLit) -> TypeInfo {
    let mut element: Option<TypeInfo> = None;
    for elem in &array.elems {
        if let Some(expr_or_spread) = elem {
            if expr_or_spread.spread.is_some() {
                return TypeInfo::Any;
            }
            if let Some(value) = infer_type_from_expr(&expr_or_spread.expr) {
                element = match element {
                    None => Some(value),
                    Some(existing) => {
                        if existing == value {
                            Some(existing)
                        } else {
                            return TypeInfo::Any;
                        }
                    }
                };
            }
        }
    }
    TypeInfo::Array(Box::new(element.unwrap_or(TypeInfo::Any)))
}

fn infer_object_type(object: &ObjectLit) -> TypeInfo {
    let mut props = HashMap::new();
    for prop in &object.props {
        if let PropOrSpread::Prop(prop) = prop {
            if let Prop::KeyValue(KeyValueProp { key, value }) = &**prop {
                if let Some(name) = property_name_from_prop(key) {
                    if let Some(value_type) = infer_type_from_expr(value) {
                        props.insert(name, value_type);
                    }
                }
            }
        }
    }
    TypeInfo::Object(props)
}

fn property_name_from_prop(name: &PropName) -> Option<String> {
    match name {
        PropName::Ident(ident) => Some(ident.sym.to_string()),
        PropName::Str(str_lit) => Some(str_lit.value.to_string()),
        _ => None,
    }
}
