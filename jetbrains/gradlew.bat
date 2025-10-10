@ECHO OFF
SETLOCAL

SET DIR=%~dp0
SET WRAPPER_JAR=%DIR%\gradle\wrapper\gradle-wrapper.jar
IF EXIST "%WRAPPER_JAR%" GOTO runGradle

ECHO Downloading Gradle wrapper...
SET WRAPPER_URL=https://services.gradle.org/distributions/gradle-8.7-bin.zip
SET TEMP_DIR=%TEMP%\gradle-wrapper-%RANDOM%
MKDIR "%TEMP_DIR%"
powershell -Command "Invoke-WebRequest -Uri '%WRAPPER_URL%' -OutFile '%TEMP_DIR%\gradle-distribution.zip'"
powershell -Command "Add-Type -AssemblyName System.IO.Compression.FileSystem; [System.IO.Compression.ZipFile]::OpenRead('%TEMP_DIR%\gradle-distribution.zip').Entries | Where-Object { $_.FullName -eq 'gradle-8.7/lib/gradle-wrapper.jar' } | ForEach-Object { $_.ExtractToFile('%WRAPPER_JAR%', $true) }"
RMDIR /S /Q "%TEMP_DIR%"

:runGradle
SET DEFAULT_JVM_OPTS=-Xmx64m -Xms64m
SET CLASSPATH=%WRAPPER_JAR%
SET JAVA_EXE=java
IF NOT "%JAVA_HOME%"=="" SET JAVA_EXE=%JAVA_HOME%\bin\java.exe

"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %GRADLE_OPTS% -Dorg.gradle.appname="gradlew" -classpath "%CLASSPATH%" org.gradle.wrapper.GradleWrapperMain %*
