import $ from "./src/main.ts";

type DemoUser = { name: string; age: number };

const myState = $.create()
	.add("count", 0)
	.add("popup", (e: unknown) => console.log(e))
	.add("nextFail", false)
	.add(
		"currentUserPromise",
		null as Promise<{ name: string; age: number }> | null
	)
	.add("someArray", [
		{ name: "Jake", age: 37 },
		{ name: "Mary", age: 37 },
		{ name: "Isaac", age: 6 },
		{ name: "Elinor", age: 4 },
	])
	.add("list", [
		{ id: 1, text: "Item 1" },
		{ id: 2, text: "Item 2" },
		{ id: 3, text: "Item 3" },
	])
	.add("addToList", () => {
		const nextId = myState.list.length
			? myState.list[myState.list.length - 1].id + 1
			: 1;
		myState.list.push({ id: nextId, text: `Item ${nextId}` });
	})
	.add("removeFromList", (key: number) => {
		myState.list.splice(key, 1);
	})
	// Provide placeholder so the property exists on the type
	.add("loadUser", () => Promise.resolve<DemoUser>({ name: "", age: 0 }))
	.build();

myState.loadUser = () => {
	const fail = !!myState.nextFail;
	myState.nextFail = !fail;

	const p: Promise<DemoUser> = new Promise((res, rej) =>
		setTimeout(() => {
			if (fail) console.log("REJECTING PROMISE");
			else console.log("RESOLVING PROMISE");

			if (fail) rej(new Error("Network"));
			else
				res({
					name: "Ada",
					age: 37 + Math.floor(Math.random() * 10),
				});
		}, 800)
	);
	myState.currentUserPromise = p;
	return p;
};

// Initial load
myState.loadUser();

export type DemoState = typeof myState;
export default myState;
