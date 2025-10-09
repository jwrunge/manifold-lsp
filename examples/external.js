import Manifold from "https://cdn.jsdelivr.net/npm/mfld@latest/dist/manifold.es.js";

export const isolatedState = Manifold.create("isolated")
	.add("isolatedVar", "I'm isolated!")
	.build();
