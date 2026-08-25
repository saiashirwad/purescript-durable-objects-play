// Run after `spago build`.
import { writeFileSync } from "node:fs";
import { config } from "../output/Example.Deploy/index.js";

writeFileSync("wrangler.jsonc", JSON.stringify(config, null, 2) + "\n");
console.log("wrote wrangler.jsonc");
