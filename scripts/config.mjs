// Run after `spago build`.
import { writeFileSync } from "node:fs";
import { config, configWithContainers } from "../output/Example.Deploy/index.js";

const chosen = process.env.CONTAINERS ? configWithContainers : config;
writeFileSync("wrangler.jsonc", JSON.stringify(chosen, null, 2) + "\n");
console.log("wrote wrangler.jsonc");
