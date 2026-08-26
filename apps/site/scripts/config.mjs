// Run after `spago build`.
import { writeFileSync } from "node:fs";
import { config, configWithContainers } from "../../../output/Site.Deploy/index.js";

const chosen = process.env.CONTAINERS ? configWithContainers : config;
writeFileSync(new URL("../wrangler.jsonc", import.meta.url), JSON.stringify(chosen, null, 2) + "\n");
console.log("wrote apps/site/wrangler.jsonc");
