// Not under `bun run check`: this imports compiled PureScript from output/, and tsc
// cannot load that without also checking dependency copies of the FFI files there.
// Write wrangler.jsonc from `Site.Deploy`. Run after `bun run build`; set CONTAINERS=1 to include containers.
import { writeFileSync } from "node:fs";
import { config, configWithContainers } from "../../../output/Site.Deploy/index.js";

const chosen = process.env.CONTAINERS ? configWithContainers : config;
writeFileSync(new URL("../wrangler.jsonc", import.meta.url), JSON.stringify(chosen, null, 2) + "\n");
console.log("wrote apps/site/wrangler.jsonc");
