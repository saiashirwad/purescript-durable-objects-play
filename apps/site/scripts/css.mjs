// Not under `bun run check`: this imports compiled PureScript from output/, and tsc
// cannot load that without also checking dependency copies of the FFI files there.
// Write the stylesheets PureScript computes. Run after the Alexandrite build.
import { mkdir, writeFile } from "node:fs/promises";
import { css as chatCss } from "../../../output/Chat.Style/index.js";
import { css } from "../../../output/UI.Styles/index.js";

await mkdir(new URL("../public/", import.meta.url), { recursive: true });
await writeFile(new URL("../public/ui.css", import.meta.url), `${css}\n`, "utf8");
await writeFile(new URL("../public/chat.css", import.meta.url), `${chatCss}\n`, "utf8");
