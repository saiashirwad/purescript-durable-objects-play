import { mkdir, writeFile } from "node:fs/promises";
import { css as chatCss } from "../output/Frontend.Chat.Style/index.js";
import { css } from "../output/UI.Styles/index.js";

await mkdir(new URL("../public/", import.meta.url), { recursive: true });
await writeFile(new URL("../public/ui.css", import.meta.url), `${css}\n`, "utf8");
await writeFile(new URL("../public/chat.css", import.meta.url), `${chatCss}\n`, "utf8");
