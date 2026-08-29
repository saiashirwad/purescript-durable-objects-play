// Not under `bun run check`: this imports compiled PureScript from output/, and tsc
// cannot load that without also checking spago's copies of the FFI files there.
// The wrangler entry point: PureScript objects and the API, exported as classes and a handler.
import { DurableObject } from "cloudflare:workers";
import { bridge } from "../../../output/Cloudflare.Durable.Bridge/index.js";
import { toExport } from "../../../output/Cloudflare.Worker/index.js";
import { counterLive } from "../../../output/Counter.Object/index.js";
import { roomLive } from "../../../output/Chat.Room.Live/index.js";
import { echoLive } from "../../../output/Echo.Object/index.js";
import { api } from "../../../output/Site.Api/index.js";

export class Counter extends bridge(DurableObject, counterLive) {}
export class Room extends bridge(DurableObject, roomLive) {}
export class Echo extends bridge(DurableObject, echoLive) {}

export default toExport(api);
