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
