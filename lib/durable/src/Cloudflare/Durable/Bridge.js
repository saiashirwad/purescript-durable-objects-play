// @ts-check

/**
 * @typedef {{ id: string, tag: string }} Socket
 *   A hibernating WebSocket's attachment: its id and the tag it subscribed with.
 *
 * @typedef {object} Handlers
 *   What `activate` returns: the object's behaviour, as PureScript effects.
 * @property {AsyncEffect<void>} alarm
 * @property {(request: Request) => AsyncEffect<Response>} fetch
 * @property {(socket: Socket) => AsyncEffect<void>} connect
 * @property {(socket: Socket) => AsyncEffect<void>} disconnect
 * @property {Record<string, (request: unknown) => AsyncEffect<unknown>>} methods
 *
 * @typedef {(ctx: DurableObjectState) => (env: Bindings) => AsyncEffect<Handlers>} Activate
 *
 * @typedef {typeof import("cloudflare:workers").DurableObject<Bindings>} Base
 *   `DurableObject` from `cloudflare:workers`, with bindings looked up by name.
 */

/**
 * Subclass a Durable Object base with handlers built by PureScript.
 *
 * `base` is `DurableObject` from `cloudflare:workers`, passed in so this file
 * has no platform import and loads under Node.
 *
 * @param {Base} base
 * @param {string[]} methods RPC method names to expose on the class
 * @param {Activate} activate
 * @returns {Base}
 */
export const bridgeImpl = (base, methods, activate) => {
  class Bridged extends base {
    /**
     * @param {DurableObjectState} ctx
     * @param {Bindings} env
     */
    constructor(ctx, env) {
      super(ctx, env);
      /** @type {Handlers | null} */
      this.handlers = null;
      ctx.blockConcurrencyWhile(async () => {
        this.handlers = await activate(ctx)(env)();
      });
    }

    /** The handlers, which `blockConcurrencyWhile` guarantees are set before any call. */
    get live() {
      if (this.handlers === null) throw new Error("Durable Object used before it activated");
      return this.handlers;
    }

    alarm() {
      return this.live.alarm();
    }

    /**
     * `GET .../socket?tag=<tag>` with an Upgrade header. The socket hibernates
     * with the object; its id and tag ride along as the attachment.
     *
     * @param {Request} request
     * @returns {Promise<Response>}
     */
    async fetch(request) {
      if (request.headers.get("Upgrade") !== "websocket") {
        return this.live.fetch(request)();
      }
      const tag = new URL(request.url).searchParams.get("tag") ?? "";
      /** @type {Socket} */
      const socket = { id: crypto.randomUUID(), tag };
      const pair = new WebSocketPair();
      const [client, server] = Object.values(pair);
      this.ctx.acceptWebSocket(server, [socket.id]);
      server.serializeAttachment(socket);
      await this.live.connect(socket)();
      return new Response(null, { status: 101, webSocket: client });
    }

    webSocketMessage() {}

    /** @param {WebSocket} ws */
    webSocketClose(ws) {
      return this.live.disconnect(attachment(ws))();
    }

    /** @param {WebSocket} ws */
    webSocketError(ws) {
      return this.live.disconnect(attachment(ws))();
    }
  }
  for (const name of methods) {
    Object.defineProperty(Bridged.prototype, name, {
      /**
       * @this {Bridged}
       * @param {unknown} request
       */
      value: function (request) {
        return this.live.methods[name](request)();
      },
      writable: true,
      configurable: true,
    });
  }
  return Bridged;
};

/**
 * @param {WebSocket} ws
 * @returns {Socket}
 */
const attachment = (ws) => /** @type {Socket} */ (ws.deserializeAttachment());
