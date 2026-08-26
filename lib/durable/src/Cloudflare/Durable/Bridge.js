
// `base` is DurableObject from cloudflare:workers, passed in so this file has
// no platform import and loads under Node.
export const bridgeImpl = (base, methods, activate) => {
  class Bridged extends base {
    constructor(ctx, env) {
      super(ctx, env);
      this.handlers = null;
      ctx.blockConcurrencyWhile(async () => {
        this.handlers = await activate(ctx)(env)();
      });
    }
    alarm() {
      return this.handlers.alarm();
    }
    // `GET .../socket?tag=<tag>` with an Upgrade header. The socket hibernates
    // with the object; its id and tag ride along as the attachment.
    async fetch(request) {
      if (request.headers.get("Upgrade") !== "websocket") {
        return this.handlers.fetch(request)();
      }
      const tag = new URL(request.url).searchParams.get("tag") ?? "";
      const socket = { id: crypto.randomUUID(), tag };
      const pair = new WebSocketPair();
      const [client, server] = Object.values(pair);
      this.ctx.acceptWebSocket(server, [socket.id]);
      server.serializeAttachment(socket);
      await this.handlers.connect(socket)();
      return new Response(null, { status: 101, webSocket: client });
    }
    webSocketMessage() {}
    webSocketClose(ws) {
      return this.handlers.disconnect(ws.deserializeAttachment())();
    }
    webSocketError(ws) {
      return this.handlers.disconnect(ws.deserializeAttachment())();
    }
  }
  for (const name of methods) {
    Object.defineProperty(Bridged.prototype, name, {
      value: function (request) {
        return this.handlers.methods[name](request)();
      },
      writable: true,
      configurable: true,
    });
  }
  return Bridged;
};
