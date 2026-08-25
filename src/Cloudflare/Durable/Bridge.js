
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
