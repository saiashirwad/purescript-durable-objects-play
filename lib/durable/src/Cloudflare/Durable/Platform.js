// @ts-check
// No imports, so this file loads under Node.

/**
 * @param {DurableObjectState} ctx
 * @returns {(key: string) => AsyncEffect<Nullable<unknown>>}
 */
export const storageGet = (ctx) => (key) => () =>
  ctx.storage.get(key).then((value) => (value === undefined ? null : value));

/**
 * @param {DurableObjectState} ctx
 * @returns {(key: string) => (value: unknown) => AsyncEffect<void>}
 */
export const storagePut = (ctx) => (key) => (value) => () =>
  ctx.storage.put(key, value);

/**
 * @param {DurableObjectState} ctx
 * @returns {(key: string) => AsyncEffect<boolean>}
 */
export const storageDelete = (ctx) => (key) => () => ctx.storage.delete(key);

/**
 * @typedef {{ prefix: string, reverse: boolean, limit: Nullable<number> }} ListOptions
 * @typedef {{ key: string, value: unknown }} Entry
 */

/**
 * @param {DurableObjectState} ctx
 * @returns {(options: ListOptions) => AsyncEffect<Entry[]>}
 */
export const storageList = (ctx) => (options) => () => {
  /** @type {DurableObjectListOptions} */
  const query = { prefix: options.prefix, reverse: options.reverse };
  if (options.limit !== null) query.limit = options.limit;
  return ctx.storage
    .list(query)
    .then((entries) => Array.from(entries, ([key, value]) => ({ key, value })));
};

/**
 * @param {DurableObjectState} ctx
 * @returns {AsyncEffect<void>}
 */
export const storageDeleteAll = (ctx) => () => ctx.storage.deleteAll();

/** @type {Effect<number>} */
export const now = () => Date.now();

/**
 * @param {DurableObjectState} ctx
 * @returns {(at: number) => AsyncEffect<void>}
 */
export const alarmSet = (ctx) => (at) => () => ctx.storage.setAlarm(at);

/**
 * @param {DurableObjectState} ctx
 * @returns {AsyncEffect<Nullable<number>>}
 */
export const alarmGet = (ctx) => () =>
  ctx.storage.getAlarm().then((at) => (at === null || at === undefined ? null : at));

/**
 * @param {DurableObjectState} ctx
 * @returns {AsyncEffect<void>}
 */
export const alarmDelete = (ctx) => () => ctx.storage.deleteAlarm();

/**
 * SQLite binds null, numbers, strings and blobs. Booleans become 0/1 and
 * anything structured becomes its JSON text.
 *
 * @param {unknown} value
 * @returns {SqlStorageValue}
 */
const bindable = (value) =>
  typeof value === "boolean"
    ? value ? 1 : 0
    : value !== null && typeof value === "object"
      ? JSON.stringify(value)
      : /** @type {SqlStorageValue} */ (value);

/**
 * @param {DurableObjectState} ctx
 * @returns {(query: string) => (bindings: unknown[]) => Effect<Record<string, SqlStorageValue>[]>}
 */
export const sqlExec = (ctx) => (query) => (bindings) => () =>
  ctx.storage.sql.exec(query, ...bindings.map(bindable)).toArray();

/**
 * The string bindings among `names`; the rest are left out.
 *
 * @param {Bindings} env
 * @returns {(names: string[]) => Effect<Record<string, string>>}
 */
export const variables = (env) => (names) => () => {
  /** @type {Record<string, string>} */
  const found = {};
  for (const name of names) {
    const value = env[name];
    if (typeof value === "string") found[name] = value;
  }
  return found;
};

/**
 * @param {DurableObjectNamespace} ns
 * @param {ObjectId} id
 */
const stub = (ns, id) =>
  ns.get(id.kind === "named" ? ns.idFromName(id.value) : ns.idFromString(id.value));

/**
 * Call an RPC method on an object. The stub's methods are only known by name.
 *
 * @param {DurableObjectNamespace} ns
 * @returns {(id: ObjectId) => (method: string) => (request: unknown) => AsyncEffect<unknown>}
 */
export const call = (ns) => (id) => (method) => (request) => () => {
  const methods = /** @type {Record<string, (request: unknown) => Promise<unknown>>} */ (
    /** @type {unknown} */ (stub(ns, id))
  );
  return methods[method](request);
};

/**
 * @param {DurableObjectNamespace} ns
 * @returns {Effect<string>}
 */
export const unique = (ns) => () => ns.newUniqueId().toString();

/**
 * @param {DurableObjectNamespace} ns
 * @returns {(id: ObjectId) => (request: Request) => AsyncEffect<Response>}
 */
export const fetchObject = (ns) => (id) => (request) => () => stub(ns, id).fetch(request);

/**
 * @param {DurableObjectState} ctx
 * @returns {(json: unknown) => Effect<void>}
 */
export const socketsBroadcast = (ctx) => (json) => () => {
  const text = JSON.stringify(json);
  for (const ws of ctx.getWebSockets()) {
    try { ws.send(text); } catch {}
  }
};

/**
 * @param {DurableObjectState} ctx
 * @returns {(id: string) => (json: unknown) => Effect<void>}
 */
export const socketsSend = (ctx) => (id) => (json) => () => {
  const text = JSON.stringify(json);
  for (const ws of ctx.getWebSockets(id)) ws.send(text);
};

/**
 * The attachments of every open socket; see `Bridge.js` for their shape.
 *
 * @param {DurableObjectState} ctx
 * @returns {Effect<unknown[]>}
 */
export const socketsConnected = (ctx) => () =>
  ctx.getWebSockets().map((ws) => ws.deserializeAttachment());

/**
 * `ctx.container` is undefined unless wrangler config lists this class under
 * "containers"; every call then fails with a clear message.
 *
 * @param {DurableObjectState} ctx
 * @returns {Container}
 */
const box = (ctx) => {
  if (!ctx.container) throw new Error("this object has no container; declare an image with Durable.container");
  return ctx.container;
};

/**
 * @param {DurableObjectState} ctx
 * @returns {Effect<boolean>}
 */
export const containerRunning = (ctx) => () => box(ctx).running;

/**
 * @typedef {{ enableInternet: boolean, env: Record<string, string>, entrypoint: Nullable<string[]> }} StartOptions
 */

/**
 * @param {DurableObjectState} ctx
 * @returns {(options: StartOptions) => Effect<void>}
 */
export const containerStart = (ctx) => (options) => () => {
  /** @type {ContainerStartupOptions} */
  const config = { enableInternet: options.enableInternet };
  if (Object.keys(options.env).length > 0) config.env = options.env;
  if (options.entrypoint !== null) config.entrypoint = options.entrypoint;
  box(ctx).start(config);
};

/**
 * True for the errors a container gives before its port is up.
 *
 * @param {unknown} e
 */
const notListening = (e) => /not listening|connection refused|ECONNREFUSED|no such instance|container port/i.test(describe(e));

/**
 * @param {unknown} e
 * @returns {string}
 */
const describe = (e) => String(e !== null && typeof e === "object" && "message" in e ? e.message : e);

/**
 * True once something answers on `port`. Any failure other than "not listening" counts as up.
 *
 * @param {DurableObjectState} ctx
 * @returns {(port: number) => AsyncEffect<boolean>}
 */
export const containerProbe = (ctx) => (port) => () =>
  box(ctx)
    .getTcpPort(port)
    .fetch("http://container/", { signal: AbortSignal.timeout(2000) })
    .then(() => true, (e) => !notListening(e));

/**
 * @param {DurableObjectState} ctx
 * @returns {(port: number) => (request: Request) => AsyncEffect<Response>}
 */
export const containerRequest = (ctx) => (port) => (request) => () =>
  box(ctx).getTcpPort(port).fetch(request);

/**
 * @param {DurableObjectState} ctx
 * @returns {(code: number) => Effect<void>}
 */
export const containerSignal = (ctx) => (code) => () => box(ctx).signal(code);

/**
 * @param {DurableObjectState} ctx
 * @returns {AsyncEffect<void>}
 */
export const containerDestroy = (ctx) => () => box(ctx).destroy();

/**
 * @typedef {{ code: Nullable<number>, lost: Nullable<string> }} Exit
 *   How the container ended: an exit code, or why we lost track of it.
 */

/**
 * @param {DurableObjectState} ctx
 * @returns {AsyncEffect<Exit>}
 */
export const containerExit = (ctx) => () =>
  box(ctx).monitor().then(
    () => ({ code: 0, lost: null }),
    (e) => (typeof e === "number" ? { code: e, lost: null } : { code: null, lost: describe(e) })
  );
