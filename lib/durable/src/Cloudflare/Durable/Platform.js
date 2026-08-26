
// No imports, so this file loads under Node.
export const storageGet = (ctx) => (key) => () =>
  ctx.storage.get(key).then((value) => (value === undefined ? null : value));

export const storagePut = (ctx) => (key) => (value) => () =>
  ctx.storage.put(key, value);

export const storageDelete = (ctx) => (key) => () => ctx.storage.delete(key);

export const storageList = (ctx) => (options) => () => {
  const query = { prefix: options.prefix, reverse: options.reverse };
  if (options.limit !== null) query.limit = options.limit;
  return ctx.storage
    .list(query)
    .then((entries) => Array.from(entries, ([key, value]) => ({ key, value })));
};

export const storageDeleteAll = (ctx) => () => ctx.storage.deleteAll();

export const now = () => Date.now();

export const alarmSet = (ctx) => (at) => () => ctx.storage.setAlarm(at);

export const alarmGet = (ctx) => () =>
  ctx.storage.getAlarm().then((at) => (at === undefined ? null : at));

export const alarmDelete = (ctx) => () => ctx.storage.deleteAlarm();

// SQLite binds null, numbers, strings and blobs. Booleans become 0/1 and
// anything structured becomes its JSON text.
const bindable = (value) =>
  typeof value === "boolean"
    ? value ? 1 : 0
    : value !== null && typeof value === "object"
      ? JSON.stringify(value)
      : value;

export const sqlExec = (ctx) => (query) => (bindings) => () =>
  ctx.storage.sql.exec(query, ...bindings.map(bindable)).toArray();

export const variables = (env) => (names) => () => {
  const found = {};
  for (const name of names) {
    if (typeof env[name] === "string") found[name] = env[name];
  }
  return found;
};

export const call = (ns) => (id) => (method) => (request) => () => {
  const objectId =
    id.kind === "named" ? ns.idFromName(id.value) : ns.idFromString(id.value);
  return ns.get(objectId)[method](request);
};

export const unique = (ns) => () => ns.newUniqueId().toString();

export const fetchObject = (ns) => (id) => (request) => () => {
  const objectId =
    id.kind === "named" ? ns.idFromName(id.value) : ns.idFromString(id.value);
  return ns.get(objectId).fetch(request);
};

export const socketsBroadcast = (ctx) => (json) => () => {
  const text = JSON.stringify(json);
  for (const ws of ctx.getWebSockets()) {
    try { ws.send(text); } catch {}
  }
};

export const socketsSend = (ctx) => (id) => (json) => () => {
  const text = JSON.stringify(json);
  for (const ws of ctx.getWebSockets(id)) ws.send(text);
};

export const socketsConnected = (ctx) => () =>
  ctx.getWebSockets().map((ws) => ws.deserializeAttachment());

// ctx.container is undefined unless wrangler config lists this class under
// "containers"; every call then fails with a clear message.
const box = (ctx) => {
  if (!ctx.container) throw new Error("this object has no container; declare an image with Durable.container");
  return ctx.container;
};

export const containerRunning = (ctx) => () => box(ctx).running;

export const containerStart = (ctx) => (options) => () => {
  const config = { enableInternet: options.enableInternet };
  if (Object.keys(options.env).length > 0) config.env = options.env;
  if (options.entrypoint !== null) config.entrypoint = options.entrypoint;
  box(ctx).start(config);
};

const notListening = (e) => /not listening|connection refused|ECONNREFUSED|no such instance|container port/i.test(String(e?.message ?? e));

export const containerProbe = (ctx) => (port) => () =>
  box(ctx)
    .getTcpPort(port)
    .fetch("http://container/", { signal: AbortSignal.timeout(2000) })
    .then(() => true, (e) => !notListening(e));

export const containerRequest = (ctx) => (port) => (request) => () =>
  box(ctx).getTcpPort(port).fetch(request);

export const containerSignal = (ctx) => (code) => () => box(ctx).signal(code);

export const containerDestroy = (ctx) => () => box(ctx).destroy();

export const containerExit = (ctx) => () =>
  box(ctx).monitor().then(
    () => ({ code: 0, lost: null }),
    (e) => (typeof e === "number" ? { code: e, lost: null } : { code: null, lost: String(e?.message ?? e) })
  );
