// Thin wrappers over Cloudflare's DurableObjectState and DurableObjectNamespace.
// No imports: this file also loads under Node, where it is never called.

export const storageGet = (ctx) => (key) => () =>
  ctx.storage.get(key).then((value) => (value === undefined ? null : value));

export const storagePut = (ctx) => (key) => (value) => () =>
  ctx.storage.put(key, value);

export const storageDelete = (ctx) => (key) => () => ctx.storage.delete(key);

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
