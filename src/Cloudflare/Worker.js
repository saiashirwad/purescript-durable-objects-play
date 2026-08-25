export const toExportImpl = (build) => ({
  fetch(request, env) {
    return build(env)().fetch(request)();
  },
});

export const variableImpl = (env) => (name) => () => {
  const value = env[name];
  if (typeof value !== "string") throw new Error(`Variable ${name} is not bound`);
  return value;
};

export const bindingImpl = (env) => (name) => () => {
  const value = env[name];
  if (value === undefined) throw new Error(`No binding named ${name}`);
  return value;
};

export const bodyImpl = (request) => () => request.json();

export const requestTo = (url) => new Request(url);

export const status = (response) => response.status;

export const responseTextImpl = (response) => () => response.text();

export const headerImpl = (request) => (name) => request.headers.get(name);

export const cookieImpl = (request) => (name) => {
  const jar = request.headers.get("cookie") ?? "";
  for (const part of jar.split(";")) {
    const [key, ...rest] = part.trim().split("=");
    if (key === name) return decodeURIComponent(rest.join("="));
  }
  return null;
};

export const textWith = (status) => (headers) => (body) => {
  const h = new Headers({ "content-type": "text/plain; charset=utf-8" });
  for (const { name, value } of headers) h.append(name, value);
  return new Response(body, { status, headers: h });
};

export const sha256Impl = (text) => () =>
  crypto.subtle.digest("SHA-256", new TextEncoder().encode(text)).then((buffer) =>
    Array.from(new Uint8Array(buffer), (b) => b.toString(16).padStart(2, "0")).join("")
  );

// The same request at another path, so an object sees `/x` not `/rpc/Class/id/http/x`.
export const rebase = (path) => (request) => {
  const url = new URL(request.url);
  url.pathname = path;
  return new Request(url, request);
};

export const url = (request) => request.url;
export const method = (request) => request.method;
export const pathname = (request) => new URL(request.url).pathname;

export const text = (status) => (body) =>
  new Response(body, {
    status,
    headers: { "content-type": "text/plain; charset=utf-8" },
  });

export const json = (status) => (body) => Response.json(body, { status });

// Base64 without Buffer, so it runs in workerd and browsers alike.
const toBase64 = (buffer) => {
  const bytes = new Uint8Array(buffer);
  let binary = "";
  for (let i = 0; i < bytes.length; i += 0x8000) {
    binary += String.fromCharCode.apply(null, bytes.subarray(i, i + 0x8000));
  }
  return btoa(binary);
};

const fromBase64 = (text) => Uint8Array.from(atob(text), (c) => c.charCodeAt(0));

export const bodyBase64Impl = (request) => () => request.arrayBuffer().then(toBase64);

export const bytes = (status) => (mime) => (base64) =>
  new Response(fromBase64(base64), {
    status,
    headers: { "content-type": mime, "cache-control": "private, max-age=31536000, immutable" },
  });

export const requestWith = ({ url, method, contentType, base64 }) =>
  new Request(url, { method, headers: { "content-type": contentType }, body: fromBase64(base64) });
