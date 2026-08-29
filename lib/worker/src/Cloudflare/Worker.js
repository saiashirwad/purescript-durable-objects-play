// @ts-check

/**
 * @typedef {{ fetch: (request: Request) => AsyncEffect<Response> }} Handler
 *   A worker as PureScript builds it: one effectful `fetch`.
 * @typedef {{ name: string, value: string }} Header
 */

/**
 * The module export wrangler loads: build the handler once per request from its bindings.
 *
 * @param {(env: Bindings) => Effect<Handler>} build
 * @returns {ExportedHandler<Bindings>}
 */
export const toExportImpl = (build) => ({
  fetch(request, env) {
    return build(env)().fetch(request)();
  },
});

/**
 * A string binding, or an error naming the missing one.
 *
 * @param {Bindings} env
 * @returns {(name: string) => Effect<string>}
 */
export const variableImpl = (env) => (name) => () => {
  const value = env[name];
  if (typeof value !== "string") throw new Error(`Variable ${name} is not bound`);
  return value;
};

/**
 * Any binding, or an error naming the missing one.
 *
 * @param {Bindings} env
 * @returns {(name: string) => Effect<unknown>}
 */
export const bindingImpl = (env) => (name) => () => {
  const value = env[name];
  if (value === undefined) throw new Error(`No binding named ${name}`);
  return value;
};

/**
 * @param {Request} request
 * @returns {AsyncEffect<unknown>}
 */
export const bodyImpl = (request) => () => request.json();

/**
 * @param {string} url
 * @returns {Request}
 */
export const requestTo = (url) => new Request(url);

/**
 * @param {Response} response
 * @returns {number}
 */
export const status = (response) => response.status;

/**
 * @param {Response} response
 * @returns {AsyncEffect<string>}
 */
export const responseTextImpl = (response) => () => response.text();
/**
 * @param {Response} response
 * @returns {(name: string) => Nullable<string>}
 */
export const responseHeaderImpl = (response) => (name) => response.headers.get(name);


/**
 * @param {Request} request
 * @returns {(name: string) => Nullable<string>}
 */
export const headerImpl = (request) => (name) => request.headers.get(name);

/**
 * One cookie's value, decoded, from the request's cookie jar.
 *
 * @param {Request} request
 * @returns {(name: string) => Nullable<string>}
 */
export const cookieImpl = (request) => (name) => {
  const jar = request.headers.get("cookie") ?? "";
  for (const part of jar.split(";")) {
    const [key, ...rest] = part.trim().split("=");
    if (key === name) return decodeURIComponent(rest.join("="));
  }
  return null;
};

/**
 * A plain-text response with extra headers.
 *
 * @param {number} status
 * @returns {(headers: Header[]) => (body: string) => Response}
 */
export const textWith = (status) => (headers) => (body) => {
  const h = new Headers({ "content-type": "text/plain; charset=utf-8" });
  for (const { name, value } of headers) h.append(name, value);
  return new Response(body, { status, headers: h });
};

/**
 * Hex SHA-256 of a string.
 *
 * @param {string} text
 * @returns {AsyncEffect<string>}
 */
export const sha256Impl = (text) => () =>
  crypto.subtle.digest("SHA-256", new TextEncoder().encode(text)).then((buffer) =>
    Array.from(new Uint8Array(buffer), (b) => b.toString(16).padStart(2, "0")).join("")
  );

/**
 * The same request at another path, so an object sees `/x` not `/rpc/Class/id/http/x`.
 *
 * @param {string} path
 * @returns {(request: Request) => Request}
 */
export const rebase = (path) => (request) => {
  const url = new URL(request.url);
  url.pathname = path;
  return new Request(url, request);
};

/** @param {Request} request */
export const url = (request) => request.url;

/** @param {Request} request */
export const method = (request) => request.method;

/** @param {Request} request */
export const pathname = (request) => new URL(request.url).pathname;

/**
 * @param {number} status
 * @returns {(body: string) => Response}
 */
export const text = (status) => (body) =>
  new Response(body, {
    status,
    headers: { "content-type": "text/plain; charset=utf-8" },
  });

/**
 * @param {number} status
 * @returns {(body: unknown) => Response}
 */
export const json = (status) => (body) => Response.json(body, { status });

/**
 * Base64 without Buffer, so it runs in workerd and browsers alike.
 *
 * @param {ArrayBuffer} buffer
 * @returns {string}
 */
const toBase64 = (buffer) => {
  const bytes = new Uint8Array(buffer);
  let binary = "";
  for (let i = 0; i < bytes.length; i += 0x8000) {
    const chunk = /** @type {number[]} */ (/** @type {unknown} */ (bytes.subarray(i, i + 0x8000)));
    binary += String.fromCharCode.apply(null, chunk);
  }
  return btoa(binary);
};

/**
 * @param {string} text
 * @returns {Uint8Array}
 */
const fromBase64 = (text) => Uint8Array.from(atob(text), (c) => c.charCodeAt(0));

/**
 * @param {Request} request
 * @returns {AsyncEffect<string>}
 */
export const bodyBase64Impl = (request) => () => request.arrayBuffer().then(toBase64);

/**
 * An immutable binary response.
 *
 * @param {number} status
 * @returns {(mime: string) => (base64: string) => Response}
 */
export const bytes = (status) => (mime) => (base64) =>
  new Response(fromBase64(base64), {
    status,
    headers: {
      "content-type": mime,
      "cache-control": "private, max-age=31536000, immutable",
      "x-content-type-options": "nosniff",
    },
  });

/**
 * A request carrying binary data.
 *
 * @param {{ url: string, method: string, contentType: string, base64: string }} spec
 * @returns {Request}
 */
export const requestWith = ({ url, method, contentType, base64 }) =>
  new Request(url, { method, headers: { "content-type": contentType }, body: fromBase64(base64) });
