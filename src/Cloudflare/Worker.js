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
