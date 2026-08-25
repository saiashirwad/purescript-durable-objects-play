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

export const headerImpl = (request) => (name) => request.headers.get(name);

export const url = (request) => request.url;
export const method = (request) => request.method;
export const pathname = (request) => new URL(request.url).pathname;

export const text = (status) => (body) =>
  new Response(body, {
    status,
    headers: { "content-type": "text/plain; charset=utf-8" },
  });

export const json = (status) => (body) => Response.json(body, { status });
