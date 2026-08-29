// Types the FFI files share. Global, so no file has to import them.
// `bun run check` loads this into every tsconfig.*.json project.

/** A PureScript `Effect a`: a thunk run for its side effect. */
type Effect<A> = () => A;

/** A PureScript `Effect (Promise a)`; `Control.Promise.toAffE` unwraps it. */
type AsyncEffect<A> = Effect<Promise<A>>;

/** A PureScript `Nullable a`. */
type Nullable<A> = A | null;

/** Worker bindings as PureScript sees them: looked up by name, checked at runtime. */
type Bindings = Record<string, unknown>;

/** A Durable Object id as `Cloudflare.Durable.Core` encodes it. */
type ObjectId = { kind: "named" | "unique"; value: string };
