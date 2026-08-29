# Working in this repo

PureScript monorepo (spago workspace). Libraries in `lib/`, applications in `apps/`.
Bun installs packages and runs scripts: `bun install`, `bun run dev`, `bun run check`.
`spago build` / `spago test` run the PureScript; tests run under Node.

## JavaScript

Write PureScript. Reach for JS only when no library in the package set covers the API
(check the registry first). What is left is FFI glue, and every JS file must be typed:

- First line: `// @ts-check`. The file is then checked by `bun run check` (`tsc`, strict).
- Every export and every helper has a JSDoc block with `@param` and `@returns`
  (or `@type` for a constant), plus one line saying what it is for when the name
  does not say. Curried PureScript signatures are written as arrow types, e.g.
  `@returns {(key: string) => AsyncEffect<string>}`.
- Use the shared types in `types/global.d.ts`: `Effect<A>` for `Effect a`,
  `AsyncEffect<A>` for `Effect (Promise a)`, `Nullable<A>`, `Bindings`, `ObjectId`.
  Add to that file when a type is used by more than one FFI file.
- Do not reach for `any`. Cast through `unknown` when a platform type is too loose,
  and say why in a comment on that line.
- Keep the `.purs` `foreign import` and the JSDoc in step; the checker cannot see
  across that boundary, so it is on you.

The exception is a file that imports compiled PureScript from `output/`
(`apps/site/worker/index.js`, `apps/site/scripts/*.mjs`): tsc cannot load `output/`
without also checking spago's copies of the FFI files there, so those few files carry
a note instead of `// @ts-check`. They still get JSDoc.

Which project checks a file is decided by its globals, since the DOM, Workers and
Node type sets clash: `tsconfig.worker.json` (workers-types), `tsconfig.browser.json`
(DOM), `tsconfig.node.json` (Node). A new JS file goes into the one matching where
it runs. The root `tsconfig.json` only references those three; it is there so an
editor's TypeScript server can find which project owns a file.

## Style

Code comments are terse. Run `purs-tidy format-in-place` on changed PureScript.
