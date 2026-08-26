# durable-ui

`durable-ui` is the local user-interface package for this repository. It is
kept in one folder so that it can move to its own repository or package later.

The package has three public layers:

- `UI.Style` and `UI.Theme` provide deterministic style composition and scoped
  theme variables.
- Small view modules such as `UI.Button` and `UI.Field` provide styled native
  HTML with required accessibility information.
- Stateful modules such as `UI.Dialog`, `UI.Menu`, and `UI.Tabs` own keyboard,
  focus, and ARIA behavior.

## Included modules

- Style and theme: `UI.Style`, `UI.Theme`, `UI.Styles`
- Controls: `UI.Button`, `UI.Input`, `UI.Field`, `UI.Checkbox`,
  `UI.RadioGroup`, `UI.Select`, `UI.Slider`
- Visual primitives: `UI.Avatar`, `UI.Icon`
- Content: `UI.Surface`, `UI.Status`, `UI.Disclosure`, `UI.Feedback`
- Stateful patterns: `UI.Dialog`, `UI.Popover`, `UI.Menu`, `UI.Tabs`
- Small overlays: `UI.Tooltip`

`UI.Select` uses the native select element. A custom combobox is not part of
this first package version. It needs its own focus, text search, virtual list,
mobile, and screen-reader test matrix before it can use the same quality claim.

Import the generated `public/ui.css` file once. Apply `Theme.auto` or another
theme to an ancestor with `Theme.scope theme style`. Do not depend on
generated class names.

## Writing styles

A `Style` is a list of declarations and a `Monoid`; the last declaration for
one property under one condition wins, so `recipe <> caller` lets the caller
override.

```purescript
import Data.Monoid (guard)
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)

chip :: Style
chip = create
  [ "border-radius" := "999px"
  , "color" := var tokens.textMuted
  , on Style.Hover [ "color" := var tokens.text ]
  ]

view = HH.span [ css $ chip <> guard active accent ] [ HH.text "Draft" ]
```

Every module exports its styles as `sheet`; a module whose CSS needs a
selector atoms cannot express (a sibling, a pseudo-element) also exports
`raw`. `UI.Styles.css` joins them all.

## Commands

```sh
spago build -p ui
spago test -p ui
spago bundle -p ui
node apps/site/scripts/css.mjs
```

Open `/ui.html` during local development to inspect the library.

Run `npm run test:e2e` for Chrome keyboard and axe checks. The stable release
gate also requires the manual screen-reader checks in `DESIGN.md`.
