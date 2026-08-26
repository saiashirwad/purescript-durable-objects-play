# Design contract

## Quality target

The library supports pages that conform to WCAG 2.2 AA. Each stateful module
follows its applicable WAI-ARIA Authoring Practices pattern. Page authors still
own page structure, text alternatives, contrast after theme changes, and
correct use of each module.

## Interface rules

1. Use native HTML behavior when it is sufficient.
2. Require an accessible name when a control has no visible text.
3. Keep state, keyboard logic, focus logic, and ARIA state in one module.
4. Expose stable state attributes. Do not expose implementation class names.
5. Use one state owner. Stateful modules own their state and emit outputs.
6. Compose caller styles after recipe styles. The final declaration for one
   property and condition wins.
7. Use semantic theme variables. Do not put brand colors in view modules.
8. Use logical CSS properties for direction-sensitive layout.

## Style limits

`UI.Style` generates atomic CSS. It resolves equal CSS properties and equal
conditions before it emits classes. Shorthand properties are not part of the
stable interface. Use longhand or logical properties so that composition has
one clear result.

Dynamic values use CSS custom properties. Static declarations go into the one
generated CSS file.

A module keeps its own CSS. When a rule needs a selector the atoms cannot say
(`input:checked+[aria-hidden]`, `::backdrop`, a parent's `:hover`), the module
exports it as `raw`, keyed on its `data-ui` attributes, and `UI.Styles`
appends it after the atoms.

## Release gates

- Pure transition tests for each stateful module.
- Keyboard and focus tests in Chromium, Firefox, and WebKit.
- Automated axe checks in each open and closed state.
- Manual VoiceOver and NVDA checks for each stable stateful module.
- Visual checks for light, dark, high contrast, RTL, reduced motion, 200%
  zoom, and 400% zoom.

