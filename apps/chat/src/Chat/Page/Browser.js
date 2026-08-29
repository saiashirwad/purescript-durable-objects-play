// @ts-check
// Browser APIs no PureScript library covers yet. Everything else is in Browser.purs.

/**
 * "granted", "denied", "default", or "unsupported".
 *
 * @type {Effect<string>}
 */
export const notificationPermission = () =>
  typeof Notification === "undefined" ? "unsupported" : Notification.permission;

/** @type {AsyncEffect<string>} */
export const requestNotifications = () =>
  typeof Notification === "undefined"
    ? Promise.resolve("unsupported")
    : Notification.requestPermission();

/**
 * Show a system notification if allowed; clicking it focuses this window.
 * One `tag` per room, so a new message replaces the old notification.
 *
 * @param {{ title: string, body: string, tag: string }} spec
 * @returns {Effect<void>}
 */
export const notify = ({ title, body, tag }) => () => {
  if (typeof Notification === "undefined" || Notification.permission !== "granted") return;
  // `renotify` is real in Chrome and Firefox but missing from lib.dom.
  const options = /** @type {NotificationOptions} */ ({ body, tag, renotify: true });
  const n = new Notification(title, options);
  n.onclick = () => { window.focus(); n.close(); };
};

/** @type {Effect<boolean>} */
export const hasFocus = () => document.hasFocus();

/**
 * Scroll an element into view and flash its outline.
 *
 * @param {Element} el
 * @returns {Effect<void>}
 */
export const reveal = (el) => () => {
  el.scrollIntoView({ behavior: "smooth", block: "center" });
  if (typeof el.animate === "function") {
    const ring = getComputedStyle(el).getPropertyValue("--ui-color-focus-ring").trim();
    el.animate(
      [ { outline: `3px solid ${ring}`, outlineOffset: "3px" }, { outline: "3px solid transparent", outlineOffset: "6px" } ],
      { duration: 900, easing: "ease-out" },
    );
  }
};
