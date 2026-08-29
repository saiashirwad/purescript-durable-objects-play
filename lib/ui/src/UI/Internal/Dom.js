// @ts-check
// The Popover API, until web-html covers it. Everything else is in Dom.purs.

/**
 * @param {HTMLElement} element
 * @returns {Effect<void>}
 */
export const showPopoverImpl = (element) => () => {
  if (typeof element.showPopover === "function") element.showPopover();
};

/**
 * @param {HTMLElement} element
 * @returns {Effect<void>}
 */
export const hidePopoverImpl = (element) => () => {
  if (typeof element.hidePopover === "function") element.hidePopover();
};
