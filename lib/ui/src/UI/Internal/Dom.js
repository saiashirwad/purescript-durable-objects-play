const previousFocus = new WeakMap();

export const showModal = (element) => () => {
  if (!(element instanceof HTMLDialogElement) || element.open) return;
  previousFocus.set(element, document.activeElement);
  element.showModal();
};

export const closeDialog = (element) => () => {
  if (!(element instanceof HTMLDialogElement)) return;
  if (element.open) element.close();
  const previous = previousFocus.get(element);
  if (previous instanceof HTMLElement && previous.isConnected) previous.focus();
  previousFocus.delete(element);
};

export const isBackdropClick = (event) =>
  event.target === event.currentTarget;

export const showPopover = (element) => () => {
  if (typeof element.showPopover === "function" && !element.matches(":popover-open")) {
    element.showPopover();
  }
};

export const hidePopover = (element) => () => {
  if (typeof element.hidePopover === "function" && element.matches(":popover-open")) {
    element.hidePopover();
  }
};

export const popoverOpen = (element) => () =>
  typeof element.matches === "function" && element.matches(":popover-open");

export const focusElement = (element) => () => element.focus();
