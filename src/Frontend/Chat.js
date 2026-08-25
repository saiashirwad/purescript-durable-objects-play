export const formatTime = (ms) =>
  new Date(ms).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });

export const nearBottom = (el) => () =>
  el.scrollHeight - el.scrollTop - el.clientHeight < 96;

export const scrollToEnd = (el) => () => {
  el.scrollTop = el.scrollHeight;
};

export const copyText = (text) => () => {
  navigator.clipboard?.writeText(text);
};
