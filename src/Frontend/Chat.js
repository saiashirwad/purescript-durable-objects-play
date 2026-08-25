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

export const interval = (ms) => (push) => () => {
  const id = setInterval(() => push()(), ms);
  return () => clearInterval(id);
};

export const notificationPermission = () =>
  typeof Notification === "undefined" ? "unsupported" : Notification.permission;

export const requestNotifications = () =>
  typeof Notification === "undefined"
    ? Promise.resolve("unsupported")
    : Notification.requestPermission();

// True when the user is not looking at this tab.
export const away = () => document.hidden || !document.hasFocus();

export const notify = ({ title, body, tag }) => () => {
  if (typeof Notification === "undefined" || Notification.permission !== "granted") return;
  const n = new Notification(title, { body, tag, renotify: true });
  n.onclick = () => { window.focus(); n.close(); };
};

export const setTitle = (title) => () => { document.title = title; };
