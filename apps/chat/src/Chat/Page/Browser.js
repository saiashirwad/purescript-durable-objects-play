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

// True when the user is not looking at this tab.
export const away = () => document.hidden || !document.hasFocus();

export const notify = ({ title, body, tag }) => () => {
  if (typeof Notification === "undefined" || Notification.permission !== "granted") return;
  const n = new Notification(title, { body, tag, renotify: true });
  n.onclick = () => { window.focus(); n.close(); };
};

export const setTitle = (title) => () => { document.title = title; };

export const scrollToId = (id) => () => {
  const el = document.getElementById(id);
  if (!el) return;
  el.scrollIntoView({ behavior: "smooth", block: "center" });
  if (typeof el.animate === "function") {
    const ring = getComputedStyle(el).getPropertyValue("--ui-color-focus-ring").trim();
    el.animate(
      [ { outline: `3px solid ${ring}`, outlineOffset: "3px" }, { outline: "3px solid transparent", outlineOffset: "6px" } ],
      { duration: 900, easing: "ease-out" },
    );
  }
};
