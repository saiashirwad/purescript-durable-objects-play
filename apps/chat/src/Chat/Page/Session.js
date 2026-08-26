export const notificationPermission = () =>
  typeof Notification === "undefined" ? "unsupported" : Notification.permission;

export const requestNotifications = () =>
  typeof Notification === "undefined"
    ? Promise.resolve("unsupported")
    : Notification.requestPermission();

export const sessionStatus = () => fetch("/session").then((r) => r.status);

export const login = (passkey) => () =>
  fetch("/login", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ passkey }),
  }).then((r) => r.status);
