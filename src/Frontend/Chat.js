export const formatTime = (ms) =>
  new Date(ms).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
