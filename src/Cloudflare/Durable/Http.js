// Works in the browser and in workerd.
export const postJson = (url) => (body) => () =>
  fetch(url, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  }).then((response) => {
    if (!response.ok) throw new Error(`HTTP ${response.status} from ${url}`);
    return response.json();
  });

// Reconnects two seconds after an unexpected close; the returned closer stops that.
export const openSocket = (path) => (onOpen) => (onClose) => (onMessage) => (onGarbled) => () => {
  const scheme = location.protocol === "https:" ? "wss" : "ws";
  const url = path.startsWith("/") ? `${scheme}://${location.host}${path}` : path;
  let ws, timer, stopped = false;
  const connect = () => {
    ws = new WebSocket(url);
    ws.onopen = () => onOpen();
    ws.onmessage = (e) => {
      let json;
      try { json = JSON.parse(e.data); } catch (err) { onGarbled(String(err))(); return; }
      onMessage(json)();
    };
    ws.onclose = () => {
      onClose();
      if (!stopped) timer = setTimeout(connect, 2000);
    };
  };
  connect();
  return () => {
    stopped = true;
    clearTimeout(timer);
    ws.close();
  };
};
