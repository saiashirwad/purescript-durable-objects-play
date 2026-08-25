// Works in the browser and in workerd: both have `fetch`.
export const postJson = (url) => (body) => () =>
  fetch(url, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  }).then((response) => {
    if (!response.ok) throw new Error(`HTTP ${response.status} from ${url}`);
    return response.json();
  });
