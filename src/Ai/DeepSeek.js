// Works in Node 18+ and in Workers. Non-JSON bodies come back as a string.
export const postJson = (url) => (key) => (body) => () =>
  fetch(url, {
    method: "POST",
    headers: { "content-type": "application/json", authorization: `Bearer ${key}` },
    body: JSON.stringify(body),
  }).then(async (response) => {
    const text = await response.text();
    let json;
    try { json = JSON.parse(text); } catch { json = text; }
    return { status: response.status, body: json };
  });
