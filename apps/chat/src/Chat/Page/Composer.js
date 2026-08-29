// @ts-check
// Image attachments: pick or paste, shrink, upload. Canvas and the file
// dialog have no PureScript binding, so this stays JS.

/**
 * Shrink to at most 1600px on the long side, as JPEG unless it has alpha.
 * Falls back to the original file whenever the browser cannot decode or encode it.
 *
 * @param {File} file
 * @returns {Promise<Blob>}
 */
const shrink = (file) =>
  new Promise((resolve) => {
    const img = new Image();
    const url = URL.createObjectURL(file);
    img.onload = () => {
      URL.revokeObjectURL(url);
      const scale = Math.min(1, 1600 / Math.max(img.width, img.height));
      if (scale === 1 && file.size < 900_000) return resolve(file);
      const canvas = document.createElement("canvas");
      canvas.width = Math.round(img.width * scale);
      canvas.height = Math.round(img.height * scale);
      const context = canvas.getContext("2d");
      if (!context) return resolve(file);
      context.drawImage(img, 0, 0, canvas.width, canvas.height);
      const type = file.type === "image/png" ? "image/png" : "image/jpeg";
      canvas.toBlob((blob) => resolve(blob ?? file), type, 0.86);
    };
    img.onerror = () => { URL.revokeObjectURL(url); resolve(file); };
    img.src = url;
  });

/**
 * Upload each image in turn; the ids the server assigns, in order.
 *
 * @param {string} endpoint
 * @param {File[]} files
 * @returns {Promise<number[]>}
 */
const upload = async (endpoint, files) => {
  /** @type {number[]} */
  const ids = [];
  for (const file of files) {
    if (!file.type.startsWith("image/")) continue;
    const blob = await shrink(file);
    const response = await fetch(endpoint, { method: "POST", headers: { "content-type": blob.type }, body: blob });
    if (!response.ok) throw new Error(`upload failed: HTTP ${response.status}`);
    const { id } = /** @type {{ id: number }} */ (await response.json());
    ids.push(id);
  }
  return ids;
};

/**
 * Open the file dialog; resolve with the uploaded image ids.
 *
 * @param {string} endpoint
 * @returns {AsyncEffect<number[]>}
 */
export const pickAndUpload = (endpoint) => () =>
  new Promise((resolve, reject) => {
    const input = document.createElement("input");
    input.type = "file";
    input.accept = "image/*";
    input.multiple = true;
    input.onchange = () => upload(endpoint, Array.from(input.files ?? [])).then(resolve, reject);
    input.oncancel = () => resolve([]);
    input.click();
  });

/**
 * Images on the clipboard of a paste event, uploaded. Empty when it was text.
 *
 * @param {string} endpoint
 * @returns {(event: ClipboardEvent) => AsyncEffect<number[]>}
 */
export const uploadPasted = (endpoint) => (event) => () => {
  const files = Array.from(event.clipboardData?.files ?? []).filter((file) => file.type.startsWith("image/"));
  if (files.length > 0) event.preventDefault();
  return upload(endpoint, files);
};
