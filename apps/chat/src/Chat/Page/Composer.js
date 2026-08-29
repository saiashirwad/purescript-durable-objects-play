// @ts-check
// Image attachments: shrink and upload. Canvas encoding and Blob request
// bodies have no complete PureScript binding, so this stays JS.

/**
 * Shrink to at most 1600px on the long side. Keep alpha-capable raster types.
 * Keep animated GIF and vector SVG files unchanged.
 * Falls back to the original file whenever the browser cannot decode or encode it.
 *
 * @param {File} file
 * @returns {Promise<Blob>}
 */
const shrink = (file) => {
  if (file.type === "image/gif" || file.type === "image/svg+xml") return Promise.resolve(file);
  return new Promise((resolve) => {
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
      const type = ["image/png", "image/webp", "image/avif"].includes(file.type) ? file.type : "image/jpeg";
      canvas.toBlob((blob) => resolve(blob ?? file), type, 0.86);
    };
    img.onerror = () => { URL.revokeObjectURL(url); resolve(file); };
    img.src = url;
  });
};

/**
 * Read an image id from an upload response.
 *
 * @param {unknown} value
 * @returns {number}
 */
const imageId = (value) => {
  if (typeof value !== "object" || value === null || !("id" in value)) {
    throw new Error("upload failed: response has no image id");
  }
  const id = value.id;
  if (typeof id !== "number" || !Number.isInteger(id) || id < 1 || id > 2_147_483_647) {
    throw new Error("upload failed: image id is not a positive 32-bit integer");
  }
  return id;
};

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
    // Response.json returns any, so keep the server value unknown until imageId checks it.
    const body = /** @type {unknown} */ (await response.json());
    ids.push(imageId(body));
  }
  return ids;
};

/**
 * Upload files that PureScript selected from the dialog or clipboard.
 *
 * @param {string} endpoint
 * @returns {(files: File[]) => AsyncEffect<number[]>}
 */
export const uploadFiles = (endpoint) => (files) => () => upload(endpoint, files);
