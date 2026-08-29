// @ts-check

/**
 * Decode enough of a base64 image to inspect its file signature.
 *
 * @param {string} base64
 * @returns {Uint8Array}
 */
const prefixBytes = (base64) => {
  try {
    return Uint8Array.from(atob(base64.slice(0, 48)), (character) => character.charCodeAt(0));
  } catch {
    return new Uint8Array();
  }
};

/**
 * Test bytes at one offset without decoding the complete upload.
 *
 * @param {Uint8Array} bytes
 * @param {number} offset
 * @param {number[]} signature
 * @returns {boolean}
 */
const hasBytes = (bytes, offset, signature) =>
  signature.every((byte, index) => bytes[offset + index] === byte);

/**
 * Check that uploaded bytes have the signature required by their allowed MIME.
 *
 * @param {string} mime
 * @returns {(base64: string) => boolean}
 */
export const matchesImageMimeImpl = (mime) => (base64) => {
  const bytes = prefixBytes(base64);
  switch (mime) {
    case "image/jpeg":
      return hasBytes(bytes, 0, [0xff, 0xd8, 0xff]);
    case "image/png":
      return hasBytes(bytes, 0, [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]);
    case "image/webp":
      return hasBytes(bytes, 0, [0x52, 0x49, 0x46, 0x46]) && hasBytes(bytes, 8, [0x57, 0x45, 0x42, 0x50]);
    case "image/gif":
      return hasBytes(bytes, 0, [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]) || hasBytes(bytes, 0, [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]);
    case "image/avif":
      return hasBytes(bytes, 4, [0x66, 0x74, 0x79, 0x70]) &&
        (hasBytes(bytes, 8, [0x61, 0x76, 0x69, 0x66]) || hasBytes(bytes, 8, [0x61, 0x76, 0x69, 0x73]));
    default:
      return false;
  }
};
