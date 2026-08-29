import { expect, test, type Page } from "@playwright/test";
import { expectNoAxeViolations } from "./accessibility";

const passkey = "playwright-passkey";
const png = Buffer.from(
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=",
  "base64",
);

async function authenticate(page: Page, url = "/") {
  await page.goto(url);
  await expect(page.getByLabel("Passkey")).toBeVisible();
  await page.getByLabel("Passkey").fill(passkey);
  await page.getByRole("button", { name: "Unlock" }).click();
}

async function joinNewRoom(page: Page, name: string) {
  await authenticate(page);
  await expect(page.getByRole("button", { name: "Create a room" })).toBeVisible();
  await page.getByRole("button", { name: "Create a room" }).click();
  await joinNamed(page, name);
}

async function joinExistingRoom(page: Page, url: string, name: string) {
  await authenticate(page, url);
  await joinNamed(page, name);
}

async function joinNamed(page: Page, name: string) {
  await expect(page.getByLabel("Your name")).toBeVisible();
  await page.getByLabel("Your name").fill(name);
  await page.getByRole("button", { name: "Join" }).click();
  await expect(page.getByRole("heading", { name: "Room" })).toBeVisible();
}

async function send(page: Page, text: string) {
  await page.getByRole("textbox", { name: "Message", exact: true }).fill(text);
  await page.getByRole("button", { name: "Send" }).click();
  await expect(page.getByRole("list", { name: "Messages" }).getByText(text, { exact: true })).toBeVisible();
}

test("workerd persists SQL messages and serves attached images safely", async ({ page }) => {
  await joinNewRoom(page, "Runtime");
  await send(page, "stored by workerd");

  await page.reload();
  await expect(page.getByRole("heading", { name: "Room" })).toBeVisible();
  await expect(page.getByRole("list", { name: "Messages" }).getByText("stored by workerd", { exact: true })).toBeVisible();

  await page.locator('input[type="file"]').setInputFiles({ name: "pixel.png", mimeType: "image/png", buffer: png });
  await expect(page.getByAltText("Image attachment preview")).toBeVisible();
  await page.getByRole("button", { name: "Send" }).click();

  const image = page.getByAltText("Runtime attached an image");
  await expect(image).toBeVisible();
  const source = await image.getAttribute("src");
  expect(source).not.toBeNull();
  const served = await page.evaluate(async (url) => {
    const response = await fetch(url);
    return {
      status: response.status,
      contentType: response.headers.get("content-type"),
      nosniff: response.headers.get("x-content-type-options"),
      size: (await response.arrayBuffer()).byteLength,
    };
  }, source!);
  expect(served).toEqual({ status: 200, contentType: "image/png", nosniff: "nosniff", size: png.length });
});

test("two browser contexts exchange messages and absolute presence", async ({ browser, page }) => {
  await joinNewRoom(page, "Alice");
  const roomUrl = page.url();

  const bobContext = await browser.newContext();
  const bob = await bobContext.newPage();
  try {
    await joinExistingRoom(bob, roomUrl, "Bob");
    await expect(page.getByText("2 online", { exact: true })).toBeVisible();
    await expect(bob.getByText("2 online", { exact: true })).toBeVisible();

    await send(page, "hello Bob");
    await expect(bob.getByRole("list", { name: "Messages" }).getByText("hello Bob", { exact: true })).toBeVisible();

    await send(bob, "hello Alice");
    await expect(page.getByRole("list", { name: "Messages" }).getByText("hello Alice", { exact: true })).toBeVisible();
    await bob.getByRole("button", { name: "Leave" }).click();
    await expect(page.getByText("just you", { exact: true })).toBeVisible();
  } finally {
    await bobContext.close();
  }
});

test("a live chat room has no automatic WCAG violations", async ({ page }) => {
  await joinNewRoom(page, "Accessible");
  await send(page, "A readable message");
  const message = page.locator("[data-chat=message]").last();
  await message.evaluate(async element => {
    const animations: Array<{ finished: Promise<unknown> }> = element.getAnimations();
    await Promise.all(animations.map(animation => animation.finished));
  });
  await expectNoAxeViolations(page);
});
