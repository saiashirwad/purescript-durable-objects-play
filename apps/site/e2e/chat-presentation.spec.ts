import { expect, test, type Page } from "@playwright/test";
import { expectNoAxeViolations } from "./accessibility";

const fixtures = [
  { name: "session-loading", expected: "Loading…" },
  { name: "session-failed", expected: "Could not check the session." },
  { name: "session-locked", expected: "That passkey is not right." },
  { name: "session-lobby", expected: "Create a room" },
  { name: "session-joining", expected: "Who are you?" },
  { name: "room-empty", expected: "It's quiet in here" },
  { name: "room-conversation", expected: "Launch notes" },
  { name: "room-uploading", expected: "Uploading an image…" },
  { name: "room-sending", expected: "One more detail in the same thread." },
] as const;

async function openFixture(page: Page, name: string, expected: string) {
  await page.goto(`/chat-presentation.html#${name}`);
  await expect(page.locator(`[data-presentation="${name}"]`)).toBeVisible();
  await expect(page.getByText(expected, { exact: true }).first()).toBeVisible();
  const lastMessage = page.locator("[data-chat=message]").last();
  if (await lastMessage.count() > 0) {
    await lastMessage.evaluate(async element => {
      const animations: Array<{ finished: Promise<unknown> }> = element.getAnimations();
      await Promise.all(animations.map(animation => animation.finished));
    });
  }
  await page.evaluate("document.fonts.ready");
}

for (const fixture of fixtures) {
  test(`${fixture.name} stays accessible and matches its light reference`, async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 1200 });
    await page.emulateMedia({ colorScheme: "light" });
    await openFixture(page, fixture.name, fixture.expected);
    await expectNoAxeViolations(page);
    await expect(page).toHaveScreenshot(`${fixture.name}-light.png`, { animations: "disabled", fullPage: true });
  });
}

test("the conversation matches its dark reference", async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 1200 });
  await page.emulateMedia({ colorScheme: "dark" });
  await openFixture(page, "room-conversation", "Launch notes");
  await expectNoAxeViolations(page);
  await expect(page).toHaveScreenshot("room-conversation-dark.png", { animations: "disabled", fullPage: true });
});

test("the conversation matches its narrow reference", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.emulateMedia({ colorScheme: "light" });
  await openFixture(page, "room-conversation", "Launch notes");
  await expectNoAxeViolations(page);
  await expect(page).toHaveScreenshot("room-conversation-narrow.png", { animations: "disabled", fullPage: true });
});

test("the conversation matches its reduced-motion reference", async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 1200 });
  await page.emulateMedia({ colorScheme: "light", reducedMotion: "reduce" });
  await openFixture(page, "room-conversation", "Launch notes");
  await expectNoAxeViolations(page);
  await expect(page).toHaveScreenshot("room-conversation-reduced-motion.png", { animations: "disabled", fullPage: true });
});
