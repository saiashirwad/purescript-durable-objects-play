import AxeBuilder from "@axe-core/playwright";
import { expect, test, type Page } from "@playwright/test";

const wcag = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa", "wcag22aa"];

async function expectNoAxeViolations(page: Page) {
  const result = await new AxeBuilder({ page }).withTags(wcag).analyze();
  expect(result.violations).toEqual([]);
}

test("the lab has names, roles, and no automatic WCAG violations", async ({ page }) => {
  await page.goto("/ui.html");

  await expect(page.getByRole("heading", { name: "durable-ui lab" })).toBeVisible();
  await expect(page.getByLabel("Name", { exact: true })).toBeVisible();
  await expect(page.getByRole("switch", { name: "Compact mode" })).toBeVisible();
  await expectNoAxeViolations(page);
});

test("dialog contains focus and closes with Escape", async ({ page }) => {
  await page.goto("/ui.html");
  const trigger = page.getByRole("button", { name: "Open dialog" });

  await trigger.click();
  const dialog = page.getByRole("dialog", { name: "Confirm the change" });
  await expect(dialog).toBeVisible();
  await expect(dialog.getByRole("button", { name: "Close dialog" })).toBeFocused();
  await expectNoAxeViolations(page);

  await page.keyboard.press("Escape");
  await expect(dialog).toBeHidden();
  await expect(trigger).toBeFocused();
});

test("menu and tabs implement arrow-key navigation", async ({ page }) => {
  await page.goto("/ui.html");

  await page.getByRole("button", { name: "Actions" }).click();
  const duplicate = page.getByRole("menuitem", { name: "Duplicate" });
  const archive = page.getByRole("menuitem", { name: "Archive" });
  await expect(duplicate).toBeFocused();
  await duplicate.press("ArrowDown");
  await expect(archive).toBeFocused();

  await page.keyboard.press("Escape");
  const profile = page.getByRole("tab", { name: "Profile" });
  const security = page.getByRole("tab", { name: "Security" });
  await profile.focus();
  await profile.press("ArrowRight");
  await expect(security).toBeFocused();
  await expect(security).toHaveAttribute("aria-selected", "true");
  await expect(page.getByRole("tabpanel")).toContainText("Security settings");
});

test("migrated application forms have connected labels", async ({ page }) => {
  await page.goto("/");
  await expect(page.getByLabel("Passkey")).toBeVisible();
  await expectNoAxeViolations(page);

  await page.goto("/counter.html");
  await expect(page.getByLabel("Object name")).toHaveValue("user-123");
  await expectNoAxeViolations(page);
});
