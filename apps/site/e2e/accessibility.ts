import AxeBuilder from "@axe-core/playwright";
import { expect, type Page } from "@playwright/test";

const wcag = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa", "wcag22aa"];

export async function expectNoAxeViolations(page: Page) {
  const result = await new AxeBuilder({ page }).withTags(wcag).analyze();
  expect(result.violations).toEqual([]);
}
