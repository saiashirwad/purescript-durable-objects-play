import { defineConfig } from "@playwright/test";

const externalBaseURL = process.env.PLAYWRIGHT_BASE_URL;

export default defineConfig({
  testDir: "./e2e",
  fullyParallel: true,
  reporter: "line",
  use: {
    baseURL: externalBaseURL ?? "http://127.0.0.1:8787",
    headless: true,
  },
  snapshotPathTemplate: "{testDir}/screenshots/{platform}/{testFileName}/{arg}{ext}",
  webServer: externalBaseURL
    ? undefined
    : {
        command: "bun run dev -- --port 8787 --var PASSKEY:playwright-passkey",
        url: "http://127.0.0.1:8787/ui.html",
        reuseExistingServer: true,
        timeout: 30_000,
      },
});
