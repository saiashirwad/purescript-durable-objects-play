import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./ui/e2e",
  fullyParallel: true,
  reporter: "line",
  use: {
    baseURL: "http://127.0.0.1:8787",
    channel: "chrome",
    headless: true,
  },
  webServer: {
    command: "npx wrangler dev --port 8787",
    url: "http://127.0.0.1:8787/ui.html",
    reuseExistingServer: true,
    timeout: 30_000,
  },
});

