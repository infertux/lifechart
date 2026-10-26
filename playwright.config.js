// Regression tests against the built app in public/ (run `make` first)
const { defineConfig, devices } = require('@playwright/test')

module.exports = defineConfig({
  testDir: 'tests',
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  reporter: process.env.CI ? [['github'], ['list']] : 'list',
  use: {
    baseURL: 'http://127.0.0.1:8000/',
    trace: 'retain-on-failure',
  },
  projects: [
    { name: 'desktop', use: { ...devices['Desktop Chrome'] }, testIgnore: /phone/ },
    { name: 'phone', use: { ...devices['Pixel 7'] }, testMatch: /phone/ },
  ],
  // Browser.application doesn't work on file:// so serve public/ over HTTP
  webServer: {
    command: 'python3 -m http.server 8000 --bind 127.0.0.1 --directory public',
    url: 'http://127.0.0.1:8000/',
    reuseExistingServer: !process.env.CI,
  },
})
