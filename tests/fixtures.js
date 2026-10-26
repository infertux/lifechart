const base = require('@playwright/test')

// Every test fails on an uncaught exception or a console error, so that "the
// page still looks fine" also means "nothing blew up behind the scenes"
const test = base.test.extend({
  page: async ({ page }, use) => {
    const errors = []
    page.on('pageerror', (error) => errors.push(error.message))
    page.on('console', (message) => {
      if (message.type() === 'error') errors.push(message.text())
    })

    await use(page)

    base.expect(errors, 'console errors').toEqual([])
  },
})

const expect = base.expect

/** The chart data saved in the page address ("#" alone is the blank chart) */
async function chartData(page) {
  const hash = new URL(page.url()).hash.slice(1)
  return hash ? JSON.parse(Buffer.from(hash, 'base64').toString()) : blankData
}

/** Opens the blank chart and waits for it to be drawn */
async function openBlank(page) {
  await page.goto('/#')
  await expect(page.locator('.chart svg')).toBeVisible()
}

/** Encodes chart data the way the app does, for shared links */
function link(data) {
  return '/#' + Buffer.from(JSON.stringify(data)).toString('base64')
}

const blankData = {
  'birth-date': '1988-07-24',
  'life-expectancy': 80,
  'kid-until': 18,
  'old-from': 70,
  'only-adult-years': false,
  events: [],
}

module.exports = { test, expect, chartData, openBlank, link, blankData }
