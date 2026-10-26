const { test, expect, link, blankData } = require('./fixtures')

// Phones get a read-only view so that shared links still look fine
test('a shared link is read-only on phones', async ({ page }) => {
  await page.goto(link({ ...blankData, events: [{ from: '2000-01-01', to: '2004-01-01', color: '#ff0000', label: 'school', overlay: false }] }))

  const chart = page.locator('.chart svg')
  await expect(chart).toBeVisible()
  const box = await chart.boundingBox()
  expect(box.width).toBeLessThanOrEqual(page.viewportSize().width)

  await expect(page.locator('.badge').filter({ hasText: 'weeks left' })).toBeVisible()
  await expect(page.locator('.d-md-none', { hasText: 'school' })).toBeVisible()

  // no editing controls
  await expect(page.locator('input:visible')).toHaveCount(0)
  await expect(page.getByRole('button', { name: 'Add event' })).toBeHidden()
  await expect(page.getByRole('button', { name: 'Export raw data' }).locator('visible=true')).toHaveCount(0)

  // nothing wider than the screen
  const scrollWidth = await page.evaluate(() => document.documentElement.scrollWidth)
  expect(scrollWidth).toBeLessThanOrEqual(page.viewportSize().width)
})
