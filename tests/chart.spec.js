const { test, expect, chartData, openBlank, link, blankData } = require('./fixtures')

// the row whose label starts with name ("Adult Years" isn't "Non-Adult Years")
const field = (page, name) =>
  page
    .locator('.form-group')
    .filter({ has: page.locator('label.col-form-label', { hasText: new RegExp('^' + name) }) })
    .locator('input')
const badges = (page) => page.locator('.badge')

test('a bare URL shows the demo chart', async ({ page }) => {
  await page.goto('/')
  await expect(page.getByText('This is a demo chart.')).toBeVisible()
  await expect(page.getByRole('link', { name: 'demo chart' })).toHaveClass(/active/)
  await expect(page.locator('.chart svg')).toBeVisible()
  await expect(page.locator('.list-group-item', { hasText: 'school' })).toBeVisible()

  // the demo is written into the URL so that bookmarking works
  expect((await chartData(page)).events.length).toBeGreaterThan(0)
})

test('"#" shows a blank chart', async ({ page }) => {
  await openBlank(page)
  await expect(page.getByRole('link', { name: 'blank chart' })).toHaveClass(/active/)
  await expect(page.getByText('no events yet')).toBeVisible()
  await expect(page.getByText('This is a demo chart.')).toBeHidden()
  await expect(field(page, 'Life Expectancy')).toHaveValue('80')
})

test('edits are saved in the URL and survive a reload', async ({ page }) => {
  await openBlank(page)

  await field(page, 'Date of Birth').fill('1990-01-15')
  await field(page, 'Life Expectancy').fill('90')
  await field(page, 'Adult Years').first().fill('21')
  await field(page, 'Adult Years').last().fill('65')

  await page.getByRole('button', { name: 'Add event' }).click()
  const form = page.locator('form').first()
  await form.locator('input[type=date]').first().fill('2008-09-01')
  await form.locator('input[type=date]').last().fill('2012-06-30')
  await form.getByPlaceholder('label').fill('college')
  await form.getByRole('button', { name: 'Add' }).click()

  await expect(page.locator('.list-group-item', { hasText: 'college' })).toContainText('2008-09-01 to 2012-06-30')
  expect(await chartData(page)).toMatchObject({
    'birth-date': '1990-01-15',
    'life-expectancy': 90,
    'kid-until': 21,
    'old-from': 65,
    events: [{ from: '2008-09-01', to: '2012-06-30', label: 'college' }],
  })

  await page.reload()
  await expect(field(page, 'Date of Birth')).toHaveValue('1990-01-15')
  await expect(field(page, 'Life Expectancy')).toHaveValue('90')
  await expect(field(page, 'Adult Years').first()).toHaveValue('21')
  await expect(field(page, 'Adult Years').last()).toHaveValue('65')
  await expect(page.locator('.list-group-item', { hasText: 'college' })).toBeVisible()
})

test('events can be edited and deleted', async ({ page }) => {
  await page.goto(link({ ...blankData, events: [{ from: '2000-01-01', to: '2001-01-01', color: '#ff0000', label: 'trip', overlay: false }] }))

  const row = page.locator('.list-group-item', { hasText: 'trip' })
  await row.getByRole('button', { name: 'edit' }).click()
  const form = page.locator('form:visible')
  await form.getByPlaceholder('label').fill('long trip')
  await form.getByRole('button', { name: 'Save' }).click()
  await expect(page.locator('.list-group-item', { hasText: 'long trip' })).toBeVisible()
  expect((await chartData(page)).events.map((e) => e.label)).toEqual(['long trip'])

  await page.locator('.list-group-item', { hasText: 'long trip' }).getByRole('button', { name: 'edit' }).click()
  await page.locator('form:visible').getByRole('button', { name: 'Delete' }).click()
  await expect(page.getByText('no events yet')).toBeVisible()
  expect((await chartData(page)).events).toEqual([])
})

test('events shorter than a week still get a square', async ({ page }) => {
  const events = [
    { from: '2020-03-07', to: '2020-03-07', color: '#ff0000', fill: 'rgba(100%,0%,0%,1)' },
    { from: '2020-04-04', to: '2020-04-07', color: '#00ff00', fill: 'rgba(0%,100%,0%,1)' },
    // 2023 starts on a Sunday, the first day of the year still has a square
    { from: '2023-01-01', to: '2023-01-01', color: '#0000ff', fill: 'rgba(0%,0%,100%,1)' },
  ]
  await page.goto(link({ ...blankData, events: events.map(({ fill, ...event }) => ({ ...event, label: fill, overlay: false })) }))

  for (const { fill } of events) {
    await expect(page.locator(`.chart svg rect[fill="${fill}"]`)).toHaveCount(1)
  }
})

test('the weeks left clock ticks', async ({ page }) => {
  await openBlank(page)
  const weeksLeft = badges(page).filter({ hasText: 'weeks left' })
  const before = await weeksLeft.textContent()
  await expect(weeksLeft).not.toHaveText(before, { timeout: 5000 })
})

test('hiding non-adult years only counts adult years', async ({ page }) => {
  await openBlank(page)
  await expect(badges(page).filter({ hasText: /^[\d.]+% elapsed$/ })).toBeVisible()

  await page.getByLabel('Hide').check()
  await expect(page.locator('.chart').getByText('adult years (18–70)')).toBeVisible()
  await expect(badges(page).filter({ hasText: '% of adult years elapsed' })).toBeVisible()
  await expect(badges(page).filter({ hasText: 'adult weeks left' })).toBeVisible()
  expect((await chartData(page))['only-adult-years']).toBe(true)
})

// the badges either share a row or stack flush left, never one on each side
// of two different lines
test('the metric badges stay aligned at any big-screen width', async ({ page }) => {
  await openBlank(page)
  for (const hide of [false, true]) {
    if (hide) await page.getByLabel('Hide').check()
    for (const width of [1366, 1024, 800]) {
      await page.setViewportSize({ width, height: 900 })
      const [elapsed, left] = await Promise.all(
        [/elapsed/, /weeks left/].map((text) => badges(page).filter({ hasText: text }).boundingBox()),
      )
      const sameRow = Math.abs(elapsed.y - left.y) < 2
      const stacked = left.y > elapsed.y && Math.abs(elapsed.x - left.x) < 2
      expect(sameRow || stacked, `badges at ${width}px, hide=${hide}`).toBe(true)
    }
  }
})

test('the help links stay on the label line at 1024px and wider', async ({ page }) => {
  await openBlank(page)
  const label = (name) => page.locator('label.col-form-label', { hasText: new RegExp('^' + name) })
  for (const width of [1366, 1024]) {
    await page.setViewportSize({ width, height: 900 })
    // a wrapped label is two lines tall
    const oneLine = (await label('Date of Birth').boundingBox()).height
    for (const name of ['Life Expectancy', 'Non-Adult Years']) {
      const { height } = await label(name).boundingBox()
      expect(height, `${name} label at ${width}px`).toBeLessThan(oneLine * 1.5)
    }
  }
})

test('export shows the chart as JSON and imports edits', async ({ page }) => {
  await openBlank(page)
  await page.getByRole('button', { name: 'Export raw data' }).first().click()
  const textarea = page.locator('.modal textarea')
  await expect(textarea).toBeVisible()
  expect(JSON.parse(await textarea.inputValue())).toEqual(blankData)

  await textarea.fill(JSON.stringify({ ...blankData, 'life-expectancy': 77 }))
  await page.getByRole('button', { name: 'Close' }).click()
  await expect(field(page, 'Life Expectancy')).toHaveValue('77')
  expect((await chartData(page))['life-expectancy']).toBe(77)
})

test.describe('invalid inputs leave the last valid chart in place', () => {
  // each test types something invalid, then checks the URL didn't change and
  // the chart is still drawn
  async function expectUnchanged(page) {
    await expect(page.locator('.chart svg')).toBeVisible()
    await expect(badges(page).filter({ hasText: 'weeks left' })).toHaveText(/^\d+\.\d{6} weeks left$/)
    expect(await chartData(page)).toEqual(blankData)
  }

  for (const value of ['', '0', '-5', '501', '3.5']) {
    test(`life expectancy "${value}"`, async ({ page }) => {
      await openBlank(page)
      await field(page, 'Life Expectancy').fill(value)
      await expectUnchanged(page)
    })
  }

  test('empty date of birth', async ({ page }) => {
    await openBlank(page)
    await field(page, 'Date of Birth').fill('')
    await expectUnchanged(page)
  })

  for (const [name, start, end] of [
    ['empty start', '', '70'],
    ['empty end', '18', ''],
    ['negative start', '-1', '70'],
    ['end below start', '18', '10'],
    ['start above end', '75', '70'],
    ['end past 500', '18', '501'],
  ]) {
    test(`adult years: ${name}`, async ({ page }) => {
      await openBlank(page)
      await field(page, 'Adult Years').first().fill(start)
      await field(page, 'Adult Years').last().fill(end)
      await expectUnchanged(page)
    })
  }

  test('adult years past life expectancy are capped on the chart', async ({ page }) => {
    await openBlank(page)
    await field(page, 'Adult Years').last().fill('95')
    await page.getByLabel('Hide').check()
    await expect(page.locator('.chart').getByText('adult years (18–80)')).toBeVisible()
    await expect(page.locator('.chart svg')).toBeVisible()
  })

  test('an event without a label is not added', async ({ page }) => {
    await openBlank(page)
    await page.getByRole('button', { name: 'Add event' }).click()
    const form = page.locator('form').first()
    await form.locator('input[type=date]').first().fill('2008-09-01')
    await form.locator('input[type=date]').last().fill('2012-06-30')
    await form.getByRole('button', { name: 'Add' }).click()
    await expect(page.getByText('no events yet')).toBeVisible()
    await expectUnchanged(page)
  })

  test('invalid JSON in the export box', async ({ page }) => {
    await openBlank(page)
    await page.getByRole('button', { name: 'Export raw data' }).first().click()
    await page.locator('.modal textarea').fill('{"birth-date": "not a date"')
    await page.getByRole('button', { name: 'Close' }).click()
    await expectUnchanged(page)
  })

  for (const [name, hash] of [
    ['garbage', '#not base64 at all!!'],
    ['truncated', link(blankData).slice(1, -12)],
    ['valid base64 but not JSON', '#' + Buffer.from('hello').toString('base64')],
    ['JSON with missing fields', link({ 'birth-date': '1988-07-24' }).slice(1)],
    ['JSON with a bad date', link({ ...blankData, 'birth-date': '1988-13-45' }).slice(1)],
  ]) {
    test(`shared link: ${name}`, async ({ page }) => {
      await page.goto('/' + hash.replace(/^\/?/, ''))
      await expect(page.locator('.chart svg')).toBeVisible()
      await expect(field(page, 'Life Expectancy')).toHaveValue('80')
      await expect(page.getByText('no events yet')).toBeVisible()
    })
  }

  test('a broken link after a valid one keeps the valid chart', async ({ page }) => {
    const data = { ...blankData, 'life-expectancy': 85 }
    await page.goto(link(data))
    await expect(field(page, 'Life Expectancy')).toHaveValue('85')
    await page.evaluate(() => { window.location.hash = '#broken' })
    await expect(page.locator('.chart svg')).toBeVisible()
    await expect(field(page, 'Life Expectancy')).toHaveValue('85')
  })
})
