# Lifechart

[![Build](https://github.com/infertux/lifechart/actions/workflows/build.yml/badge.svg)](https://github.com/infertux/lifechart/actions/workflows/build.yml)

Spend your time wisely.

https://infertux.github.io/lifechart/

Lifechart draws your whole life on a single page, one square per week: each row is a year, and every week you have already lived is filled in. Seeing it all at once makes it hard to ignore how many weeks are left. The idea comes from Wait But Why's [Your Life in Weeks](https://waitbutwhy.com/2014/05/life-weeks.html).

## Features

- Enter your date of birth and life expectancy to see what share of your life has gone by and how many weeks you have left.
- Add events such as school, jobs or travels as colored periods on the chart. Overlay events, such as the places you lived, show their initial on top of the other colors instead.
- Hide your childhood and old age to focus on your adult years.

## Privacy

Everything runs in your browser and no data is ever sent to a server. Your chart is stored in the page's URL after the `#`, which browsers never send to the server, so to keep your chart, bookmark the "bookmark your own chart" link. You can also export or import your data as JSON with "Export raw data".

## Development

Install the same tools as the CI, then build with `make`, which writes `public/lifechart.min.js`:

```sh
npm install --global elm@0.19.3-0 elm-format@0.8.8 uglify-js@3.19.3
make
```

Elm apps can't run from a `file://` URL, so serve the `public` folder over HTTP to try it, then open http://localhost:8000/:

```sh
python3 -m http.server --directory public
```

The [Playwright](https://playwright.dev/) tests in `tests/` drive the built app in a real browser, covering the main features and checking that invalid input never breaks the page. Build with `make` first, then run them (they serve `public` themselves):

```sh
npm install
npx playwright install chromium
npx playwright test
```

Every push to `master` is checked with `elm-format`, built, tested and deployed to GitHub Pages by the [build workflow](.github/workflows/build.yml), which also builds and tests every pull request.

## Elm

[Elm](http://elm-lang.org/) rocks! Check it out.

## License

AGPLv3+
