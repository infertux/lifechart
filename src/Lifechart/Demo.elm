module Lifechart.Demo exposing (base64)

import Base64


base64 : String
base64 =
    let
        json =
            """
{
  "birth-date": "1988-07-24",
  "life-expectancy": 80,
  "kid-until": 18,
  "old-from": 70,
  "only-adult-years": false,
  "events": [
    {
      "from": "1994-09-05",
      "to": "2006-06-30",
      "color": "#edd400",
      "label": "school",
      "overlay": false
    },
    {
      "from": "2006-09-11",
      "to": "2010-06-25",
      "color": "#f57900",
      "label": "university",
      "overlay": false
    },
    {
      "from": "2010-08-02",
      "to": "2011-03-31",
      "color": "#73d216",
      "label": "travel",
      "overlay": false
    },
    {
      "from": "2011-05-02",
      "to": "2016-02-26",
      "color": "#3465a4",
      "label": "first job",
      "overlay": false
    },
    {
      "from": "2014-09-01",
      "to": "2018-08-31",
      "color": "#2e3436",
      "label": "lisbon",
      "overlay": true
    },
    {
      "from": "2016-04-04",
      "to": "2023-12-22",
      "color": "#75507b",
      "label": "new job",
      "overlay": false
    },
    {
      "from": "2020-03-16",
      "to": "2020-09-30",
      "color": "#2e3436",
      "label": "baby",
      "overlay": true
    },
    {
      "from": "2024-02-01",
      "to": "2030-12-31",
      "color": "#c17d11",
      "label": "startup",
      "overlay": false
    }
  ]
}
"""
    in
    Base64.encode json
