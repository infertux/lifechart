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
  "hide-unproductive-years": false,
  "events": [
    {
      "from": "2010-09-14",
      "to": "2014-02-01",
      "color": "#f57900",
      "label": "college",
      "overlay": false
    },
    {
      "from": "2015-06-01",
      "to": "2016-05-31",
      "color": "#73d216",
      "label": "trip",
      "overlay": false
    }
  ]
}
"""
    in
        Base64.encode json |> Result.toMaybe |> Maybe.withDefault ""
