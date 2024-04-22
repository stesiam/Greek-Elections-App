# 🗳️ Greek Elections App v2

[![Docker Image CI](https://github.com/stesiam/Greek-Elections-App/actions/workflows/docker-image.yml/badge.svg)](https://github.com/stesiam/Greek-Elections-App/actions/workflows/docker-image.yml)

[Shiny App Website](https://stesiam.shinyapps.io/Greek-Elections-App/)

A Shiny App that calculates the seats of each party in the Greek Parliament. You can calculate the seats of the parties based on their percentage for the last four electoral laws. Below, I enumerate those, and in parentheses are the elections for which they have been in effect:

|Electoral Law| Elections| Description |
|:---:|:---:|---|
|**Bonus 40**| 2007 <br> 2009|260 🪑 are being allocated proportionally <br>  First party + 40 🪑 <br> Electoral threshold: 3% |
|**Bonus 50**| 2012 <br> 2015 <br> 2019|250 🪑 are being allocated proportionally <br> First party + 50 🪑 <br> Electoral threshold: 3% |
|**No Bonus**| 2023A |300 🪑 are being allocated proportionally <br>No bonus 🪑 <br> Electoral threshold: 3% |
|**Proportional Bonus**| 2023B | Varied seats (260 to 300) are being allocated proportionally <br> Depends on first party percentage <br> Electoral threshold: 3% |


Proportional Bonus has been set as the default electoral system as this is the current system from 2023B and onwards.

## Implemented Features

- Multilanguage ( 🇬🇷 / 🇬🇧 ) - [{shiny.i18n}](https://github.com/Appsilon/shiny.i18n)
- DaisyUI Table style - [{shiny.tailwind}](https://github.com/kylebutts/shiny.tailwind)
- Cookies to save language selection - [{cookies}](https://github.com/shinyworks/cookies) R package

## Future Plans

- Add unallocated percenage in app (make it red if it goes negative)
- A popup window at start which will explain the electoral system on Greece
- A cookie for electoral system
- UNDER CONDITIONS a cookie for parties' percentages (I should also give a reset button as an option)

