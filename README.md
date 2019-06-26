# legcoplus: Improving Hong Kong Legislative Council Data
An R package to present data from the Hong Kong Legislative Council (LegCo) APIs in a better way. 

Inspired by [evanodell/hansard](https://github.com/evanodell/hansard).

Under development.

To install this package, run:
```
#install.pacakges("devtools")
#devtools::install_github("elgarteo/legco")
devtools::install_github("elgarteo/legcoplus")
```
or
```
install.packages("https://elgarteo.ga/legco/legcoplus/legcoplus_0.0.9999.tar.gz", repos = NULL, type = "source")
```

## What it does
This R package is designed to work with the package [elgarteo/legco](https://github.com/elgarteo/legco). 

The LegCo APIs mainly work with hansard PDF files and rely on the document structure like 
section headers and paragraphs to locate data. For example, a query to fetch questions raised by LegCo
members only tells you in which hansard PDF file and in which section the questions are located in.
To find out the question text and the answering body, you'd have to crawl through the hansard structure
and make multiple API calls.

This package solves just that by providing functions that return more usable result from the API.

## Disclaimer
This package is not affiliated or endorsed by the Legislative Council of Hong Kong.
