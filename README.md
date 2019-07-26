# legcoplus: Improving Hong Kong Legislative Council Data
An R package to clean data retrieved from the Hong Kong Legislative Council (LegCo) APIs. 

Under active development.

To install this package, run:
```
#install.packages("devtools")
#devtools::install_github("elgarteo/legco")
devtools::install_github("elgarteo/legcoplus")
```

## What it does
This R package is designed to work with the package [elgarteo/legco](https://github.com/elgarteo/legco),
which retrieves data from the LegCo APIs. 

While it is great that LegCo provides APIs to allow open data access, they are not perfect.
For exmaple, the presence of seperate APIs for different type of data means searching the 
right data can be frustrating.

This package solves just that by providing functions that patch these deficiencies and facilitate 
the use of the APIs.

## How to use
Read the [vignettes](https://elgarteo.ga/legco/legcoplus/) for details.

## Disclaimer
This package is not affiliated or endorsed by the Legislative Council of Hong Kong. 

The Legislative Council of Hong Kong is the copyright owner of data retieved from its open data APIs.
