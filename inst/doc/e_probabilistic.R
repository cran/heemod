## ----echo=FALSE, include=FALSE------------------------------------------------
library(heemod)

## ----echo = FALSE-------------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN,
  screenshot.force = FALSE
)

