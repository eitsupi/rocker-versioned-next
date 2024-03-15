library(rversions)
library(jsonlite)
library(pak)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(tibble)
library(httr2)
library(purrr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(gert)


.latest_rspm_cran_url_linux <- function(date, distro_version_name, r_version) {
  n_retry_max <- 6

  dates_try <- if (is.na(date)) {
    NA_real_
  } else {
    seq(as.Date(date), as.Date(date) - n_retry_max, by = -1)
  }

  fallback_distro <- if (distro_version_name == "jammy") {
    "focal"
  } else {
    NULL
  }

  urls_try <- tidyr::expand_grid(
    date = dates_try,
    distro_version_name = c(distro_version_name, fallback_distro),
    type = c("binary")
  ) |>
    purrr::pmap_chr(.make_rspm_cran_url_linux) |>
    unique()

  for (i in seq_along(urls_try)) {
    .url <- urls_try[i]
    if (.is_cran_url_available(.url, r_version)) break
    .url <- NA_character_
  }

  if (is.na(.url)) stop("\nCRAN mirrors are not available!\n")

  .url
}


.make_rspm_cran_url_linux <- function(date, distro_version_name, type = "source") {
  base_url <- "https://p3m.dev/cran"
  .url <- dplyr::case_when(
    type == "source" & is.na(date) ~ glue::glue("{base_url}/latest"),
    type == "binary" & is.na(date) ~ glue::glue("{base_url}/__linux__/{distro_version_name}/latest"),
    type == "source" ~ glue::glue("{base_url}/{date}"),
    type == "binary" ~ glue::glue("{base_url}/__linux__/{distro_version_name}/{date}")
  )

  .url
}
