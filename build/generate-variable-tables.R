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


#' Search the latest P3M CRAN mirror URL for Linux at a given date
#' @param date A single character of date like `"2023-10-30"` or `NA`.
#' If `NA`, the "latest" URL will be returned.
#' @param distro_version_name A character of distro version name like `"focal"`.
#' @param r_version A character of R version like `"4.3.0"`.
#' @return A character of P3M CRAN mirror URL.
#' @examples
#' latest_p3m_cran_url_linux("2023-10-30", "focal", "4.3.0")
#' latest_p3m_cran_url_linux(NA, "focal", "4.3.0")
latest_p3m_cran_url_linux <- function(date, distro_version_name, r_version) {
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
    purrr::pmap_chr(make_p3m_cran_url_linux) |>
    unique()

  for (i in seq_along(urls_try)) {
    .url <- urls_try[i]
    if (is_cran_url_available(.url, r_version)) break
    .url <- NA_character_
  }

  if (is.na(.url)) rlang::abort("\nCRAN mirrors are not available!\n")

  .url
}


#' A funtion to make P3M CRAN mirror URL for Linux
#' @param date A character vector of dates like `"2023-10-30"`.
#' If `NA`, the "latest" URL will be returned.
#' @param distro_version_name A character of distro version name like `"focal"`.
#' @param type A character of package type, `"source"` (default) or `"binary"`.
#' @return A character of P3M CRAN mirror URL.
#' @examples
#' make_p3m_cran_url_linux(c("2023-10-30", NA), "focal", "binary")
make_p3m_cran_url_linux <- function(date, distro_version_name, type = "source") {
  base_url <- "https://p3m.dev/cran"

  dplyr::case_when(
    type == "source" & is.na(date) ~ glue::glue("{base_url}/latest"),
    type == "binary" & is.na(date) ~ glue::glue("{base_url}/__linux__/{distro_version_name}/latest"),
    type == "source" ~ glue::glue("{base_url}/{date}"),
    type == "binary" ~ glue::glue("{base_url}/__linux__/{distro_version_name}/{date}")
  )
}


#' Check if a CRAN URL is available via [pak::repo_ping()]
#' @param url A single character of CRAN URL.
#' @param r_version A character of R version like `"4.3.0"`.
is_cran_url_available <- function(url, r_version) {
  glue::glue("\n\nfor R {r_version}, repo_ping to {url}\n\n") |>
    cat()

  is_available <- pak::repo_ping(cran_mirror = url, r_version = r_version, bioc = FALSE) |>
    dplyr::filter(name == "CRAN") |>
    dplyr::pull(ok)

  is_available
}


#' Get the commit date from a GitHub API URL for a Git commit
#' @param commit_url A single character of GitHub API URL for a commit.
#' e.g. `"https://api.github.com/repos/rstudio/rstudio/commits/7d165dcfc1b6d300eb247738db2c7076234f6ef0"`
#' @return A character of commit date.
#' @examples
#' get_github_commit_date("https://api.github.com/repos/rstudio/rstudio/commits/7d165dcfc1b6d300eb247738db2c7076234f6ef0")
get_github_commit_date <- function(commit_url) {
  res <- httr2::request(commit_url) |>
    httr2::req_headers(Accept = "application/vnd.github.v3+json") |>
    httr2::req_perform()

  commit_date <- res |>
    httr2::resp_body_json() |>
    purrr::pluck("commit", "committer", "date", .default = NA) |>
    lubridate::as_date()

  commit_date
}


#' Check if an RStudio deb package (amd64) is available
#' @param rstudio_version A single character of RStudio version like `"2023.12.0+369"`.
#' @param ubuntu_series A character of Ubuntu series like `"jammy"`.
#' @return A logical value.
#' @examples
#' is_rstudio_deb_available("2023.12.0+369", "jammy")
is_rstudio_deb_available <- function(rstudio_version, ubuntu_series) {
  os_ver <- dplyr::case_match(
    ubuntu_series,
    "focal" ~ "bionic",
    .default = ubuntu_series
  )

  is_available <- glue::glue(
    "https://download2.rstudio.org/server/{os_ver}/amd64/rstudio-server-{rstudio_version}-amd64.deb"
  ) |>
    stringr::str_replace_all("\\+", "-") |>
    httr2::request() |>
    httr2::req_error(is_error = \(...) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_is_error() |>
    isFALSE()

  is_available
}


#' Get the latest CTAN URL for a given date
#' @param date A [Date] class vector.
#' If `NA`, the "latest" URL will be returned.
#' @return A character of CTAN URL.
#' @examples
#' latest_ctan_url(as.Date(c("2023-10-30", NA)))
latest_ctan_url <- function(date) {
  .url <- dplyr::if_else(
    is.na(date), "https://mirror.ctan.org/systems/texlive/tlnet",
    stringr::str_c("https://www.texlive.info/tlnet-archive/", format(date, "%Y/%m/%d"), "/tlnet")
  )

  .url
}


#' Get the latest version from Git remote tags
#' @param remote_repo A single character of Git remote repository URL.
#' @return A character of the latest version.
#' @examples
#' latest_version_of_git_repo("https://github.com/OSGeo/PROJ.git")
latest_version_of_git_repo <- function(remote_repo) {
  gert::git_remote_ls(remote = remote_repo) |>
    dplyr::pull(ref) |>
    stringr::str_subset(r"(^refs/tags/v?(\d+\.){2}\d+$)") |>
    stringr::str_extract(r"((\d+\.)*\d+$)") |>
    package_version() |>
    sort() |>
    utils::tail(1) |>
    as.character()
}


#' Paste each element of vectors in a cartesian product
#' @param ... Dynamic dots. Character vectors to paste.
#' @return A character vector.
#' @examples
#' outer_paste(c("a", "b"), "-", c("c", "d", "e"))
outer_paste <- function(...) {
  .paste <- function(x, y) {
    outer(x, y, stringr::str_c) |>
      c()
  }

  out <- rlang::list2(...) |>
    purrr::reduce(.paste)

  out
}
