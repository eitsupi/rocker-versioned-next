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

  glue::glue("\n\nChecking RStudio Sever {rstudio_version} deb package for {os_ver}\n\n") |>
    cat()

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


#' Get R versions table
#' @param min_version A single character of minimum R version like `"4.0.0"`.
#' @return A [tibble::tibble] of R versions.
#' - r_version: Character, R version. e.g. `"4.0.0"`.
#' - r_release_date: Date, R release date.
#' - r_freeze_date: Date, The date before the next R release.
#' @examples
#' r_versions_with_freeze_dates()
r_versions_with_freeze_dates <- function(min_version = "4.0.0") {
  rversions::r_versions() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      r_version = version,
      r_release_date = as.Date(date),
      r_freeze_date = dplyr::lead(r_release_date, 1) - 1,
      .keep = "none"
    ) |>
    dplyr::filter(package_version(r_version) >= package_version(min_version)) |>
    dplyr::arrange(r_release_date)
}


#' Get Ubuntu LTS versions table
#' @return A [tibble::tibble] of Ubuntu LTS versions.
#' - ubuntu_version: Character, Ubuntu version. e.g. `"20.04"`.
#' - ubuntu_series: Character, Ubuntu series. e.g. `"focal"`.
#' - ubuntu_release_date: Date, Ubuntu release date.
#' @examples
#' ubuntu_lts_versions()
ubuntu_lts_versions <- function() {
  # On Ubuntu, the local file `/usr/share/distro-info/ubuntu.csv` is the same.
  readr::read_csv(
    "https://git.launchpad.net/ubuntu/+source/distro-info-data/plain/ubuntu.csv",
    show_col_types = FALSE
  ) |>
    suppressWarnings() |>
    dplyr::filter(stringr::str_detect(version, "LTS")) |>
    dplyr::mutate(
      ubuntu_version = stringr::str_extract(version, "^\\d+\\.\\d+"),
      ubuntu_series = series,
      ubuntu_release_date = release,
      .keep = "none"
    ) |>
    dplyr::arrange(ubuntu_release_date)
}


#' Get RSutdio IDE versions table
#' @param ... Ignored.
#' @param .n A single integer of the number of versions to get.
#' This function calls the GitHub API this times.
#' @return A [tibble::tibble] of RStudio IDE versions.
#' - rstudio_version: Character, RStudio version. e.g. `"2023.12.0+369"`.
#' - rstudio_commit_date: Date, the date of the release commit.
rstudio_versions <- function(..., .n = 10) {
  gert::git_remote_ls(remote = "https://github.com/rstudio/rstudio.git") |>
    dplyr::filter(stringr::str_detect(ref, "^refs/tags/v")) |>
    dplyr::mutate(
      rstudio_version = stringr::str_extract(ref, r"(\d+\.\d+\.\d+.{0,1}\d*)"),
      commit_url = glue::glue("https://api.github.com/repos/rstudio/rstudio/commits/{oid}"),
      .keep = "none"
    ) |>
    dplyr::slice_tail(n = .n) |>
    dplyr::rowwise() |>
    dplyr::mutate(rstudio_commit_date = get_github_commit_date(commit_url)) |>
    dplyr::ungroup() |>
    tidyr::drop_na() |>
    dplyr::select(
      rstudio_version,
      rstudio_commit_date
    ) |>
    dplyr::arrange(rstudio_commit_date)
}


rocker_versioned_args <- function(
    ...,
    r_versions_file = "variable-tables/r-versions.tsv",
    ubuntu_lts_versions_file = "variable-tables/ubuntu-lts-versions.tsv",
    rstudio_versions_file = "variable-tables/rstudio-versions.tsv") {
  df_all <- readr::read_tsv(r_versions_file, show_col_types = FALSE) |>
    tidyr::expand_grid(
      readr::read_tsv(
        ubuntu_lts_versions_file,
        show_col_types = FALSE,
        col_types = list(ubuntu_version = readr::col_character())
      )
    ) |>
    dplyr::filter(r_release_date >= ubuntu_release_date + 90) |>
    dplyr::slice_max(ubuntu_release_date, with_ties = FALSE, by = r_version) |>
    tidyr::expand_grid(
      readr::read_tsv(
        rstudio_versions_file,
        show_col_types = FALSE
      )
    ) |>
    dplyr::filter(
      r_freeze_date > rstudio_commit_date | is.na(r_freeze_date)
    )

  df_available_rstudio <- df_all |>
    dplyr::distinct(ubuntu_series, rstudio_version) |>
    dplyr::filter(
      purrr::map2_lgl(rstudio_version, ubuntu_series, is_rstudio_deb_available)
    )

  df_all |>
    dplyr::semi_join(df_available_rstudio, by = c("ubuntu_series", "rstudio_version")) |>
    dplyr::slice_max(rstudio_version, with_ties = FALSE, by = c(r_version, ubuntu_series)) |>
    dplyr::mutate(
      ctan = latest_ctan_url(r_freeze_date),
      cran = purrr::pmap_chr(
        list(r_freeze_date, ubuntu_series, r_version),
        \(...) latest_p3m_cran_url_linux(...)
      )
    ) |>
    dplyr::select(
      r_version,
      r_release_date,
      r_freeze_date,
      ubuntu_series,
      cran,
      rstudio_version,
      ctan
    )
}


r_versions_with_freeze_dates() |>
  readr::write_tsv("variable-tables/r-versions.tsv", na = "")


ubuntu_lts_versions() |>
  readr::write_tsv("variable-tables/ubuntu-lts-versions.tsv", na = "")


rstudio_versions() |>
  readr::write_tsv("variable-tables/rstudio-versions.tsv", na = "")


rocker_versioned_args() |>
  purrr::pwalk(
    \(...) {
      dots <- rlang::list2(...)
      list(
        r_version = dots$r_version,
        r_release_date = dots$r_release_date,
        r_freeze_date = dots$r_freeze_date,
        ubuntu_series = dots$ubuntu_series,
        cran = dots$cran,
        rstudio_version = dots$rstudio_version,
        ctan = dots$ctan
      ) |>
        jsonlite::write_json(
          glue::glue("versioned-args/{dots$r_version}.json"),
          auto_unbox = TRUE,
          pretty = TRUE
        )
    }
  )
