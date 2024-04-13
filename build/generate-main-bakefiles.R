#' @param ... Ignored.
#' @param data A data frame of variables.
#' Must have a column named `r_version` and `ubuntu_series`.
#' @param bakefile_template A character of a template for docker-bake.json
#' @param path_template A character of output path template
#' @return A data frame invisibly.
write_bakefiles <- function(..., data, bakefile_template, path_template) {
  data |>
    dplyr::mutate(
      dockerfile = glue::glue(
        bakefile_template,
        .open = "{{",
        .close = "}}",
        .trim = FALSE
      )
    )
}


# TODO: update json
# TODO: generate tags
# TODO: update cache-from/to

df_args <- fs::dir_ls(path = "versioned-args", glob = "*.json") |>
  purrr::map(
    \(x) jsonlite::fromJSON(x, flatten = TRUE) |>
      purrr::modify_if(is.null, \(x) NA) |>
      tibble::as_tibble()
  ) |>
  purrr::list_rbind()
