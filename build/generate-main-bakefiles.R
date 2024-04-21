# TODO: update json
# TODO: update cache-from/to
#' @param ... Named arguments
#' @param bakefile_template A character of a template for docker-bake.json
#' @param path_template A character of output path template
#' @return A data frame invisibly.
#' @examples
#' write_bakefiles(
#'   r_version = "4.0.0",
#'   bakefile_template = r"({"target": {"foo": {"dockerfile": "dockerfiles/r-ver_{{r_version}}.Dockerfile"}}})",
#'   path_template = "bakefiles/{{r_version}}.docker-bake.json"
#' )
write_bakefiles <- function(..., bakefile_template, path_template) {
  dots <- rlang::list2(...)
  bake_json_content <- glue::glue_data(
    dots,
    bakefile_template,
    .open = "{{",
    .close = "}}",
    .trim = FALSE
  )
}


#' Outer paste of vectors
#' @param ... Character vectors to paste
#' @return A character vector
#' @examples
#' outer_paste("foo", c("-", "+"), c("bar", "baz"))
outer_paste <- function(...) {
  rlang::list2(...) |>
    purrr::reduce(\(x, y) {
      outer(x, y, stringr::str_c) |> c()
    })
}


generate_tags <- function(base_name,
                          ...,
                          r_version,
                          r_minor_latest = FALSE,
                          r_major_latest = FALSE,
                          use_latest_tag = TRUE,
                          tag_suffix = "",
                          latest_tag = "latest") {
  rlang::check_dots_empty()

  .tags <- outer_paste(base_name, ":", r_version, tag_suffix)

  r_minor_version <- stringr::str_extract(r_version, "^\\d+\\.\\d+")
  r_major_version <- stringr::str_extract(r_version, "^\\d+")

  if (r_minor_latest == TRUE) {
    .tags <- c(.tags, outer_paste(base_name, ":", r_minor_version, tag_suffix))
  }
  if (r_major_latest == TRUE) {
    .tags <- c(.tags, outer_paste(base_name, ":", r_major_version, tag_suffix))
    if (use_latest_tag == TRUE) {
      .tags <- c(.tags, outer_paste(base_name, ":", latest_tag))
    }
  }

  .tags
}


df_args <- fs::dir_ls(path = "versioned-args", glob = "*.json") |>
  purrr::map(
    \(x) jsonlite::fromJSON(x, flatten = TRUE) |>
      purrr::modify_if(is.null, \(x) NA) |>
      tibble::as_tibble()
  ) |>
  purrr::list_rbind()

df_args |>
  purrr::pwalk(
    \(...) {
      write_bakefile(
        ...,
        bakefile_template = readr::read_file("bakefile-templates/main.bakefile.json"),
        path_template = "bakefiles/{{{{r_version}}}}.docker-bake.json"
      )
    }
  )
