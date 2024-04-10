#' Write a Dockerfiles with the given template and data frame of variables
#' @param ... Ignored.
#' @param data A data frame of variables.
#' Must have a column named `r_version`.
#' @param dockerfile_template A character of a Dockerfile template
#' @param path_template A character of output Dockerfile path template
#' @return A data frame invisibly.
#' @examples
#' write_dockerfiles(
#'   data = data.frame(r_version = "4.0.0"),
#'   dockerfile_template = "FROM rocker/r-ver:{{r_version}}",
#'   path_template = "dockerfiles/r-ver_{{r_version}}.Dockerfile"
#' )
write_dockerfiles <- function(..., data, dockerfile_template, path_template) {
  data |>
    dplyr::mutate(
      dockerfile = glue::glue(
        dockerfile_template,
        .open = "{{",
        .close = "}}",
        .trim = FALSE
      )
    ) |>
    purrr::pwalk(
      \(...) {
        dots <- list(...)
        readr::write_file(
          dots$dockerfile,
          glue::glue(
            path_template,
            r_version = dots$r_version,
            .open = "{{",
            .close = "}}",
            .trim = FALSE
          )
        )
      }
    )
}


df_args <- fs::dir_ls(path = "versioned-args", glob = "*.json") |>
  purrr::map(
    \(x) jsonlite::fromJSON(x, flatten = TRUE) |>
      purrr::modify_if(is.null, \(x) NA) |>
      tibble::as_tibble()
  ) |>
  purrr::list_rbind()


tibble::tribble(
  ~.name, ~.template_path,
  "r-ver", "dockerfile-templates/r-ver.Dockerfile.txt",
  "rstudio", "dockerfile-templates/rstudio.Dockerfile.txt",
) |>
  purrr::pwalk(
    \(...) {
      dots <- rlang::list2(...)
      df_args |>
        write_dockerfiles(
          data = _,
          dockerfile_template = readr::read_file(dots$.template_path),
          path_template = glue::glue(
            "dockerfiles/{dots$.name}_{{{{r_version}}}}.Dockerfile",
            .trim = FALSE
          ),
        )
    }
  )
