library(readr)
library(glue)

df_versions <- readr::read_tsv("variable-tables/cuda.tsv")

out <- df_versions |>
  glue::glue_data(
    readr::read_file("dockerfile-templates/cuda.Dockerfile.txt"),
    .open = "{{",
    .close = "}}",
    .trim = FALSE
  )

purrr::walk2(
  out,
  df_versions$r_version,
  \(text, r_version) readr::write_file(
    text,
    glue::glue("dockerfiles/cuda_{r_version}.Dockerfile")
  )
)
