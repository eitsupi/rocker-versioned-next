library(readr)
library(glue)

df_versions <- readr::read_tsv("variable-tables/r-ver.tsv")

template <- readr::read_file("dockerfile-templates/r-ver.Dockerfile.txt")

out <- df_versions |>
  glue::glue_data(
    template,
    .open = "{{",
    .close = "}}"
  )

purrr::walk2(
  out,
  df_versions$r_version,
  \(text, r_version) readr::write_file(
    text,
    glue::glue("dockerfiles/r-ver_{r_version}.Dockerfile")
  )
)
