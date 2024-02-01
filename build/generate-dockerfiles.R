library(readr)
library(glue)
library(tibble)

df_versions <- tibble::tribble(
  ~r_version, ~ubuntu_version, ~cran_url,
  "4.3.2", "22.04", "https://p3m.dev/cran/__linux__/jammy/latest",
  "4.0.0", "20.04", "https://p3m.dev/cran/__linux__/focal/2020-06-04"
)

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
