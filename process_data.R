# Process the SAS Data Files and Convert them to R/Parquet objects


library(foreign)
library(arrow)
library(tidyverse)

t <- tempdir()

sas_files <- list.files("sas_data/", full.names = TRUE)

sas_file_names <- list.files("sas_data/") |>
    str_remove(".zip")

xpt_files <- sas_files |>
    map_chr(~ utils::unzip(.x, exdir = t)) |>
    set_names(sas_file_names)

data_files <- xpt_files |>
    map(~ read.xport(.x)) |>
    {
        \(x) map2(x, names(x), ~ saveRDS(.x, file.path("rds", str_glue("{.y}.RDS"))))
    }()

unlink(xpt_files)

