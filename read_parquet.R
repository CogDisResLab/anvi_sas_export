# Process the AEI Data


library(arrow)
library(tidyverse)

ccaei103 <- open_dataset("parquet/ccaei103") |>
    select(ENROLID, depression, AGE) |>
    filter(AGE <= 65, AGE >= 18) |>
    group_by(depression) |>
    distinct() |>
    collect() |>
    summarise(num = sum(depression)) |>
    filter(depression == 1)

ccaea10a <- open_dataset("parquet/ccaea10a") |>
    collect()
