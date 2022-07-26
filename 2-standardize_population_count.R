# Create the Standardized population Dataset

library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE)

population <- read_csv("raw/nst-est2019-alldata.csv") |>
    filter(SUMLEV == "040") |>
    select(NAME, CENSUS2010POP) |>
    rename(STATE = NAME,
           CENSUSPOP = CENSUS2010POP) |>
    mutate(STD_POP_CS = CENSUSPOP / 100000)

population <- open_dataset("parquet/ccaei103") |>
    count(STATE) |>
    collect() |>
    inner_join(state_codes) |>
    ungroup() |>
    select(-STATE) |>
    rename(STATE = NAME,
           DS_POP = n) |>
    mutate(STD_POP_DS = DS_POP / 1000) |>
    inner_join(population) |>
    write_csv("ancillary/standardized_population.csv")
