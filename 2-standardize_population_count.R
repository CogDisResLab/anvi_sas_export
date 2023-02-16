# Create the Standardized population Dataset

library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE) |>
    filter(NAME != "Nation")

population_all <- read_csv("raw/nst-est2019-alldata.csv") |>
    filter(SUMLEV == "040") |>
    select(NAME, CENSUS2010POP) |>
    rename(STATE = NAME,
           CS_POP = CENSUS2010POP) |>
    mutate(STD_POP_CS = CS_POP / 100000) |>
    arrange(STATE)

population_dataset <- open_dataset("parquet/ccaei103") |>
    count(STATE) |>
    collect() |>
    inner_join(state_codes) |>
    ungroup() |>
    select(-STATE) |>
    rename(STATE = NAME,
           DS_POP = n) |>
    mutate(STD_POP_DS = DS_POP / 1000) |>
    select(STATE, DS_POP, STD_POP_DS) |>
    mutate(STATE = if_else(STATE == "Washington DC", "District of Columbia", STATE)) |>
    arrange(STATE) |>
    inner_join(population_all) |>
    write_csv("ancillary/standardized_population.csv")
