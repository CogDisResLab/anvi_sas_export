# Process the AEI Data


library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE)

ccaei103 <- open_dataset("parquet/ccaei103") |>
    select(ENROLID, STATE, regional_enteritis, AGE) |>
    filter(AGE <= 65, AGE >= 18) |>
    group_by(STATE, regional_enteritis) |>
    distinct() |>
    collect() |>
    summarise(num = sum(regional_enteritis)) |>
    filter(regional_enteritis == 1) |>
    select(-regional_enteritis) |>
    inner_join(state_codes) |>
    ungroup() |>
    select(-STATE) |>
    rename(STATE = NAME,
           CASES = num)

population <- read_csv("raw/nst-est2019-alldata.csv") |>
    filter(SUMLEV == "040") |>
    select(NAME, CENSUS2010POP) |>
    rename(STATE = NAME) |>
    mutate(STD_POP = CENSUS2010POP / 100000)

rates <- inner_join(population, ccaei103) |>
    mutate(rates = round(CASES / STD_POP, 4)) |>
    write_csv("results/regional_enteritis_incidence_rates_population.csv")
