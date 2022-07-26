# Process the AEI Data


library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE)

depression_matched <- read_csv("results/matched/matched_depression.csv") |>
    pull(ENROLID)

ccaei103 <- open_dataset("parquet/ccaei103") |>
    select(ENROLID, STATE, depression, AGE) |>
    filter(AGE <= 65, AGE >= 18,
           ENROLID %in% depression_matched) |>
    group_by(STATE, depression) |>
    distinct() |>
    collect() |>
    summarise(num = sum(depression)) |>
    filter(depression == 1) |>
    select(-depression) |>
    inner_join(state_codes) |>
    ungroup() |>
    select(-STATE) |>
    rename(STATE = NAME,
           CASES = num)

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
    inner_join(population)

rates <- inner_join(population, ccaei103) |>
    mutate(DS_RATE = round(CASES / STD_POP_DS, 4),
           CS_RATE = round(CASES / STD_POP_CS, 4)) |>
    select(STATE, CASES, CENSUSPOP, DS_POP, CS_RATE, DS_RATE) |>
    write_csv("results/depression_matched_incidence_rates_population.csv")
