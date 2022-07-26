# Process the AEI Data


library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE)

population <- read_csv("ancillary/standardized_population.csv")

calculate_state_incidences <- function(disease) {

    matched_data_file <- file.path(
        "results",
        "matched",
        str_glue("matched_{disease}.csv")
    )

    rate_output_file <- file.path(
        "results",
        str_glue("{disease}_incidence_rates_state.csv")
    )

    matched <- read_csv(matched_data_file) |>
        pull(ENROLID)

    ccaei103 <- open_dataset("parquet/ccaei103") |>
        select(ENROLID, STATE, !!disease, AGE) |>
        filter(AGE <= 65, AGE >= 18,
               ENROLID %in% matched) |>
        rename(disease := !!disease) |>
        group_by(STATE, disease) |>
        distinct() |>
        collect() |>
        summarise(num = sum(disease)) |>
        filter(disease == 1) |>
        select(-disease) |>
        inner_join(state_codes) |>
        ungroup() |>
        select(-STATE) |>
        rename(STATE = NAME,
               CASES = num)

    rates <- inner_join(population, ccaei103) |>
        mutate(DS_RATE = round(CASES / STD_POP_DS, 4),
               CS_RATE = round(CASES / STD_POP_CS, 4)) |>
        select(STATE, CASES, CENSUSPOP, DS_POP, CS_RATE, DS_RATE) |>
        write_csv(rate_output_file)

    rates

}

diseases <- c("depression", "regional_enteritis")

diseases |>
    walk(calculate_state_incidences)



