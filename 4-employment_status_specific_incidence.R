# Calculate EESTATU Specific Incidence


library(arrow)
library(tidyverse)

state_codes <- read_csv("raw/sas_state_code.csv") |>
    rename(STATE = CODE)

population <- read_csv("ancillary/standardized_population.csv")

calculate_state_incidences <- function(disease, var) {

    matched_data_file <- file.path(
        "results",
        "matched",
        str_glue("matched_{disease}.csv")
    )

    rate_output_file <- file.path(
        "results",
        str_glue("{disease}_incidence_rates_{str_to_lower(var)}.csv")
    )

    var_code_file <- file.path(
        "raw",
        str_glue("sas_{str_to_lower(var)}_codes.csv")
    )

    var_codes <- read_csv(var_code_file, col_types = cols(.default = col_character())) |>
        rename(!!var := NAME)

    annotation <- expand_grid(state_codes, var_codes)

    matched <- read_csv(matched_data_file) |>
        pull(ENROLID)

    ccaei103 <- open_dataset("parquet/ccaei103") |>
        select(ENROLID, !!var, !!disease, AGE, STATE) |>
        filter(AGE <= 65, AGE >= 18,
               ENROLID %in% matched) |>
        rename(disease := !!disease,
               var := !!var) |>
        group_by(var, disease, STATE) |>
        distinct() |>
        collect() |>
        filter(var %in% var_codes$CODE) |>
        summarise(num = sum(disease)) |>
        filter(disease == 1,
               var != "") |>
        select(-disease) |>
        full_join(annotation, by = c("var" = "CODE", "STATE" = "STATE")) |>
        ungroup() |>
        select(-STATE, -var, -disease) |>
        rename(CASES = num,
               STATE = NAME) |>
        mutate(CASES = if_else(is.na(CASES), 0, CASES))

    rates <- inner_join(population, ccaei103) |>
        mutate(DS_RATE = round(CASES / STD_POP_DS, 4),
               CS_RATE = round(CASES / STD_POP_CS, 4)) |>
        select(STATE, !!var, CASES, CENSUSPOP, DS_POP, CS_RATE, DS_RATE) |>
        write_csv(rate_output_file)

    rates
}

diseases <- c("depression", "regional_enteritis", "alcohol_dependence", "anxiety",
              "bipolar", "diabetes", "drug_dependence", "hypertension",
              "lipid_metabolism", "obesity", "schizophrenia", "suicide")

diseases |>
    walk(~ calculate_state_incidences(.x, "EESTATU"))



