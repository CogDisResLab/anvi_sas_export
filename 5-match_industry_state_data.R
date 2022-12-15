# Match the Enteritis and Depression data by Industry

library(tidyverse)

diseases <-
    c(
        "depression",
        "regional_enteritis",
        "alcohol_dependence",
        "anxiety",
        "bipolar",
        "diabetes",
        "drug_dependence",
        "hypertension",
        "lipid_metabolism",
        "obesity",
        "schizophrenia",
        "suicide"
    )

vars <- c("INDSTRY", "EECLASS", "EESTATU")

inputs <- expand_grid(disease_a = diseases, disease_b = diseases, var = vars) |>
    filter(disease_a != disease_b)

ranked_correlation <- function(disease_a, disease_b, var) {
    disease_a_data_file <- file.path(
        "results",
        disease_a,
        "incidence",
        var,
        str_glue("{disease_a}_incidence_rates_{str_to_lower(var)}.csv")
    )

    disease_b_data_file <- file.path(
        "results",
        disease_b,
        "incidence",
        var,
        str_glue("{disease_b}_incidence_rates_{str_to_lower(var)}.csv")
    )

    disease_a_data <- read_csv(disease_a_data_file, show_col_types = FALSE) |>
        select(STATE, !!var, DS_RATE) |>
        rename(!!disease_a := DS_RATE,
               var := !!var)

    disease_b_data <- read_csv(disease_b_data_file, show_col_types = FALSE) |>
        select(STATE, !!var, DS_RATE) |>
        rename(!!disease_b := DS_RATE,
               var := !!var)

    combined <- full_join(disease_a_data, disease_b_data, by = c("STATE", "var")) |>
        group_by(var) |>
        nest() |>
        mutate(var_wise = map(data, ~ cor(.x[[2]], .x[[3]], method = "spearman"))) |>
        select(Value := var, Correlation = var_wise) |>
        ungroup() |>
        unnest(cols = c(Correlation)) |>
        mutate(Correlation = round(Correlation, 4),
               Variable = var,
               Reference = disease_a,
               Comparison = disease_b)

    combined
}

ranked <- inputs |>
    pmap_dfr(~ ranked_correlation(..1, ..2, ..3)) |>
    write_csv("results/correlations_across_variables.csv")

