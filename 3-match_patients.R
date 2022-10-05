# Get matched patients
#


library(MatchIt)
library(tidyverse)
library(arrow)

match_patients <- function(disease) {
    dataset <- open_dataset("parquet/ccaei103") |>
        select(ENROLID, !!disease, AGE, SEX) |>
        filter(AGE <= 65, AGE >= 18) |>
        distinct() |>
        collect() |>
        mutate(SEX = as.numeric(SEX) - 1) |>
        drop_na(ENROLID, AGE, SEX) |>
        rename_with(str_to_upper)

    output_file <- file.path("datastore",
                             str_glue("matched_ibm_{disease}.RDS"))

    output_data_file <- file.path("results",
                                  "matched",
                                  str_glue("matched_{disease}.csv"))

    match_formula <- as.formula(str_glue("{str_to_upper(disease)} ~ SEX + AGE"))

    if (file.exists(output_file)) {
        model <- readRDS(output_file)
    } else {
        model <- matchit(formula = match_formula, data = dataset)
        saveRDS(model, file = output_file)
    }

    matched <- match.data(model) |>
        write_csv(output_data_file)

    out <- list(
        match_model = model,
        model_data = matched
    )
    out
}


diseases <- c("depression", "regional_enteritis", "alcohol_dependence", "anxiety",
              "bipolar", "diabetes", "drug_dependence", "hypertension",
              "lipid_metabolism", "obesity", "schizophrenia", "suicide")


walk(diseases, match_patients)
