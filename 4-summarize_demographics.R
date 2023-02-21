# Summarize Demographic Information

library(arrow)
library(tidyverse)

report_total_counts <- function(disease) {
    step_one <- open_dataset("parquet/ccaei103") |>
        collect()
    step_two <- step_one |>
        select(ENROLID, !!disease, AGE, SEX) |>
        drop_na(ENROLID)
    step_three <- step_two |>
        filter(AGE <= 65, AGE >= 18)
    step_four <- step_three |>
        distinct()
    step_five <- step_four |>
        mutate(SEX = as.numeric(SEX) - 1) |>
        drop_na(ENROLID, AGE, SEX)


    out <- list(
        total_cases = nrow(step_one),
        disease_cases = nrow(step_two),
        aged_cases = nrow(step_three),
        unique_cases = nrow(step_four),
        nonmissing_cases = nrow(step_five)
    )

    cat(
        str_glue(
            "

             {str_to_title(disease) |> str_replace_all('_', ' ')} Report
             Total records: {out$total_cases}
             Records with {disease}: {out$disease_cases}
             Records between 18 and 65 (Inclusive): {out$aged_cases}
             Unique Patients: {out$unique_cases}
             Unique Patients with Complete Data: {out$nonmissing_cases}
             ---------

             "
        )
    )

}

report_matched_counts <- function(disease) {
    disease_data_file <- file.path("results",
                                   "matched",
                                   str_glue("matched_{disease}.csv"))

    matched_data <- read_csv(disease_data_file, show_col_types = FALSE)

    total_group <- nrow(matched_data)
    total_cases <- sum(matched_data[[str_to_upper(disease)]])

    case_male <- matched_data |>
        filter(SEX == 0) |> pull(2) |> sum()
    case_female <- matched_data |>
        filter(SEX == 1) |> pull(2) |> sum()

    out <- list(
        case_male = case_male,
        ctrl_male = case_male,
        case_female = case_female,
        ctrl_female = case_female,
        total_records = total_group,
        total_cases = total_cases
    )

    cat(
        str_glue(
            "

{str_to_title(disease) |> str_replace_all('_', ' ')} Demographic Report
Total records: {out$total_records}
Records with {disease}: {out$total_cases}

Breakdown
\t Control \t Case
Male \t {out$ctrl_male} \t\t {out$case_male}
Female \t {out$ctrl_female} \t\t {out$case_female}
---------

             "
        )
    )
}



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

diseases |>
    walk(report_total_counts)

diseases |>
    walk(report_matched_counts)
