# Get matched patients
#
#
#

library(MatchIt)
library(tidyverse)
library(arrow)


# Matched Depression
dataset <- open_dataset("parquet/ccaei103") |>
    select(ENROLID, depression, AGE, SEX) |>
    filter(AGE <= 65, AGE >= 18) |>
    distinct() |>
    collect() |>
    mutate(SEX = as.numeric(SEX) - 1) |>
    drop_na(ENROLID, AGE, SEX) |>
    rename_with(str_to_upper)


if (file.exists("datastore/matched_ibm_depression.RData")) {
    load("datastore/matched_ibm_depression.RData")
} else {
    model <- matchit(DEPRESSION ~ SEX + AGE, data = dataset)
    save(model, file = "datastore/matched_ibm_depression.RData")
}

matched <- match.data(model) |>
    write_csv("results/matched/matched_depression.csv")

# Matched Enteritis
dataset <- open_dataset("parquet/ccaei103") |>
    select(ENROLID, regional_enteritis, AGE, SEX) |>
    filter(AGE <= 65, AGE >= 18) |>
    distinct() |>
    collect() |>
    mutate(SEX = as.numeric(SEX) - 1) |>
    drop_na(ENROLID, AGE, SEX) |>
    rename_with(str_to_upper)


if (file.exists("datastore/matched_ibm_enteritis.RData")) {
    load("datastore/matched_ibm_enteritis.RData")
} else {
    model <- matchit(REGIONAL_ENTERITIS ~ SEX + AGE, data = dataset)
    save(model, file = "datastore/matched_ibm_enteritis.RData")
}

matched <- match.data(model) |>
    write_csv("results/matched/matched_regional_enteritis.csv")
