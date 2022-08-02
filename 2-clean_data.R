# Filter and Convert Data to Parquet format

library(tidyverse)
library(arrow)

source("m-icd9_codes.R")
source("m-variables.R")

ccaei103 <- readRDS("rds/ccaei103.RDS")
ccaea10a <- readRDS("rds/ccaea10a.RDS")

ccaei103_clean <- ccaei103 |>
    select(any_of(variables)) |>
    drop_na(ENROLID) |>
    pivot_longer(starts_with("DX"), names_to = "DXID", values_to = "DX") |>
    mutate(diagnosis = case_when(
        DX %in% alcohol_dependence ~ "alcohol_dependence",
        DX %in% anxiety ~ "anxiety",
        DX %in% bipolar ~ "bipolar",
        DX %in% depression ~ "depression",
        DX %in% diabetes ~ "Diabetes",
        DX %in% drug_dependence ~ "drug_dependence",
        DX %in% hypertension ~ "hypertension",
        DX %in% lipid_metabolism ~ "lipid_metabolism",
        DX %in% obesity ~ "obesity",
        DX %in% regional_enteritis ~ "regional_enteritis",
        DX %in% schizophrenia ~ "schizophrenia",
        DX %in% suicide ~ "suicide",
        TRUE ~ NA_character_
    ),
    value = if_else(is.na(diagnosis), 0, 1)) |>
    select(-DX, -DXID) |>
    distinct() |>
    pivot_wider(names_from = diagnosis, values_fill = 0) |>
    write_dataset(file.path("parquet", "ccaei103"))

ccaea10a_clean <- ccaea10a |>
    select(any_of(variables)) |>
    drop_na(ENROLID) |>
    write_dataset(file.path("parquet", "ccaea10a"))
