# Filter and Convert Data to Parquet format

library(tidyverse)
library(arrow)

source("icd9_codes.R")
source("variables.R")

ccaei103 <- readRDS("rds/ccaei103.RDS")
ccaea10a <- readRDS("rds/ccaea10a.RDS")

ccaei103_clean <- ccaei103 |>
    select(any_of(variables)) |>
    drop_na(ENROLID) |>
    pivot_longer(starts_with("DX"), names_to = "DXID", values_to = "DX") |>
    mutate(alcohol_dependence = if_else(DX %in% alcohol_dependence |
                                            PDX %in% alcohol_dependence, 1, 0),
           anxiety = if_else(DX %in% anxiety |
                                            PDX %in% anxiety, 1, 0),
           bipolar = if_else(DX %in% bipolar |
                                            PDX %in% bipolar, 1, 0),
           depression = if_else(DX %in% depression |
                                            PDX %in% depression, 1, 0),
           diabetes = if_else(DX %in% diabetes |
                                            PDX %in% diabetes, 1, 0),
           drug_dependence = if_else(DX %in% drug_dependence |
                                            PDX %in% drug_dependence, 1, 0),
           hypertension = if_else(DX %in% hypertension |
                                            PDX %in% hypertension, 1, 0),
           lipid_metabolism = if_else(DX %in% lipid_metabolism |
                                            PDX %in% lipid_metabolism, 1, 0),
           obesity = if_else(DX %in% obesity |
                                            PDX %in% obesity, 1, 0),
           regional_enteritis = if_else(DX %in% regional_enteritis |
                                            PDX %in% regional_enteritis, 1, 0),
           schizophrenia = if_else(DX %in% schizophrenia |
                                            PDX %in% schizophrenia, 1, 0),
           suicide = if_else(DX %in% suicide |
                                            PDX %in% suicide, 1, 0)) |>
    write_dataset(file.path("parquet", "ccaei103"))

ccaea10a_clean <- ccaea10a |>
    select(any_of(variables)) |>
    drop_na(ENROLID) |>
    write_dataset(file.path("parquet", "ccaea10a"))
