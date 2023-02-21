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

inputs <-
    expand_grid(disease_a = diseases,
                disease_b = diseases,
                var = vars) |>
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

    disease_a_data <-
        read_csv(disease_a_data_file, show_col_types = FALSE) |>
        select(STATE, !!var, DS_RATE) |>
        rename(!!disease_a := DS_RATE,
               var := !!var)

    disease_b_data <-
        read_csv(disease_b_data_file, show_col_types = FALSE) |>
        select(STATE, !!var, DS_RATE) |>
        rename(!!disease_b := DS_RATE,
               var := !!var)

    combined <-
        full_join(disease_a_data, disease_b_data, by = c("STATE", "var")) |>
        group_by(var) |>
        nest() |>
        mutate(var_wise = map(data, ~ cor(.x[[2]], .x[[3]], method = "spearman"))) |>
        select(Value := var, Correlation = var_wise) |>
        ungroup() |>
        unnest(cols = c(Correlation)) |>
        mutate(
            Correlation = round(Correlation, 4),
            Variable = var,
            Reference = disease_a,
            Comparison = disease_b
        )

    combined
}

make_heatmap <- function(variable, category, df) {
    g <-
        ggplot(df,
               aes(
                   x = Reference,
                   y = Comparison,
                   fill = Correlation,
                   label = Correlation
               ))

    variable_name <- if (variable == "INDSTRY") {
        "Industry Type"
    } else if (variable == "EECLASS") {
        "Employment Classification"
    } else if (variable == "EESTATU") {
        "Employment Status"
    } else {
        variable
    }

    p <- g + geom_tile(width = 0.8) +
        geom_text() +
        scale_fill_gradient2(
            low = "orange",
            mid = "white",
            high = scales::muted("red"),
            limits = c(-1, 1)
        ) +
        scale_x_discrete(labels = \(x) {
            x |> str_replace_all("_", "\n") |> str_to_title()
        }) +
        scale_y_discrete(labels = \(x) {
            x |> str_replace_all("_", "\n") |> str_to_title()
        }) +
        theme_minimal() +
        ggtitle(
            str_glue("Disease Correlations for \"{variable_name}\""),
            str_glue("Category: {category}")
        ) +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        )
    normalized_category <- category |>
        str_replace_all("\\W+", "_") |>
        str_to_lower()

    outfile_path <- file.path(
        "figures",
        variable,
        str_glue('{normalized_category}-correlation.png')
    )
    dir.create(dirname(outfile_path), showWarnings = FALSE, recursive = TRUE)
    ggsave(outfile_path, plot = p, width = 12, height = 12, units = "in", bg = "white")
    out <- list(p)
}

v_make_heatmap <- Vectorize(make_heatmap)

ranked <- inputs |>
    pmap_dfr(~ ranked_correlation(..1, ..2, ..3)) |>
    write_csv("results/correlations_across_variables.csv")

split_ranked <- ranked |>
    group_by(Variable) |>
    nest() |>
    mutate(split_data = {
        data |> map(~ {
            group_by(.x, Value) |> nest()
        })
    }) |>
    select(-data) |>
    unnest(split_data) |>
    mutate(plot = v_make_heatmap(Variable, Value, data))
