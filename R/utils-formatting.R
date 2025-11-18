#' @importFrom stringr str_replace_all str_squish str_split_fixed
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate across where
#' @noRd
parse_factominer_rownames <- function(text) {
  # This helper function parses complex rownames from FactoMineR
  # e.g., "var.name=modality_one" or "quantitative.var"
  # It returns a list with two elements: Variable and Modalite.

  # Split into max 2 parts at the first '='
  # 'n = 2' ensures that any '=' in the modality name is kept
  parts <- stringr::str_split_fixed(text, "=", n = 2)

  # Clean the variable name (part 1)
  # Replace all dots with spaces and trim whitespace
  var_name <- stringr::str_replace_all(parts[1, 1], "\\.", " ")
  var_name <- stringr::str_squish(var_name)

  mod_name <- NA_character_

  if (nzchar(parts[1, 2])) {
    # A modality exists (part 2)
    # Replace all dots and underscores with spaces and trim
    mod_name <- stringr::str_replace_all(parts[1, 2], "[\\._]", " ")
    mod_name <- stringr::str_squish(mod_name)
  }

  return(list(Variable = var_name, Modalite = mod_name))
}


#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate across where
#' @noRd
format_stats_as_markdown <- function(
    df_stats,
    title = "Statistical Results",
    round_digits = 2
) {
  # This helper function converts any statistics data frame
  # (from catdes, condes, etc.) into a clean Markdown table.
  # It uses 'parse_factominer_rownames' to clean the item names.

  if (is.null(df_stats) || nrow(df_stats) == 0) {
    return(paste0("### ", title, "\n\n*No significant data to display.*\n"))
  }

  # --- 1. Clean Rownames and Round ---
  df_proc <- df_stats |>
    # Round all numeric columns to keep the table clean
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., digits = round_digits))) |>
    tibble::rownames_to_column(var = "RawItem")

  # --- 2. Parse Rownames into new columns ---
  # do.call(rbind, ...) is a fast way to apply the list-returning
  # function to each row and bind the results into a data frame.
  parsed_cols <- do.call(rbind, lapply(df_proc$RawItem, parse_factominer_rownames))
  df_proc <- cbind(as.data.frame(parsed_cols), df_proc)

  # --- 3. Select and Reorder Columns ---
  # We want "Variable" and "Modalite" first, then the stats.
  # Drop "RawItem" and "Modalite" if all are NA (i.e., quantitative data)

  stat_cols <- colnames(df_stats) # Get original stat column names

  if (all(is.na(df_proc$Modalite))) {
    # Quantitative case: Only keep 'Variable' and stats
    df_final <- df_proc[, c("Variable", stat_cols)]
  } else {
    # Qualitative case: Keep 'Variable', 'Modalite', and stats
    df_final <- df_proc[, c("Variable", "Modalite", stat_cols)]

    # Replace any potential NAs in Modalite with a dash
    df_final$Modalite[is.na(df_final$Modalite)] <- "-"
  }

  # --- 4. Build Markdown Table String ---

  # Create header (e.g., "| Variable | Modalite | v.test |")
  header <- paste("|", paste(colnames(df_final), collapse = " | "), "|")

  # Create separator (e.g., "| --- | --- | --- |")
  separator <- paste("|", paste(rep("---", ncol(df_final)), collapse = " | "), "|")

  # Create all content rows
  rows <- apply(df_final, 1, function(row) {
    # Replace any remaining NAs with a neutral string for the table
    row[is.na(row)] <- "NA"
    paste("|", paste(row, collapse = " | "), "|")
  })

  # Combine all parts into a single string
  md_table <- paste(
    header,
    separator,
    paste(rows, collapse = "\n"),
    sep = "\n"
  )

  return(paste0("### ", title, "\n\n", md_table, "\n"))
}

# ---------------------------------------------------------------------------
# sample_numeric_distribution
# ---------------------------------------------------------------------------
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr slice_sample group_by
#' @importFrom stats quantile
#' @importFrom rlang sym
sample_numeric_distribution <- function(data, num_var_index, sample_pct, method = "stratified", bins = 5, return_matrix = TRUE, seed = NULL) {

  # Extract variable name
  num_var <- colnames(data)[num_var_index]

  # Ensure numeric variable
  if (!is.numeric(data[[num_var]])) {
    stop("The selected column is not numeric.")
  }

  # Convert row names to a column to preserve them
  data <- tibble::rownames_to_column(data, var = "OriginalRowName")

  # Set seed for reproducibility if specified
  if (!is.null(seed)) set.seed(seed)

  # Calculate sample size based on percentage
  sample_size <- max(1, round(nrow(data) * sample_pct))

  if (method == "probability") {
    data$prob <- data[[num_var]] - min(data[[num_var]], na.rm = TRUE) + 1
    data$prob <- data$prob / sum(data$prob, na.rm = TRUE)
    sampled_data <- data[sample(1:nrow(data), size = sample_size, prob = data$prob, replace = FALSE), ]

  } else if (method == "stratified") {
    bins <- min(bins, length(unique(data[[num_var]])) - 1)

    if (bins < 1) {
      warning("Not enough unique values for stratified sampling. Defaulting to random sampling.")
      sampled_data <- dplyr::slice_sample(data, n = sample_size, replace = FALSE)
    } else {
      breaks <- unique(quantile(data[[num_var]], probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))

      if (length(breaks) <= 1) {
        warning("Insufficient variation in data. Using random sampling.")
        sampled_data <- dplyr::slice_sample(data, n = sample_size, replace = FALSE)
      } else {
        data$bin <- cut(data[[num_var]], breaks = breaks, include.lowest = TRUE, labels = FALSE)

        sampled_data <- data %>%
          dplyr::group_by(bin) %>%
          dplyr::filter(dplyr::n() > 0) %>%
          dplyr::slice_sample(n = max(1, round(sample_size / bins)), replace = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(-bin)
      }
    }
  } else {
    stop("Invalid method. Choose 'probability' or 'stratified'.")
  }

  sampled_data <- sampled_data %>%
    dplyr::arrange(dplyr::desc(.data[[num_var]])) %>%
    tibble::column_to_rownames(var = "OriginalRowName")

  if (return_matrix) {
    return(as.matrix(sampled_data))
  } else {
    return(sampled_data)
  }
}
