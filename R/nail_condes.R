#' @importFrom dplyr mutate across where case_when filter desc arrange
#' @importFrom glue glue
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr slice_sample group_by select ungroup
#' @importFrom stats quantile
#' @importFrom FactoMineR condes

# ---------------------------------------------------------------------------
# get_bins (Helper Function)
# ---------------------------------------------------------------------------
# This helper function is specific to nail_condes.
# It discretizes quantitative variables into categories.
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom dplyr case_when
get_bins = function(dataset, keep, quanti.threshold, quanti.cat){

  dta = dataset |>
    # CORRECTION: '.keep = "all"' conserve les variables non-numeriques (ex: 'Soil')
    # '.keep = "unused"' les supprimait, ce qui est un bug.
    mutate(across(where(is.numeric), scale), .keep = 'all')

  dta = dta |>
    mutate(across(where(is.numeric),
                  ~as.factor(case_when(
                    . >= quanti.threshold ~ quanti.cat[1],
                    . <= - quanti.threshold ~ quanti.cat[2],
                    .default = quanti.cat[3]))))

  # We assume 'keep' is the index
  dta = dta %>% cbind(dataset[, keep, drop = FALSE], .)

  return(dta)
}


# ---------------------------------------------------------------------------
# get_sentences_condes (REFACTORED)
# ---------------------------------------------------------------------------
#' @importFrom dplyr filter arrange desc
get_sentences_condes = function(res_cd, sample.pct){

  res_cd <- res_cd$category

  # Return early if no significant categories
  if (is.null(res_cd) || nrow(as.data.frame(res_cd)) == 0) {
    # This error will be caught by the tryCatch in the main function
    stop("No significant differences found.")
  }

  res_mat <- as.data.frame(res_cd)

  # --- SAMPLING ---
  # Use the global 'sample_numeric_distribution' from utils-formatting.R
  # We sample on 'Estimate' (column 1)
  if (sample.pct < 1) {
    res_sampled <- sample_numeric_distribution(
      res_mat,
      num_var_index = 1, # 'Estimate' is the first col
      sample_pct = sample.pct,
      method = "stratified",
      bins = 5,
      return_matrix = FALSE
    )
  } else {
    res_sampled <- res_mat
  }

  # --- SPLIT DATA ---
  # Keep the user's logic of splitting left/right

  # Select only the relevant columns for the table
  cols_to_show <- c("Estimate", "p.value")

  left_df <- res_sampled |>
    dplyr::filter(Estimate < 0) |>
    # MODIFICATION: Tri par p.value (croissant), puis par Estimate (croissant)
    dplyr::arrange(p.value, Estimate)

  right_df <- res_sampled |>
    dplyr::filter(Estimate > 0) |>
    # MODIFICATION: Tri par p.value (croissant), puis par Estimate (decroissant)
    dplyr::arrange(p.value, dplyr::desc(Estimate))

  # --- FORMAT TABLES ---
  # Call the global formatter twice
  # (Assumes format_stats_as_markdown is in utils-formatting.R)

  ppt1 <- format_stats_as_markdown(
    left_df[, cols_to_show, drop = FALSE],
    title = "Features associated with the *Left* side of the scale (Negative Estimate)"
  )

  ppt2 <- format_stats_as_markdown(
    right_df[, cols_to_show, drop = FALSE],
    title = "Features associated with the *Right* side of the scale (Positive Estimate)"
  )

  return(paste(ppt1, ppt2, sep = '\n\n'))
}

# ---------------------------------------------------------------------------
# nail_condes (Main Function - REFACTORED)
# ---------------------------------------------------------------------------
#' Interpret a continuous latent variable
#'
#' Generate an LLM response to analyze a continuous latent variable.
#'
#' @param dataset a data frame made up of at least one quantitative variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the index of the variable to be characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name (e.g., 'llama3').
#' @param quanti.threshold the threshold above (resp. below) which a scaled variable is considered significantly above (resp.below) the average. Used when converting continuous variables to categorical ones.
#' @param quanti.cat a vector of the 3 possible categories for continuous variables converted to categorical ones according to the threshold. Default is "Significantly above average", "Significantly below average", "Average".
#' @param sample.pct the proportion of features to be sampled.
#' @param weights weights for the individuals (see [FactoMineR::condes()]).
#' @param proba the significance threshold considered to characterize the category (by default 0.05).
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#' @param ... Additional arguments passed to 'ollamar::generate'
#' (e.g., `temperature`, `seed`).
#'
#' @return A data frame containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function (when generate=TRUE) sends a prompt to an Ollama LLM.
#'
#' @export
#' @examples
#'\dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: decathlon dataset ###
#'
#' library(FactoMineR)
#' data(decathlon)
#'
#' names(decathlon) <- c('Time taken to complete the 100m',
#' 'Distance reached for the long jump',
#' 'Distance reached for the shot put',
#' 'Height reached for the high jump',
#' 'Time taken to complete the 400m',
#' 'Time taken to complete the 110m hurdle',
#' 'Distance reached for the discus',
#' 'Height reached for the pole vault',
#' 'Distance reached for the javeline',
#' 'Time taken to complete the 1500 m',
#' 'Rank/Counter-performance indicator',
#' 'Points', 'Competition')
#'
#' res_pca_deca <- FactoMineR::PCA(decathlon,
#' quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
#' plot.PCA(res_pca_deca, choix = 'var')
#' deca_work <- res_pca_deca$ind$coord |> as.data.frame()
#' deca_work <- deca_work[,1] |> cbind(decathlon)
#'
#' intro_deca <- "A study was led on athletes
#' participating in a decathlon event.
#' Their performance was assessed on each part of the decathlon,
#' and they were all placed on an unidimensional scale."
#' intro_deca <- gsub('\n', ' ', intro_deca) |>
#' stringr::str_squish()
#'
#' res_deca <- nail_condes(deca_work,
#'                         num.var = 1,
#'                         quanti.threshold = 1,
#'                         quanti.cat = c('High', 'Low', 'Average'),
#'                         introduction = intro_deca,
#'                         generate = TRUE)
#'
#' cat(res_deca$response)
#'
#'
#' ### Example 2: agri_studies dataset ###
#'
#' data(agri_studies)
#'
#' set.seed(1)
#' res_mca_agri <- FactoMineR::MCA(agri_studies, quali.sup = 39:42,
#' level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_agri, choix = 'ind',
#' invisible = c('var', 'quali.sup'), label = 'none')
#'
#' agri_work <- res_mca_agri$ind$coord |> as.data.frame()
#' agri_work <- agri_work[,1] |> cbind(agri_studies)
#'
#' intro_agri <- "These data were collected after a survey
#' on students' expectations of agribusiness studies.
#' Participants had to rank how much they agreed with 38 statements
#' about possible benefits from agribusiness studies;
#' then, they were asked personal questions."
#' intro_agri <- gsub('\n', ' ', intro_agri) |>
#' stringr::str_squish()
#'
#' res_agri <- nail_condes(agri_work,
#'                         num.var = 1,
#'                         introduction = intro_agri,
#'                         generate = TRUE)
#'
#' cat(res_agri$response)
#'
#' ### Example 3: glossophobia dataset ###
#'
#' data(glossophobia)
#'
#' set.seed(1)
#' res_mca_phobia <- FactoMineR::MCA(glossophobia,
#' quali.sup = 26:41, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_phobia, choix = 'ind',
#' invisible = c('var', 'quali.sup'), label = 'none')
#'
#' phobia_work <- res_mca_phobia$ind$coord |> as.data.frame()
#' phobia_work <- phobia_work[,1] |> cbind(glossophobia)
#'
#' intro_phobia <- "These data were collected after a survey
#' on participants' feelings about speaking in public.
#' Participants had to rank how much they agreed with
#' 25 descriptions of speaking in public;
#' then, they were asked personal questions."
#' intro_phobia <- gsub('\n', ' ', intro_phobia) |>
#' stringr::str_squish()
#'
#' res_phobia <- nail_condes(phobia_work,
#'                           num.var = 1,
#'                           introduction = intro_phobia,
#'                           generate = TRUE)
#'
#' cat(res_phobia$response)
#'
#' ### Example 4: beard_cont dataset ###
#'
#' data(beard_cont)
#'
#' set.seed(1)
#' res_ca_beard <- FactoMineR::CA(beard_cont, graph = FALSE)
#' plot.CA(res_ca_beard, invisible = 'col')
#'
#' beard_work <- res_ca_beard$row$coord |> as.data.frame()
#' beard_work <- beard_work[,1] |> cbind(beard_cont)
#'
#' intro_beard <- "These data refer to 8 types of beards.
#' Each beard was evaluated by 62 assessors."
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' req_beard <- "Please explain what differentiates beards
#' on both sides of the scale.
#' Then, give the scale a name."
#' req_beard <- gsub('\n', ' ', req_beard) |>
#' stringr::str_squish()
#'
#' res_beard <- nail_condes(beard_work,
#'                          num.var = 1,
#'                          quanti.threshold = 0.5,
#'                          quanti.cat = c('Very often used', 'Never used', 'Sometimes used'),
#'                          introduction = intro_beard,
#'                          request = req_beard)
#'
#' res_beard
#'
#' ppt <- stringr::str_replace_all(res_beard, 'observations', 'beards')
#' cat(ppt)
#'
#' res_beard <- ollamar::generate(model = 'llama3', prompt = ppt, output = 'text')
#'
#' cat(res_beard)
#' }
#'
nail_condes = function(dataset, num.var,
                       introduction = NULL,
                       request = NULL,
                       model = 'llama3',
                       quanti.threshold = 0,
                       quanti.cat = c("Significantly above average", "Significantly below average", 'Average'),
                       sample.pct = 1,
                       weights = NULL, proba = 0.05,
                       generate = FALSE,
                       ...){

  if (is.null(introduction)) introduction <- "Observations were placed on a quantitative scale."

  if (is.null(request)) request <- 'Please explain what differentiates observations from both sides of the scale. Then give a name to the scale, and briefly explain why you chose that name.'

  # --- Inject the 'How to Read' Guide ---
  GUIDE_CONDES <- paste(
    "## How to Read the Tables",
    "The data shows which descriptive categories are associated with the high or low ends of the quantitative scale.",
    "The 'Variable' and 'Modalite' columns describe a category (e.g., 'Age = High').",
    "",
    "* **Estimate**: This value corresponds to the coordinate of the category on the scale, i.e., the coordinate of the center of gravity of the individuals who chose this category. A positive value means the individuals who chose this category are mostly on the *Right* (high) side of the scale. A negative value means the individuals who chose this category are mostly on the *Left* (low) side.",
    "* **p.value**: The significance level of the association.",
    sep = "\n"
  )
  introduction <- paste(introduction, GUIDE_CONDES, sep = "\n\n---\n\n")
  # --- END Guide ---

  # Preprocess data
  dta = get_bins(
    dataset,
    keep = num.var,
    quanti.threshold = quanti.threshold,
    quanti.cat = quanti.cat
  )

  # This call is based on your original, correct logic.
  # dta[-(num.var + 1)] removes the *binned* Y variable.
  # '1' tells condes to use the first column (the *original* Y) as the variable to characterize.
  res_cd = FactoMineR::condes(dta[-(num.var + 1)], 1,
                              weights = weights, proba = proba)

  # --- Handle 'stop()' gracefully ---
  ppt <- tryCatch({
    get_sentences_condes(res_cd, sample.pct)
  },
  error = function(e) {
    if (grepl("No significant differences", conditionMessage(e))) {
      return("NAILER_NO_RESULTS_FOUND")
    } else {
      # Re-throw any other unexpected error
      stop(e)
    }
  })

  # Build the text block
  body_text <- ""
  if (identical(ppt, "NAILER_NO_RESULTS_FOUND")) {
    body_text <- "*No significant categories were found to be associated with this scale at this probability threshold.*"
  } else {
    body_text <- ppt
  }

  # Build the final prompt
  final_prompt <- glue::glue(
    "# Introduction\n\n{introduction}\n\n",
    "# Task\n\n{request}\n\n",
    "# Data\n\n{body_text}"
  )
  final_prompt <- gsub("\n{3,}", "\n\n", final_prompt)

  # --- Handle generate = FALSE ---
  if (!generate) return(final_prompt)

  # --- Handle generate = TRUE (with no results) ---
  if (identical(ppt, "NAILER_NO_RESULTS_FOUND")) {
    message("Execution halted: No significant differences found. Nothing to generate.")
    return(data.frame(
      model = model,
      created_at = Sys.time(),
      response = "No significant differences found.",
      done = TRUE,
      prompt = final_prompt,
      stringsAsFactors = FALSE
    ))
  }

  # --- Handle generate = TRUE (with results) ---

  # Capture and filter ... args
  extra_args <- list(...)
  valid_ollama_opts <- c("temperature", "top_p", "top_k", "seed",
                         "system", "template", "context", "keep_alive",
                         "stream", "format")
  ollama_api_options <- extra_args[names(extra_args) %in% valid_ollama_opts]

  # Build argument list
  call_args <- c(
    list(
      model = model,
      prompt = final_prompt,
      output = 'df' # Using 'df' as in your original code
    ),
    ollama_api_options
  )

  # Call Ollama safely
  res_llm <- tryCatch({
    do.call(ollamar::generate, call_args)
  }, error = function(e) {
    stop(paste("Ollama API call (generate=TRUE) failed:", conditionMessage(e)))
  })

  # --- MISSING LINES ARE NOW ADDED ---
  res_llm$prompt = final_prompt
  return(res_llm)

}
