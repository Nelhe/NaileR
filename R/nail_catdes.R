#' @importFrom dplyr mutate filter arrange desc pull select slice_sample group_by n ungroup
#' @importFrom glue glue
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom stats quantile
#' @importFrom rlang sym
#' @importFrom FactoMineR catdes

# ---------------------------------------------------------------------------
# get_sentences_quali
# ---------------------------------------------------------------------------
#' @importFrom dplyr select filter
get_sentences_quali <- function(res_cd, quali.sample, drop.negative) {
  # This function formats the catdes$category output into
  # a Markdown table.
  # The 'drop.negative' flag pre-filters the table.

  res_cd <- res_cd$category
  ppts <- list()

  for (i in seq_along(res_cd)) {
    group_name <- names(res_cd)[i]
    res_mat <- as.data.frame(res_cd[[i]])

    # Check valid matrix (same as original)
    if (!is.data.frame(res_mat) || nrow(res_mat) == 0 || ncol(res_mat) == 0) {
      message(glue::glue("Skipping group {group_name}: empty or invalid data (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Ensure numeric column exists (same as original)
    is_num_col <- sapply(res_mat, is.numeric)
    if (!any(is_num_col)) {
      message(glue::glue("Skipping group {group_name}: no numeric column found (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Sampling step (same as original)
    num_index <- which(is_num_col)[1]
    if (quali.sample < 1) {
      res_cd2 <- sample_numeric_distribution(
        res_mat,
        num_var_index = num_index,
        sample_pct = quali.sample,
        method = "stratified",
        bins = 5,
        return_matrix = FALSE
      )
    } else {
      res_cd2 <- res_mat
    }

    # --- REFACTORED PART ---

    # --- CORRECTION ---
    # The actual column names from FactoMineR do NOT include (%%).
    cols_to_show <- c("Cla/Mod", "Mod/Cla", "Global", "p.value", "v.test")
    # --- END CORRECTION ---

    # Ensure the columns exist before selecting
    cols_exist <- cols_to_show[cols_to_show %in% colnames(res_cd2)]

    if (length(cols_exist) == 0 || !"v.test" %in% colnames(res_cd2)) {
      message(glue::glue("Skipping group {group_name}: no standard stat/v.test columns found."))
      ppts[[group_name]] <- ""
      next
    }

    df_to_format <- res_cd2[, cols_exist, drop = FALSE]

    # Apply drop.negative filter
    if (isTRUE(drop.negative)) {
      df_to_format <- dplyr::filter(df_to_format, .data$v.test > 0)
    }

    # Call the new global formatting utility
    # (Assumed to be in R/utils-formatting.R and imported)
    ppts[[group_name]] <- format_stats_as_markdown(
      df_stats = df_to_format,
      title = "Characteristic Qualitative Variables"
    )
    # --- END REFACTORED PART ---
  }

  return(ppts)
}

# ---------------------------------------------------------------------------
# get_sentences_quanti
# ---------------------------------------------------------------------------
#' @importFrom dplyr filter
get_sentences_quanti <- function(res_cd, quanti.sample, drop.negative) {
  # This function formats the catdes$quanti output into
  # a Markdown table.
  # The 'drop.negative' flag pre-filters the table.

  res_cd <- res_cd$quanti
  ppts <- list()

  for (i in seq_along(res_cd)) {
    group_name <- names(res_cd)[i]
    res_mat <- as.data.frame(res_cd[[i]])

    # Check valid matrix (same as original)
    if (!is.data.frame(res_mat) || nrow(res_mat) == 0 || ncol(res_mat) == 0) {
      message(glue::glue("Skipping group {group_name}: empty or invalid data (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Ensure numeric column exists (same as original)
    is_num_col <- sapply(res_mat, is.numeric)
    if (!any(is_num_col)) {
      message(glue::glue("Skipping group {group_name}: no numeric column found (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Sampling step (same as original)
    num_index <- which(is_num_col)[1]
    if (quanti.sample < 1) {
      res_cd2 <- sample_numeric_distribution(
        res_mat,
        num_var_index = num_index,
        sample_pct = quanti.sample,
        method = "stratified",
        bins = 5,
        return_matrix = FALSE
      )
    } else {
      res_cd2 <- res_mat
    }

    # --- REFACTORED PART ---

    # Select standard columns from catdes$quanti
    # These names were correct, so no change is needed here.
    cols_to_show <- c("Mean in category", "Overall mean", "sd in category", "Overall sd", "p.value", "v.test")

    cols_exist <- cols_to_show[cols_to_show %in% colnames(res_cd2)]

    if (length(cols_exist) == 0 || !"v.test" %in% colnames(res_cd2)) {
      message(glue::glue("Skipping group {group_name}: no standard quanti stat/v.test columns found."))
      ppts[[group_name]] <- ""
      next
    }

    df_to_format <- res_cd2[, cols_exist, drop = FALSE]

    # Apply drop.negative filter
    if (isTRUE(drop.negative)) {
      df_to_format <- dplyr::filter(df_to_format, .data$v.test > 0)
    }

    # Call the new global formatting utility
    # (Assumed to be in R/utils-formatting.R and imported)
    ppts[[group_name]] <- format_stats_as_markdown(
      df_stats = df_to_format,
      title = "Characteristic Quantitative Variables"
    )
    # --- END REFACTORED PART ---
  }

  return(ppts)
}

# ---------------------------------------------------------------------------
# get_prompt_catdes
# ---------------------------------------------------------------------------
get_prompt_catdes <- function(res_cd, introduction, request, isolate.groups, quali.sample, quanti.sample, drop.negative) {

  if ("category" %in% names(res_cd)) {
    # 'drop.negative' argument is now passed down
    stces_quali <- get_sentences_quali(res_cd, quali.sample, drop.negative)
  } else {
    stces_quali <- list()
  }

  if ("quanti" %in% names(res_cd)) {
    # 'drop.negative' argument is now passed down
    stces_quanti <- get_sentences_quanti(res_cd, quanti.sample, drop.negative)
  } else {
    stces_quanti <- list()
  }

  if (length(stces_quali) == 0 && length(stces_quanti) == 0) {
    stop("No significant differences between groups, execution was halted.")
  }

  all_groups <- union(names(stces_quali), names(stces_quanti))
  stces <- c()

  for (grp in all_groups) {
    qual <- stces_quali[[grp]]
    quant <- stces_quanti[[grp]]
    if (is.null(qual) || nchar(trimws(qual)) == 0) qual <- NULL
    if (is.null(quant) || nchar(trimws(quant)) == 0) quant <- NULL

    block_parts <- c(qual, quant)
    block_parts <- block_parts[!sapply(block_parts, is.null)]

    block <- paste(block_parts, collapse = "\n\n")

    if (nchar(block) > 0) {
      ppt_grp <- glue::glue('## Group "{grp}":\n\n{block}')
      stces <- c(stces, ppt_grp)
    }
  }

  body <- if (isolate.groups) {
    paste(stces, collapse = "\n\n---\n\n")
  } else {
    paste(stces, collapse = "\n\n")
  }

  header <- glue::glue(
    "# Introduction\n\n{introduction}\n\n",
    "# Task\n\n{request}\n\n",
    "# Data\n\n"
  )

  if (!isolate.groups) {
    return(paste0(header, body))
  } else {
    prompts <- list()
    for (grp in all_groups) {
      qual <- stces_quali[[grp]]
      quant <- stces_quanti[[grp]]
      if (is.null(qual) || nchar(trimws(qual)) == 0) qual <- NULL
      if (is.null(quant) || nchar(trimws(quant)) == 0) quant <- NULL

      block_parts <- c(qual, quant)
      block_parts <- block_parts[!sapply(block_parts, is.null)]
      block_parts <- block_parts[nzchar(trimws(block_parts))]

      block <- paste(block_parts, collapse = "\n\n")

      if (nchar(block) > 0) {
        grp_intro <- glue::glue("# Introduction\n\n{introduction}\n\n")
        grp_task  <- glue::glue("# Task\n\n{request}\n\n")
        grp_data  <- glue::glue("# Data\n\n## Group \"{grp}\":\n\n{block}")
        prompts[[grp]] <- paste0(grp_intro, grp_task, grp_data)
      }
    }
    return(prompts)
  }
}

# ---------------------------------------------------------------------------
# nail_catdes (Main Function)
# ---------------------------------------------------------------------------
#' Interpret a categorical latent variable
#'
#' Generate an LLM response to analyze a categorical latent variable.
#'
#' @param dataset a data frame made up of at least one categorical variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the index of the variable to be characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name for Ollama (e.g., 'llama3').
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per category. Recommended with long catdes results.
#' @param quali.sample from 0 to 1, the proportion of qualitative features that are randomly kept.
#' @param quanti.sample from 0 to 1, the proportion of quantitative features that are randomly kept.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests).
#' @param proba the significance threshold considered to characterize the categories (by default 0.05).
#' @param row.w a vector of integers corresponding to an optional row weights (by default, a vector of 1 for uniform row weights)
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#' @param ... Additional arguments passed to 'ollamar::generate'
#' (e.g., `temperature`, `seed`).
#'
#' @return A data frame, or a list of data frames, containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function (when generate=TRUE) sends a prompt to an Ollama LLM.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: Fisher's iris ###
#' library(NaileR)
#' data(iris)
#'
#' intro_iris <- "A study measured various parts of iris flowers
#' from 3 different species: setosa, versicolor and virginica.
#' I will give you the results from this study.
#' You will have to identify what sets these flowers apart."
#' intro_iris <- gsub('\n', ' ', intro_iris) |>
#' stringr::str_squish()
#'
#' req_iris <- "Please explain what makes each species distinct.
#' Also, tell me which species has the biggest flowers,
#' and which species has the smallest."
#' req_iris <- gsub('\n', ' ', req_iris) |>
#' stringr::str_squish()
#'
#' res_iris <- nail_catdes(iris,
#'                         num.var = 5,
#'                         introduction = intro_iris,
#'                         request = req_iris,
#'                         generate = TRUE)
#'
#' cat(res_iris$response)
#'
#' ### Example 2: food waste dataset ###
#'
#' library(FactoMineR)
#'
#' data(waste)
#' waste <- waste[-14]    # no variability on this question
#'
#' set.seed(1)
#' res_mca_waste <- MCA(waste, quali.sup = c(1,2,50:76),
#' ncp = 35, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_waste, choix = "ind",
#' invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_waste <- HCPC(res_mca_waste, nb.clust = 3, graph = FALSE)
#' plot.HCPC(res_hcpc_waste, choice = "map", draw.tree = FALSE,
#' ind.names = FALSE)
#' don_clust_waste <- res_hcpc_waste$data.clust
#'
#' intro_waste <- 'These data were collected
#' after a survey on food waste,
#' with participants describing their habits.'
#' intro_waste <- gsub('\n', ' ', intro_waste) |>
#' stringr::str_squish()
#'
#' req_waste <- 'Please summarize the characteristics of each group.
#' Then, give each group a new name, based on your conclusions.
#' Finally, give each group a grade between 0 and 10,
#' based on how wasteful they are with food:
#' 0 being "not at all", 10 being "absolutely".'
#' req_waste <- gsub('\n', ' ', req_waste) |>
#' stringr::str_squish()
#'
#' res_waste <- nail_catdes(don_clust_waste,
#'                          num.var = ncol(don_clust_waste),
#'                          introduction = intro_waste,
#'                          request = req_waste,
#'                          drop.negative = TRUE,
#'                          generate = TRUE)
#'
#' cat(res_waste$response)
#'
#'
#' ### Example 3: local_food dataset ###
#'
#' data(local_food)
#'
#' set.seed(1)
#' res_mca_food <- MCA(local_food, quali.sup = 46:63,
#' ncp = 100, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_food, choix = "ind",
#' invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_food <- HCPC(res_mca_food, nb.clust = 3, graph = FALSE)
#' plot.HCPC(res_hcpc_food, choice = "map", draw.tree = FALSE,
#' ind.names = FALSE)
#' don_clust_food <- res_hcpc_food$data.clust
#'
#' intro_food <- 'A study on sustainable food systems
#' was led on several French participants.
#' This study had 2 parts. In the first part,
#' participants had to rate how acceptable
#' "a food system that..." (e.g, "a food system that
#' only uses renewable energy") was to them.
#' In the second part, they had to say
#' if they agreed or disagreed with some statements.'
#' intro_food <- gsub('\n', ' ', intro_food) |>
#' stringr::str_squish()
#'
#' req_food <- 'I will give you the answers from one group.
#' Please explain who the individuals of this group are,
#' what their beliefs are.
#' Then, give this group a new name,
#' and explain why you chose this name.
#' Do not use 1st person ("I", "my"...) in your answer.'
#' req_food <- gsub('\n', ' ', req_food) |>
#' stringr::str_squish()
#'
#' res_food <- nail_catdes(don_clust_food,
#'                         num.var = 64,
#'                         introduction = intro_food,
#'                         request = req_food,
#'                         isolate.groups = TRUE,
#'                         drop.negative = TRUE,
#'                         generate = TRUE)
#'
#' res_food[[1]]$response |> cat()
#' res_food[[2]]$response |> cat()
#' res_food[[3]]$response |> cat()
#' }
nail_catdes = function(dataset, num.var,
                       introduction = NULL,
                       request = NULL,
                       model = 'llama3',
                       isolate.groups = FALSE,
                       quali.sample = 1,
                       quanti.sample = 1,
                       drop.negative = FALSE,
                       proba = 0.05,
                       row.w = NULL,
                       generate = FALSE,
                       ...
){

  # Default introduction
  if (is.null(introduction)) {
    introduction <- "For this study, observations were grouped according to their similarities."
  }

  # --- Inject the 'How to Read' Guide ---
  GUIDE_CATDES <- paste(
    "## How to Read the Tables",
    "The tables show the characteristic features of each group.",
    "",
    "### Characteristic Qualitative Variables",
    "* **Cla/Mod**: Percentage of individuals who selected this modality AND belong to this class.",
    "* **Mod/Cla**: Percentage of individuals WITHIN this class who selected this modality.",
    "* **Global**: Overall percentage of individuals (all classes) who selected this modality.",
    "* **p.value**: Significance level of the test.",
    "* **v.test**: Test value. A positive value (e.g., > +2) means the modality is overrepresented. A negative value (e.g., < -2) means it is underrepresented.",
    "",
    "### Characteristic Quantitative Variables",
    "* **Mean in category**: The average value of the variable for this group.",
    "* **Overall mean**: The average value of the variable for the entire dataset.",
    "* **sd in category**: The standard deviation of the variable for this group.",
    "* **Overall sd**: The standard deviation of the variable for the entire dataset.",
    "* **p.value**: Significance level of the test.",
    "* **v.test**: Test value. A positive value means the group has a significantly higher mean. A negative value means a significantly lower mean.",
    sep = "\n"
  )
  introduction <- paste(introduction, GUIDE_CATDES, sep = "\n\n---\n\n")
  # --- END Guide ---

  # Default request logic
  if (isolate.groups == FALSE){
    if (is.null(request)) request <- "Based on the results, please describe what characterize the observations of each group and what set them apart from the other groups. Then, based on these characteristics, give each group a new name."
  } else {
    if (is.null(request)) request <- "Based on the results, please describe what characterize the observations of this group and what make them so special. Then, based on these characteristics, give the group a new name."
  }

  # Run FactoMineR analysis
  res_cd = FactoMineR::catdes(dataset, num.var = num.var, proba = proba, row.w = row.w)

  # --- Handle 'stop()' gracefully ---

  # 1. We use tryCatch to capture the "stop()" call
  ppt <- tryCatch({
    get_prompt_catdes(
      res_cd,
      introduction = introduction,
      request = request,
      isolate.groups = isolate.groups,
      quali.sample = quali.sample,
      quanti.sample = quanti.sample,
      drop.negative = drop.negative
    )
  },
  # 2. This 'error' function is triggered IF get_prompt_catdes stops
  error = function(e) {
    # Check if it's the specific error we expect
    if (grepl("No significant differences", conditionMessage(e))) {
      # If so, return a special marker instead of crashing
      return("NAILER_NO_RESULTS_FOUND")
    } else {
      # If it's a *different* error, let it crash (to debug)
      stop(e)
    }
  })

  # 3. Check if our special marker was returned
  if (identical(ppt, "NAILER_NO_RESULTS_FOUND")) {

    no_results_message <- "*No significant differences were found between the groups at this probability threshold.*"

    if (generate) {
      # If generating, stop with a friendly message (not an error)
      message("Execution halted: No significant differences found. Nothing to generate.")

      # Return an empty, valid object
      if (isolate.groups) {
        return(list())
      } else {
        return(data.frame(
          model = model,
          created_at = Sys.time(),
          response = "No significant differences found.",
          done = TRUE,
          prompt = no_results_message,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # If generate=FALSE, return the prompt with the "no results" message
      header <- glue::glue(
        "# Introduction\n\n{introduction}\n\n",
        "# Task\n\n{request}\n\n",
        "# Data\n\n"
      )
      return(paste0(header, no_results_message))
    }
  }
  # --- END MODIFICATION ---

  # If ppt is NOT the error marker, the function continues as normal
  if (!generate) return(ppt)

  # --- CORRECTED LLM CALL (Ollama-only) ---

  # 1. Capture additional arguments passed to '...'
  extra_args <- list(...)

  # 2. Filter for valid ollamar arguments only
  valid_ollama_opts <- c("temperature", "top_p", "top_k", "seed",
                         "system", "template", "context", "keep_alive",
                         "stream", "format")

  ollama_api_options <- extra_args[names(extra_args) %in% valid_ollama_opts]

  if (isolate.groups == FALSE) {

    # 3. Build the full argument list for do.call
    call_args <- c(
      list(
        model = model,
        prompt = ppt,
        output = 'df' # Using 'df' as in your original code
      ),
      ollama_api_options # Add filtered options like temperature, seed
    )

    # 4. Call ollamar::generate safely
    res_llm <- tryCatch({
      do.call(ollamar::generate, call_args)
    }, error = function(e) {
      stop(paste("Ollama API call (generate=TRUE) failed:", conditionMessage(e)))
    })

    res_llm$prompt = ppt
    return(res_llm)

  } else {
    # 'ppt' is a LIST of prompts
    list_rep = list()

    for (i in seq_along(ppt)) {
      prpt <- ppt[[i]]

      # 3. Build argument list for the loop
      call_args_loop <- c(
        list(
          model = model,
          prompt = prpt,
          output = 'df'
        ),
        ollama_api_options
      )

      # 4. Call ollamar::generate safely in loop
      res_llm <- tryCatch({
        do.call(ollamar::generate, call_args_loop)
      }, error = function(e) {
        stop(paste("Ollama API call (generate=TRUE, isolate.groups=TRUE) failed:", conditionMessage(e)))
      })

      res_llm$prompt = prpt

      # Use the name from the 'ppt' list to add the element
      if (!is.null(names(ppt)) && names(ppt)[i] != "") {
        list_rep[[names(ppt)[i]]] <- res_llm
      } else {
        # Fallback if there are no names
        list_rep[[length(list_rep) + 1]] <- res_llm
      }
    }
    return(list_rep)
  }
  # --- END CORRECTED LLM CALL ---
}
