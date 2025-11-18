#' @importFrom dplyr filter select arrange desc
#' @importFrom glue glue
#' @importFrom tibble rownames_to_column
#' @importFrom utils globalVariables

# Set global variables to avoid R CMD check NOTES
utils::globalVariables(c("v.test", "p.value", ".data"))

# ---------------------------------------------------------------------------
# get_sentences_descfreq (REFACTORED)
# ---------------------------------------------------------------------------
#' @importFrom dplyr filter arrange desc
get_sentences_descfreq <- function(res_df, sample.pct, drop.negative) {

  ppts <- list() # Stores the Markdown strings for each group

  for (i in seq_along(res_df)) {
    grp_name <- names(res_df)[i]
    res_mat <- as.data.frame(res_df[[i]])

    # Check for valid data
    if (is.null(res_mat) || nrow(res_mat) == 0 || !"v.test" %in% colnames(res_mat)) {
      ppts[[grp_name]] <- "This row has **no significant descriptive attributes** (at the p <= 0.05 level)."
      next
    }

    # --- Sampling ---
    # (Assumes sample_numeric_distribution is in utils-formatting.R)
    num_index <- which(colnames(res_mat) == "v.test")
    if (sample.pct < 1) {
      res_sampled <- sample_numeric_distribution(
        res_mat,
        num_var_index = num_index,
        sample_pct = sample.pct,
        method = "stratified",
        bins = 5,
        return_matrix = FALSE # We need a data.frame
      )
    } else {
      res_sampled <- res_mat
    }

    # --- Column Selection ---
    # Select useful columns for the LLM
    cols_to_show <- c("Intern %", "glob %", "p.value", "v.test")
    # Ensure they exist (handle potential edge cases)
    cols_to_show <- cols_to_show[cols_to_show %in% colnames(res_sampled)]

    # --- Split Data ---
    positive_df <- res_sampled |>
      dplyr::filter(v.test > 0) |>
      dplyr::arrange(p.value)

    negative_df <- res_sampled |>
      dplyr::filter(v.test < 0) |>
      dplyr::arrange(p.value)

    # --- Handle "No Results" Text ---
    has_positive_results <- nrow(positive_df) > 0
    has_negative_results <- nrow(negative_df) > 0

    if (!has_positive_results && (drop.negative || !has_negative_results)) {

      if (drop.negative && has_negative_results) {
        ppts[[grp_name]] <- paste(
          "This row has **no significant over-represented attributes** (at the p <= 0.05 level).",
          "(Note: Under-represented attributes were found but are hidden because `drop.negative` is TRUE.)",
          sep = "\n"
        )
      } else if (drop.negative) {
        ppts[[grp_name]] <- paste(
          "This row has **no significant over-represented attributes** (at the p <= 0.05 level).",
          "(Note: Under-represented attributes are excluded from this analysis.)",
          sep = "\n"
        )
      } else {
        ppts[[grp_name]] <- "This row has **no significant descriptive attributes** (neither over- nor under-represented) at the p <= 0.05 level."
      }

    } else {

      # --- Format Tables ---
      # (Assumes format_stats_as_markdown is in utils-formatting.R)

      ppt1 <- ""
      if (has_positive_results) {
        ppt1 <- format_stats_as_markdown(
          positive_df[, cols_to_show, drop = FALSE],
          title = "Over-represented Attributes (v.test > 0)"
        )
      } else {
        ppt1 <- "This row has no significant **over-represented** attributes."
      }

      ppt2 <- ""
      if (!drop.negative) {
        if (has_negative_results) {
          ppt2 <- format_stats_as_markdown(
            negative_df[, cols_to_show, drop = FALSE],
            title = "Under-represented Attributes (v.test < 0)"
          )
        } else {
          ppt2 <- "This row has no significant **under-represented** attributes."
        }
      }

      ppts[[grp_name]] <- paste(ppt1, ppt2, sep = "\n\n")
    }
  } # --- End of for loop ---

  return(ppts)
}


# ---------------------------------------------------------------------------
# get_prompt_descfreq (NEW HELPER)
# ---------------------------------------------------------------------------
#' @importFrom glue glue
get_prompt_descfreq <- function(res_df, introduction, request, conclusion,
                                isolate.groups, sample.pct, drop.negative) {

  stces_list <- get_sentences_descfreq(res_df, sample.pct, drop.negative)

  if (length(stces_list) == 0 || all(nchar(stces_list) == 0))
    stop("No significant differences between rows, execution was halted.")

  all_groups <- names(stces_list)
  stces <- c()

  for (grp in all_groups) {
    quant <- ifelse(is.null(stces_list[[grp]]), "", stces_list[[grp]])

    ppt_grp <- glue::glue(
      "## **Row '{grp}'**:\n",
      "### **Key Descriptive Attributes (Compared to Average)**\n\n",
      "{quant}",
      .trim = TRUE
    )

    stces <- c(stces, ppt_grp)
  }

  # --- Build Final Prompt ---

  header <- glue::glue(
    "# Introduction - Objective\n\n",
    "{introduction}\n\n",
    "# Task\n\n",
    "{request}\n\n",
    "---\n\n",
    "# Data\n\n"
  )

  if (!isolate.groups) {
    # Single prompt for all groups
    body <- paste(stces, collapse = "\n\n---\n\n") # Add separator
    return(paste(header, body, "\n\n", conclusion, sep = ""))

  } else {
    # List of prompts, one per group
    prompts_list <- list()
    for (i in seq_along(stces)) {
      grp_name <- names(stces_list)[i]
      grp_body <- stces[i]
      # We add the conclusion to each individual prompt
      prompts_list[[grp_name]] <- paste(header, grp_body, "\n\n", conclusion, sep = "")
    }
    return(prompts_list)
  }
}


#' Interpret the rows of a contingency table
#'
#' Describes the rows of a contingency table. For each row, this description is based on the columns of the contingency table that are significantly related to it.
#'
#' @param dataset a data frame corresponding to a contingency table.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param conclusion the conclusion for the LLM prompt.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per row. Recommended if the contingency table has a great number of rows.
#' @param sample.pct from 0 to 1, the proportion of descriptive features that are randomly kept.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests).
#' @param proba the significance threshold considered to characterize the category (by default 0.05).
#' @param by.quali a factor used to merge the data from different rows of the contingency table; by default NULL and each row is characterized.
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#' @param ... Additional arguments passed to 'ollamar::generate'
#' (e.g., `temperature`, `seed`).
#'
#' @return A data frame, or a list of data frames, containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: beard dataset ###
#'
#' data(beard_cont)
#'
#' intro_beard_iso <- 'A survey was conducted about beards
#' and 8 types of beards were described.
#' I will give you the results for one type of beard.'
#' intro_beard_iso <- gsub('\n', ' ', intro_beard_iso) |>
#' stringr::str_squish()
#'
#' req_beard_iso <- 'Please give a name to this beard
#' and summarize what makes this beard unique.'
#' req_beard_iso <- gsub('\n', ' ', req_beard_iso) |>
#' stringr::str_squish()
#'
#' res_beard <- nail_descfreq(beard_cont,
#'                            introduction = intro_beard_iso,
#'                            request = req_beard_iso,
#'                            isolate.groups = TRUE,
#'                            generate = FALSE)
#'
#' res_beard[[1]]
#' res_beard[[2]]
#'
#' intro_beard <- 'A survey was conducted about beards
#' and 8 types of beards were described.
#' In the data that follow, beards are named B1 to B8.'
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' req_beard <- 'Please give a name to each beard
#' and summarize what makes this beard unique.'
#' req_beard <- gsub('\n', ' ', req_beard) |>
#' stringr::str_squish()
#'
#' res_beard <- nail_descfreq(beard_cont,
#'                            introduction = intro_beard,
#'                            request = req_beard,
#'                            generate = TRUE)
#'
#' cat(res_beard$response)
#'
#' text <- res_beard$response
#' titles <- stringr::str_extract_all(text, "\\*\\*B[0-9]+: [^\\*\\*]+\\*\\*")[[1]]
#'
#' titles
#'
#' # for the following code to work, the response must have the beards'
#' # new names with this format: **B1: The Nice beard**, etc.
#'
#' titles <- stringr::str_replace_all(titles, "\\*\\*", "")  # remove asterisks
#' names <- stringr::str_extract(titles, ": .+")
#' names <- stringr::str_replace_all(names, ": ", "")  # remove the colon and space
#'
#' rownames(beard_cont) <- names
#'
#' library(FactoMineR)
#'
#' res_ca_beard <- CA(beard_cont, graph = F)
#' plot.CA(res_ca_beard, invisible = "col")
#'
#'
#' ### Example 2: children dataset ###
#'
#' data(children)
#'
#' children <- children[1:14, 1:5] |> t() |> as.data.frame()
#' rownames(children) <- c('No education', 'Elementary school',
#' 'Middle school', 'High school', 'University')
#'
#' intro_children <- 'The data used here is a contingency table
#' that summarizes the answers
#' given by different categories of people to the following question:
#' "according to you, what are the reasons that can make
#' a woman of a couple hesitate to have children?".
#' Each row corresponds to a level of education, and columns are reasons.'
#' intro_children <- gsub('\n', ' ', intro_children) |>
#' stringr::str_squish()
#'
#' req_children <- "Please explain the main differences
#' between more educated and less educated couples,
#' when it comes to hesitating to have children."
#' req_children <- gsub('\n', ' ', req_children) |>
#' stringr::str_squish()
#'
#' res_children <- nail_descfreq(children,
#'                               introduction = intro_children,
#'                               request = req_children,
#'                               generate = TRUE)
#'
#' cat(res_children$response)
#' }
#'
#' @importFrom FactoMineR descfreq
#' @importFrom glue glue
#' @export
nail_descfreq = function(dataset,
                         introduction = NULL,
                         request = NULL,
                         conclusion = NULL,
                         model = 'llama3',
                         isolate.groups = FALSE,
                         sample.pct = 1,
                         drop.negative = FALSE,
                         by.quali = NULL,
                         proba = 0.05,
                         generate = FALSE,
                         ...){

  # --- Default Introduction ---
  if (is.null(introduction)) {
    introduction <- "The table we are analyzing is a contingency table. Each row represents a category to be described, and the data shows which attributes (from the columns) are associated with it."
  }

  # --- Default Request ---
  if (is.null(request)) {
    if (!isolate.groups) {
      request <- 'Based on the results, please describe what makes each row unique and what sets them apart. Then, assign a descriptive name to each row.'
    } else {
      request <- 'Based on the results, please describe this row according to its specific features. Then, assign a descriptive name to this row.'
    }
  }

  # --- Default Conclusion ---
  if (is.null(conclusion)) {
    if (!isolate.groups) {
      conclusion <- sprintf("# Final Summary Task\nAt the end, provide:\n1. **A comparison of all rows** (e.g., a summary of key contrasts).\n2. **A list of row names you assigned** and their distinguishing features.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**.")
    } else {
      conclusion <- sprintf("# Final Summary Task\nAt the end, provide:\n1. **A description of the row** (e.g., a summary of key characteristics).\n2. **A row name you assigned** based on this description.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**.")
    }
  }

  # --- "How to Read" Guide ---
  GUIDE_DESCFREQ <- paste(
    "## How to Read the Tables",
    "The data shows which attributes are significantly associated with each row, compared to the average.",
    "Only attributes with a p-value <= {proba} are shown.",
    "",
    "* **Intern %**: Percentage of the attribute's frequency *within* this row's responses.",
    "* **glob %**: Percentage of the attribute's frequency *globally* (across all rows).",
    "* **p.value**: The significance level. Low p-values indicate a reliable association.",
    "* **v.test**: A test value indicating the strength and direction. A high positive value means 'significantly over-represented' (characteristic *of* this row). A low negative value means 'significantly under-represented' (characteristic *not* of this row).",
    sep = "\n"
  )

  # Inject the guide into the introduction
  introduction <- paste(glue::glue(introduction),
                        glue::glue(GUIDE_DESCFREQ, proba = proba),
                        sep = "\n\n---\n\n")

  # --- Run Analysis ---
  res_df = FactoMineR::descfreq(dataset, by.quali = by.quali, proba = proba)

  # --- Handle 'stop()' gracefully ---
  ppt <- tryCatch({
    get_prompt_descfreq(
      res_df,
      introduction,
      request,
      conclusion,
      isolate.groups,
      sample.pct,
      drop.negative
    )
  },
  error = function(e) {
    if (grepl("No significant differences", conditionMessage(e))) {
      return("NAILER_NO_RESULTS_FOUND")
    } else {
      stop(e) # Re-throw other errors
    }
  })

  # --- Handle "No Results" Case ---
  if (identical(ppt, "NAILER_NO_RESULTS_FOUND")) {

    no_results_message <- "*No significant differences were found between the rows at this probability threshold.*"

    if (generate) {
      message("Execution halted: No significant differences found. Nothing to generate.")
      if (isolate.groups) {
        return(list())
      } else {
        return(data.frame(
          model = model,
          response = "No significant differences found.",
          prompt = no_results_message,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # Return the prompt with the "no results" message
      header <- glue::glue(
        "# Introduction - Objective\n\n{introduction}\n\n",
        "# Task\n\n{request}\n\n",
        "---\n\n",
        "# Data\n\n"
      )
      return(paste0(header, no_results_message))
    }
  }

  # --- Handle generate = FALSE ---
  if (!generate) return(ppt)

  # --- Handle generate = TRUE (with results) ---

  # Capture and filter ... args
  extra_args <- list(...)
  valid_ollama_opts <- c("temperature", "top_p", "top_k", "seed",
                         "system", "template", "context", "keep_alive",
                         "stream", "format")
  ollama_api_options <- extra_args[names(extra_args) %in% valid_ollama_opts]

  if (!isolate.groups) {

    call_args <- c(
      list(
        model = model,
        prompt = ppt,
        output = 'df' # 'df' is not a standard ollamar argument, use 'text'
      ),
      ollama_api_options
    )
    # Use output = 'text' as in nail_qda
    call_args$output <- "text"

    res_llm <- tryCatch({
      do.call(ollamar::generate, call_args)
    }, error = function(e) {
      stop(paste("Ollama API call (generate=TRUE) failed:", conditionMessage(e)))
    })

    return(list(prompt = ppt, response = res_llm, model = model))

  } else {
    # 'ppt' is a LIST of prompts
    list_rep <- lapply(names(ppt), function(grp_name) {
      prpt <- ppt[[grp_name]]

      call_args_loop <- c(
        list(
          model = model,
          prompt = prpt,
          output = "text" # Use 'text'
        ),
        ollama_api_options
      )

      res_llm <- tryCatch({
        do.call(ollamar::generate, call_args_loop)
      }, error = function(e) {
        stop(paste("Ollama API call (generate=TRUE, isolate.groups=TRUE) failed:", conditionMessage(e)))
      })

      return(list(prompt = prpt, response = res_llm, model = model))
    })
    names(list_rep) <- names(ppt) # Name the final list
    return(list_rep)
  }
}
