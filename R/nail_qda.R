#' @importFrom dplyr mutate filter arrange select
#' @importFrom glue glue
#' @importFrom knitr kable
#' @importFrom tibble rownames_to_column
#' @importFrom utils globalVariables

# Set global variables to avoid R CMD check NOTES
utils::globalVariables(c(".data", "p.value", "v.test"))

get_sentences_qda <- function(res_cd, drop.negative) {
  res_cd <- res_cd$quanti
  ppts <- list() # Stores the Markdown strings for each group

  for (i in seq_along(names(res_cd))) {
    grp_name <- names(res_cd)[i]
    res_df <- res_cd[[i]]

    # --- Data Preparation ---
    # Initialize empty dataframes to ensure logic works even if res_df is NULL
    res_cd_work_left <- data.frame(Variable=character(), Coeff=numeric(), `Adjust mean`=numeric(), p.value=numeric(), v.test=numeric(), check.names=FALSE)
    res_cd_work_right <- data.frame(Variable=character(), Coeff=numeric(), `Adjust mean`=numeric(), p.value=numeric(), v.test=numeric(), check.names=FALSE)

    if (!is.null(res_df) && nrow(as.data.frame(res_df)) > 0) {

      # Prepare the working dataframe
      res_cd_work <- as.data.frame(res_cd[[i]]) |>
        tibble::rownames_to_column(var = "Variable") |>
        select(Variable, .data$Coeff, .data$`Adjust mean`, p.value, v.test)

      # 1. Split data: Higher than average (v.test > 0)
      res_cd_work_left <- res_cd_work |>
        filter(v.test > 0) |>
        arrange(p.value) # Sort by ascending p.value (most significant first)

      # 2. Split data: Lower than average (v.test < 0)
      res_cd_work_right <- res_cd_work |>
        filter(v.test < 0) |>
        arrange(p.value) # Sort by ascending p.value
    }

    # --- NEW LOGIC BLOCK ---

    has_positive_results <- nrow(res_cd_work_left) > 0
    has_negative_results <- nrow(res_cd_work_right) > 0

    # CASE 1: No results to show based on criteria
    if (!has_positive_results && (drop.negative || !has_negative_results)) {

      if (drop.negative && has_negative_results) {
        # No positive, but negatives exist and are hidden.
        ppts[[grp_name]] <- paste(
          "This stimulus has **no significant sensory attributes higher than the average** (at the p <= 0.05 level).",
          "(Note: Attributes significantly lower than the average were found but are hidden because `drop.negative` is TRUE.)",
          sep = "\n"
        )
      } else if (drop.negative) {
        # No positive, and no negatives to hide (drop.negative is still the policy).
        ppts[[grp_name]] <- paste(
          "This stimulus has **no significant sensory attributes higher than the average** (at the p <= 0.05 level).",
          "(Note: Attributes lower than the average are excluded from this analysis.)",
          sep = "\n"
        )
      } else {
        # No positive AND no negative. The truly neutral product.
        ppts[[grp_name]] <- "This stimulus has **no significant sensory attributes** (neither higher nor lower than the average) that distinguish it from the other products (at the p <= 0.05 level)."
      }

    } else {

      # CASE 2: There are results to show.

      ppt1 <- ""
      if (has_positive_results) {
        table_left <- knitr::kable(res_cd_work_left,
                                   digits = 2,
                                   format = "pipe",
                                   align = c('l', 'r', 'r', 'r', 'r'))
        ppt1 <- glue::glue(
          "#### ** Higher than Average (Most Significant First)**\n\n",
          paste(table_left, collapse = "\n")
        )
      } else {
        # This implies !drop.negative and has_negative_results must be true
        ppt1 <- "This stimulus has no sensory attributes significantly **higher** than the average."
      }

      ppt2 <- ""
      if (!drop.negative) {
        if (has_negative_results) {
          table_right <- knitr::kable(res_cd_work_right,
                                      digits = 2,
                                      format = "pipe",
                                      align = c('l', 'r', 'r', 'r', 'r'))
          ppt2 <- glue::glue(
            "\n\n#### ** Lower than Average (Most Significant First)**\n\n",
            paste(table_right, collapse = "\n")
          )
        } else {
          # This implies has_positive_results must be true
          ppt2 <- "\n\nThis stimulus has no sensory attributes significantly **lower** than the average."
        }
      }

      ppts[[grp_name]] <- paste0(ppt1, ppt2)
    }
  } # --- End of for loop ---

  return(ppts)
}


#' @importFrom glue glue
get_prompt_qda <- function(res_cd, introduction, request, conclusion, isolate.groups, drop.negative) {

  stces_quanti <- if ("quanti" %in% names(res_cd)) get_sentences_qda(res_cd, drop.negative) else list()

  if (length(stces_quanti) == 0 || all(nchar(stces_quanti) == 0))
    stop("No significant differences between stimuli, execution was halted.")

  all_groups <- names(stces_quanti)
  stces <- c()

  for (grp in all_groups) {
    quant <- ifelse(is.null(stces_quanti[[grp]]), "", stces_quanti[[grp]])

    ppt_grp <- glue::glue(
      "## **Stimulus '{grp}'**:\n",
      "### **Key Sensory Attributes (Compared to the Average)**\n\n",
      "{quant}",
      .trim = TRUE
    )

    stces <- c(stces, ppt_grp)
  }

  if (!isolate.groups) stces <- paste(stces, collapse = "\n\n---\n\n") # Added horizontal rule

  deb <- glue::glue(
    "# Introduction - Objective\n\n",
    "{introduction}\n\n",
    "# Task\n\n",
    "{request}\n\n",
    "---\n\n",
    "# Data\n\n"
  )

  return(paste(deb, stces, "\n\n", conclusion, sep = ""))
}

#' Interpret QDA data
#'
#' Generate an LLM response to analyze QDA data.
#'
#' @param dataset a data frame made up of at least two categorical variables (product, panelist) and a set of quantitative variables (sensory attributes).
#' @param formul the analyis of variance model to be evaluated for each sensory attribute.
#' @param firstvar the index of the first sensory attribute.
#' @param lastvar the index of the last sensory attribute.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request for the LLM prompt.
#' @param conclusion the conclusion for the LLM prompt.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per product.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests).
#' @param proba the significance threshold considered to characterize the products (by default 0.05).
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#'
#' @return A data frame, or a list of data frames, containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: QDA data on chocolates with isolate.groups = FALSE ###
#' library(NaileR)
#' library(SensoMineR)
#' data(chocolates)
#'
#' intro_sensochoc <- "Six chocolates were measured according
#' to sensory attributes by a trained panel.
#' I will give you the results from this study.
#' You will have to identify what sets these chocolates apart."
#' intro_sensochoc <- gsub('\n', ' ', intro_sensochoc) |>
#' stringr::str_squish()
#'
#' req_sensochoc <- "Please explain what makes each chocolate different
#' and provide a sensory profile of each chocolate, as well as a name."
#' req_sensochoc <- gsub('\n', ' ', req_sensochoc) |>
#' stringr::str_squish()
#'
#' res_nail_qda <- nail_qda(sensochoc,
#'                          formul="~Product+Panelist",
#'                          firstvar = 5,
#'                          introduction = intro_sensochoc,
#'                          request = req_sensochoc,
#'                          model = 'llama3',
#'                          isolate.groups = FALSE,
#'                          drop.negative = FALSE,
#'                          proba = 0.05,
#'                          generate = TRUE)
#'
#' cat(res_nail_qda$prompt)
#' cat(res_nail_qda$response)
#'
#' ### Example 2: QDA data on chocolates with isolate.groups = TRUE ###
#' library(NaileR)
#' library(SensoMineR)
#' data(chocolates)
#'
#' intro_sensochoc <- "A chocolate was measured according
#' to sensory attributes by a trained panel.
#' I will give you the results from this study.
#' You will have to identify the characteristics of this chocolate."
#' intro_sensochoc <- gsub('\n', ' ', intro_sensochoc) |>
#' stringr::str_squish()
#'
#' req_sensochoc <- "Please provide a detailed sensory profile for this chocolate,
#' as well as a name."
#' req_sensochoc <- gsub('\n', ' ', req_sensochoc) |>
#' stringr::str_squish()
#'
#' res_nail_qda <- nail_qda(sensochoc,
#'                          formul="~Product+Panelist",
#'                          firstvar = 5,
#'                          introduction = intro_sensochoc,
#'                          request = req_sensochoc,
#'                          model = 'llama3',
#'                          isolate.groups = TRUE,
#'                          drop.negative = FALSE,
#'                          proba = 0.05,
#'                          generate = TRUE)
#'
#' cat(res_nail_qda[[1]]$prompt)
#' cat(res_nail_qda[[1]]$response)
#' }

#' @importFrom SensoMineR decat
#' @export
nail_qda <- function(dataset, formul, firstvar, lastvar = length(colnames(dataset)),
                     introduction = NULL, request = NULL, conclusion = NULL, model = "llama3",
                     isolate.groups = FALSE, drop.negative = FALSE, proba = 0.05, generate = FALSE) {

  # Set default introduction and request if missing
  introduction <- ifelse(is.null(introduction),
                         if (!isolate.groups) "For this study, some stimuli have been evaluated by panelists using a common list of perceptual or sensory attributes."
                         else "For this study, a stimulus has been evaluated by panelists using a common list of perceptual or sensory attributes.",
                         introduction)

  request <- ifelse(is.null(request),
                    if (!isolate.groups) "Based on the results, please describe what characterizes the stimuli and what sets them apart. Then, based on these characteristics, assign each stimulus a new name."
                    else "Based on the results, please describe that particular stimulus according to its specific features. Then, based on these characteristics, give the stimulus a new name.",
                    request)

  conclusion <- ifelse(is.null(conclusion),
                       if (!isolate.groups) sprintf("# Final Summary Task\nAt the end, provide:\n1. **A comparison of all stimuli** (e.g., a summary of key contrasts).\n2. **A list of stimulus names you assigned** and their distinguishing features.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**.")
                       else sprintf("# Final Summary Task\nAt the end, provide:\n1. **A description of the stimulus** (e.g., a summary of key characteristics).\n2. **A stimulus name you assigned** based on this description.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**."),
                       conclusion)

  # --- MODIFICATION: Inject the 'How-to-Read' Guide ---
  GUIDE_DECAT <- paste(
    "## How to Read the Tables",
    "The data shows which sensory attributes are associated with each stimulus, compared to the average.",
    "Only attributes with a p-value <= {proba} are shown.",
    "",
    "* **Variable**: The sensory attribute (e.g., 'Bitterness').",
    "* **Coeff**: The coefficient from the linear model, indicating the direction of the association.",
    "* **Adjust mean**: The adjusted mean score for this stimulus on this attribute.",
    "* **Vtest**: A test value indicating the strength and direction of the association. A high positive value means 'significantly above average'. A low negative value means 'significantly below average'.",
    "* **p.value**: The significance level. Low p-values indicate a reliable association.",
    sep = "\n"
  )
  # 'glue' is used here to insert the 'proba' value into the guide
  introduction <- paste(glue::glue(introduction),
                        glue::glue(GUIDE_DECAT, proba = proba),
                        sep = "\n\n---\n\n")
  # --- END OF MODIFICATION ---

  # Perform QDA analysis (unchanged)
  res_cd <- SensoMineR::decat(dataset, formul = formul, firstvar = firstvar,
                              lastvar = lastvar, proba = proba, graph = FALSE)

  names(res_cd)[6] <- "quanti"

  # Ensure column names are correctly labeled (unchanged)
  for (i in seq_along(res_cd$quanti)) {
    if (!is.null(res_cd$quanti[[i]])) { # Add a null check
      colnames(res_cd$quanti[[i]])[3:4] <- c("p.value", "v.test")
    }
  }

  # Generate the structured prompt (unchanged)
  ppt <- get_prompt_qda(res_cd, introduction, request, conclusion, isolate.groups, drop.negative)

  if (!generate) return(ppt)

  # Generate LLM response using Ollama API (unchanged)
  if (!isolate.groups) {
    res_llm <- ollamar::generate(model = model, prompt = ppt, output = "text")
    return(list(prompt = ppt, response = res_llm, model = model))
  } else {
    list_rep <- lapply(ppt, function(prpt) {
      res_llm <- ollamar::generate(model = model, prompt = prpt, output = "text")
      return(list(prompt = prpt, response = res_llm, model = model))
    })
    return(list_rep)
  }
}
