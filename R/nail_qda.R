
#' @importFrom dplyr mutate pull arrange
#' @importFrom glue glue

get_sentences_qda <- function(res_cd, drop.negative) {
  res_cd <- res_cd$quanti
  ppts <- list()

  for (i in seq_along(names(res_cd))) {
    if (!is.null(res_cd[[i]])) {

      res_cd_work <- res_cd[[i]] |>
        as.data.frame() |>
        select(v.test, p.value) |>
        mutate(Variable = rownames(res_cd[[i]]) |> sapply(tidy_answer_catdes)) |>
        mutate(Variable = glue("- **{Variable}**"))

      res_cd_work_left <- res_cd_work |> filter(v.test > 0)
      res_cd_work_right <- res_cd_work |> filter(v.test < 0) |> arrange(v.test)

      left <- res_cd_work_left |>
        mutate(Variable = glue("{Variable} (p-value = {formatC(p.value, format = 'e', digits = 2)})")) |>
        pull(Variable) |>
        paste(collapse = "\n")

      right <- if (!drop.negative) {
        res_cd_work_right |>
          mutate(Variable = glue("{Variable} (p-value = {formatC(p.value, format = 'e', digits = 2)})")) |>
          pull(Variable) |>
          paste(collapse = "\n")
      } else ""

      ppt1 <- ifelse(nchar(left) > 0, glue("#### ** Higher than Average (Most Discriminative First)**\n{left}"), "")
      ppt2 <- ifelse(nchar(right) > 0, glue("#### ** Lower than Average (Most Discriminative First)**\n{right}"), "")

      ppts[[names(res_cd)[i]]] <- paste(ppt1, ppt2, sep = "\n\n")
    } else {
      ppts[[names(res_cd)[i]]] <- ""
    }
  }
  return(ppts)
}

get_prompt_qda <- function(res_cd, introduction, request, conclusion, isolate.groups, drop.negative) {

  stces_quanti <- if ("quanti" %in% names(res_cd)) get_sentences_qda(res_cd, drop.negative) else list()

  if (length(stces_quanti) == 0 || all(nchar(stces_quanti) == 0))
    stop("No significant differences between stimuli, execution was halted.")

  all_groups <- names(stces_quanti)
  stces <- c()

  for (grp in all_groups) {
    quant <- ifelse(is.null(stces_quanti[[grp]]), "", stces_quanti[[grp]])

    ppt_grp <- glue(
      "## **Stimulus '{grp}'**:\n",
      "### **Key Sensory Attributes (Compared to the Average)**\n\n",
      "{quant}",
      .trim = TRUE
    )

    stces <- c(stces, ppt_grp)
  }

  if (!isolate.groups) stces <- paste(stces, collapse = "\n\n")

  deb <- glue(
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
                       #sprintf("# Final Summary Task\nAt the end, provide:\n1. **A comparison of all stimuli** (e.g., a summary of key contrasts).\n2. **A list of stimulus names you assigned** and their distinguishing features.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**."),
                       conclusion)

  # Perform QDA analysis
  res_cd <- SensoMineR::decat(dataset, formul = formul, firstvar = firstvar,
                              lastvar = lastvar, proba = proba, graph = FALSE)

  names(res_cd)[6] <- "quanti"

  # Ensure column names are correctly labeled
  for (i in seq_along(res_cd$quanti)) {
    colnames(res_cd$quanti[[i]])[3:4] <- c("p.value", "v.test")
  }

  # Generate the structured prompt
  ppt <- get_prompt_qda(res_cd, introduction, request, conclusion, isolate.groups, drop.negative)

  if (!generate) return(ppt)

  # Generate LLM response using Ollama API
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
