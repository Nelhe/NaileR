
remove_punctuation <- function(text) {
  gsub("[[:punct:]]", "", text)
}


#' Sort textual data
#'
#' Group textual data according to their similarity, in a context in which the assessors have commented on a set of stimuli.
#'
#' @param dataset a data frame where each row is a stimulus and each column is an assessor.
#' @param name_size the maximum number of words in a group name created by the LLM.
#' @param stimulus_id the nature of the stimulus. Customizing it is highly recommended.
#' @param introduction the introduction to the LLM prompt.
#' @param measure the type of measure used in the experiment.
#' @param request the request of the LLM prompt.
#' @param model the model name ('llama3.1' by default).
#' @param nb.clusters the maximum number of clusters the LLM can form per assessor.
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#' @param max.attempts the maximum number of attempts for a column.
#'
#' @return A list consisting of:
#' * a list of prompts (one per assessor);
#' * a list of results (one per assessor);
#' * a data frame with the group names.
#'
#' @details This function uses a while loop to ensure that the LLM gives the right number of groups. Therefore, customizing the stimulus ID, prompt introduction and measure is highly recommended; a clear prompt can help the LLM finish its task faster.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' library(NaileR)
#' data(beard_wide)
#'
#' intro_beard <- "As a barber, you make
#' recommendations based on consumers comments.
#' Examples of consumers descriptions of beards
#' are as follows."
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' req_beard <- "Each group should contain beards with descriptions
#' that relate to a similar type of person - not
#' necessarily the same person, but sharing common traits.
#' Each group must have a short,
#' meaningful name that characterizes the person."
#' req_beard <- gsub('\n', ' ', req_beard) |>
#' stringr::str_squish()
#'
#' res <- nail_sort(beard_wide[,1:5], name_size = 3,
#' stimulus_id = "beard", introduction = intro_beard,
#' measure = 'the description was',
#' request = req_beard,
#' nb.clusters = 6,
#' generate = TRUE)
#'
#' cat(res$prompt_llm[[1]])
#' cat(res$res_llm[[1]])
#' res$dta_sort
#' }


#' @importFrom glue glue
#' @importFrom ollamar generate
#' @importFrom stringr str_split_1 str_squish str_count str_remove_all str_extract
#' @importFrom utils tail

nail_sort <- function(dataset, name_size = 3, stimulus_id = "individual",
                      introduction = NULL, measure = NULL, request = NULL, model = "llama3.1",
                      nb.clusters = 4, generate = FALSE, max.attempts = 5) {  # New parameter for max attempts

  ppt_llm <- vector("list", ncol(dataset))
  res_llm <- vector("list", ncol(dataset))
  dta_sort <- dataset[, FALSE]  # Creates an empty data frame with same structure

  # Ensure introduction and measure are not NULL to avoid concatenation issues
  introduction <- ifelse(is.null(introduction), "Individuals are described by free comments.", introduction)
  measure <- ifelse(is.null(measure), "the description was", measure)
  request <- ifelse(is.null(request), "Each group should contain individuals with similar descriptions and have a short, meaningful name.", request)

  for (j in seq_len(ncol(dataset))) {
    dta_j <- dataset[[j]]
    liste <- character(nrow(dataset))  # Preallocate for efficiency

    for (i in seq_len(nrow(dataset))) {
      texte_j <- stringr::str_split_1(dta_j[i], pattern = ";") |>
        paste(collapse = ", ")  # Avoid unnecessary `sep = ""`
      liste[i] <- glue::glue("For {stimulus_id} {i}, {measure} '{texte_j}'.")
    }

    descr <- paste(liste, collapse = " ")  # More efficient concatenation
    ppt_q <- glue::glue(
      "Please categorize the {stimulus_id}s into groups while strictly ensuring that the total number of groups is between **2 and {nb.clusters}**. ",
      "This is a hard constraint: **DO NOT exceed {nb.clusters} groups** and **DO NOT use fewer than 2 groups**. ",
      #"Each group should contain {stimulus_id}s with similar descriptions and have a short, meaningful name. ",
      "DO NOT provide explanations, justifications, or any extra text. ",
      "Strictly preserve the original order of the {stimulus_id}s in your response. ",
      "Output the results in a single line, following this exact format:\n\n",
      '"{stimulus_id} 1 belongs to group \"...\", {stimulus_id} 2 belongs to group \"...\", {stimulus_id} 3 belongs to group \"...\", ..."',
      "\n\n**Failure to follow the formatting or group limit will result in an invalid response.**",
      "\n\n**{request}** "
    )

    ppt <- paste(introduction, descr, ppt_q)
    ppt_llm[[j]] <- ppt

    grps <- NULL
    nb_words <- name_size
    max_attempts_reached <- FALSE  # Flag to track if max attempts reached

    if (generate) {
      counter <- 0

      while (is.null(grps) ||
             length(grps) != nrow(dataset) ||
             nb_words > (name_size + 1) ||
             length(unique(grps)) > nb.clusters) {

        counter <- counter + 1
        if (counter > max.attempts) {
          message(glue::glue("Column {j}: Maximum attempts ({max.attempts}) reached. Moving to next column."))
          grps <- rep(NA, nrow(dataset))  # Return NA values if unsuccessful
          max_attempts_reached <- TRUE  # Mark as failed due to max attempts
          break
        }

        response <- tryCatch({
          ollamar::generate(model, ppt, output = 'df')$response
        }, error = function(e) NULL)  # Handle API failures

        if (is.null(response)) next  # Retry if response is NULL

        grps <- tolower(response) |>
          stringr::str_split_1(pattern = glue::glue("{stimulus_id} "))  # More precise pattern matching

        if (length(grps) >= nrow(dataset)) {
          grps <- utils::tail(grps, nrow(dataset))  # Ensure correct number of groups

          numbers <- as.numeric(gsub("\\D+", "", grps))
          grps <- grps[order(numbers)]  # Sort based on extracted numbers

          grps <- grps |>
            stringr::str_extract("to group \".*\"") |>  # Extract only the group name
            stringr::str_remove_all("to group |\"") |>  # Clean up formatting
            stringr::str_squish()

          nb_words <- max(stringr::str_count(grps, "\\w+"))
        }
      }

      dta_sort[, j] <- grps
      res_llm[[j]] <- response
      colnames(dta_sort)[j] <- colnames(dataset)[j]

      # Only print success message if max attempts were **NOT** reached
      if (!max_attempts_reached) {
        attempt_word <- ifelse(counter == 1, "attempt", "attempts")
        message(glue::glue("Column {j} generated after {counter} {attempt_word}."))
      }
    }
  }

  return(list(prompt_llm = ppt_llm, res_llm = res_llm, dta_sort = dta_sort))
}
