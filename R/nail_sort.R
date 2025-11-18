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
#' @importFrom jsonlite fromJSON

nail_sort <- function(dataset, name_size = 3, stimulus_id = "individual",
                      introduction = NULL, measure = NULL, request = NULL, model = "llama3.1",
                      nb.clusters = 4, generate = FALSE, max.attempts = 5) {

  ppt_llm <- vector("list", ncol(dataset))
  res_llm <- vector("list", ncol(dataset))
  dta_sort <- dataset[, FALSE] # Creates an empty data frame with same structure

  # Ensure introduction and measure are not NULL to avoid concatenation issues
  introduction <- ifelse(is.null(introduction), "Individuals are described by free comments.", introduction)
  measure <- ifelse(is.null(measure), "the description was", measure)
  request <- ifelse(is.null(request), "Each group should contain individuals with similar descriptions and have a short, meaningful name.", request)

  for (j in seq_len(ncol(dataset))) {
    dta_j <- dataset[[j]]
    liste <- character(nrow(dataset)) # Preallocate for efficiency

    for (i in seq_len(nrow(dataset))) {
      texte_j <- stringr::str_split_1(dta_j[i], pattern = ";") |>
        paste(collapse = ", ")
      liste[i] <- glue::glue("For {stimulus_id} {i}, {measure} '{texte_j}'.")
    }

    descr <- paste(liste, collapse = " ")

    # --- MODIFICATION : Nouveau prompt demandant du JSON ---
    ppt_q <- glue::glue(
      "Please categorize the {nrow(dataset)} {stimulus_id}s into groups while strictly ensuring that the total number of groups is between **2 and {nb.clusters}**. ",
      "This is a hard constraint: **DO NOT exceed {nb.clusters} groups** and **DO NOT use fewer than 2 groups**. ",
      "DO NOT provide explanations, justifications, or any extra text. ",
      "You MUST output **only** a valid JSON array of objects, with no other text before or after. ",
      "Each object in the array must represent one {stimulus_id} and have two keys: ",
      "1. `stimulus_id`: The numeric ID of the {stimulus_id} (e.g., 1, 2, 3...). ",
      "2. `group_name`: The short, meaningful name of the group (max {name_size} words). ",
      "Ensure all {nrow(dataset)} {stimulus_id}s are present in the JSON array. ",
      "Example format:\n\n",
      '[',
      '  {{"stimulus_id": 1, "group_name": "Group Name A"}},',
      '  {{"stimulus_id": 2, "group_name": "Group Name B"}},',
      '  ...',
      '  {{"stimulus_id": {nrow(dataset)}, "group_name": "Group Name A"}}',
      ']',
      "\n\n**Failure to follow the JSON format or group limit will result in an invalid response.**",
      "\n\n**{request}** "
    )
    # Les {{ et }} sont necessaires pour que glue::glue ignore les accolades du JSON

    ppt <- paste(introduction, descr, ppt_q)
    ppt_llm[[j]] <- ppt

    grps <- NULL
    max_attempts_reached <- FALSE # Flag to track if max attempts reached

    if (generate) {
      counter <- 0
      valid_response <- FALSE # Flag pour controler la boucle while

      while (!valid_response) {
        counter <- counter + 1
        if (counter > max.attempts) {
          message(glue::glue("Column {j}: Maximum attempts ({max.attempts}) reached. Moving to next column."))
          grps <- rep(NA, nrow(dataset)) # Return NA values if unsuccessful
          max_attempts_reached <- TRUE
          break # Sortir de la boucle while
        }

        # --- MODIFICATION : Logique de generation et parsing ---

        response_raw <- tryCatch({
          ollamar::generate(model, ppt, output = 'df')$response
        }, error = function(e) {
          message(glue::glue("Column {j}, Attempt {counter}: API call failed. Retrying..."))
          return(NULL) # Retourne NULL en cas d'echec de l'API
        })

        if (is.null(response_raw)) {
          res_llm[[j]] <- "API call failed"
          next # Nouvelle tentative
        }

        res_llm[[j]] <- response_raw # Stocker la reponse brute (pour debogage)

        # Tenter de parser le JSON
        parsed_data <- tryCatch({
          jsonlite::fromJSON(response_raw, simplifyDataFrame = TRUE)
        }, error = function(e) {
          message(glue::glue("Column {j}, Attempt {counter}: Failed to parse JSON. Retrying..."))
          return(NULL) # Retourne NULL en cas d'echec du parsing
        })

        if (is.null(parsed_data)) next # Nouvelle tentative

        # --- DeBUT DES VALIDATIONS ---

        # 1. Valider la structure du JSON
        if (!is.data.frame(parsed_data) || !all(c("stimulus_id", "group_name") %in% names(parsed_data))) {
          message(glue::glue("Column {j}, Attempt {counter}: JSON structure incorrect (missing keys). Retrying..."))
          next
        }

        # 2. Valider le nombre de lignes
        if (nrow(parsed_data) != nrow(dataset)) {
          message(glue::glue("Column {j}, Attempt {counter}: Incorrect number of items in JSON ({nrow(parsed_data)} found, {nrow(dataset)} expected). Retrying..."))
          next
        }

        # 3. Assurer l'ordre et extraire les groupes
        parsed_data$stimulus_id <- as.numeric(parsed_data$stimulus_id)
        parsed_data <- parsed_data[order(parsed_data$stimulus_id), ]

        # 4. Verifier que tous les IDs sont presents (de 1 a N)
        if (!all(parsed_data$stimulus_id == 1:nrow(dataset))) {
          message(glue::glue("Column {j}, Attempt {counter}: JSON missing or has duplicate stimulus_ids. Retrying..."))
          next
        }

        grps <- stringr::str_squish(as.character(parsed_data$group_name))

        # 5. Valider le nombre de mots par nom de groupe
        nb_words <- max(stringr::str_count(grps, "\\w+"), na.rm = TRUE)
        if (nb_words > name_size) {
          message(glue::glue("Column {j}, Attempt {counter}: Group name too long ({nb_words} words, max {name_size}). Retrying..."))
          next
        }

        # 6. Valider le nombre de groupes uniques (min et max)
        nb_unique_grps <- length(unique(grps))
        if (nb_unique_grps > nb.clusters || nb_unique_grps < 2) {
          message(glue::glue("Column {j}, Attempt {counter}: Incorrect number of groups ({nb_unique_grps} found, expected 2-{nb.clusters}). Retrying..."))
          next
        }

        # --- FIN DES VALIDATIONS ---

        # Si on arrive ici, tout est valide
        valid_response <- TRUE

      } # Fin de la boucle while

      dta_sort[, j] <- grps
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
