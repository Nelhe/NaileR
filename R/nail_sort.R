
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
#' @param nb_max the maximum number of clusters the LLM can form per assessor.
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#'
#' @return A list consisting of:
#' * a list of prompts (one per assessor);
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
#' res <- nail_sort(beard_wide[,1:5], name_size = 3,
#' stimulus_id = "beard", introduction = intro_beard,
#' measure = 'the description was', generate = TRUE)
#'
#' res$dta_sort
#' cat(res$prompt_llm[[1]])
#' }


#' @importFrom glue glue
#' @importFrom ollamar generate
#' @importFrom stringr str_split_1
#' @importFrom stringr str_squish
#' @importFrom stringr str_count


nail_sort <- function(dataset, name_size = 3, stimulus_id = "stimulus", introduction = "", measure="", nb_max = 6, generate = FALSE) {

  res_llm <- list()
  dta_sort = dataset[,FALSE]

  for (j in c(1:dim(dataset)[2])){
    dta_j = dataset[[j]]
    liste = c()

    for (i in c(1:dim(dataset)[1])){
      texte_j = dta_j[i] |>
        str_split_1(pattern = ';') |>
        paste(sep = '', collapse = ', ')
      liste = c(liste, glue('For ', stimulus_id, ' {i}, ', measure, ' \'{texte_j}\'.'))
    }

    descr = paste(liste, sep = '', collapse = ' ')
    ppt_q = glue('Please group the ', stimulus_id, 's into a minimum of 2 groups and a maximum of ', nb_max,' groups, such that ', stimulus_id, 's of a same group have the same description. Give the groups a short name. Without any justification, write for each ', stimulus_id, ' which group it belongs to, from the first one to the last one, accordingly to this exact format, in a single line without asterisks: "', stimulus_id,' 1 belongs to group "...", ', stimulus_id,' 2 belongs to group "...", ', stimulus_id,' 3 belongs to group "...",...')
    ppt = paste(introduction, descr, ppt_q)
    res_llm[[j]] <- ppt
    grps <- ""

    nb_words <- name_size

    if (generate == TRUE){
      while(length(grps)!=dim(dataset)[1] | nb_words>(name_size+1)){
        grps = generate('llama3', ppt, output = 'df')$response
        grps <- tolower(grps)
        grps <- grps |> str_split_1(pattern = c(glue(stimulus_id,' ')))
        #print(grps)
        #print(j)

        if (length(grps)-(dim(dataset)[1]-1)>0){
          grps <- grps[(length(grps)-(dim(dataset)[1]-1)):length(grps)]
          numbers <- as.numeric(gsub("\\D+", "", grps))
          grps <- grps[order(numbers)]
          grps = grps[(length(grps)-(dim(dataset)[1]-1)):length(grps)] |>
            str_split_i(pattern = "to group", i = -1) |>
            str_squish() |>
            remove_punctuation()
          nb_words <- max(str_count(grps, "\\w+"))
        }
      }
      dta_sort[,j] = grps
    }
  }
  return(list(prompt_llm = res_llm, dta_sort = dta_sort))
}
