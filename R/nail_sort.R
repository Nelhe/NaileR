
remove_punctuation <- function(text) {
  gsub("[[:punct:]]", "", text)
}


#' Sort sensometrics textual data
#'
#' Performs clustering on textual data from a sensometrics experiment, with an LLM.
#'
#' @param dta_text a data frame where each row is a stimulus and each column is a judge.
#' @param name_size the maximum number of words in a new group name created by the LLM.
#' @param stimulus_id the name of the stimulus. Customizing it is highly recommended.
#' @param ppt_intro the introduction to the LLM prompt.
#' @param measure the type of measure used in the experiment.
#' @param nb_max the maximum number of clusters the LLM can form.
#'
#' @return A data frame where each column has been sorted independently into groups.
#'
#' @details This function uses a while loop to ensure that the LLM gives the right number of groups. Therefore, customizing the stimulus ID, prompt introduction and measure is highly recommended; a clear prompt can help the LLM finish its task faster.
#'
#' @export
#'
#' @examples
#' res <- nail_sort(beard_wide[,1:5], name_size = 3, stimulus_id = "beard", ppt_intro = "As a barber, you make recommendations based on consumers comments. Examples of consumers descriptions of beards are as follows.", measure = 'the description was')
#'
#' res$dta_sort
#' res$prompt_llm


#' @importFrom glue glue
#' @importFrom ollamar generate
#' @importFrom stringr str_split_1
#' @importFrom stringr str_squish
#' @importFrom stringr str_count


nail_sort <- function(dta_text, name_size = 3, stimulus_id = "", ppt_intro = "stimulus", measure="", nb_max = 6) {

  res_llm <- list()
  dta_sort = dta_text[,FALSE]

  for (j in c(1:dim(dta_text)[2])){
    dta_j = dta_text[[j]]
    liste = c()

    for (i in c(1:dim(dta_text)[1])){
      texte_j = dta_j[i] |>
        str_split_1(pattern = ';') |>
        paste(sep = '', collapse = ', ')
      liste = c(liste, glue('For ', stimulus_id, ' {i}, ', measure, ' \'{texte_j}\'.'))
    }

    descr = paste(liste, sep = '', collapse = ' ')
    ppt_q = glue('Please group the ', stimulus_id, 's into a minimum of 2 groups and a maximum of ', nb_max,' groups, such that ', stimulus_id, 's of a same group have the same description. Give the groups a short name. Without any justification, write for each', stimulus_id, ' which group it belongs to, from the first one to the last one, accordingly to this exact format, in a single line without asterisks: "', stimulus_id,' 1 belongs to group "...", ', stimulus_id,' 2 belongs to group "...", ', stimulus_id,' 3 belongs to group "...",...')
    ppt = paste(ppt_intro, descr, ppt_q)
    res_llm[[j]] <- ppt
    grps <- ""

    nb_words <- name_size

    while(length(grps)!=dim(dta_text)[1] | nb_words>(name_size+1)){
      grps = generate('llama3', ppt, output = 'df')$response
      grps <- tolower(grps)
      grps <- grps |> str_split_1(pattern = c(glue(stimulus_id,' ')))

      if (length(grps)-(dim(dta_text)[1]-1)>0){
        grps <- grps[(length(grps)-(dim(dta_text)[1]-1)):length(grps)]
        numbers <- as.numeric(gsub("\\D+", "", grps))
        grps <- grps[order(numbers)]
        grps = grps[(length(grps)-(dim(dta_text)[1]-1)):length(grps)] |>
          str_split_i(pattern = "to group", i = -1) |>
          str_squish() |>
          remove_punctuation()
        nb_words <- max(str_count(grps, "\\w+"))
      }
    }
    dta_sort[,j] = grps
  }
  return(list(prompt_llm = res_llm, dta_sort = dta_sort))
}
