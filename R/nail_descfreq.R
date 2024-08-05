
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish

tidy_answer_descfreq = function(texte){

  return(texte |>
           str_replace_all('\\.', ' ') |>
           str_squish())
}

#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom glue glue

get_sentences_descfreq = function(res_df, isolate.groups){

  ppts = c()

  for (i in c(1:length(names(res_df)))){

    if (is.null(res_df[[i]])) {
      res = '* *This row is not characterised by any particular modality/column.*'

    } else {

      res_df_work = res_df[[i]] |>
        as.data.frame() |>
        select(v.test, p.value) |>
        mutate(Variable = sapply(rownames(res_df[[i]]), tidy_answer_descfreq)) |>
        mutate(Variable = glue('"{Variable}"'))

      left = res_df_work$Variable[res_df_work$v.test > 0] |>
        paste(collapse = ', ')
      right = res_df_work$Variable[res_df_work$v.test < 0] |>
        paste(collapse = ', ')

      res = dplyr::case_when(
        nchar(left) == 0 ~ glue('* Here are the categories that characterize the row {names(res_df)[i]} and whose frequency of use is significantly below average: {right}.'),
        nchar(right) == 0 ~ glue('* Here are the categories that characterize the row {names(res_df)[i]} and whose frequency of use is significantly above average: {left}.'),
        .default = glue('* Here are the categories that characterize the row {names(res_df)[i]} and whose frequency of use is significantly above average: {left}.
      * Here are the categories that characterize the row {names(res_df)[i]} and whose frequency of use is significantly below average: {right}.'))

    }

    ppt = glue("## {names(res_df)[i]}

               {res}")

    ppts = c(ppts, ppt)
  }

  if (isolate.groups == T) return(ppts) else return(paste(ppts, collapse = '\n\n'))
}


#' Interpret the rows of a contingency table
#'
#' Describes the rows of a contingency table. For each row, this description is based on the columns of the contingency table that are significantly related to it.
#'
#' @param dataset a data frame corresponding to a contingency table.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per row. Recommended if the contingency table has a great number of rows.
#' @param proba the significance threshold considered to characterize the category (by default 0.05).
#' @param by.quali a factor used to merge the data from different rows of the contingency table; by default NULL and each row is characterized.
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
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
#' introduction = intro_beard_iso,
#' request = req_beard_iso,
#' isolate.groups = TRUE, generate = FALSE)
#'
#' res_beard$prompt[1]
#' res_beard$prompt[2]
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
#' introduction = intro_beard,
#' request = req_beard)
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
#' introduction = intro_children, request = req_children)
#'
#' cat(res_children$response)
#' }


nail_descfreq = function(dataset,
                         introduction = '',
                         request = NULL,
                         model = 'llama3', isolate.groups = FALSE,
                         by.quali = NULL, proba = 0.05,
                         generate = TRUE){

  if (is.null(request)) request <- 'Based on the results, please describe what makes each row unique.'
  res_df = FactoMineR::descfreq(dataset, by.quali = by.quali, proba = proba)

  ppt = glue("# Introduction

             {introduction}

             # Task

             {request}

             # Data

             {get_sentences_descfreq(res_df, isolate.groups = isolate.groups)}")

  if (!generate) return(data.frame(prompt = ppt))

  if (isolate.groups == F){
    res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
    res_llm$prompt = ppt
    return(res_llm)
  } else {
    list_rep = list()
    for (prpt in ppt){
      res_llm = ollamar::generate(model = model, prompt = prpt, output = 'df')
      res_llm$prompt = prpt
      list_rep[[length(list_rep) + 1]] = res_llm
    }
    return(list_rep)
  }
}
