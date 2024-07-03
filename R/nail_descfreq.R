
#' @importFrom stringr str_replace
#' @importFrom stringr str_squish

tidy_answer_descfreq = function(texte){

  return(texte |>
           str_replace('\\.', ' ') |>
           str_squish())
}

#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom glue glue

get_sentences_descfreq = function(res_df, isolate.groups){

  ppts = c()

  for (i in c(1:length(names(res_df)))){
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
      nchar(left) == 0 & nchar(right) == 0 ~ '*no information*',
      nchar(left) == 0 ~ glue('* Least used words: {right}'),
      nchar(right) == 0 ~ glue('* Most used words: {left}'),
      .default = glue('* Most used words: {left}
      * Least used words: {right}'))

    ppt = glue("## {names(res_df)[i]}

               {res}")

    ppts = c(ppts, ppt)
  }

  if (isolate.groups == T) return(ppts) else return(paste(ppts, collapse = '\n\n'))
}


#' Analyze a latent variable in a contingency table
#'
#' Generate a LLM response to analyze a latent variable in a contingency table.
#'
#' @param dataset a data frame corresponding to a contingency table (quantitative data).
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per row. Recommended if the contingency table has several rows.
#' @param proba the significance threshold considered to characterized the category (by default 0.05).
#' @param by.quali a factor used to merge the data from different rows of the contingency table; by default NULL and each row is characterized
#'
#' @return A data frame, or a list of data frames, containing the LLM's response.
#'
#' @details This function directly sends a prompt to a LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' data(beard_cont)
#'
#' res_beard <- nail_descfreq(beard_cont, introduction = 'A survey about beards was led, and 62 participants had to describe 8 types of beards. I will give you the results for one type of beard and you will have to summarise what makes this beard unique.', request = 'Please summarise what makes this beard unique, in no more than 3 sentences', isolate.groups = TRUE)
#'
#' cat(res_beard$response)

nail_descfreq = function(dataset,
                         introduction = '',
                         request = 'Based on the results, please describe what characterizes the individuals of each group. Then, based on these characteristics, give each group a new name.',
                         model = 'llama3', isolate.groups = F,
                         by.quali = NULL, proba = 0.05){

  res_df = FactoMineR::descfreq(dataset, by.quali = by.quali, proba = proba)

  ppt = glue("# Introduction

             {introduction}

             # Task

             {request}

             # Data

             {get_sentences_descfreq(res_df, isolate.groups = isolate.groups)}")

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
