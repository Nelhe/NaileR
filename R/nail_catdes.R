
#' @importFrom stringr str_split_i
#' @importFrom stringr str_replace
#' @importFrom stringr str_squish
#' @importFrom stringr str_count

tidy_answer_catdes = function(texte){

  if (str_count(texte, '\\.') == 0){
    qu = str_split_i(texte, '=', 1)
    ans = str_split_i(texte, '=', -1)

  } else {
    texte_cut = str_split_i(texte, '=', -1)

    ans = str_split_i(texte_cut, '_', -1)
    qu = str_replace(texte_cut, paste('_', ans, sep = ''), '')
  }
  return(c(qu, ans))
}


#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom glue glue

get_sentences_quali = function(res_cd, isolate.groups){

  res_cd = res_cd$category
  ppts = c()

  for (i in c(1:length(names(res_cd)))){
    res_cd2 = res_cd[[i]]

    res_cd_work = rownames(res_cd2) |>
      sapply(tidy_answer_catdes) |>
      t() |>
      cbind(res_cd2) |>
      as.data.frame() |>
      select(V1, V2, v.test)

    colnames(res_cd_work) = c('Variable', 'Level', 'v.test')

    more = res_cd_work |>
      filter(v.test > 0) |>
      mutate(Variable = glue('"{Variable}"')) |>
      mutate(Sentence = tolower(paste(Level, 'to', Variable)))

    less = res_cd_work |>
      filter(v.test < 0) |>
      mutate(Variable = glue('"{Variable}"')) |>
      mutate(Sentence = tolower(paste(Level, 'to', Variable)))

    ppt1 = ifelse(nrow(more) == 0,
                  "",
                  glue('Individuals from group "{names(res_cd)[i]}" gave the following answers significantly more often: {paste(more$Sentence, sep = "", collapse = ", ")}'))
    ppt2 = case_when(nrow(less) == 0 & nrow(more) == 0 ~ "",
                     nrow(less) == 0 ~ ".",
                     nrow(more) == 0 ~ glue('Individuals from group "{names(res_cd)[i]}" gave the following answers significantly less often: {paste(less$Sentence, sep = "", collapse = ", ")}.'),
                     .default = glue('; they also gave the following answers significantly less often: {paste(less$Sentence, sep = "", collapse = ", ")}.'))

    ppts = c(ppts, paste(ppt1, ppt2))

  }
  if (isolate.groups == T) return(ppts) else return(paste(ppts, sep = '', collapse = ' ') |> str_squish())
}

#' @importFrom dplyr select

get_sentences_quanti = function(res_cd, isolate.groups){

  res_cd = res_cd$quanti
  ppts = c()

  for (i in c(1:length(names(res_cd)))){
    res_cd_work = res_cd[[i]] |>
      as.data.frame() |>
      select(v.test, p.value) |>
      mutate(Variable = rownames(res_cd[[i]])) |>
      mutate(Variable = glue('"{Variable}"'))

    left = res_cd_work$Variable[res_cd_work$v.test > 0] |>
      paste(collapse = ', ')
    right = res_cd_work$Variable[res_cd_work$v.test < 0] |>
      paste(collapse = ', ')

    ppt = case_when(
      nchar(left) == 0 & nchar(right) == 0 ~ '',
      nchar(left) == 0 ~ glue('In individuals from group "{names(res_cd)[i]}", variables {right} are significantly lower.'),
      nchar(right) == 0 ~ glue('In individuals from group "{names(res_cd)[i]}", variables {left} are significantly higher.'),
      .default = glue('In individuals from group "{names(res_cd)[i]}", variables {left} are significantly higher, and variables {right} are significantly lower.'))

    ppts = c(ppts, ppt)
  }

  if (isolate.groups == T) return(ppts) else return(paste(ppts, sep = '', collapse = ' ') |> str_squish())
}


get_prompt_catdes = function(res_cd, introduction, request, isolate.groups){

  if ("category" %in% names(res_cd)){
    stces_quali = get_sentences_quali(res_cd, isolate.groups)
  } else stces_quali = ''

  if ("quanti" %in% names(res_cd)){
    stces_quanti = get_sentences_quanti(res_cd, isolate.groups)
  } else stces_quanti = ''

  if (nchar(stces_quali[1]) == 0 & nchar(stces_quanti[1]) == 0) stop('No significant differences between groups, execution was halted.')

  stces = case_when(
    nchar(stces_quali[1]) == 0 ~ stces_quanti,
    nchar(stces_quanti[1]) == 0 ~ stces_quali,
    .default = paste('First, here are the results for numeric variables.',
                     stces_quanti,
                     'Second, here are the results for categorical variables.',
                     stces_quali)
  )

  return(paste(introduction, stces, request) |>
           str_squish())
}

#' Analyze a categorical latent variable
#'
#' Generate a LLM response to analyze a categorical latent variable.
#'
#' @param dataset a data frame made up of at least one categorical variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the number of the variable to characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per category. Recommended if the categorical variable has several categories.
#' @param proba the significance threshold considered to characterized the category (by default 0.05).
#' @param row.w a vector of integers corresponding to an optional row weights (by default, a vector of 1 for uniform row weights)
#'
#' @return A data frame, or a list of data frames, containing the LLM's response.
#'
#' @details This function directly sends a prompt to a LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns to clear, unshortened and unambiguous names.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' data(iris)
#'
#' res_iris <- nail_catdes(iris, num.var = 5, introduction = "A study measured various parts of iris flowers from 3 different species: setosa, versicolor and virginica. I will give you the results from this study. You will have to identify what sets these flowers apart.", request = "Please explain what makes each species distinct. Also, tell me which species has the biggest flowers, and which species has the smallest.")
#'
#' cat(res_iris$response)

nail_catdes = function(dataset, num.var,
                       introduction = '',
                       request = 'Based on the results, please describe what characterizes the individuals of each group and what sets them apart from the other groups. Then, based on these characteristics, give each group a new name.',
                       model = 'llama3', isolate.groups = F,
                       proba = 0.05, row.w = NULL){

  res_cd = FactoMineR::catdes(dataset, num.var = num.var, proba = proba, row.w = row.w)

  ppt = get_prompt_catdes(res_cd, introduction = introduction, request = request,
                          isolate.groups = isolate.groups)

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
