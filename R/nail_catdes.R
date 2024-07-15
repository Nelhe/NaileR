
#' @importFrom stringr str_split_1
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish

tidy_answer_catdes = function(texte){
  split_mid = str_split_1(texte, '=')

  if ((length(split_mid)) > 1 & str_detect(split_mid[length(split_mid)], '_')){
    split_right = str_split_1(split_mid[2], '_')
    return(split_right)

  } else {
    split_mid[1] = str_replace_all(split_mid[1], '\\.', ' ') |> str_squish()
    return(split_mid)
  }
}


#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom glue glue

get_sentences_quali = function(res_cd, drop.negative){
  res_cd = res_cd$category
  ppts = list()

  for (i in c(1:length(names(res_cd)))){
    res_cd2 = res_cd[[i]]

    if (!is.null(res_cd2)){

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
        mutate(Sentence = tolower(glue("* {Variable}: {Level}")))

      if (!drop.negative){
        less = res_cd_work |>
          filter(v.test < 0) |>
          mutate(Variable = glue('"{Variable}"')) |>
          mutate(Sentence = tolower(glue("* {Variable}: {Level}")))
      } else less = ''

      ppt1 = ifelse(nrow(more) == 0,
                    "",
                    glue('The following answers appear *more* often:
{paste(more$Sentence, collapse = "\n")}'))

      ppt2 = ifelse(nrow(less) == 0,
                    "",
                    glue('The following answers appear *less* often:
{paste(less$Sentence, collapse = "\n")}'))

      ppts[[names(res_cd)[i]]] = paste(ppt1, ppt2, sep = "\n")

    } else ppts[[names(res_cd)[i]]] = ''

  }
  return(ppts)
}


#' @importFrom dplyr select

get_sentences_quanti = function(res_cd, drop.negative){

  res_cd = res_cd$quanti
  ppts = list()

  for (i in c(1:length(names(res_cd)))){
    if (!is.null(res_cd[[i]])){

      res_cd_work = res_cd[[i]] |>
        as.data.frame() |>
        select(v.test, p.value) |>
        mutate(Variable = rownames(res_cd[[i]]) |>
                 sapply(tidy_answer_catdes)) |>
        mutate(Variable = glue('* {Variable}'))

      left = res_cd_work$Variable[res_cd_work$v.test > 0] |>
        paste(collapse = '\n')

      if (!drop.negative){
        right = res_cd_work$Variable[res_cd_work$v.test < 0] |>
          paste(collapse = '\n')
      } else right = ''

      ppt1 = ifelse(nchar(left) == 0,
                    '',
                    glue('The following variables are *higher*:
                       {left}'))

      ppt2 = ifelse(nchar(right) == 0,
                    '',
                    glue('The following variables are *lower*:
                       {right}'))

      ppts[[names(res_cd)[i]]] = paste(ppt1, ppt2, sep = "\n")

    } else ppts[[names(res_cd)[i]]] = ''
  }

  return(ppts)
}


get_prompt_catdes = function(res_cd, introduction, request, isolate.groups, drop.negative){

  if ("category" %in% names(res_cd)){
    stces_quali = get_sentences_quali(res_cd, drop.negative)
  } else stces_quali = list()

  if ("quanti" %in% names(res_cd)){
    stces_quanti = get_sentences_quanti(res_cd, drop.negative)
  } else stces_quanti = list()

  if (nchar(stces_quali[1]) == 0 & nchar(stces_quanti[1]) == 0) stop('No significant differences between groups, execution was halted.')

  all_groups = union(names(stces_quali), names(stces_quanti))

  stces = c()

  for (grp in all_groups){
    qual = ifelse(is.null(stces_quali[[grp]]), '', stces_quali[[grp]])
    quant = ifelse(is.null(stces_quanti[[grp]]), '', stces_quanti[[grp]])

    ppt_grp = glue('## Group "{grp}":

                   {qual}

                   {quant}')

    stces = c(stces, ppt_grp)
  }

  if (!isolate.groups) stces = paste(stces, collapse = '\n\n')

  deb = glue('# Introduction

             {introduction}

             # Task

             {request}

             # Data

             ')

  return(paste(deb, stces, sep = ''))
}

#' Interpret a categorical latent variable
#'
#' Generate an LLM response to analyze a categorical latent variable.
#'
#' @param dataset a data frame made up of at least one categorical variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the index of the variable to be characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per category. Recommended with long catdes results.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests). Recommended with long catdes results.
#' @param proba the significance threshold considered to characterize the categories (by default 0.05).
#' @param row.w a vector of integers corresponding to an optional row weights (by default, a vector of 1 for uniform row weights)
#'
#' @return A data frame, or a list of data frames, containing the LLM's response.
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' ### Example 1: Fisher's iris ###
#' data(iris)
#'
#' res_iris <- nail_catdes(iris, num.var = 5, introduction = "A study measured various parts of iris flowers from 3 different species: setosa, versicolor and virginica. I will give you the results from this study. You will have to identify what sets these flowers apart.", request = "Please explain what makes each species distinct. Also, tell me which species has the biggest flowers, and which species has the smallest.")
#' cat(res_iris$response)
#'
#'
#' ### Example 2: food waste dataset ###
#'
#' library(FactoMineR)
#'
#' data(waste)
#' waste <- waste[-14]    # no variability on this question
#'
#' set.seed(1)
#' res_mca_waste <- MCA(waste, quali.sup = c(1,2,50:76), ncp = 35, level.ventil = 0.05, graph = F)
#' plot.MCA(res_mca_waste, choix = "ind", invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_waste <- HCPC(res_mca_waste, nb.clust = 3, graph = F)
#' plot.HCPC(res_hcpc_waste, choice = "map", draw.tree = F, ind.names = F)
#' don_clust_waste <- res_hcpc_waste$data.clust
#'
#' res_waste <- nail_catdes(don_clust_waste, num.var = ncol(don_clust_waste), introduction = 'These data were collected after a survey on food waste, with participants describing their habits.', request = 'Please summarize the characteristics of each group. Then, give each group a new name, based on your conclusions. Finally, give each group a grade between 0 and 10, based on how wasteful they are with food: 0 being "not at all", 10 being "absolutely".', drop.negative = T)
#' cat(res_waste$response)
#'
#'
#' \dontrun{
#' ### Example 3: local_food dataset ###
#'
#' data(local_food)
#'
#' set.seed(1)
#' res_mca_food <- MCA(local_food, quali.sup = 46:63, ncp = 100, level.ventil = 0.05, graph = F)
#' plot.MCA(res_mca_food, choix = "ind", invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_food <- HCPC(res_mca_food, nb.clust = 3, graph = F)
#' plot.HCPC(res_hcpc_food, choice = "map", draw.tree = F, ind.names = F)
#' don_clust_food <- res_hcpc_food$data.clust
#'
#' res_food <- nail_catdes(don_clust_food, num.var = ncol(don_clust_food), introduction = 'A study on sustainable food systems was led on several French participants. This study had 2 parts. In the first part, participants had to rate how acceptable "a food system that..." (e.g, "a food system that only uses renewable energy") was to them. In the second part, they had to say if they agreed or disagreed with some statements.', request = 'I will give you the answers from one group. Please explain who the individuals of this group are, what their beliefs are. Then, give this group a new name, and explain why you chose this name. Do not use 1st person ("I", "my"...) in your answer.', isolate.groups = T, drop.negative = T)
#' res_food[[1]]$response |> cat()
#' res_food[[2]]$response |> cat()
#' res_food[[3]]$response |> cat()
#' }

nail_catdes = function(dataset, num.var,
                       introduction = '',
                       request = 'Based on the results, please describe what characterizes the individuals of each group and what sets them apart from the other groups. Then, based on these characteristics, give each group a new name.',
                       model = 'llama3', isolate.groups = F, drop.negative = F,
                       proba = 0.05, row.w = NULL,
                       generate = T){

  res_cd = FactoMineR::catdes(dataset, num.var = num.var, proba = proba, row.w = row.w)

  ppt = get_prompt_catdes(res_cd, introduction = introduction, request = request,
                          isolate.groups = isolate.groups, drop.negative = drop.negative)

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
