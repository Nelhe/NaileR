
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


nail_catdes = function(dataset, num.var,
                       introduction = '',
                       request = 'Based on the results, please describe what characterizes the individuals of each group and what sets them apart from the other groups.
                       Then, based on these characteristics, give each group a new name.',
                       model = 'llama3', isolate.groups = F,
                       proba = 0.05, row.w = NULL){

  res_cd = FactoMineR::catdes(dataset, num.var = num.var, proba = proba, row.w = row.w)

  ppt = get_prompt_catdes(res_cd, introduction = introduction, request = request,
                          isolate.groups = isolate.groups)

  if (isolate.groups == F){
    res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
    return(res_llm)
  } else {
    list_rep = list()
    for (prpt in ppt){
      res_llm = ollamar::generate(model = model, prompt = prpt, output = 'df')
      list_rep[[length(list_rep) + 1]] = res_llm
    }
    return(list_rep)
  }
}
