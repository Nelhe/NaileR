
tidy_answer_descfreq = function(texte){

  return(texte |>
           str_replace('\\.', ' ') |>
           str_squish())
}


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

    ppt = case_when(
      nchar(left) == 0 & nchar(right) == 0 ~ '',
      nchar(left) == 0 ~ glue('For the object "{names(res_df)[i]}", the words {right} are used significantly less often.'),
      nchar(right) == 0 ~ glue('For the object "{names(res_df)[i]}", the words {left} are used significantly more often.'),
      .default = glue('For the object "{names(res_df)[i]}", the words {left} are used significantly more often;
                      and the words {right} are used significantly less often.'))

    ppts = c(ppts, ppt)
  }

  if (isolate.groups == T) return(ppts) else return(paste(ppts, sep = '', collapse = ' ') |> str_squish())
}


nail_descfreq = function(dataset,
                         introduction = '',
                         request = 'Based on the results, please describe what characterizes the individuals of each group.
                       Then, based on these characteristics, give each group a new name..',
                         model = 'llama3', isolate.groups = F,
                         by.quali = NULL, proba = 0.05){

  res_df = descfreq(dataset, by.quali = by.quali, proba = proba)

  ppt = paste(introduction,
              get_sentences_descfreq(res_df, isolate.groups = isolate.groups),
              request) |> str_squish()

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
