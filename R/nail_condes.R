
tidy_answer_condes = function(texte){

  texte_cut = str_split_1(texte, '=')

  ans = str_split_i(texte_cut[2], '_', -1) |> str_squish()

  qu = texte_cut[1] |>
    str_replace('\\.', ' ') |>
    str_squish()

  return(c(qu, ans))
}


get_bins = function(dataset, keep, recode = 1){

  dta = dataset |>
    mutate(across(where(is.numeric), scale), .keep = 'none')

  if (recode == 1){
    dta = dta |>
      mutate(across(where(is.numeric), ~as.factor(ifelse(. >= 0, "Above average", "Below average"))))
  } else if (recode == 2){
    dta = dta |>
      mutate(across(where(is.numeric), ~as.factor(case_when(
        . > 2 ~ "Significantly above average",
        . > 0 ~ "Above average",
        . > -2 ~ "Below average",
        .default = "Significantly below average"
      ))))
  }
  dta = dta %>% cbind(dataset[keep], .)

  return(dta)
}


get_sentences_condes = function(res_cd){

  res_cd = res_cd$category

  res_cd_work = rownames(res_cd) |>
    sapply(tidy_answer_condes) |>
    t() |>
    cbind(res_cd) |>
    as.data.frame()

  colnames(res_cd_work) = c('Variable', 'Level', 'Estimate', 'p.value')

  left = res_cd_work |>
    filter(Estimate > 0) |>
    mutate(Sentence = tolower(paste(Variable, 'is', Level)))
  right = res_cd_work |>
    filter(Estimate < 0) |>
    mutate(Sentence = tolower(paste(Variable, 'is', Level)))

  ppt1 = glue('On one side of the scale, the individuals have the following characteristics : {paste(left$Sentence, sep = "", collapse = ", ")}.')
  ppt2 = glue('On the other side of the scale, the individuals have the following characteristics : {paste(right$Sentence, sep = "", collapse = ", ")}.')

  return(paste(ppt1, ppt2))
}


nail_condes = function(dataset, num.var,
                       introduction = '',
                       request = 'Please explain what differentiates individuals from both sides of the scale.
                       Then give a name to the scale, and briefly explain why you chose that name.',
                       model = 'llama3',
                       recode = 1, weights = NULL, proba = 0.05){

  dta = get_bins(dataset, keep = num.var, recode = recode)

  res_cd = condes(dta[-(num.var + 1)], 1, weights = weights, proba = proba)

  ppt = paste(introduction,
              get_sentences_condes(res_cd),
              request) |> str_squish()

  res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
  return(res_llm)
}
