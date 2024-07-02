
#' @importFrom stringr str_split_1
#' @importFrom stringr str_split_i
#' @importFrom stringr str_replace
#' @importFrom stringr str_squish

tidy_answer_condes = function(texte){

  texte_cut = str_split_1(texte, '=')

  ans = str_split_i(texte_cut[2], '_', -1) |>
    str_squish()

  qu = texte_cut[1] |>
    str_replace('\\.', ' ') |>
    str_squish()

  return(c(qu, ans))
}

#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom dplyr case_when

get_bins = function(dataset, keep, recode = 1){

  dta = dataset |>
    mutate(across(where(is.numeric), scale), .keep = 'none')

  if (recode == 1){
    dta = dta |>
      mutate(across(where(is.numeric),
                    ~as.factor(ifelse(. >= 0, "Above average", "Below average"))))
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


#' @importFrom dplyr filter
#' @importFrom glue glue

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
    mutate(Sentence = tolower(glue("* {Variable}: {Level}")))
  right = res_cd_work |>
    filter(Estimate < 0) |>
    mutate(Sentence = tolower(glue("* {Variable}: {Level}")))

  ppt1 = glue('## Left side of the scale

The individuals have the following characteristics:
{paste(left$Sentence, collapse = "\n")}')

  ppt2 = glue('## Right side of the scale

The individuals have the following characteristics:
{paste(right$Sentence, collapse = "\n")}')

  return(paste(ppt1, ppt2, sep = '\n\n'))
}


#' Analyze a continuous latent variable
#'
#' Generate a LLM response to analyze a continuous latent variable
#'
#' @param dataset a data frame made up of at least one quantitative variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the number of the variable to characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param recode an integer specifying how continuous variables are converted to categorical, for the analysis. recode = 1 (default) converts values to "more/less than average"; recode = 2 adds the categories "significantly more/less than average" when a value is more/less than 2 standard deviations away from the average.
#' @param weights weights for the individuals (see [FactoMineR::condes()]).
#' @param proba the significance threshold considered to characterized the category (by default 0.05).
#'
#' @return A data frame containing the LLM's response.
#'
#' @details This function directly sends a prompt to a LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns to clear, unshortened and unambiguous names.
#'
#' @export
#'
#' @examples
#' library(FactoMineR)
#' data(decathlon)
#'
#' names(decathlon) <- c('Time taken to complete the 100m', 'Distance reached for the long jump', 'Distance reached for the shot put', 'Height reached for the high jump',  'Time taken to complete the 400m', 'Time taken to complete the 110m hurdle', 'Distance reached for the discus', 'Height reached for the pole vault', 'Distance reached for the javeline', 'Time taken to complete the 1500 m', 'Rank', 'Points', 'Competition')
#'
#' res_pca_deca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
#' deca_work <- res_pca_deca$ind$coord |> as.data.frame()
#' deca_work <- deca_work[1:3] |> cbind(decathlon)
#'
#' res_deca <- nail_condes(deca_work, 1, recode = 2, introduction = "A study was led on athletes participating to a decathlon event. Their performance was assessed on each part of the decathlon, and they were all placed on a unidimensional scale. You will analyze what sets apart individuals from either side of the scale. If an event is listed as both below (or above) and significantly below (or above) average, consider it significantly so.")
#'
#' cat(res_deca$response)
#' ## a really interesting thing to note is that depending on the variable names and the exact prompt, the LLM sometimes confuses a "below average time" in speed-based events with a "bad performance".
#' ## This shows how important the prompt is, and how the LLM's response should be double-checked.


nail_condes = function(dataset, num.var,
                       introduction = '',
                       request = 'Please explain what differentiates individuals from both sides of the scale. Then give a name to the scale, and briefly explain why you chose that name.',
                       model = 'llama3',
                       recode = 1, weights = NULL, proba = 0.05){

  dta = get_bins(dataset, keep = num.var, recode = recode)

  res_cd = FactoMineR::condes(dta[-(num.var + 1)], 1, weights = weights, proba = proba)

  ppt = glue("# Introduction

             {introduction}

             # Task

             {request}

             # Data

             {get_sentences_condes(res_cd)}")

  res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
  res_llm$prompt = ppt
  return(res_llm)
}
