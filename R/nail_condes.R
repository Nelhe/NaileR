
#' @importFrom stringr str_split_1
#' @importFrom stringr str_split_i
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish

tidy_answer_condes = function(texte){

  texte_cut = str_split_1(texte, '=')

  ans = str_split_i(texte_cut[2], '_', -1) |>
    str_squish()

  qu = texte_cut[1] |>
    str_replace_all('\\.', ' ') |>
    str_squish()

  return(c(qu, ans))
}

#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom dplyr case_when

get_bins = function(dataset, keep, quanti.threshold, quanti.mod){

  dta = dataset |>
    mutate(across(where(is.numeric), scale), .keep = 'unused')

  dta = dta |>
    mutate(across(where(is.numeric),
                  ~as.factor(case_when(
                    . >= quanti.threshold ~ quanti.mod[1],
                    . <= - quanti.threshold ~ quanti.mod[2],
                    .default = quanti.mod[3]))))

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
    filter(Estimate < 0) |>
    mutate(Sentence = tolower(glue("* {Variable}: {Level}")))
  right = res_cd_work |>
    filter(Estimate > 0) |>
    mutate(Sentence = tolower(glue("* {Variable}: {Level}")))

  ppt1 = glue('## Left side of the scale

The individuals have the following characteristics:
{paste(left$Sentence, collapse = "\n")}')

  ppt2 = glue('## Right side of the scale

The individuals have the following characteristics:
{paste(right$Sentence, collapse = "\n")}')

  return(paste(ppt1, ppt2, sep = '\n\n'))
}


#' Interpret a continuous latent variable
#'
#' Generate an LLM response to analyze a continuous latent variable.
#'
#' @param dataset a data frame made up of at least one quantitative variable and a set of quantitative variables and/or categorical variables.
#' @param num.var the number of the variable to be characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param quanti.threshold the threshold above (resp. below) which a scaled variable is considered significantly above (resp.below) the average. Used when converting continuous variables to categorical.
#' @param quanti.mod a vector of the 3 possible modalities for continuous variables converted to categorical according to the threshold. Default is "above average", "below average" and "average".
#' @param weights weights for the individuals (see [FactoMineR::condes()]).
#' @param proba the significance threshold considered to characterize the category (by default 0.05).
#'
#' @return A data frame containing the LLM's response.
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' @export
#'
#' @examples
#' library(FactoMineR)
#' data(decathlon)
#'
#' names(decathlon) <- c('Time taken to complete the 100m', 'Distance reached for the long jump', 'Distance reached for the shot put', 'Height reached for the high jump',  'Time taken to complete the 400m', 'Time taken to complete the 110m hurdle', 'Distance reached for the discus', 'Height reached for the pole vault', 'Distance reached for the javeline', 'Time taken to complete the 1500 m', 'Rank', 'Points', 'Competition')
#'
#' res_pca_deca <- FactoMineR::PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)
#' deca_work <- res_pca_deca$ind$coord |> as.data.frame()
#' deca_work <- deca_work[1,] |> cbind(decathlon)
#'
#' res_deca <- nail_condes(deca_work, 1, quanti.threshold = 1, quanti.mod = c('High', 'Low', 'Average'), introduction = "A study was led on athletes participating to a decathlon event. Their performance was assessed on each part of the decathlon, and they were all placed on a unidimensional scale.")
#'
#' cat(res_deca$response)

nail_condes = function(dataset, num.var,
                       introduction = '',
                       request = 'Please explain what differentiates individuals from both sides of the scale. Then give a name to the scale, and briefly explain why you chose that name.',
                       model = 'llama3',
                       quanti.threshold = 0, quanti.mod = c("Significantly above average", "Significantly below average", 'Average'),
                       weights = NULL, proba = 0.05,
                       generate = T){

  dta = get_bins(dataset, keep = num.var, quanti.threshold = quanti.threshold, quanti.mod = quanti.mod)

  res_cd = FactoMineR::condes(dta[-(num.var + 1)], 1, weights = weights, proba = proba)

  ppt = glue("# Introduction

             {introduction}

             # Task

             {request}

             # Data

             {get_sentences_condes(res_cd)}")

  if (!generate) return(ppt)

  res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
  res_llm$prompt = ppt
  return(res_llm)
}
