
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

get_bins = function(dataset, keep, quanti.threshold, quanti.cat){

  dta = dataset |>
    mutate(across(where(is.numeric), scale), .keep = 'unused')

  dta = dta |>
    mutate(across(where(is.numeric),
                  ~as.factor(case_when(
                    . >= quanti.threshold ~ quanti.cat[1],
                    . <= - quanti.threshold ~ quanti.cat[2],
                    .default = quanti.cat[3]))))

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
#' @param num.var the index of the variable to be characterized.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param model the model name ('llama3' by default).
#' @param quanti.threshold the threshold above (resp. below) which a scaled variable is considered significantly above (resp.below) the average. Used when converting continuous variables to categorical ones.
#' @param quanti.cat a vector of the 3 possible categories for continuous variables converted to categorical ones according to the threshold. Default is "above average", "below average" and "average".
#' @param weights weights for the individuals (see [FactoMineR::condes()]).
#' @param proba the significance threshold considered to characterize the category (by default 0.05).
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#'
#' @return A data frame containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: decathlon dataset ###
#'
#' library(FactoMineR)
#' data(decathlon)
#'
#' names(decathlon) <- c('Time taken to complete the 100m',
#' 'Distance reached for the long jump',
#' 'Distance reached for the shot put',
#' 'Height reached for the high jump',
#' 'Time taken to complete the 400m',
#' 'Time taken to complete the 110m hurdle',
#' 'Distance reached for the discus',
#' 'Height reached for the pole vault',
#' 'Distance reached for the javeline',
#' 'Time taken to complete the 1500 m',
#' 'Rank/Counter-performance indicator',
#' 'Points', 'Competition')
#'
#' res_pca_deca <- FactoMineR::PCA(decathlon,
#' quanti.sup = 11:12, quali.sup = 13, graph = FALSE)
#' plot.PCA(res_pca_deca, choix = 'var')
#' deca_work <- res_pca_deca$ind$coord |> as.data.frame()
#' deca_work <- deca_work[,1] |> cbind(decathlon)
#'
#' intro_deca <- "A study was led on athletes
#' participating in a decathlon event.
#' Their performance was assessed on each part of the decathlon,
#' and they were all placed on an unidimensional scale."
#' intro_deca <- gsub('\n', ' ', intro_deca) |>
#' stringr::str_squish()
#'
#' res_deca <- nail_condes(deca_work, num.var = 1,
#' quanti.threshold = 1, quanti.cat = c('High', 'Low', 'Average'),
#' introduction = intro_deca)
#'
#' cat(res_deca$response)
#'
#'
#' ### Example 2: agri_studies dataset ###
#'
#' data(agri_studies)
#'
#' set.seed(1)
#' res_mca_agri <- FactoMineR::MCA(agri_studies, quali.sup = 39:42,
#' level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_agri, choix = 'ind',
#' invisible = c('var', 'quali.sup'), label = 'none')
#'
#' agri_work <- res_mca_agri$ind$coord |> as.data.frame()
#' agri_work <- agri_work[,1] |> cbind(agri_studies)
#'
#' intro_agri <- "These data were collected after a survey
#' on students' expectations of agribusiness studies.
#' Participants had to rank how much they agreed with 38 statements
#' about possible benefits from agribusiness studies;
#' then, they were asked personal questions."
#' intro_agri <- gsub('\n', ' ', intro_agri) |>
#' stringr::str_squish()
#'
#' res_agri <- nail_condes(agri_work, num.var = 1,
#' introduction = intro_agri)
#' cat(res_agri$response)
#'
#'
#' ### Example 3: glossophobia dataset ###
#'
#' data(glossophobia)
#'
#' set.seed(1)
#' res_mca_phobia <- FactoMineR::MCA(glossophobia,
#' quali.sup = 26:41, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_phobia, choix = 'ind',
#' invisible = c('var', 'quali.sup'), label = 'none')
#'
#' phobia_work <- res_mca_phobia$ind$coord |> as.data.frame()
#' phobia_work <- phobia_work[,1] |> cbind(glossophobia)
#'
#' intro_phobia <- "These data were collected after a survey
#' on participants' feelings about speaking in public.
#' Participants had to rank how much they agreed with
#' 25 descriptions of speaking in public;
#' then, they were asked personal questions."
#' intro_phobia <- gsub('\n', ' ', intro_phobia) |>
#' stringr::str_squish()
#'
#' res_phobia <- nail_condes(phobia_work, num.var = 1,
#' introduction = intro_phobia)
#' cat(res_phobia$response)
#'
#'
#' ### Example 4: beard_cont dataset ###
#'
#' data(beard_cont)
#'
#' set.seed(1)
#' res_ca_beard <- FactoMineR::CA(beard_cont, graph = FALSE)
#' plot.CA(res_ca_beard, invisible = 'col')
#'
#' beard_work <- res_ca_beard$row$coord |> as.data.frame()
#' beard_work <- beard_work[,1] |> cbind(beard_cont)
#'
#' intro_beard <- "These data refer to 8 types of beards.
#' Each beard was evaluated by 62 assessors."
#' intro_beard <- gsub('\n', ' ', intro_beard) |>
#' stringr::str_squish()
#'
#' req_beard <- "Please explain what differentiates beards
#' on both sides of the scale.
#' Then, give the scale a name."
#' req_beard <- gsub('\n', ' ', req_beard) |>
#' stringr::str_squish()
#'
#' res_beard <- nail_condes(beard_work, num.var = 1,
#' quanti.threshold = 0.5, quanti.cat = c('Very often', 'Never', 'Sometimes'),
#' introduction = intro_beard, request = req_beard,
#' generate = FALSE)
#'
#' cat(res_beard$prompt)
#'
#' ppt <- stringr::str_replace_all(res_beard$prompt, 'individuals', 'beards')
#' cat(ppt)
#'
#' res_beard <- ollamar::generate(model = 'llama3', prompt = ppt, output = 'df')
#'
#' cat(res_beard$response)
#' }

nail_condes = function(dataset, num.var,
                       introduction = '',
                       request = NULL,
                       model = 'llama3',
                       quanti.threshold = 0, quanti.cat = c("Significantly above average", "Significantly below average", 'Average'),
                       weights = NULL, proba = 0.05,
                       generate = TRUE){

  if (is.null(request)) request <- 'Please explain what differentiates individuals from both sides of the scale. Then give a name to the scale, and briefly explain why you chose that name.'
  dta = get_bins(dataset, keep = num.var, quanti.threshold = quanti.threshold, quanti.cat = quanti.cat)

  res_cd = FactoMineR::condes(dta[-(num.var + 1)], 1, weights = weights, proba = proba)

  ppt = glue("# Introduction

             {introduction}

             # Task

             {request}

             # Data

             {get_sentences_condes(res_cd)}")

  if (!generate) return(data.frame(prompt = ppt))

  res_llm = ollamar::generate(model = model, prompt = ppt, output = 'df')
  res_llm$prompt = ppt
  return(res_llm)
}
