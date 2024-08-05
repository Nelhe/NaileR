
#' @importFrom dplyr mutate
#' @importFrom glue glue

get_sentences_qda <- function(res_cd, drop.negative){

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

        res_cd_work_left <- res_cd_work[res_cd_work$v.test>0,]
        res_cd_work_right <- res_cd_work[res_cd_work$v.test<0,]
        res_cd_work_right <- res_cd_work_right[order(res_cd_work_right$v.test),]

        left = res_cd_work_left$Variable |>
          paste(collapse = '\n')

        if (!drop.negative){
          right = res_cd_work_right$Variable |>
            paste(collapse = '\n')
        } else right = ''

        ppt1 = ifelse(nchar(left) == 0,
                      '',
                      glue('For the following perceptual attributes, this stimulus has been scored with rather *high* values compared to the average over all stimuli; attributes have been sorted from the most discriminative one to the less discriminative one:
                       {left}'))

        ppt2 = ifelse(nchar(right) == 0,
                      '',
                      glue('For the following perceptual attributes, this stimulus has been scored with rather *low* values compared to the average over all stimuli; attributes have been sorted from the most discriminative one to the less discriminative one:
                       {right}'))

        ppts[[names(res_cd)[i]]] = paste(ppt1, ppt2, sep = "\n")

      } else ppts[[names(res_cd)[i]]] = ''
    }

    return(ppts)
  }

get_prompt_qda = function(res_cd, introduction, request, isolate.groups, drop.negative){

    if ("category" %in% names(res_cd)){
      stces_quali = get_sentences_quali(res_cd, drop.negative)
    } else stces_quali = list()

    if ("quanti" %in% names(res_cd)){
      stces_quanti = get_sentences_qda(res_cd, drop.negative)
    } else stces_quanti = list()

    if (nchar(stces_quali[1]) == 0 & nchar(stces_quanti[1]) == 0) stop('No significant differences between stimuli, execution was halted.')

    all_groups = union(names(stces_quali), names(stces_quanti))

    stces = c()

    for (grp in all_groups){
      qual = ifelse(is.null(stces_quali[[grp]]), '', stces_quali[[grp]])
      quant = ifelse(is.null(stces_quanti[[grp]]), '', stces_quanti[[grp]])

      ppt_grp = glue('## Stimuli "{grp}":

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


#' Interpret QDA data
#'
#' Generate an LLM response to analyze QDA data.
#'
#' @param dataset a data frame made up of at least two categorical variables (product, panelist) and a set of quantitative variables (sensory attributes).
#' @param formul the analyis of variance model to be evaluated for each sensory attribute.
#' @param firstvar the index of the first sensory attribute.
#' @param lastvar the index of the last sensory attribute.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request for the LLM prompt.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per product.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests).
#' @param proba the significance threshold considered to characterize the products (by default 0.05).
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#'
#' @return A data frame, or a list of data frames, containing the LLM's prompt and response (if generate = TRUE).
#'
#' @details This function directly sends a prompt to an LLM. Therefore, to get a consistent answer, we highly recommend to customize the parameters introduction and request and add all relevant information on your data for the LLM. We also recommend renaming the columns with clear, unshortened and unambiguous names.
#'
#' Additionally, if isolate.groups = TRUE, you will need an introduction and a request that take into account the fact that only one group is analyzed at a time.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Processing time is often longer than ten seconds
#' # because the function uses a large language model.
#'
#' ### Example 1: QDA data on chocolates  ###
#' library(NaileR)
#' library(SensoMineR)
#' data(chocolates)
#'
#' intro_sensochoc <- "Six chocolates were measured according
#' to sensory attributes by a trained panel.
#' I will give you the results from this study.
#' You will have to identify what sets these chocolates apart."
#' intro_sensochoc <- gsub('\n', ' ', intro_sensochoc) |>
#' stringr::str_squish()
#'
#' req_sensochoc <- "Please explain what makes each chocolate distinct
#' and provide a sensory profile of each chocolate."
#' req_sensochoc <- gsub('\n', ' ', req_sensochoc) |>
#' stringr::str_squish()
#'
#' res_nail_qda <- nail_qda(sensochoc,
#'                          formul="~Product+Panelist",
#'                          firstvar = 5,
#'                          introduction = intro_sensochoc,
#'                          request = NULL,
#'                          model = 'llama3',
#'                          isolate.groups = FALSE,
#'                          drop.negative = FALSE,
#'                          proba = 0.05,
#'                          generate = TRUE)
#'
#' cat(res_nail_qda$response)
#' }

#' @importFrom SensoMineR decat

nail_qda = function(dataset,
                    formul,
                    firstvar,
                    lastvar = length(colnames(dataset)),
                    introduction = '',
                    request = NULL,
                    model = 'llama3',
                    isolate.groups = FALSE,
                    drop.negative = FALSE,
                    proba = 0.05,
                    generate = TRUE){


    if (is.null(request)) request <- "Based on the results, please describe what characterizes the stimuli and what sets them apart. Then, based on these characteristics, give each stimulus a new name."

    res_cd = SensoMineR::decat(dataset, formul = formul, firstvar = firstvar, lastvar = lastvar, proba = proba, graph = FALSE)

    names(res_cd)[6] <- "quanti"
    for (i in 1:length(res_cd$quanti)){
      colnames(res_cd$quanti[[i]])[3] <-  "p.value"
      colnames(res_cd$quanti[[i]])[4] <- "v.test"
    }

    ppt = get_prompt_qda(res_cd, introduction = introduction, request = request,
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
