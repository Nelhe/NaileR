#' @importFrom dplyr filter select arrange desc
#' @importFrom glue glue glue_collapse
#' @importFrom tibble rownames_to_column
#' @importFrom utils globalVariables

# ---------------------------------------------------------------------------
# get_sentences_textual (MODIFIED)
# ---------------------------------------------------------------------------
#' @importFrom glue glue glue_collapse
get_sentences_textual = function(dataset, num.var, num.text, sample.pct){

  var_name <- colnames(dataset)[num.var]
  text_name <- colnames(dataset)[num.text]

  if (!is.factor(dataset[[var_name]])) {
    dataset[[var_name]] <- as.factor(dataset[[var_name]])
  }

  grouped_data <- split(dataset, dataset[[var_name]])

  ppts = list()

  for (grp_name in names(grouped_data)) {

    group_df <- grouped_data[[grp_name]]

    corpus <- group_df[[text_name]]
    corpus <- corpus[!is.na(corpus) & nzchar(trimws(corpus))]

    if (length(corpus) == 0) {
      ppts[[grp_name]] <- "This group has **no textual responses** to display."
      next
    }

    total_responses <- length(corpus)
    header <- ""

    # --- MODIFIED Sampling Logic ---
    if (sample.pct < 1.0) {
      # Calculer la taille de l'echantillon, avec un minimum de 1
      sample_size <- max(1, round(total_responses * sample.pct))
      corpus_sampled <- sample(corpus, sample_size)

      header <- glue::glue("Showing a sample of **{length(corpus_sampled)}** out of **{total_responses}** total responses for this group ({sample.pct * 100}% sample):")

    } else {
      corpus_sampled <- corpus
      header <- glue::glue("Showing all **{total_responses}** responses for this group:")
    }
    # --- END MODIFICATION ---

    corpus_md <- glue::glue_collapse(glue::glue("* {corpus_sampled}"), sep = "\n")

    ppts[[grp_name]] <- paste(header, corpus_md, sep = "\n\n")
  }

  return(ppts)
}

# ---------------------------------------------------------------------------
# get_prompt_textual (MODIFIED)
# ---------------------------------------------------------------------------
#' @importFrom glue glue
get_prompt_textual = function(dataset, num.var, num.text,
                              introduction, request, conclusion,
                              isolate.groups, sample.pct){ # <- Changed

  sentences_list <- get_sentences_textual(dataset, num.var, num.text, sample.pct) # <- Changed

  if (length(sentences_list) == 0 || all(nchar(sentences_list) == 0))
    stop("No textual data found to process, execution was halted.")

  all_groups <- names(sentences_list)
  stces <- c()

  for (grp in all_groups){
    sent = ifelse(is.null(sentences_list[[grp]]), '', sentences_list[[grp]])

    ppt_grp = glue::glue(
      "## **Group '{grp}'**:\n\n",
      "{sent}",
      .trim = TRUE
    )

    stces <- c(stces, ppt_grp)
  }

  header <- glue::glue(
    "# Introduction - Objective\n\n",
    "{introduction}\n\n",
    "# Task\n\n",
    "{request}\n\n",
    "---\n\n",
    "# Data\n\n"
  )

  if (!isolate.groups) {
    body <- paste(stces, collapse = "\n\n---\n\n")
    return(paste(header, body, "\n\n", conclusion, sep = ""))

  } else {
    prompts_list <- list()
    for (i in seq_along(stces)) {
      grp_name <- names(sentences_list)[i]
      grp_body <- stces[i]
      prompts_list[[grp_name]] <- paste(header, grp_body, "\n\n", conclusion, sep = "")
    }
    return(prompts_list)
  }
}

#' Interpret a group based on answers to open-ended questions
#'
#' Generate an LLM response to analyze a categorical latent variable, based on answers to open-ended questions.
#'
#' @param dataset a data frame made up of at least one categorical variable and a textual variable.
#' @param num.var the index of the categorical variable to be characterized.
#' @param num.text the index of the textual variable that characterizes the categorical variable of interest.
#' @param introduction the introduction for the LLM prompt.
#' @param request the request made to the LLM.
#' @param conclusion the conclusion for the LLM prompt.
#' @param model the model name ('llama3' by default).
#' @param isolate.groups a boolean that indicates whether to give the LLM a single prompt, or one prompt per category. Recommended with long catdes results.
#' @param sample.pct The proportion (0.0 to 1.0) of textual responses to include *per group*. Default is 1.0 (100%).
#' @param generate a boolean that indicates whether to generate the LLM response. If FALSE, the function only returns the prompt.
#' @param ... Additional arguments passed to 'ollamar::generate'
#' (e.g., `temperature`, `seed`).
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
#' ### Example 1: Car alone survey ###
#' library(NaileR)
#' library(dplyr)
#' data(car_alone)
#'
#' sampled_car_alone <- car_alone %>%
#' group_by(car_alone_capable_restrictive) %>%
#' dplyr::sample_frac(0.5)
#' sampled_car_alone <- as.data.frame(sampled_car_alone)
#'
#' intro_car <- "Knowing the impact on the climate,
#' I have made these choices based on
#' the following benefits and constraints..."
#' intro_car <- gsub('\n', ' ', intro_car) |>
#' stringr::str_squish()
#'
#' res_nail_textual <- nail_textual(sampled_car_alone, num.var = 1,
#'                                  num.text = 2,
#'                                  introduction = intro_car,
#'                                  request = NULL,
#'                                  model = 'llama3', isolate.groups = TRUE,
#'                                  generate = TRUE)
#'
#' res_nail_textual[[1]]$response |> cat()
#' res_nail_textual[[3]]$response |> cat()
#' res_nail_textual[[2]]$response |> cat()
#' res_nail_textual[[4]]$response |> cat()
#'
#' ### Example 2: Atomic habits survey ###
#' library(NaileR)
#' library(dplyr)
#' data(atomic_habit_clust)
#'
#' intro_atomic <- "These data were collected
#' after a survey on atomic habits: we asked
#' what people were prepared to change about their daily habits
#' to make the world a better place,
#' what habits they felt able to adopt,
#' what habits were restrictive."
#' intro_atomic <- gsub('\n', ' ', intro_atomic) |>
#' stringr::str_squish()
#'
#' dta_plane <- atomic_habit_clust[,c(32,51)] %>%
#'             filter(never_plane_text != 'THAT')
#'
#' sampled_dta_plane <- dta_plane %>%
#'                     group_by(clust) %>%
#'                     dplyr::sample_frac(0.75)
#'
#' sampled_dta_plane <- as.data.frame(sampled_dta_plane)
#' summary(sampled_dta_plane)
#'
#' res_nail_textual_plane <- nail_textual(sampled_dta_plane, num.var = 2,
#'                                        num.text = 1,
#'                                        introduction = intro_atomic,
#'                                        request = NULL,
#'                                        model = 'llama3',
#'                                        isolate.groups = TRUE,
#'                                        generate = TRUE)
#'
#' cat(res_nail_textual_plane[[1]]$prompt)
#' cat(res_nail_textual_plane[[1]]$response)
#'
#' cat(res_nail_textual_plane[[2]]$prompt)
#' cat(res_nail_textual_plane[[2]]$response)
#'
#' cat(res_nail_textual_plane[[3]]$prompt)
#' cat(res_nail_textual_plane[[3]]$response)
#'
#' res_nail_textual_plane <- nail_textual(sampled_dta_plane, num.var = 2,
#'                                        num.text = 1,
#'                                        introduction = intro_atomic,
#'                                        request = NULL,
#'                                        model = 'llama3',
#'                                        isolate.groups = FALSE,
#'                                        generate = TRUE)
#'
#' cat(res_nail_textual_plane$prompt)
#' cat(res_nail_textual_plane$response)
#'
#' ### Example 3: Car seat fabrics ###
#'
#' # Drivers of liking and disliking
#' # isolate.groups = F
#'
#' intro_car <- "In this consumer study, a number of car seat fabrics
#' were rated by consumers who gave their reasons
#' for liking or disliking the fabrics.
#' Reasons for disliking the fabrics were reported in group '0',
#' while reasons for liking the fabrics were reported in group '1'."
#' intro_car <- gsub('\n', ' ', intro_car) |>
#' stringr::str_squish()
#'
#' request_car <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were not appreciated (group '0'),
#' and the reasons why fabrics were appreciated (group '1').
#' In other words, what are the drivers for disliking
#' and liking the fabrics."
#' request_car <- gsub('\n', ' ', request_car) |>
#' stringr::str_squish()
#'
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car,
#'                                         request = request_car,
#'                                         model = 'llama3',
#'                                         isolate.groups = FALSE,
#'                                         generate = TRUE)
#'
#' cat(res_nail_textual_fabric$response)
#'
#' # Drivers of disliking with a specific prompt
#' # isolate.groups = T
#'
#' intro_car_disliking <- "In this consumer study, a range of car seat fabrics
#' were rated by consumers who gave their reasons
#' for disliking the fabrics.
#' In these data, only the reasons for disliking the fabrics were reported."
#' intro_car_disliking <- gsub('\n', ' ', intro_car_disliking) |>
#' stringr::str_squish()
#'
#' request_car_disliking <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were not appreciated.
#' In other words, what are the drivers for disliking the fabrics."
#' request_car_disliking <- gsub('\n', ' ', request_car_disliking) |>
#' stringr::str_squish()
#'
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car_disliking,
#'                                         request = request_car_disliking,
#'                                         model = 'llama3',
#'                                         isolate.groups = TRUE,
#'                                         generate = FALSE)
#'
#' ppt <- res_nail_textual_fabric[1]
#' cat(ppt)
#'
#' res_disliking <- ollamar::generate(model = 'llama3', prompt = ppt,
#'                                    output = "df")
#' cat(res_disliking$response)
#'
#' # Drivers of liking with a specific prompt
#' # isolate.groups = T
#'
#' intro_car_liking <- "In this consumer study, a range of car seat fabrics
#' were rated by consumers who gave their reasons
#' for liking the fabrics.
#' In these data, only the reasons for liking the fabrics were reported."
#' intro_car_liking <- gsub('\n', ' ', intro_car_liking) |>
#' stringr::str_squish()
#'
#' request_car_liking <- "Based on the comments provided by the consumers,
#' please explain the reasons why
#' the fabrics were appreciated.
#' In other words, what are the drivers for liking the fabrics."
#' request_car_liking <- gsub('\n', ' ', request_car_liking) |>
#' stringr::str_squish()
#'
#' res_nail_textual_fabric <- nail_textual(fabric, num.var = 4,
#'                                         num.text = 3,
#'                                         introduction = intro_car_liking,
#'                                         request = request_car_liking,
#'                                         model = 'llama3',
#'                                         isolate.groups = TRUE,
#'                                         generate = FALSE)
#'
#' ppt <- res_nail_textual_fabric[2]
#' cat(ppt)
#'
#' res_liking <- ollamar::generate(model = 'llama3', prompt = ppt,
#'                                 output = "df")
#' cat(res_liking$response)
#'
#' ### Example 4: Rorschach inkblots ###
#'
#' # Description of each inkblot
#' # isolate.groups = TRUE
#'
#' intro_rorschach <- "For this study,
#' we asked sixty people to briefly describe
#' one of the inkblots of the Rorschach test."
#' intro_rorschach <- gsub('\n', ' ', intro_rorschach) |>
#' stringr::str_squish()
#'
#' request_rorschach <- "Based on the comments of the 60 people,
#' please give me a description of that inkblot
#' in terms of how it was perceived. Tell me if it was
#' a rather positive or negative perception."
#' request_rorschach <- gsub('\n', ' ', request_rorschach) |>
#' stringr::str_squish()
#'
#' res_nail_textual_rorschach <- nail_textual(rorschach, num.var = 2,
#'                                            num.text = 5,
#'                                            introduction = intro_rorschach,
#'                                            request = request_rorschach,
#'                                            model = 'llama3',
#'                                            isolate.groups = TRUE,
#'                                            generate = FALSE)
#'
#' cat(res_nail_textual_rorschach[[10]])
#'
#' ppt <- gsub("## Group", "## Stimulus", res_nail_textual_rorschach[[10]])
#' cat(ppt)
#'
#' res_inkblot_10 <- ollamar::generate(model = 'llama3', prompt = ppt, output = "df")
#' cat(res_inkblot_10$response)
#'
#' cat(res_nail_textual_rorschach[[5]])
#'
#' ppt <- gsub("## Group", "## Stimulus", res_nail_textual_rorschach[[5]])
#' cat(ppt)
#'
#' res_inkblot_5 <- ollamar::generate(model = 'llama3', prompt = ppt,
#'                                    output = "df")
#' cat(res_inkblot_5$response)
#'
#'
#' #Comparison of panels
#'
#' rorschach_10 <- droplevels(rorschach[rorschach$Inkblot=="10",])
#'
#' intro_rorschach <- "For this study,
#' we asked sixty people to briefly describe
#' one of the inkblots of the Rorschach test.
#' The sixty people belonged to three different panels,
#' with 20 people per panel."
#' intro_rorschach <- gsub('\n', ' ', intro_rorschach) |>
#' stringr::str_squish()
#'
#' request_rorschach <- "Based on the comments of the 60 people,
#' please tell me what is common from panel to panel
#' and what is specific to each panel
#' in terms of the perception of the inkblot."
#' request_rorschach <- gsub('\n', ' ', request_rorschach) |>
#' stringr::str_squish()
#'
#' res_nail_textual_rorschach <- nail_textual(rorschach_10, num.var = 1,
#'                                            num.text = 5,
#'                                            introduction = intro_rorschach,
#'                                            request = request_rorschach,
#'                                            model = 'llama3',
#'                                            isolate.groups = FALSE,
#'                                            generate = TRUE)
#'
#' cat(res_nail_textual_rorschach$prompt)
#' cat(res_nail_textual_rorschach$response)
#' }
#'
#' @importFrom glue glue
#' @export
nail_textual = function(dataset, num.var,
                        num.text,
                        introduction = NULL,
                        request = NULL,
                        conclusion = NULL,
                        model = 'llama3',
                        isolate.groups = TRUE,
                        sample.pct = 1.0, # <- MODIFIED (default = 100%)
                        generate = FALSE,
                        ...){

  # --- Defaults ---
  if (is.null(introduction)) {
    introduction <- "For this study, individuals answered an open-ended question. These individuals were grouped into different categories."
  }

  if (is.null(request)) {
    if (!isolate.groups) {
      request <- "Based on the raw textual responses, please describe what characterizes the individuals of each group and what sets them apart from the other groups. Then, based on these characteristics, give each group a new name."
    } else {
      request <- "Based on the raw textual responses, please describe what characterizes the individuals of this group and what makes them so special. Then, based on these characteristics, give the group a new name."
    }
  }

  if (is.null(conclusion)) {
    if (!isolate.groups) {
      conclusion <- sprintf("# Final Summary Task\nAt the end, provide:\n1. **A comparison of all groups**.\n2. **A list of group names you assigned**.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**.")
    } else {
      conclusion <- sprintf("# Final Summary Task\nAt the end, provide:\n1. **A description of the group**.\n2. **A group name you assigned**.\n\n# Output format\nYour output must be **formatted using valid Quarto Markdown**.")
    }
  }

  # --- "How to Read" Guide ---
  GUIDE_TEXTUAL <- paste(
    "## How to Read the Data",
    "The data provided below is a *sample* of the raw, verbatim responses from individuals within each group.",
    "Each bullet point (`*`) represents one person's answer.",
    "Your task is to synthesize these qualitative responses to understand the profile of the group.",
    sep = "\n"
  )

  introduction <- paste(glue::glue(introduction),
                        glue::glue(GUIDE_TEXTUAL),
                        sep = "\n\n---\n\n")

  # --- Handle 'stop()' gracefully ---
  ppt <- tryCatch({
    get_prompt_textual(
      dataset = dataset,
      num.var = num.var,
      num.text = num.text,
      introduction = introduction,
      request = request,
      conclusion = conclusion,
      isolate.groups = isolate.groups,
      sample.pct = sample.pct # <- MODIFIED
    )
  },
  error = function(e) {
    if (grepl("No textual data found", conditionMessage(e))) {
      return("NAILER_NO_RESULTS_FOUND")
    } else {
      stop(e)
    }
  })

  # --- Handle "No Results" Case ---
  if (identical(ppt, "NAILER_NO_RESULTS_FOUND")) {

    no_results_message <- "*No textual data was found for the specified groups.*"

    if (generate) {
      message("Execution halted: No textual data found. Nothing to generate.")
      if (isolate.groups) {
        return(list())
      } else {
        return(data.frame(
          model = model,
          response = "No textual data found.",
          prompt = no_results_message,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      header <- glue::glue(
        "# Introduction - Objective\n\n{introduction}\n\n",
        "# Task\n\n{request}\n\n",
        "---\n\n",
        "# Data\n\n"
      )
      return(paste0(header, no_results_message))
    }
  }

  # --- Handle generate = FALSE ---
  if (!generate) return(ppt)

  # --- Handle generate = TRUE (with results) ---

  extra_args <- list(...)
  valid_ollama_opts <- c("temperature", "top_p", "top_k", "seed",
                         "system", "template", "context", "keep_alive",
                         "stream", "format")
  ollama_api_options <- extra_args[names(extra_args) %in% valid_ollama_opts]

  if (!isolate.groups) {

    call_args <- c(
      list(
        model = model,
        prompt = ppt,
        output = "text"
      ),
      ollama_api_options
    )

    res_llm <- tryCatch({
      do.call(ollamar::generate, call_args)
    }, error = function(e) {
      stop(paste("Ollama API call (generate=TRUE) failed:", conditionMessage(e)))
    })

    return(list(prompt = ppt, response = res_llm, model = model))

  } else {
    list_rep <- lapply(names(ppt), function(grp_name) {
      prpt <- ppt[[grp_name]]

      call_args_loop <- c(
        list(
          model = model,
          prompt = prpt,
          output = "text"
        ),
        ollama_api_options
      )

      res_llm <- tryCatch({
        do.call(ollamar::generate, call_args_loop)
      }, error = function(e) {
        stop(paste("Ollama API call (generate=TRUE, isolate.groups=TRUE) failed:", conditionMessage(e)))
      })

      return(list(prompt = prpt, response = res_llm, model = model))
    })
    names(list_rep) <- names(ppt)
    return(list_rep)
  }
}
