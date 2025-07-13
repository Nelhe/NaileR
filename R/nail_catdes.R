
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

get_sentences_quali <- function(res_cd, quali.sample, drop.negative) {
  res_cd <- res_cd$category
  ppts <- list()

  for (i in seq_along(res_cd)) {
    group_name <- names(res_cd)[i]
    res_mat <- as.data.frame(res_cd[[i]])

    # Check valid matrix
    if (!is.data.frame(res_mat) || nrow(res_mat) == 0 || ncol(res_mat) == 0) {
      message(glue::glue("Skipping group {group_name}: empty or invalid data (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Ensure numeric column exists
    is_num_col <- sapply(res_mat, is.numeric)
    if (!any(is_num_col)) {
      message(glue::glue("Skipping group {group_name}: no numeric column found (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Sampling step
    num_index <- which(is_num_col)[1]
    if (quali.sample < 1) {
      res_cd[[i]] <- sample_numeric_distribution(
        res_mat,
        num_var_index = num_index,
        sample_pct = quali.sample,
        method = "stratified",
        bins = 5,
        return_matrix = FALSE
      )
    } else {
      res_cd[[i]] <- res_mat
    }

    res_cd2 <- res_cd[[i]]

    # Skip if bad structure
    if (is.null(res_cd2) || !is.data.frame(res_cd2) ||
        nrow(res_cd2) == 0 || is.null(rownames(res_cd2)) ||
        !all(nzchar(rownames(res_cd2)))) {
      message(glue::glue("Skipping group {group_name}: res_cd2 is NULL or malformed."))
      ppts[[group_name]] <- ""
      next
    }

    # Parse rownames
    parsed_rows <- try(sapply(rownames(res_cd2), tidy_answer_catdes), silent = TRUE)

    if (inherits(parsed_rows, "try-error")) {
      message(glue::glue("Skipping group {group_name}: error while parsing rownames -> {parsed_rows}"))
      ppts[[group_name]] <- ""
      next
    }

    parsed_df <- as.data.frame(t(parsed_rows), stringsAsFactors = FALSE)

    if (ncol(parsed_df) != 2) {
      message(glue::glue("Skipping group {group_name}: tidy_answer_catdes returned {ncol(parsed_df)} columns"))
      ppts[[group_name]] <- ""
      next
    }

    colnames(parsed_df) <- c("Variable", "Level")

    # Join with v.test
    if (!"v.test" %in% colnames(res_cd2)) {
      message(glue::glue("Skipping group {group_name}: v.test column missing in res_cd2"))
      ppts[[group_name]] <- ""
      next
    }

    res_cd_work <- cbind(parsed_df, v.test = res_cd2$v.test)

    # Format positively associated categories
    more <- res_cd_work |>
      dplyr::filter(v.test > 0) |>
      dplyr::mutate(Variable = glue::glue("\"{Variable}\"")) |>
      dplyr::mutate(Sentence = tolower(glue::glue("* {Variable}: {Level}")))

    # Format negatively associated categories
    if (!drop.negative) {
      less <- res_cd_work |>
        dplyr::filter(v.test < 0) |>
        dplyr::arrange(v.test) |>  # from most to least discriminant (most negative first)
        dplyr::mutate(Variable = glue::glue("\"{Variable}\"")) |>
        dplyr::mutate(Sentence = tolower(glue::glue("* {Variable}: {Level}")))
    } else {
      less <- NULL
    }

    # Build paragraphs
    ppt1 <- if (nrow(more) > 0) {
      glue::glue(
        "Observations in this group are *more* likely to be associated with the following response categories. In this output, the name of the variable precedes the category that characterises our observations by its strong association:\n{paste(more$Sentence, collapse = '\n')}"
      )
    } else ""

    ppt2 <- if (!drop.negative && !is.null(less) && nrow(less) > 0) {
      glue::glue(
        "Observations in this group are *less* likely to be associated with the following response categories. In this output, the name of the variable precedes the category that characterises our observations by its weak association (listed from most to least discriminant):\n{paste(less$Sentence, collapse = '\n')}"
      )
    } else ""

    ppts[[group_name]] <- paste(ppt1, ppt2, sep = "\n")
  }

  return(ppts)
}

#' @importFrom dplyr select

get_sentences_quanti <- function(res_cd, quanti.sample, drop.negative) {
  res_cd <- res_cd$quanti
  ppts <- list()

  for (i in seq_along(res_cd)) {
    group_name <- names(res_cd)[i]
    res_mat <- as.data.frame(res_cd[[i]])

    # Vérification que le tableau est valide
    if (!is.data.frame(res_mat) || nrow(res_mat) == 0 || ncol(res_mat) == 0) {
      message(glue::glue("Skipping group {group_name}: empty or invalid data (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    # Détection d'une colonne numérique
    is_num_col <- sapply(res_mat, is.numeric)
    if (!any(is_num_col)) {
      message(glue::glue("Skipping group {group_name}: no numeric column found (res_cd[[{i}]])"))
      ppts[[group_name]] <- ""
      next
    }

    num_index <- which(is_num_col)[1]

    # Échantillonnage si requis
    if (quanti.sample < 1) {
      res_cd[[i]] <- sample_numeric_distribution(
        res_mat,
        num_var_index = num_index,
        sample_pct = quanti.sample,
        method = "stratified",
        bins = 5,
        return_matrix = FALSE
      )
    } else {
      res_cd[[i]] <- res_mat
    }

    res_cd2 <- res_cd[[i]]

    if (!is.null(res_cd2) && is.data.frame(res_cd2) && nrow(res_cd2) > 0) {

      # Vérification des colonnes requises
      if (!all(c("v.test", "p.value") %in% colnames(res_cd2))) {
        message(glue::glue("Skipping group {group_name}: missing v.test or p.value in res_cd2"))
        ppts[[group_name]] <- ""
        next
      }

      # Création du tableau
      res_cd_work <- res_cd2 |>
        dplyr::select(v.test, p.value) |>
        dplyr::mutate(Variable = rownames(res_cd2) |>
                        sapply(tidy_answer_catdes)) |>
        dplyr::mutate(Variable = glue::glue("* {Variable}"))

      # Variables avec valeurs élevées (v.test > 0)
      left <- res_cd_work |>
        dplyr::filter(v.test > 0) |>
        dplyr::arrange(desc(v.test)) |>
        dplyr::pull(Variable) |>
        paste(collapse = "\n")

      # Variables avec valeurs faibles (v.test < 0)
      if (!drop.negative) {
        right <- res_cd_work |>
          dplyr::filter(v.test < 0) |>
          dplyr::arrange(v.test) |>  # De la plus discriminante à la moins
          dplyr::pull(Variable) |>
          paste(collapse = "\n")
      } else {
        right <- ""
      }

      # Génération du texte
      ppt1 <- if (nchar(left) > 0) {
        glue::glue("Observations in this group have quite *high* values for the following variables (listed from the most to the least discriminant):\n{left}")
      } else ""

      ppt2 <- if (nchar(right) > 0) {
        glue::glue("Observations in this group have quite *low* values for the following variables (listed from the most to the least discriminant):\n{right}")
      } else ""

      ppts[[group_name]] <- paste(ppt1, ppt2, sep = "\n")
    } else {
      message(glue::glue("Skipping group {group_name}: res_cd2 is NULL or empty."))
      ppts[[group_name]] <- ""
    }
  }

  return(ppts)
}

get_prompt_catdes <- function(res_cd, introduction, request, isolate.groups, quali.sample, quanti.sample, drop.negative) {
  if ("category" %in% names(res_cd)) {
    stces_quali <- get_sentences_quali(res_cd, quali.sample, drop.negative)
  } else {
    stces_quali <- list()
  }

  if ("quanti" %in% names(res_cd)) {
    stces_quanti <- get_sentences_quanti(res_cd, quanti.sample, drop.negative)
  } else {
    stces_quanti <- list()
  }

  if (length(stces_quali) == 0 && length(stces_quanti) == 0) {
    stop("No significant differences between groups, execution was halted.")
  }

  all_groups <- union(names(stces_quali), names(stces_quanti))
  stces <- c()

  for (grp in all_groups) {
    qual <- stces_quali[[grp]]
    quant <- stces_quanti[[grp]]
    if (is.null(qual) || nchar(trimws(qual)) == 0) qual <- NULL
    if (is.null(quant) || nchar(trimws(quant)) == 0) quant <- NULL

    block_parts <- c(qual, quant)
    block_parts <- block_parts[!sapply(block_parts, is.null)]

    block <- paste(block_parts, collapse = "\n\n")

    if (nchar(block) > 0) {
      ppt_grp <- glue::glue('## Group "{grp}":\n\n{block}')
      stces <- c(stces, ppt_grp)
    }
  }

  body <- if (isolate.groups) {
    paste(stces, collapse = "\n\n---\n\n")
  } else {
    paste(stces, collapse = "\n\n")
  }

  header <- glue::glue(
    "# Introduction\n\n{introduction}\n\n",
    "# Task\n\n{request}\n\n",
    "# Data\n\n"
  )

  if (!isolate.groups) {
    return(paste0(header, body))
  } else {
    prompts <- list()
    for (grp in all_groups) {
      qual <- stces_quali[[grp]]
      quant <- stces_quanti[[grp]]
      if (is.null(qual) || nchar(trimws(qual)) == 0) qual <- NULL
      if (is.null(quant) || nchar(trimws(quant)) == 0) quant <- NULL

      block_parts <- c(qual, quant)
      block_parts <- block_parts[!sapply(block_parts, is.null)]
      block_parts <- block_parts[nzchar(trimws(block_parts))]

      block <- paste(block_parts, collapse = "\n\n")

      if (nchar(block) > 0) {
        grp_intro <- glue::glue("# Introduction\n\n{introduction}\n\n")
        grp_task  <- glue::glue("# Task\n\n{request}\n\n")
        grp_data  <- glue::glue("# Data\n\n## Group \"{grp}\":\n\n{block}")
        prompts[[grp]] <- paste0(grp_intro, grp_task, grp_data)
      }
    }
    return(prompts)
  }
}

#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom dplyr slice_sample group_by
#' @importFrom stats quantile
#' @importFrom rlang sym

sample_numeric_distribution <- function(data, num_var_index, sample_pct, method = "stratified", bins = 5, return_matrix = TRUE, seed = NULL) {

  # Extract variable name
  num_var <- colnames(data)[num_var_index]

  # Ensure numeric variable
  if (!is.numeric(data[[num_var]])) {
    stop("The selected column is not numeric.")
  }

  # Convert row names to a column to preserve them
  data <- tibble::rownames_to_column(data, var = "OriginalRowName")

  # Set seed for reproducibility if specified
  if (!is.null(seed)) set.seed(seed)

  # Calculate sample size based on percentage
  sample_size <- max(1, round(nrow(data) * sample_pct))

  if (method == "probability") {
    # Ensure values are positive for probability weights
    data$prob <- data[[num_var]] - min(data[[num_var]], na.rm = TRUE) + 1

    # Normalize probabilities to sum to 1
    data$prob <- data$prob / sum(data$prob, na.rm = TRUE)

    # Sample rows with probability proportional to num_var
    sampled_data <- data[sample(1:nrow(data), size = sample_size, prob = data$prob, replace = FALSE), ]

  } else if (method == "stratified") {
    # Ensure bins do not exceed unique values
    bins <- min(bins, length(unique(data[[num_var]])) - 1)

    # Handle case where all values are identical (avoid cut() failure)
    if (bins < 1) {
      warning("Not enough unique values for stratified sampling. Defaulting to random sampling.")
      sampled_data <- dplyr::slice_sample(data, n = sample_size, replace = FALSE)
    } else {
      # Create bins using quantiles, ensuring unique breakpoints
      breaks <- unique(quantile(data[[num_var]], probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))

      # Avoid cut() failure due to non-unique breaks
      if (length(breaks) <= 1) {
        warning("Insufficient variation in data. Using random sampling.")
        sampled_data <- dplyr::slice_sample(data, n = sample_size, replace = FALSE)
      } else {
        # Create bins
        data$bin <- cut(data[[num_var]], breaks = breaks, include.lowest = TRUE, labels = FALSE)

        # Sample proportionally from each bin
        sampled_data <- data %>%
          dplyr::group_by(bin) %>%
          dplyr::filter(dplyr::n() > 0) %>%  # Avoid empty bins
          dplyr::slice_sample(n = max(1, round(sample_size / bins)), replace = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(-bin)  # Remove bin column
      }
    }
  } else {
    stop("Invalid method. Choose 'probability' or 'stratified'.")
  }

  # Convert back to a matrix or return data frame
  sampled_data <- sampled_data %>%
    dplyr::arrange(dplyr::desc(.data[[num_var]])) %>%  # Correct dynamic reference
    tibble::column_to_rownames(var = "OriginalRowName")

  if (return_matrix) {
    return(as.matrix(sampled_data))
  } else {
    return(sampled_data)
  }
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
#' @param quali.sample from 0 to 1, the proportion of qualitative features that are randomly kept.
#' @param quanti.sample from 0 to 1, the proportion of quantitative features that are randomly kept.
#' @param drop.negative a boolean that indicates whether to drop negative v.test values for interpretation (keeping only positive v.tests). Recommended with long catdes results.
#' @param proba the significance threshold considered to characterize the categories (by default 0.05).
#' @param row.w a vector of integers corresponding to an optional row weights (by default, a vector of 1 for uniform row weights)
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
#' ### Example 1: Fisher's iris ###
#' library(NaileR)
#' data(iris)
#'
#' intro_iris <- "A study measured various parts of iris flowers
#' from 3 different species: setosa, versicolor and virginica.
#' I will give you the results from this study.
#' You will have to identify what sets these flowers apart."
#' intro_iris <- gsub('\n', ' ', intro_iris) |>
#' stringr::str_squish()
#'
#' req_iris <- "Please explain what makes each species distinct.
#' Also, tell me which species has the biggest flowers,
#' and which species has the smallest."
#' req_iris <- gsub('\n', ' ', req_iris) |>
#' stringr::str_squish()
#'
#' res_iris <- nail_catdes(iris,
#'                         num.var = 5,
#'                         introduction = intro_iris,
#'                         request = req_iris,
#'                         generate = TRUE)
#'
#' cat(res_iris$response)
#'
#' ### Example 2: food waste dataset ###
#'
#' library(FactoMineR)
#'
#' data(waste)
#' waste <- waste[-14]    # no variability on this question
#'
#' set.seed(1)
#' res_mca_waste <- MCA(waste, quali.sup = c(1,2,50:76),
#' ncp = 35, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_waste, choix = "ind",
#' invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_waste <- HCPC(res_mca_waste, nb.clust = 3, graph = FALSE)
#' plot.HCPC(res_hcpc_waste, choice = "map", draw.tree = FALSE,
#' ind.names = FALSE)
#' don_clust_waste <- res_hcpc_waste$data.clust
#'
#' intro_waste <- 'These data were collected
#' after a survey on food waste,
#' with participants describing their habits.'
#' intro_waste <- gsub('\n', ' ', intro_waste) |>
#' stringr::str_squish()
#'
#' req_waste <- 'Please summarize the characteristics of each group.
#' Then, give each group a new name, based on your conclusions.
#' Finally, give each group a grade between 0 and 10,
#' based on how wasteful they are with food:
#' 0 being "not at all", 10 being "absolutely".'
#' req_waste <- gsub('\n', ' ', req_waste) |>
#' stringr::str_squish()
#'
#' res_waste <- nail_catdes(don_clust_waste,
#'                          num.var = ncol(don_clust_waste),
#'                          introduction = intro_waste,
#'                          request = req_waste,
#'                          drop.negative = TRUE,
#'                          generate = TRUE)
#'
#' cat(res_waste$response)
#'
#'
#' ### Example 3: local_food dataset ###
#'
#' data(local_food)
#'
#' set.seed(1)
#' res_mca_food <- MCA(local_food, quali.sup = 46:63,
#' ncp = 100, level.ventil = 0.05, graph = FALSE)
#' plot.MCA(res_mca_food, choix = "ind",
#' invisible = c("var", "quali.sup"), label = "none")
#' res_hcpc_food <- HCPC(res_mca_food, nb.clust = 3, graph = FALSE)
#' plot.HCPC(res_hcpc_food, choice = "map", draw.tree = FALSE,
#' ind.names = FALSE)
#' don_clust_food <- res_hcpc_food$data.clust
#'
#' intro_food <- 'A study on sustainable food systems
#' was led on several French participants.
#' This study had 2 parts. In the first part,
#' participants had to rate how acceptable
#' "a food system that..." (e.g, "a food system that
#' only uses renewable energy") was to them.
#' In the second part, they had to say
#' if they agreed or disagreed with some statements.'
#' intro_food <- gsub('\n', ' ', intro_food) |>
#' stringr::str_squish()
#'
#' req_food <- 'I will give you the answers from one group.
#' Please explain who the individuals of this group are,
#' what their beliefs are.
#' Then, give this group a new name,
#' and explain why you chose this name.
#' Do not use 1st person ("I", "my"...) in your answer.'
#' req_food <- gsub('\n', ' ', req_food) |>
#' stringr::str_squish()
#'
#' res_food <- nail_catdes(don_clust_food,
#'                         num.var = 64,
#'                         introduction = intro_food,
#'                         request = req_food,
#'                         isolate.groups = TRUE,
#'                         drop.negative = TRUE,
#'                         generate = TRUE)
#'
#' res_food[[1]]$response |> cat()
#' res_food[[2]]$response |> cat()
#' res_food[[3]]$response |> cat()
#' }

nail_catdes = function(dataset, num.var,
                       introduction = NULL,
                       request = NULL,
                       model = 'llama3',
                       isolate.groups = FALSE,
                       quali.sample = 1,
                       quanti.sample = 1,
                       drop.negative = FALSE,
                       proba = 0.05,
                       row.w = NULL,
                       generate = FALSE){


  #if (is.null(request)) request <- "Based on the results, please describe what characterizes the individuals of each group and what sets them apart from the other groups. Then, based on these characteristics, give each group a new name."
  if (is.null(introduction)) introduction <- "For this study, observations were grouped according to their similarities."

  if (isolate.groups == F){
    if (is.null(request)) request <- "Based on the results, please describe what characterize the observations of each group and what set them apart from the other groups. Then, based on these characteristics, give each group a new name."
  } else {
    if (is.null(request)) request <- "Based on the results, please describe what characterize the observations of this group and what make them so special. Then, based on these characteristics, give the group a new name."
  }

  res_cd = FactoMineR::catdes(dataset, num.var = num.var, proba = proba, row.w = row.w)

  ppt = get_prompt_catdes(res_cd, introduction = introduction, request = request,
                          isolate.groups = isolate.groups, quali.sample = quali.sample,
                          quanti.sample = quanti.sample, drop.negative = drop.negative)

  #if (!generate) return(data.frame(prompt = ppt))
  if (!generate) return(ppt)

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
