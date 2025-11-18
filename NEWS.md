# NaileR 2.0.0

This is a major release focused on robustness and maintainability.
The most significant changes include a complete rewrite of `nail_sort()` to use JSON, 
the addition of a new robust Google Gemini API client, 
and the centralization of code into helper functions.

## New Features

**Google Gemini Support:**
    * Added a new standalone function `gemini_generate()`.
    * This function acts as a robust, production-ready client for the Google Gemini API.
    * It supports detailed generation parameters, including `system_instruction` and `seed`.

**Centralized Utility Functions:**
    * A new `utils-formatting.R` file has been created to centralize common logic.
    * `format_stats_as_markdown()`: A new helper that cleanly formats statistics data frames (from `FactoMineR`) into Markdown tables for LLM prompts.
    * `parse_factominer_rownames()`: A new helper to parse complex `FactoMineR` row names (e.g., "variable=modality") into clean "Variable" and "Modalite" columns.
    * `sample_numeric_distribution()`: A centralized function for stratified sampling, now used by `nail_catdes()`.

## Major Refactoring & Enhancements

**`nail_sort()`: JSON Parsing for Robustness**
    * The prompt has been rewritten to **explicitly request a JSON array** from the LLM.
    * The core logic now uses `jsonlite::fromJSON()` to parse the response.
    * A robust validation loop now checks for valid JSON, correct item count (`nrow(dataset)`), correct `stimulus_id` order, group name length (`name_size`), and cluster count (`nb.clusters`).

**Markdown Formatting & "How to Read" Guides**
    * The prompt generation in `nail_catdes()`, `nail_condes()`, `nail_descfreq()`, and `nail_qda()` has been rewritten.
    * They now call the new `format_stats_as_markdown()` helper to present statistical data to the LLM in a clean, tabular Markdown format.
    * Each of these functions now injects a `GUIDE_*` (e.g., `GUIDE_QDA`) into the prompt. This "How to Read" guide explains the meaning of statistical columns (like `v.test`, `p.value`, `Cla/Mod`) to the LLM, significantly improving the quality and relevance of its analysis.

**Improved Robustness & Error Handling**
    * All data-processing functions (`nail_catdes`, `nail_condes`, `nail_descfreq`, `nail_qda`, `nail_textual`) now wrap their core `FactoMineR` calls and prompt generation in a `tryCatch` block.
    * If an underlying analysis yields no significant results, the function now stops gracefully with an informative message (e.g., "No significant differences between stimuli, execution was halted.") instead of erroring during prompt generation.

**LLM Parameter Control (`...`)**
    * All functions calling `ollamar::generate` (`nail_catdes`, `nail_condes`, `nail_descfreq`, `nail_qda`, `nail_textual`) now accept the `...` (dots) argument.
    * These arguments (e.g., `temperature`, `seed`) are correctly passed to the LLM using `do.call(ollamar::generate, ...)`, allowing for fine-grained control over the generation process.

**Enhanced `nail_qda()` Prompts**
    * The default `introduction`, `request`, and `conclusion` prompts in `nail_qda()` have been significantly improved to guide the LLM toward producing a structured, report-style summary. The prompt now explicitly asks for key contrasts and valid Quarto Markdown output.

**Centralized Sampling in `nail_catdes()`**
    * The local `sample_numeric_distribution` function was removed from `nail_catdes.R`.
    * The function now uses the new centralized version from `utils-formatting.R`, driven by the existing `quali.sample` and `quanti.sample` parameters.

## Bug Fixes

**`nail_condes()`:** Fixed a critical bug in the `get_bins` helper function. It now uses `.keep = 'all'` instead of `.keep = 'unused'` during the `mutate(across(...))` step, ensuring that the original data (including the variable of interest) is not dropped.

# NaileR 1.2.3

* Added a `NEWS.md` file to track changes to the package.
* Enhanced prompt generation in `nail_sort()` to enforce stricter group limits
* Added new parameters to `nail_catdes()` and `nail_condes()` to sample significant results when too many
* Enhanced prompt in `nail_qda()` to generate reports
