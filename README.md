<br />
<div align="center">
  <a href="https://github.com/Nelhe/NaileR">
    <img src="images/Nailer_final.png" alt="Logo" width="244" height="284">
  </a>

  <h3 align="center">NaileR</h3>

  <p align="center">
    <i>Prep, polish, top coat, this vanity case is exactly what you need to put the final touch to your statistical analysis.</i>
</div>



## Overview

Thanks to the Ollama API that allows to use Large Language Model (LLM) locally, we developed a small package designed for interpreting continuous or categorical latent variables. You provide a data set with a latent variable you want to understand and some other explanatory variables. It provides a description of the latent variable based on the explanatory variables. It also provides a name to the latent variable. 'NaileR' is an R package that uses convenience functions offered by the <a href="https://CRAN.R-project.org/package=FactoMineR">'FactoMineR' package</a> (condes(), catdes(), descfreq()) in conjunction with the <a href="https://CRAN.R-project.org/package=ollamar">'ollamar' package</a>.

Its two main goals are to:
* generate latent variables descriptions with the help of AI
* offer similarity measure tools for textual data



## Installation (from GitHub)

1. If needed, install the devtools package.
``` r
install.packages('devtools')
```

2. Install and load the 'NaileR' package from GitHub.
``` r
devtools::install_github('Nelhe/NaileR')
library(NaileR)
```

## Usage

'NaileR' currently features 15 datasets and 9 functions.

### Datasets

* agri_studies: contains the results of a Q method-like survey on agribusiness studies
* beard, beard_cont and beard_wide: contain the results of a sensometrics experiment on beards
* boss: contains the results of a Q method-like survey on the ideal boss
* glossophobia: contains the results of a Q method-like survey on feelings about speaking in public
* local_food: contains the results of a Q method-like survey on sustainable food systems
* quality: contains the results of a survey on French food certification logos
* waste: contains the results of a survey on food waste
* rorschach: this dataset was initially collected to understand the perception of the Rorschach test
* fabric: this dataset was initially collected to understand the free jar data
* atomic_habit, car_alone, atomic_habit_clust: a survey for understanding atomic habits
* nutriscore: these data were collected after a survey on the nutri-score

### Functions

* nail_catdes(): performs a catdes analysis on a dataset and describes each category
* nail_condes(): performs a condes analysis on a dataset and describes the chosen continuous variable
* nail_descfreq(): performs a descfreq analysis on a contingency table and describes the rows
* nail_textual(): generate an LLM response to analyze a categorical latent variable, based on answers to open-ended questions
* nail_qda(): performs a decat analysis on QDA dara and describes the stimuli
* nail_sort(): performs clustering on textual data from sensometrics experiments
* sim_llm(): computes the similarity between texts
* dist_mat_llm(): computes a distance matrix based on sim_llm
* dist_ref_llm(): computes a distance vector based on sim_llm

## Example

For complete case studies and a showcase of the main functions of the 'NaileR' package, see the [documentation](https://github.com/Nelhe/NaileR/tree/master/doc).

Let's have a look at how we can interpret HCPC clusters:

``` r
library(FactoMineR)
data(local_food)

set.seed(1)      # for consistency

res_mca <- MCA(local_food, quali.sup = 46:63, ncp = 100, level.ventil = 0.05, graph = F)
plot.MCA(res_mca, choix = "ind", invisible = c("var", "quali.sup"), label = "none")
res_hcpc <- HCPC(res_mca, nb.clust = 3, graph = F)
plot.HCPC(res_hcpc, choice = "map", draw.tree = F, ind.names = F)
don_clust <- res_hcpc$data.clust
```

Due to the very long and explicit variable names, the category description result is practically illegible. Let's provide clear context and see how a LLM can make sense of it:

``` r
res = nail_catdes(don_clust, ncol(don_clust),
                   
                   introduction = 'A study on sustainable food systems was led on several French participants. This study had 2 parts. 
                   In the first part, participants had to rate how acceptable "a food system that..." (e.g, "a food system that only uses renewable energy") was to them.
                   In the second part, they had to say if they agreed or disagreed with some statements.',
                   
                   request = 'I will give you the answers from one group.
                   Please explain who the individuals of this group are, what their beliefs are. Then, give this group a new name, and explain why you chose this name.',
                   
                   isolate.groups = T, drop.negative = T)
```

Out comes a list of results, for each group.

In the same fashion, nail_condes can be used to interpret axis from a PCA - although a bit more work is needed, to bind the original data frame with the coordinates on the PCA axis.


## Roadmap

- [X] Implement a validation function to test the consistency of a response
- [X] Implement a function to generate multiple responses and pick the most "central"
- [X] Add a <s>nail_textual</s> nail_sort for textual data
- [ ] Consider adding a nail_decat
- [ ] Implement a way to generate reports (pptx)


## License

This package is under the GPL (>= 2) License. Details can be found [here](https://cran.r-project.org/web/licenses/).

## Contact

Sébastien Lê - sebastien.le@institut-agro.fr

Project link: [https://github.com/Nelhe/NaileR](https://github.com/Nelhe/NaileR)

## Acknowledgements

This work has benefited from a government grant managed by the Agence Nationale de la Recherche under the France 2030 programme under the reference ANR-23-PESA-0005.

Ce travail a bénéficié d'une aide de l'Etat gérée par l'Agence Nationale de la Recherche au titre de France 2030 portant la référence ANR-23-PESA-0005.
