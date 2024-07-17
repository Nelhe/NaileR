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

NaileR in an R package that uses convenience functions offered by the <a href="https://cran.r-project.org/web/packages/FactoMineR/index.html">FactoMineR package</a> (condes, catdes, descfreq) in conjunction with the <a href="https://cran.r-project.org/web/packages/ollamar/index.html">ollamar package</a>.

Its two main goals are to:
* generate latent variables descriptions with the help of AI
* offer similarity measure tools for textual data



## Installation (from GitHub)

1. If needed, install the devtools package.
``` r
install.packages('devtools')
```

2. Install and load the NaileR package from GitHub.
``` r
devtools::install_github('Nelhe/NaileR')
library(NaileR)
```

## Usage

NaileR currently features 8 datasets and 7 functions.

### Datasets

* agri_studies: contains the results of a Q method-like survey on agribusiness studies
* beard, beard_cont and beard_wide: contain the results of a sensometrics experiment on beards
* boss: contains the results of a Q method-like survey on the ideal boss
* glossophobia: contains the results of a Q method-like survey on feelings about speaking in public
* local_food: contains the results of a Q method-like survey on sustainable food systems
* waste: contains the results of a survey on food waste

### Functions

* nail_catdes: performs a catdes analysis on a dataset and describes each category
* nail_condes: performs a condes analysis on a dataset and describes the chosen continuous variable
* nail_descfreq: performs a descfreq analysis on a contingency table and describes the rows
* sim_llm: computes the similarity between texts
* dist_mat_llm: computes a distance matrix based on sim_llm
* dist_ref_llm: computes a distance vector based on sim_llm
* nail_sort: performs clustering on textual data from sensometrics experiments

## Example

For complete case studies and a showcase of NaileR's main functions, see the [documentation](doc).

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

This package is under the GPL (>= 2) License. Details can be found [here](https://cran.r-project.org/web/licenses).

## Contact

Sébastien Lê - sebastien.le@institut-agro.fr

Project link: [https://github.com/Nelhe/NaileR](https://github.com/Nelhe/NaileR)
