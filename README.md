<br />
<div align="center">
  <a href="https://github.com/Nelhe/NaileR">
    <img src="images/Nailer_final.png" alt="Logo" width="122" height="142">
  </a>

  <h3 align="center">NaileR</h3>

  <p align="center">
    <i>Prep, polish, top coat, this vanity case is exactly what you need to put the final touch to your statistical analysis.</i>
</div>



## Overview

NaileR is a R package developed during my internship at l'Institut Agro Rennes-Angers as a tool to help describe latent variables in a multidimensional analysis.

NaileR uses convenience functions offered by the <a href="https://cran.r-project.org/web/packages/FactoMineR/index.html">FactoMineR package</a> (condes, catdes, descfreq) in conjunction with the <a href="https://cran.r-project.org/web/packages/ollamar/index.html">ollamar package</a>, to generate latent variables descriptions with the help of AI.



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

NaileR currently features 3 datasets and 6 functions.

### Datasets

* beard and beard_cont: contain the results of a sensometrics experiment on beards
* local_food: contains the results of a Q method-like survey on sustainable food systems

### Functions

* nail_catdes: performs a catdes analysis on a dataset and describes each category
* nail_condes: performs a condes analysis on a dataset and describes the chosen continuous variable
* nail_descfreq: performs a descfreq analysis on a contingency table and describes the rows
* sim_llm: computes the simularity between texts
* dist_mat_llm: computes a distance matrix based on sim_llm
* dist_ref_llm: computes a distance vector based on sim_llm

## Example

Let's have a look at how we can interpret HCPC clusters:

``` r
library(FactoMineR)
data(local_food)

set.seed(1)
res.mca = MCA(local_food, quali.sup = 46:63, level.ventil = 0.05, ncp = 100)
res.hcpc = HCPC(res.mca, nb.clust = 3)
don_clust = res.hcpc$data.clust
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
- [ ] Consider adding a nail_decat
- [ ] Implement a way to generate reports (pptx)
