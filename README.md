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

NaileR currently features 3 datasets and 3 functions.

### Datasets

* beard and beard_cont: contain the results of a sensometrics experiment on beards
* local_food: contains the results of a Q method-like survey on sustainable food systems

### Functions

* nail_catdes: performs a catdes analysis on a dataset and describes each category
* nail_condes: performs a condes analysis on a dataset and describes the chosen continuous variable
* nail_descfreq: performs a descfreq analysis on a contingency table and describes the rows

## Examples
