# SimPsychReview
This repository contains data, code, and output related to the paper

Siepe, B.S., Bartoš, F., Morris, T.P., Boulesteix, A.-L., Heck, D.W. & Pawel, S. (2023). Simulation Studies for Methodological Research in Psychology: A Standardized Template for Planning, Preregistration, and Reporting. doi: [10.31234/osf.io/ufgy6](https://osf.io/preprints/psyarxiv/ufgy6)

## Reproducing our results

* To reproduce the results of our literature review, refer to the files in the folder `/literature-review/`:
  1. `literature-review/analysis.Rmd` contains the main analyses in the paper and code to reproduce all figures.
  2. `literature-review/coding_agreement.Rmd` contains code to reproduce the agreement analyses in the appendix.
  3. `literature-review/lit_review_coding.pdf` contains the questions used in the literature review and displayed in the agreement analyses in the appendix. 

* The folder `/data/` contain the raw data used in these scripts.

* To reproduce our simulation example, refer to the files in the folder `/simulation-example/`:
  1. `simulation-example/simulation_example.R` is the main simulation file used to generate, analyse, and summarize the simulation datasets. Execute this file to rerun the simulation study. 
  2. `simulation-example/simulation_analysis.R` contains code to analyse and visualize the results of the simulation study, and to reproduce the figures in the manuscript.
  3. `simulation-example/session-info.txt` includes information on packages used and the computational environment of the simulation study.
  4. `simulation-example/simulation-data.csv` contains intermediate data from the simulation study (method and simulation wise estimates and p-values)
  5. `simulation-example/simulation-summaries.csv` contains performance measure summaries from the simulation study

<!-- The R packages required for the simulation study can be installed by running from an R session (refer to the ) -->
<!-- ``` r -->
<!-- install.packages(c("SimDesign", "mvtnorm", "xtable", "ggplot2", "here", -->
<!--                   "tidyverse", "scales", "ggpubr", "sessioninfo", "showtext", -->
<!--                   "sysfonts")) -->
<!-- ``` -->

* To rerun the simulation via a Docker container, refer to the files
  1. `./Dockerfile` Dockerfile to recreate the computational environment used in the simulation study
  2. `./Makefile` Makefile to conveniently build and run the Docker analysis: Make sure to have Docker and Make installed, then run `make docker` from the root directory of this git repository. This will install all necessary dependencies. RStudio Server can then be opened from a browser (<http://localhost:8787>), and the R scripts in `/simulation-example/` can be rerun. 

*  `/figures/` contains all figures related to both parts of the manuscript. 

## ADEMP-PreReg template
The ADEMP-PreReg template can be found in another [GitHub repository](https://github.com/bsiepe/ADEMP-PreReg/). Feel free to open an issue or a pull request there to give feedback or suggest improvements.

The version (v.0.1.0) used in the manuscript is archived on [Zenodo](https://zenodo.org/records/10057884).
