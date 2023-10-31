# SimPsychReview
This repository contains data, code and output related to the paper
Siepe, B.S., Bartoš, F., Morris, T.P., Boulesteix, A.-L., Heck, D.W. & Pawel, S. (2023). Simulation Studies for Methodological Research in Psychology: A Standardized Template for Planning, Preregistration, and Reporting. doi: [10.31234/osf.io/ufgy6](https://osf.io/preprints/psyarxiv/ufgy6)

## Reproducing our results

To reproduce the results of our literature review, refer to the files in the folder `/literature-review/`:
1. `analysis.Rmd` contains the main analyses in the paper and code to reproduce all figures.
2. `coding_agreement.Rmd` contains code to reproduce the agreement analyses in the appendix.
3. `lit_review_coding.pdf` contains the questions used in the literature review and displayed in the agreement analyses in the appendix. 

The folder `/data/` contain the raw data used in these scripts.


To reproduce our simulation example, refer to the files in the folder `/simulation-example/`:
1. `simulation_example.R` is the main simulation file used to generate, analyse, and summarize the simulation datasets.
2. `simulation_analysis.R` contains code to analyse and visualize the results of the simulation study and to reproduce the figures in the manuscript.
3. `session-info.txt` includes information on packages used and the computational environment.

  `/figures/` contains all figures related to both parts of the manuscript. 

## ADEMP-PreReg template
The ADEMP-PreReg template can be found in another [GitHub repository](https://github.com/bsiepe/ADEMP-PreReg/). Feel free to open an issue or a pull request there to give feedback or suggest improvements.

The version (v.0.1.0) used in the manuscript is archived on [Zenodo](https://zenodo.org/records/10057884).
