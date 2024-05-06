## set R version (https://hub.docker.com/r/rocker/verse/tags)
FROM rocker/verse:4.3.1

## set up directories
WORKDIR home/rstudio
RUN mkdir /home/rstudio/figures /home/rstudio/simulation-example

## install R packages from CRAN the last day of the specified R version
RUN install2.r --error --skipinstalled --ncpus -1 \
    SimDesign mvtnorm xtable ggplot2 here tidyverse scales ggpubr sessioninfo showtext sysfonts
