# Bayesian Stochastic Frontier Analysis (BSFA)

Files needed to reproduce empirical application for "A Bayesian framework for estimating human capabilities":

- yop_child_data.csv: raw data describing sociodemographics of treated and untreated individuals and educational outcomes of their children.
- bmodel_yop.stan: stan file containing model information.
- yop_analysis.r: r file, which reads in yop_child_data.csv and compiles bmodel_yop.stan

Files produced after running yop_analysis.r
- coefficients.csv
- functioning_counterfactual.csv
- functioning_factual.csv
- potentials.csv


R packages required: rstan, ggplot2, dplyr

R packages recommended: gridExtra
