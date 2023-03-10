# Systematic review of models used for Clumped Isotope Thermometry

We include the basic scripts that were used to generate the results and figures in [Roman-Palacios et al.](https://www.essoar.org/doi/10.1002/essoar.10507995.1). This repo has the following structure:

- `Analyses`
  - `Datasets`: Raw datasets that were used to fit the different regression models. Here, S1 indicates low-error, S2 intermediate-error, and S3 high-error, on the synthetic datasets. We generated datasets with different sample sizes (n=10, 50, 500) for each scenario of error.
  - `Results`: Results from processing either using the `RunSingleScript.R` script or [BayClump](https://bayclump.tripatilab.epss.ucla.edu/).
    - `10_obs`: Results for models fit in 10-datapoint datasets. Synthetic datasets.
    - `50_obs`: Results for models fit in 50-datapoint datasets. Synthetic datasets.
    - `500_obs`: Results for models fit in 500-datapoint datasets. Synthetic datasets.
  - `RunSingleScript.R`: Script used to fit most of the regression models in the `Datasets` folder.

- `Figures`
  - `Plots`: Final plots.
  - `*R files`: R scripts used to generate each of the figures.

