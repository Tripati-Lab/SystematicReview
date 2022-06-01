# BayesPaper

We include the basic scripts that were used to generate the results and figures in [Roman-Palacios et al.] (https://www.essoar.org/doi/10.1002/essoar.10507995.1). This repo has the following structure:

- `Analyses`
  - `Datasets`: Raw datasets that were used to fit the different regression models. Here, S1 indicates low-error, S2 intermediate-error, and S3 high-error, on the synthetic datasets.
  - `Results`: Results from processing either using the `RunSingleScript.R` script or [BayClump](https://bayclump.tripatilab.epss.ucla.edu/).
    - `10_obs`: Results for models fit in 10-datapoint datasets. Synthetic datasets.
    - `50_obs`: Results for models fit in 50-datapoint datasets. Synthetic datasets.
    - `500_obs`: Results for models fit in 500-datapoint datasets. Synthetic datasets.
    - `Anderson_ATM`: Results for the Anderson et al. (2021) reanalysis on the subsampled dataset.
    - `Anderson_Full`: Results for the Anderson et al. (2021) reanalysis on the full dataset.
    - `Petersen`: Results for the Peterse et al. (2019) reanalysis on the ATM dataset.
    - `Sun`: Results for the Sun et al. (2021) reanalysis on Anderson and Petersen calibration datasets.
  - `RunSingleScript.R`: Script used to fit most of the regression models in the `Datasets` folder. Exceptions include those analyses that were run on real-world datasets.

- `Figures`
  - `Data`: The data used to generate Figures 2 - 11.
  - `Plots`: Final plots.
  - `*R files`: R scripts used to generate each of the figures.

