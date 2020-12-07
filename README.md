# 2020-neurodevelopmental-predictors-of-conversion
Scripts to reproduce the results reported in Jutla A (co-first author), Califano A (co-first author), Dishy G, Hesson H, Kennedy L, Lundgren B, Pia T, Veenstra-VanderWeele J, Brucato G, Girgis RR. "Neurodevelopmental predictors of conversion to schizophrenia and other psychotic disorders in adolescence and young adulthood in clinical high risk individuals." *Schizophrenia Research*. 2020;224:170-172.

# How to use
Place data (not freely distributable, but available on request) in `./data/raw` and `./data/recoded`. 

Create an R environment with the correct dependencies by running `conda env create --file environment.yml` (will need to replace some dependencies with platform-specific versions if not on macOS).

Activate the environment and run `./r/_run.r.` Look in `./r/output/table_1` for tables and in `./r/output/models` for models.  

# Changelog
20201207: initial commit.

# Questions
Contact <Amandeep.Jutla@nyspi.columbia.edu>.
