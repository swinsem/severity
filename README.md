# severity
### Basal area loss from fire using field-calibrated remote sensing refines western U.S. fire severity measurements

This repo is very much in progress, and I will update this readme as I get it cleaned up.
Code is written by me (Sara Winsemius) and Mike Koontz, though the paper in review has many coauthors who did not contribute to code development.

Here is an INTERIM script (please use at your own risk, as the paper has not been peer-reviewed yet) for the Google Earth Engine code:
https://code.earthengine.google.com/c33da377946e0f78c0b8ee0e404f307c

All of the R scripts will be in a sandbox until they are clean enough for others to see, at which point they will be moved to /deploy. Once things are in /deploy, they will have some description added here. 

#### Deploy

01_make_ard.R: this takes the outputs from Earth Engine and combines them with coordinate data. You can reference this if you're adding your own plot data to the analysis.

03_randomforest_cv.R: this is a spatial cross-validation using the model chosen from the CPI Pareto frontier. We are rerunning it so we get the predictions for each plot.

04_tables_viz.R: There are a variety of statistics, table values and a figure produced here, along with some other visualizations and stats that aren't in the paper but that I thought could be interesting. This is where you get the number of plots and R2 for each ecoregion from the general model (from Table 1); plot Figure 2; and get the confusion matrix (Supplement) and class statistics for the binned observations and predictions.

05_ecoregion_models.R: This takes the CPI results that were run for each ecoregion and gets the model parameters and variables for the model that has the highest R2. There is also some code to look at the Pareto frontier for each, but we don't do that in the paper.

