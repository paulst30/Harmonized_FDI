# Harmonized_FDI
In this project, a harmonized FDI data series is estimated by adjusting for methodological changes. FDI data of different vintages are combined to one large FDI dataset. 

## Data
The data set needed to execute all scripts can be downloaded [here](https://drive.google.com/drive/folders/1aWDlLN6OIaNCg84fvqO13VB6rTD_iHHs?usp=sharing). 

## Scripts
`MASTER.R` runs all scripts and produces the output presented in the paper. In the process, the harmonized FDI data set is saved as csv-file under the current working directory. The following scripts are executed within `MASTER.R`:

1. `data.R` loads the necessary data into R (needs to be downloaded via the link above), and defines which vintage each country pair is hamonized to.
2. `training_loop.R` defines the files to be tracked while looping over all six prediction tasks. It also executes the training and prediction by calling `Setup.R` and `mob.R` for each of the prediction tasks.
3. `results.R` cleans up the results (performance metrics, best tunes) and plots the results.
4. `practical_test.R` produces the harmonized FDI data set and saves it as csv-file in the current directory. It further loads control variables and runs several fixed effects estimations to test the effect of harmonizing FDI vintages on research outcomes.
5. `graphical_analysis.R` produces the graphs presented in the paper.
