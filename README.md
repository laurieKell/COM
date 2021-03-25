# Catch-only Models
Catch-only models have been the focus of much recent research into data-poor stock assessment methods. A reason for this is because they are useful for providing summaries of the state of fisheries on both a regional and global scale. However, results are highly sensitive to the choice of priors for initial and final depletion. Therefore there are multiple implementattions of catch-only models with different heuristic to generate depletion priors. There is therefore a need for 
model validation to increases confidence in the outputs of these models and to identify model limitations that should be addressed in future research.

Model validation requires estimates to be compared to known values (i.e. observations) or well estimated historical values. However, the only observations used in catch-only models are the catches themselves, and if these observations are removed then the model can not be run. Therefore, to evaluate catch-only models we used the RAM legacy database (https://www.ramlegacy.org/) as a reference set of data-rich stocks. The database has stock assessment time series from a variety of regions, species and fisheries. 

We configure the JABBA state-space biomass dynamic model (https://github.com/jabbamodel/JABBA) as a catch-only model using FLR (https://flr-project.org/). We then evaluate the knowledge requirements, in the form of priors for population growth rate (r), initial and final depletion, and the form of the production function, for catch-only models to provide assessments of stock status relative to maximum sustainable yield (MSY) reference points. We also compare catch-only models to assessments that use an index of relative abundance for calibration to evaluate the value of extra infmation. 

To evaluate the ability of the models, and the value of additional infomation, to provide estimates of current depletion we use Receiver Operating Characteristic (ROC) curves to evaluate the ability of models to classify and rank stocks with respect to being overfished. 

Results are found at https://rpubs.com/laurie/744918 and a draft manuscript describing the work can be found on Overleaf (https://www.overleaf.com/read/kwctdqrjftwd)

## Tasks

Develop a reference dataset from the RAM legacy DB for a set of stocks where time series of stock status, catch and MSY and virgin biomass reference points are known. Develop priors for the production function, i.e. r, K, and shape. Develop depletion priors.
Run SRA+ (or equivalent model framework) to create a reference ”best case” assessment for the RAM database using catch and a perfect index of abundance.
Conduct a Value-of-Information analysis by running SRA+ (or equivalent model framework) using different approaches for deriving priors for i) depletion, ii) r, iii) K, iv) shape of the production function and data availability and quality for indices of abundance and catch
Evaluate the ability of the different approaches to i) estimate current stock status relative to FMSY, BMSY and K, and ii) trends in abundance.


## Code

The code, based on the FLR and JABBA packages, is available from this repository.

## Data

Data used are from the RAM Legacy DB, for the stock assessment time series, and FishBase for life history priors. The data and results are not on this repository, but stored in dropbox. They can be downloaded either manually or using the ‘rdrop2’ package https://cran.r-project.org/web/packages/rdrop2/rdrop2.pdf

Assuming you have a share to the COM folder in dropbox, there are folders for the `data`, and `results` which contain R data sets. `inputs` contains text files with assessment model results.

Example of how to load data from dropbox

```{r, eval=FALSE}
library(rdrop2)

## get and save token
token<-drop_auth()
saveRDS(token, "Dropbox/token.RDS")

## load time series from RAM Legacy DB
drop_download(path='COM/data/ts.RData',overwrite=T)
load("ts.RData")

## load results
drop_download(path='COM/results/com.RData',overwrite=T)
load("com.RData")
```

### RAM Legacy DB

A summary of the assessment time series can be found at https://rpubs.com/laurie/744533

### Tuna RFMO

The extra tun stocks added to the RAM DB are summarised at https://rpubs.com/laurie/744958

### Priors

The life history parameters were obtained from https://github.com/fishnets/fishnets and a summary of the priors for r are found at https://rpubs.com/laurie/744917

## Running

The simulation were run in parallel as array jobs on a Portable Batch System (PBS) see 'runCom.R' for the code and 'runCOM.qsub' for the PBS script, and `process.R` summarises the results
