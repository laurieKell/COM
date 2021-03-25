# Catch-only Models
Catch-only models have been the focus of much recent research into data-poor stock assessment methods. A reason for this is because they are useful for providing summaries of the state of fisheries on both a regional and global scale. However, results are highly sensitive to the choice of priors for initial and final depletion and variations. Variations of catch-only models have steadily grown focusing on the heuristic used to generate the depletion priors. 

Model validation requires estimates to be compared to known values (i.e. observations) or well estimated historical values. However, the only observations used in catch-only models are the catches themselves, and if these observations are removed then the model can not be run. Therefore, to evaluate catch-only models a reference set of data-rich stocks. We chose the RAM legacy database (https://www.ramlegacy.org/) as this has stock assessment time series from a variety of regions, species and fisheries. 

We therefore configure the JABBA state-space biomass dynamic model (https://github.com/jabbamodel/JABBA) as a catch-only model using FLR (https://flr-project.org/). We then evaluate knowledge requirements, in the form of priors for population growth rate (r) and initial and final depletion, and the form of the production function, for catch-only models to provide assessments of stock status relative to maximum sustainable yield (MSY) targets reference points. We also compare catch-only models to assessments that use an index of relative abundance for calibration. To do this we use Receiver Operating Characteristic (ROC) curves to evaluate the ability of models and reference points to classify and rank stocks with respect to being overfished. 

Results are found at https://rpubs.com/laurie/744918 and a draft manuscript describing the work can be found on Overleaf (https://www.overleaf.com/read/kwctdqrjftwd)

## Code

All the code is available in this repository, using FLP and JABBA packages

## Data

The data used are from the RAM Legacy DB for the stock time series and FishBase for life history priors. The data and results are not on this github repository, but stored in dropbox. They can be downloaded either manually or using the ‘rdrop2’ package https://cran.r-project.org/web/packages/rdrop2/rdrop2.pdf

Assuming you have a share to the COM folder in dropbox, which has folders for the input data, and results

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

https://rpubs.com/laurie/744533

### Priors

https://github.com/fishnets/fishnets

https://rpubs.com/laurie/744917

## Results

