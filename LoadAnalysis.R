# Bacteria Load Analysis
# Last Updated: 2014/08/20
# This script runs sub-models and then summarizes load from sources and locations

## set options
options(stringsAsFactors = FALSE)
## load packages
library(doBy)

# define directories
chr.PEST.dir <- "C:/Temp/PEST/BigElkPEST/host"
chr.load.analysis.dir <- "M:/Models/Bacteria/HSPF/bacloadanalysis"
chr.sub.models.dir <- paste0(chr.PEST.dir,"/sub-models")
# load source summary functions
source(paste0(chr.load.analysis.dir,"/","LoadAnalysisFunctions.R"))

## get sub-model folders
chr.sub.model.dirs <- paste0(chr.sub.models.dir,"/",list.files(path=chr.sub.models.dir))

## get sub-model files (exclude "General" folder because that does not have a sub-model, yet)
chr.sub.model.files <- paste0(chr.sub.model.dirs[-grep("./General",chr.sub.model.dirs)],"/",list.files(path=chr.sub.model.dirs[-grep("./General",chr.sub.model.dirs)],pattern="\\.R"))

# define number of subwatersheds
sub.wtsd.num <- 18

# run source sub-models and get raw source data 
lt.raw <- raw.source.data(sub.wtsd.num,chr.sub.model.dirs)

# get annual load on.land and in.stream by sub-watershed
df.annual.subwtsd <- annual.load.by.subwtsd(lt.raw)


