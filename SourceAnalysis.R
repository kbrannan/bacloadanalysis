# Bacteria Source Analysis
# Last Updated: 2014/07/28
# This script runs sub-models and then summarizes the source information

## set options
options(stringsAsFactors = FALSE)
## load packages
library(doBy)

# define directories
chr.PEST.dir <- "C:/Temp/PEST/BigElkPEST/host"
chr.sub.models.dir <- paste0(chr.PEST.dir,"/sub-models")
chr.source.summary.dir <- paste0(chr.PEST.dir,"/SourceAnalysis")
# load source summary functions
source(paste0(chr.source.summary.dir,"/","SourceAnalysisFunctions.R"))

## get sub-model folders
chr.sub.model.dirs <- paste0(chr.sub.models.dir,"/",list.files(path=chr.sub.models.dir))

## get sub-model files (exclude "General" folder because that does not have a sub-model, yet)
chr.sub.model.files <- paste0(chr.sub.model.dirs[-grep("./General",chr.sub.model.dirs)],"/",list.files(path=chr.sub.model.dirs[-grep("./General",chr.sub.model.dirs)],pattern="\\.R"))

# define number of subwatersheds
sub.wtsd.num <- 18

# run source sub-models and get raw source data 
df.raw <- raw.source.data(sub.wtsd.num,chr.sub.model.dirs)

## wildlife total population by sub-wtsd
get.sum <- function(wild,raw) {
  library(doBy)
  tmp.df <- data.frame(raw[wild])
  names(tmp.df) <- gsub(paste0(wild,"."),"",names(data.frame(raw[wild])))
  df.out <- data.frame(subwtsd=c(as.character(unique(tmp.df$subwtsd)),"total"),pop=NA)
  df.out$pop<-c(summaryBy(pop.total ~ subwtsd,tmp.df,FUN=max)[,2],sum(summaryBy(pop.total ~ subwtsd,tmp.df,FUN=max)$pop.total))
#   if(sum(grepl("Month",names(tmp.df))) > 0) {
#     df.out$pop<-c(summaryBy(pop.total ~ subwtsd,tmp.df,FUN=max)$pop.total,sum(summaryBy(pop.total ~ subwtsd,tmp.df,FUN=max)$pop.total))
#     return(df.out)
#   }
#   df.out$pop <- c(tmp.df$pop.total,sum(tmp.df$pop.total))
  return(df.out)
}
wildlife <- names(df.raw[-grep("(cow\\.calf\\.all)|(onsite\\.pets\\.all)",names(df.raw))])
for(ii in 1:length(wildlife)) {
  if(ii == 1) {
    df.wild.pop <- get.sum(wildlife[ii],df.raw)
    names(df.wild.pop) <- c("subwtsd", gsub(".all","",wildlife[ii]))
  } else {
    df.wild.pop <- cbind(df.wild.pop,get.sum(wildlife[ii],df.raw)$pop)
    names(df.wild.pop) <- c(names(df.wild.pop)[-(ii+1)],gsub(".all","",wildlife[ii]))
  }
}
df.out <- df.wild.pop
##df.wild.pop <- data.frame(source=gsub("*\\.all","",wildlife),pop=sapply(wildlife,get.sum,df.raw))

## cow-calf total population by sub-wtsd
junk <- unique(df.raw$cow.calf.all[,c("subwtsd","NumOfPairs")])
junk$subwtsd <- as.factor(junk$subwtsd)
names(junk) <- c("subwtsd","x")
junk <- summaryBy(x ~ subwtsd, junk, FUN=sum,keep.names=TRUE)
junk$subwtsd <- as.character(junk$subwtsd)
df.cc.pairs <- data.frame(subwtsd=c(junk$subwtsd,"total"),cc.NumOfPairs=NA)
junk$x <- round(junk$x,0)
df.cc.pairs$cc.NumOfPairs <- c(junk$x,sum(junk$x))
rm(junk)
df.out <- cbind(df.out,cc.NumOfPairs=df.cc.pairs$cc.NumOfPairs)

## get onsite systems failing directly into stream
junk <- unique(df.raw$onsite.pets.all[,c("subwtsd","num.onsite.NearStrmStrctFailureInStream")])
junk$subwtsd <- as.factor(junk$subwtsd)
names(junk) <- c("subwtsd","x")
df.onsite.instream <- summaryBy(x ~ subwtsd, junk, FUN=sum)
names(df.onsite.instream) <- c("subwtsd","onsite.system")
df.onsite.instream$subwtsd <- as.character(df.onsite.instream$subwtsd)
df.onsite.instream <- rbind(df.onsite.instream,c(subwtd="total",onsite.system=sum(df.onsite.instream$onsite.system)))
rm(junk)
df.out <- cbind(df.out,onsite.instr=round(as.numeric(df.onsite.instream$onsite.system),digits=3))

## write summary to csv file
write.csv(df.out,file=paste0(chr.source.summary.dir,"/popsummary.csv"),row.names=FALSE)
