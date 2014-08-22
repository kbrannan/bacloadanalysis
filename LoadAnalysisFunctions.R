raw.source.data <- function(sub.wtsd.num, sub.model.dirs) {
  
	# Last Updated: 2014/08/21
	# Description: calculates source data frames and returns a list containing them
	# Arguments:
  # sub.wtsd.num - number of sub-wtsds
	# sub.model.dirs - folders where the sub-models are located
	# sub.model.input - file name or partial name for sub-model input files

  ## turn off warnings
  options(warn=-1)

  ## generate numbers for sub-wtsds 
  sub.wtsds <- formatC(1:sub.wtsd.num, width = 2, format = "d", flag = "0")
  
  ## get sub-model files (exclude "General" folder because that does not have a sub-model, yet)
  sub.model.files <- paste0(sub.model.dirs[-grep("./General",sub.model.dirs)],"/",list.files(path=sub.model.dirs[-grep("./General",sub.model.dirs)],pattern="\\.R"))
  
  ## load sub-model functions
  junk <- sapply(sub.model.files,source)
  rm(junk)
  
  # create input vectors
  sub.model.input <- unique(gsub(pattern="[0-9]{2}\\.txt","",list.files(path=sub.model.dirs,pattern="\\.txt")))
  
	# Run source models and put the results for all the sub-watersheds in a single
  # data frame for each source all of the data frame are then put in a list
	for (ii in 1:length(sub.wtsds)) {
	# Run CowCalf model
	  cow.calf.in <- paste0(grep("[Cc]ow",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
		cow.calf.out <- cow.calf(chr.input=cow.calf.in,chr.wrkdir=grep("[Cc]ow",sub.model.dirs,value=TRUE))
		# Change NaN & Inf to 0 in cow.calf.out
	  cow.calf.out$Accum.Pasture[!is.finite(cow.calf.out$Accum.Pasture)] <- 0
	  cow.calf.out$Accum.Forest[!is.finite(cow.calf.out$Accum.Forest)] <- 0
	  cow.calf.out$Bacteria.InForestInStream[!is.finite(cow.calf.out$Bacteria.InForestInStream)] <- 0
	  cow.calf.out$Bacteria.OnPastureInStream[!is.finite(cow.calf.out$Bacteria.OnPastureInStream)] <- 0
	  cow.calf.out <- data.frame(cow.calf.out, bac.total.on.land=(cow.calf.out$Bacteria.OnPastureWOStreamAccess + cow.calf.out$Bacteria.OnPastureWStreamAccess + cow.calf.out$Bacteria.InConfinementvsTime + cow.calf.out$Bacteria.InForest), bac.total.in.stream=(cow.calf.out$Bacteria.InForestInStream + cow.calf.out$Bacteria.OnPastureInStream))
		if(ii == 1) {
		  cow.calf.all <- data.frame(subwtsd=ii,cow.calf.out)
		} else {
		  cow.calf.all <- rbind(cow.calf.all,data.frame(subwtsd=ii,cow.calf.out))
		}
	# Run onsite_pets model
	  onsite.pets.in <- paste0(grep("[Oo]n[Ss]ite",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  onsite.pets.out<- onsite_pets(chr.input=onsite.pets.in,chr.wrkdir=grep("[Ss]ite",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in onsite.pets.out
	  onsite.pets.out$Accum.RAOCUT[!is.finite(onsite.pets.out$Accum.RAOCUT)] <- 0
	  onsite.pets.out$bac.onsite.NearStrmStrctFailure[!is.finite(onsite.pets.out$bac.onsite.NearStrmStrctFailure)] <- 0
	  onsite.pets.out <- data.frame(onsite.pets.out,bac.total.in.stream=onsite.pets.out$bac.onsite.NearStrmStrctFailure)
  	if(ii == 1) {
  	  onsite.pets.all <- data.frame(subwtsd=ii,onsite.pets.out)
  	} else {
  	  onsite.pets.all <- rbind(onsite.pets.all,data.frame(subwtsd=ii,onsite.pets.out))
  	}
  # Run Wildlife-Beaver model
	  beaver.in <- paste0(grep("[Bb]eaver",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  beaver.out <- wildlifeBeaver(chr.input=beaver.in,chr.wrkdir=grep("Wildlife-[Bb]eaver",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in beaver.out
	  beaver.out$Accum.Forest[!is.finite(beaver.out$Accum.Forest)] <- 0
	  beaver.out$bac.total.in.stream[!is.finite(beaver.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  beaver.all <- data.frame(subwtsd=ii,beaver.out)
  	} else {
  	  beaver.all <- rbind(beaver.all,data.frame(subwtsd=ii,beaver.out))
  	}
	# Run Wildlife-Coyote model
	  coyote.in <- paste0(grep("[Cc]oyote",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  coyote.out <- wildlifeCoyote(chr.input=coyote.in,chr.wrkdir=grep("Wildlife-[Cc]oyote",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in coyote.out
	  coyote.out$Accum.Pasture[!is.finite(coyote.out$Accum.Pasture)] <- 0
	  coyote.out$Accum.Forest[!is.finite(coyote.out$Accum.Forest)] <- 0
	  coyote.out$Accum.RAOCUT[!is.finite(coyote.out$Accum.RAOCUT)] <- 0
	  coyote.out$bac.total.in.stream[!is.finite(coyote.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  coyote.all <- data.frame(subwtsd=ii,coyote.out)
  	} else {
  	  coyote.all <- rbind(coyote.all,data.frame(subwtsd=ii,coyote.out))
  	}
	# Run Wildlife-Deer model
	  deer.in <- paste0(grep("[Dd]eer",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  deer.out <- wildlifeDeer(chr.input=deer.in,chr.wrkdir=grep("Wildlife-[Dd]eer",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in deer.out
	  deer.out$Accum.Pasture[!is.finite(deer.out$Accum.Pasture)] <- 0
	  deer.out$Accum.Forest[!is.finite(deer.out$Accum.Forest)] <- 0
	  deer.out$bac.total.in.stream[!is.finite(deer.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  deer.all <- data.frame(subwtsd=ii,deer.out)
  	} else {
  	  deer.all <- rbind(deer.all,data.frame(subwtsd=ii,deer.out))
  	}
	# Run Wildlife-Duck model
	  duck.in <- paste0(grep("[Dd]uck",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  duck.out <- wildlifeDuck(chr.input=duck.in,chr.wrkdir=grep("Wildlife-[Dd]uck",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in duck.out
	  duck.out$Accum.Pasture[!is.finite(duck.out$Accum.Pasture)] <- 0
	  duck.out$Accum.Forest[!is.finite(duck.out$Accum.Forest)] <- 0
	  duck.out$Accum.RAOCUT[!is.finite(duck.out$Accum.RAOCUT)] <- 0
	  duck.out$bac.total.in.stream[!is.finite(duck.out$bac.total.in.stream)] <- 0	
  if(ii == 1) {
  	  duck.all <- data.frame(subwtsd=ii,duck.out)
  	} else {
  	  duck.all <- rbind(duck.all,data.frame(subwtsd=ii,duck.out))
  	}
  # Run Wildlife-Elk model
	  elk.in <- paste0(grep("[Ee]lk",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
		elk.out <- wildlifeElk(chr.input=elk.in,chr.wrkdir=grep("Wildlife-[Ee]lk",sub.model.dirs,value=TRUE))
		# Change NaN & Inf to 0 in elk.out
	  elk.out$Accum.Pasture[!is.finite(elk.out$Accum.Pasture)] <- 0
	  elk.out$Accum.Forest[!is.finite(elk.out$Accum.Forest)] <- 0
	  elk.out$Accum.RAOCUT[!is.finite(elk.out$Accum.RAOCUT)] <- 0
	  elk.out$bac.total.in.stream[!is.finite(elk.out$bac.from.pasture.in.stream)] <- 0
  	if(ii == 1) {
  	  elk.all <- data.frame(subwtsd=ii,elk.out)
  	} else {
  	  elk.all <- rbind(elk.all,data.frame(subwtsd=ii,elk.out))
  	}
	# Run Wildlife-Geese model
	  geese.in <- paste0(grep("[Gg]eese",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  geese.out <- wildlifeGeese(chr.input=geese.in,chr.wrkdir=grep("Wildlife-[Gg]eese",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in geese.out
	  geese.out$Accum.Pasture[!is.finite(geese.out$Accum.Pasture)] <- 0
	  geese.out$Accum.Forest[!is.finite(geese.out$Accum.Forest)] <- 0
	  geese.out$Accum.RAOCUT[!is.finite(geese.out$Accum.RAOCUT)] <- 0
	  geese.out$bac.total.in.stream[!is.finite(geese.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  geese.all <- data.frame(subwtsd=ii,geese.out)
  	} else {
  	  geese.all <- rbind(geese.all,data.frame(subwtsd=ii,geese.out))
  	}
  # Run Wildlife-Gulls model
	  gulls.in <- paste0(grep("[Gg]ulls",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  gulls.out <- wildlifeGulls(chr.input=gulls.in,chr.wrkdir=grep("Wildlife-[Gg]ulls",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in gulls.out
	  gulls.out$Accum.Pasture[!is.finite(gulls.out$Accum.Pasture)] <- 0
	  gulls.out$Accum.Forest[!is.finite(gulls.out$Accum.Forest)] <- 0
	  gulls.out$Accum.RAOCUT[!is.finite(gulls.out$Accum.RAOCUT)] <- 0
	  gulls.out$bac.total.in.stream[!is.finite(gulls.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  gulls.all <- data.frame(subwtsd=ii,gulls.out)
  	} else {
  	  gulls.all <- rbind(gulls.all,data.frame(subwtsd=ii,gulls.out))
  	}
  # Run Wildlife-HerEgr model
	  heregr.in <- paste0(grep("[Hh]er[Ee]gr",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  heregr.out <- wildlifeHerEgr(chr.input=heregr.in,chr.wrkdir=grep("Wildlife-[Hh]erons_[Ee]grets",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  heregr.out$Accum.Pasture[!is.finite(heregr.out$Accum.Pasture)] <- 0
	  heregr.out$Accum.Forest[!is.finite(heregr.out$Accum.Forest)] <- 0
	  heregr.out$Accum.RAOCUT[!is.finite(heregr.out$Accum.RAOCUT)] <- 0
	  heregr.out$bac.total.in.stream[!is.finite(heregr.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  heregr.all <- data.frame(subwtsd=ii,heregr.out)
  	} else {
  	  heregr.all <- rbind(heregr.all,data.frame(subwtsd=ii,heregr.out))
  	}
	# Run Wildlife-Otter model
	  otter.in <- paste0(grep("[Oo]tter",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  otter.out <- wildlifeOtter(chr.input=otter.in,chr.wrkdir=grep("Wildlife-[Oo]tter",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  otter.out$Accum.Pasture[!is.finite(otter.out$Accum.Pasture)] <- 0
	  otter.out$Accum.Forest[!is.finite(otter.out$Accum.Forest)] <- 0
	  otter.out$bac.total.in.stream[!is.finite(otter.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  otter.all <- data.frame(subwtsd=ii,otter.out)
  	} else {
  	  otter.all <- rbind(otter.all,data.frame(subwtsd=ii,otter.out))
  	}
	# Run Wildlife-Racoon model
	  racoon.in <- paste0(grep("[Rr]acoon",sub.model.input,value=TRUE),sub.wtsds[ii],".txt")
	  racoon.out <- wildlifeRacoon(chr.input=racoon.in,chr.wrkdir=grep("Wildlife-[Rr]acoon",sub.model.dirs,value=TRUE))
	  # Change NaN & Inf to 0 in heregr.out
	  racoon.out$Accum.Pasture[!is.finite(racoon.out$Accum.Pasture)] <- 0
	  racoon.out$Accum.Forest[!is.finite(racoon.out$Accum.Forest)] <- 0
	  racoon.out$Accum.RAOCUT[!is.finite(racoon.out$Accum.RAOCUT)] <- 0
	  racoon.out$bac.total.in.stream[!is.finite(racoon.out$bac.total.in.stream)] <- 0
  	if(ii == 1) {
  	  racoon.all <- data.frame(subwtsd=ii,racoon.out)
  	} else {
  	  racoon.all <- rbind(racoon.all,data.frame(subwtsd=ii,racoon.out))
  	}
	# Clean up
  	rm(list=ls(pattern="(*.in$)|(*.out$)"))
	}
  raw.source.out <- as.list(mget(ls(pattern="*.all$")))
  rm(list=ls(pattern="*all$"))
  return(raw.source.out)
}

annual.load.by.subwtsd <- function(lt.data) {
  library(doBy)
  ## get the names and the count of the data frames in the list
  chr.names <- names(lt.data)
  n.dfs <- length(chr.names)
  ## get number of sub-watersheds
  df.tmp <- as.data.frame(lt.data[chr.names[1]])
  names(df.tmp) <- gsub(paste0(chr.names[1],"."),"",names(df.tmp))
  sub.wtsd.num <- max(df.tmp$subwtsd)
  rm(df.tmp)
  ## get the total annual load to land and stream by sub-watershed from all the sources
  df.annual.total.load <- data.frame(subwtsd=1:sub.wtsd.num,on.land=0,in.stream=0)
  for(ii in 1:n.dfs) {
    df.tmp <- as.data.frame(lt.data[chr.names[ii]])
    names(df.tmp) <- gsub(paste0(chr.names[ii],"."),"",names(df.tmp))
    if(length(grep("[Mm]onth",names(df.tmp))) == 0) {
      df.annual.total.load$on.land <- df.annual.total.load$on.land + df.tmp$bac.total.on.land
      df.annual.total.load$in.stream <- df.annual.total.load$in.stream + df.tmp$bac.total.in.stream
    }
    if(length(grep("[Mm]onth",names(df.tmp))) > 0) {
      df.sum <- summaryBy(bac.total.on.land + bac.total.in.stream ~ subwtsd, data=df.tmp,FUN=c(sum), keep.names=TRUE)
      df.annual.total.load$on.land <- df.annual.total.load$on.land + df.sum$bac.total.on.land
      df.annual.total.load$in.stream <- df.annual.total.load$in.stream + df.sum$bac.total.in.stream
      rm(df.sum)
    }
    rm(df.tmp)
  }
  return(df.annual.total.load)
}

monthly.load.by.subwtsd <- function(lt.data) {
  library(doBy)
  ## get the names and the count of the data frames in the list
  chr.names <- names(lt.data)
  n.dfs <- length(chr.names)
  ## get number of sub-watersheds
  df.tmp <- as.data.frame(lt.data[chr.names[1]])
  names(df.tmp) <- gsub(paste0(chr.names[1],"."),"",names(df.tmp))
  sub.wtsd.num <- max(df.tmp$subwtsd)
  rm(df.tmp)
  ## create data frame for monthly loads
  df.monthly.total.load <- data.frame(subwtsd=1,month=1:12,on.land=0,in.stream=0)
  for(sii in 2:18) {
    df.monthly.total.load <-rbind(df.monthly.total.load,data.frame(subwtsd=sii,month=1:12,on.land=0,in.stream=0))
  }
  for(ii in 1:n.dfs) {
    df.tmp <- as.data.frame(lt.data[chr.names[ii]])
    names(df.tmp) <- gsub(paste0(chr.names[ii],"."),"",names(df.tmp))
    if(length(grep("[Mm]onth",names(df.tmp))) == 0) {
      df.annual.total.load$on.land <- df.annual.total.load$on.land + df.tmp$bac.total.on.land
      df.annual.total.load$in.stream <- df.annual.total.load$in.stream + df.tmp$bac.total.in.stream
    }
    if(length(grep("[Mm]onth",names(df.tmp))) > 0) {
      df.sum <- summaryBy(bac.total.on.land + bac.total.in.stream ~ subwtsd, data=df.tmp,FUN=c(sum), keep.names=TRUE)
      df.annual.total.load$on.land <- df.annual.total.load$on.land + df.sum$bac.total.on.land
      df.annual.total.load$in.stream <- df.annual.total.load$in.stream + df.sum$bac.total.in.stream
      rm(df.sum)
    }
    rm(df.tmp)
  }
  return(df.annual.total.load)
}

numberOfDaysInMonth <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}