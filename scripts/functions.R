gts <- function(timescale, level="stage"){
	valid <-c(2004, 2008, 2012, 2016, 2020)
	
	if(!timescale %in% valid) errorCondition("Please specify a valid timescale")
	if(!level %in% c("stage", "tens")) errorCondition("Please specify a valid level")
	
	timescale <- paste0("gts", timescale)
	
	scl <-read.csv("data/timescale.csv")
	
	
	if(level=="stage"){
		data("stages", package="divDyn")
		
		stages$bottom <- scl[,timescale]
		stages$top <- c(scl[2:95, timescale],0)
		
	} else{
		data("tens", package = "divDyn")
		scl <- scl[4:95,]
		scl <- sort(
			tapply(scl[,timescale], scl$tens, max), decreasing=T)
		stages <- tens
		
		stages$bottom <- scl
		stages$top <- c(scl[-1], 0)
	}
	
	stages$mid <- (stages$bottom+stages$top)/2
	stages$dur <- stages$bottom - stages$top
	
	
	return(stages)
}
# Find stage
find_stg <- function(age, timescale){
	scl <-read.csv("data/timescale.csv")
	
	if(is.null(timescale)) errorCondition("Please specify a valid timescale")
	
	valid <-c(2004, 2008, 2012, 2016, 2020)
	
	if(!timescale %in% valid) errorCondition("Please specify a valid timescale")
	
	timescale <- paste0("gts", timescale)
	
	# check which bin it corresponds to
	base <- c(scl[,timescale], 0)
	
	bin <- rep(NA, length(age))
	
	for(i in 1:95){
		n <- which(age <= base[i] & age > base[i+1])
		
		if (length(n)>0) bin[n] <- i
	}
	
	bin
}
