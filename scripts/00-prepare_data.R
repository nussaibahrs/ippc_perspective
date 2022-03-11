cats <- read.csv("data/cats.csv") #cols

# IPCC chapters ------------------------------------------------------------
fl_ch <- list.files("data/original", "ch", full.names = T)

ipcc <- do.call(rbind, 
        lapply(fl_ch, read.csv, skip=1))


ipcc <- ipcc[ipcc$Scope != "",]
ipcc$Notes <- NULL

ipcc <- janitor::clean_names(ipcc)
ipcc$scored_sum <- apply(ipcc[,cats$short], 1, sum, na.rm=T)

ipcc <- ipcc[ipcc$scored_sum > 0,] # remove all irrelevant ones

write.csv(ipcc, "data/ipcc_pubs.csv", row.names=F)

# Random ------------------------------------------------------------------
fl_r <- list.files("data/original", "random", full.names = T)

random <- do.call(rbind, 
                lapply(fl_r, function(x){
                  temp <- read.csv(x, skip=1)
                  
                  temp$Notes <- NULL
                  
                  temp <- janitor::clean_names(temp)
                  temp$scored_sum <- apply(temp[,cats$short], 1, sum, na.rm=T)
                  temp <- temp[temp$scored_sum > 0,]
                  temp <- temp[sample(1:nrow(temp), 50),]
                  
                  return(temp)
                }))


nrow(random)

write.csv(random, "data/random_pubs.csv", row.names=F)


