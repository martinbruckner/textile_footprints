##############################################################################################
##  Aggregated FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
# read region classification
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.1/regions.csv")
# read commodity classification
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.1/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$name, each = nrcom),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/E.rds"))


year <- 2013
years <- c(1986,1990,2000,2010,2013)

Y_codes <- data.frame(code = substr(colnames(Y[[as.character(year)]]), 1, str_locate(colnames(Y[[as.character(year)]]), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$region = regions$region[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Y[[as.character(year)]]), str_locate(colnames(Y[[as.character(year)]]), "_")[,1]+1, 100)

extensions <- colnames(E[[as.character(year)]])[c(8,10:11)]
# consumption_categories <- c("food","other","stock_addition","losses","balancing")
consumption_categories <- c("food","other","stock_addition","balancing")
consumption = "all"
country = "AUT"
extension = "landuse"
consumption = "food"
allocation = "value"

results <- data.table()
for(year in years){
  print(year)
  # Read data
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  if(allocation=="mass") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/",year,"_L_mass.rds"))
  if(allocation=="value") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/losses/",year,"_L_value.rds"))
  
  results <- rbind(results, footprint(country = country, year = year))
}

results$item <- items$item[match(results$com_code,items$comm_code)]
data.table::fwrite(results, file=paste0("./output/FABIO_v1.1_land_results_",country,".csv"), sep=",")


# Calculate aggregate footprints
footprint <- function(country = character(), year=integer()){
  # Prepare Multipliers
  ext <- as.vector(as.matrix(as.vector(Ei[, ..extension]) / as.vector(Xi)))
  ext[!is.finite(ext)] <- 0
  # ext[ext < 0] <- 0         # eliminate negative values
  MP <- ext * L
  
  FP <- Yi[, Y_codes$iso3c == country] * colSums(MP)
  # FP[FP<0] <- 0     # filter negative values
  FP <- t(FP)
  FP <- agg(FP)
  # FP <- reshape2::melt(FP)
  FP <- data.table(year = year, country = country, com_code = colnames(FP), t(round(FP,0)))
  return(FP)
}


