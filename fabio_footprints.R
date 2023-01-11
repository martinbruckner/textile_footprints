##############################################################################################
##  FABIO Footprints
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
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.2/regions.csv")
# read commodity classification
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$name, each = nrcom),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E.rds"))


# country = "EU27"
# extension = "landuse"
# consumption = "food"
# allocation = "value"

footprint <- function(country = "EU27", extension = "landuse", consumption = "food", allocation = "value"){
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  ext <- as.vector(as.matrix(as.vector(Ei[, ..extension]) / as.vector(Xi)))
  ext[!is.finite(ext)] <- 0
  # ext[ext < 0] <- 0         # eliminate negative values
  MP <- ext * L
  
  #-------------------------------------------------------------------------
  # Calculate detailed Footprints
  #-------------------------------------------------------------------------
  if(country=="EU27"){
    Y_country <- Yi[, (Y_codes$continent == "EU" & Y_codes$iso3c != "GBR"), with=FALSE]
    colnames(Y_country) <- Y_codes$fd[Y_codes$continent == "EU" & Y_codes$iso3c != "GBR"]
    Y_country <- agg(Y_country)
  } else if(country=="EU"){
    Y_country <- Yi[, Y_codes$continent == "EU", with=FALSE]
    colnames(Y_country) <- Y_codes$fd[Y_codes$continent == "EU"]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Yi[, Y_codes$iso3c == country]
    colnames(Y_country) <- Y_codes$fd[Y_codes$iso3c == country]
  }
  FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))
  colnames(FP) <- rownames(FP) <- paste0(index$iso3c, "_", index$item)
  results <- FP %>% 
    as.matrix() %>% 
    as_tibble() %>% 
    mutate(origin = paste0(index$iso3c, "_", index$item)) %>% 
    gather(index, value, -origin) %>% 
    mutate(country_origin = substr(origin,1,3)) %>% 
    mutate(item_origin = substr(origin,5,100)) %>% 
    mutate(country_target = substr(index,1,3)) %>% 
    mutate(final_product = substr(index,5,100)) %>% 
    select(-index, -origin) %>% 
    filter(value != 0)
  
  results$group_origin <- items$comm_group[match(results$item_origin,items$item)]
  results$final_product_group <- items$comm_group[match(results$final_product,items$item)]
  results$continent_origin <- regions$continent[match(results$country_origin, regions$iso3c)]
  results$continent_origin[results$country_origin==country] <- country
  
  # fwrite(results, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_full.csv"), sep=",")
  
  data <- results %>%
    group_by(final_product, group_origin, country_origin) %>%
    summarise(value = round(sum(value))) %>%
    filter(value != 0) %>%
    spread(group_origin, value)

  fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_detailed.csv"), sep=",")
  
  data <- results %>% 
    mutate(group = ifelse(group_origin=="Grazing", "Grazing", "Crops")) %>%
    mutate(group = ifelse(grepl("Livestock", group_origin), "Livestock", group)) %>% 
    #mutate(group = ifelse(group_origin=="Fish", "Livestock", group)) %>%    # fish has no direct land or water use
    mutate(group = paste(group, continent_origin, sep = "_")) %>% 
    group_by(final_product, group) %>% 
    filter(value != 0) %>% 
    summarise(value = round(sum(value))) %>% 
    spread(group, value, fill = 0)
  data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_continent.csv"), sep=",")
  
  # data <- results %>% 
  #   group_by(item_origin, continent_origin) %>% 
  #   filter(value != 0) %>% 
  #   summarise(value = round(sum(value))) %>% 
  #   spread(continent_origin, value, fill = 0)
  # data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_primarycrop_continent.csv"), sep=",")
  
}



#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
allocations = c("mass","value")
year <- 2019

# Read data
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]
# fwrite(Ei, "output/extensions_2012.csv")

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$area_code)]
Y_codes$continent = regions$region[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

extensions <- colnames(Ei)[c(8,10:11)]
# consumption_categories <- c("food","other","stock_addition","losses","balancing")
consumption_categories <- c("food","other","stock_addition","balancing")
# countries <- c("USA","CAN","AUS","EU27")
# countries <- c("DEU")
countries <- "EU27"
allocation = "value"
country = "EU27"
extension = "landuse"
consumption = "food"

for(allocation in allocations){
  if(allocation=="mass") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/",year,"_L_mass.rds"))
  if(allocation=="value") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/",year,"_L_value.rds"))
  
  for(country in countries){
    for(extension in extensions){
      for(consumption in consumption_categories){
        # calculate footprints
        footprint(country = country, extension = extension, consumption = consumption, allocation = allocation)
      }
    }
  }
}






# #-------------------------------------------------------------------------
# # Calculate aggregate footprints
# #-------------------------------------------------------------------------
# footprint <- function(region = character(), Y = matrix(), year=integer(), MP = matrix(), type=character()){
#   FP <- Y[,1:2] * colSums(MP)
#   FP[FP<0] <- 0     # filter negative values
#   FP[rep(items$Item,nrreg)=="Cocoa Beans and products",][FP[rep(items$Item,nrreg)=="Cocoa Beans and products",] > 
#                                                            mean(FP)*1000] <- 0   # filter outliers
#   FP <- t(FP)
#   colnames(FP) <- rep(c(rep("crop",96),rep("lvst",130-96)), nrreg)
#   FP <- agg(FP)
#   FP <- reshape2::melt(FP)
#   FP$year <- year
#   FP$region <- region
#   FP$type <- type
#   return(FP)
# }
# results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP, allocation))
# results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP, allocation))
# 
# imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP[rep(regions$Continent, each = nrow(items)) != "EU", ], allocation))
# imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP[rep(regions$Continent, each = nrow(items)) != "EU", ], allocation))
# 
# data.table::fwrite(results, file="./results/FABIO_land_results.csv", sep=",")
# data.table::fwrite(imports, file="./results/FABIO_land_imports.csv", sep=",")




