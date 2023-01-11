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
# select fabio version
vers <- "1.1" # or "1.2"

# load FABIO data
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/Y.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/X.rds")) # total output

# load and prepare extensions
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.1/E.rds")) # environmental extensions
# # NOTE: all extensions currently only available for v1.2!
# E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E.rds")) # environmental extensions
# E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/gwp_value.rds"))
# E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/luh_value.rds"))
# items_ghg <- read.csv("/mnt/nfs_fineprint/tmp/fabio/ghg/gwp_names.csv")
# items_luh <- read.csv("/mnt/nfs_fineprint/tmp/fabio/ghg/luh_names.csv")
# # aggregate emission categories
# E_ghg_agg <- lapply(E_ghg, colSums)
# E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("5 years", items_luh$Element),])})
# # bind with E table
# E <- Map(function(e, e_ghg, e_luh){cbind(e,"ghg" = e_ghg*1000, "luh" = e_luh*1000)},E, E_ghg_agg, E_luh2_agg)

# read region classification
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/regions.csv"))
# read commodity classification
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
# create index of all region-commodity combinations
index <- data.table(area_code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    area = rep(regions$name, each = nrcom),
                    continent = rep(ifelse(vers=="1.1",regions$continent,regions$region), each = nrcom),
                    comm_code = rep(items$comm_code, nrreg),
                    item_code = rep(items$item_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg),
                    comm_group = rep(items$comm_group, nrreg))

# # map extensions to v1.1
# range <- rep(c(1:65,NA,66:120,NA,121:123),192)+rep(((0:191)*121), each=125)
# 
# if(vers == "1.1"){
#   for(i in 1:length(E)){
#     data <- E[[i]][range,8:16]
#     data[!is.finite(data)] <- 0
#     E[[i]] <- cbind(index, data)
#   }
# }



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
year <- 2013

# Read data
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]
# fwrite(Ei, "output/extensions_2012.csv")

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

extensions <- colnames(Ei)[c(10:18)]
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
  if(allocation=="mass") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/",year,"_L_mass.rds"))
  if(allocation=="value") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/",year,"_L_value.rds"))
  
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




