##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

footprint <- function(country = "EU", extension = "cropland", extensions = extensions){
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  ext <- extensions[, get(extension)] / as.vector(x)
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  #-------------------------------------------------------------------------
  # Prepare Final Demand
  #-------------------------------------------------------------------------
  if(country=="EU"){
    Y_country <- Y[,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    Y_country <- agg(Y_country)
  } else if(country=="EU27"){
    Y_country <- Y[,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Y[,Y.codes$`Region Name` == country]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` == country]
  }
  # #-------------------------------------------------------------------------
  # # Prepare F_hh
  # #-------------------------------------------------------------------------
  # if(country=="EU"){ F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  # } else if(country=="EU27"){ F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
  # } else { F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` == country] }
  # F_hh_country <- sum(F_hh_country)
  # 
  # if(country=="EU"){ F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  # } else { F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` == country] }
  # F_hh_country_marginal <- sum(F_hh_country_marginal)
  
  #-------------------------------------------------------------------------
  # Calculate detailed Footprints
  #-------------------------------------------------------------------------
  FP <- t(t(MP) * rowSums(Y_country))
  colnames(FP) <- rownames(FP) <- paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)
  
  results <- FP %>%
    as.matrix() %>% 
    as_tibble() %>% 
    mutate(origin = paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)) %>% 
    gather(index, value, -origin) %>%
    mutate(country_origin = substr(origin,1,2)) %>% 
    mutate(item_origin = substr(origin,4,100)) %>% 
    mutate(country_target = substr(index,1,2)) %>% 
    mutate(final_product = substr(index,4,100)) %>% 
    select(-index, -origin) %>% 
    filter(value != 0)
  
  results$group_origin <- IO.codes$Sector.Group[match(results$item_origin, IO.codes$Product.Name)]
  results$group_target <- IO.codes$Sector.Group[match(results$final_product, IO.codes$Product.Name)]
  results$continent_origin <- conc_reg$continent[match(results$country_origin, conc_reg$exiobase_code)]
  results$continent_origin[results$country_origin==country] <- country
  
  data <- results %>%
    group_by(final_product, item_origin, country_origin, continent_origin) %>%
    summarise(value = round(sum(value))) %>%
    filter(value != 0) %>%
    spread(item_origin, value, fill = 0)
  fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_detailed.csv"), sep=",")
  
  data <- results %>% 
    group_by(final_product, item_origin) %>% 
    summarise(value = round(sum(value))) %>% 
    filter(value != 0) %>% 
    spread(item_origin, value, fill = 0)
  data$index <- IO.codes$Index[IO.codes$Country.Code=="AT"][match(substr(data$final_product,1,50), 
    substr(IO.codes$Product.Name[IO.codes$Country.Code=="AT"],1,50))]
  data <- data[,c(ncol(data),1:(ncol(data)-1))]
  data <- data[order(data$index),]
  fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_products-inputs.csv"), sep=",")
  
  data <- results %>% 
    group_by(item_origin, continent_origin) %>% 
    filter(value != 0) %>% 
    summarise(value = round(sum(value))) %>% 
    spread(continent_origin, value, fill = 0)
  data.table::fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_inputs-continent.csv"), sep=",")
  
  
    # add_row(final_product = "Households", country_origin = country, value = F_hh_country) %>% 
    # add_row(final_product = "Households - marginal use", country_origin = country, value = F_hh_country_marginal) %>% 
  
}


#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
year <- 2012

# Read data
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_L.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_E.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_F_hh.RData"))

load(file="/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/Q.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/pxp/IO.codes.RData")

E_fabio <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds"))
E_fabio <- E_fabio[[as.character(year)]]


# Harmonize environmental data -------------------------------------------
conc_reg <- fread("input/reg_fabio_exio.csv")
conc_items <- fread("input/items_fabio_exio.csv")

E_fabio <- merge(E_fabio, conc_reg[, .(area_code, Country.Code = exiobase_code)], 
  by = "area_code", all.x = TRUE)
E_fabio <- merge(E_fabio, conc_items[, .(comm_code, Product.Code = exiobase_code)], 
                 by = "comm_code", all.x = TRUE)
E_fabio <- E_fabio[, .(landuse = sum(landuse, na.rm = TRUE), 
                       biomass = sum(biomass, na.rm = TRUE), 
                       green = sum(green, na.rm = TRUE), 
                       blue = sum(blue, na.rm = TRUE)), 
                   by = c("Country.Code", "Product.Code")]
E_fabio <- E_fabio[Country.Code != "" & Product.Code != ""]

E_exio <- data.table(IO.codes, 
  fodder = as.vector(colSums(E[grepl("Cropland - Fodder crops", Q.codes$Stressor),])),
  grazing = as.vector(colSums(E[grepl("Permanent pastures - Grazing", Q.codes$Stressor),])))
E_exio <- E_exio[, .(Product.Code, fodder = fodder / sum(fodder), 
  grazing = grazing / sum(grazing)), by = .(Country.Code)]
E_exio <- merge(E_exio, E_fabio[Product.Code == "fodder", 
  .(Country.Code, landuse_fodder = landuse, biomass_fodder = biomass, 
    green_fodder = green, blue_fodder =  blue)],
  by = "Country.Code", all.x = TRUE)
E_exio <- merge(E_exio, E_fabio[Product.Code == "grazing", 
  .(Country.Code, landuse_grazing = landuse, biomass_grazing = biomass, 
    green_grazing = green, blue_grazing =  blue)],
  by = "Country.Code", all.x = TRUE)
E_exio <- E_exio[, `:=`(landuse_grazing = round(landuse_grazing * grazing), 
  biomass_grazing = round(biomass_grazing * grazing), 
  green_grazing = round(green_grazing * grazing), 
  blue_grazing = round(blue_grazing * grazing),
  landuse_fodder = round(landuse_fodder * fodder), 
  biomass_fodder = round(biomass_fodder * fodder), 
  green_fodder = round(green_fodder * fodder), 
  blue_fodder = round(blue_fodder * fodder),
  grazing = NULL, fodder = NULL)]

# add footprints of crops
E_exio <- merge(E_exio, E_fabio[!Product.Code %in% c("fodder", "grazing")
  & !Product.Code %in% c("p01.i","p01.j","p01.k","p01.l","p01.m","p01.n","p01.o"),
  .(Country.Code, Product.Code, landuse_crops = landuse, biomass_crops = biomass,
    green_crops = green, blue_crops = blue)],
  by = c("Country.Code", "Product.Code"), all.x = TRUE)

# add blue water footprint of livestock
E_exio <- merge(E_exio, E_fabio[!Product.Code %in% c("fodder", "grazing")
  & Product.Code %in% c("p01.i","p01.j","p01.k","p01.l","p01.m","p01.n","p01.o"),
  .(Country.Code, Product.Code, blue_lvst = blue)],
  by = c("Country.Code", "Product.Code"), all.x = TRUE)

E_exio[is.na(E_exio)] <- 0

extensions <- merge(IO.codes,
  E_exio[, .(cropland = landuse_fodder + landuse_crops, grazing = landuse_grazing,
    blue = blue_fodder + blue_crops + blue_grazing + blue_lvst,
    green = green_fodder + green_crops + green_grazing),
    by = c("Country.Code", "Product.Code")],
  by = c("Country.Code", "Product.Code"), all.x = TRUE)

extensions <- as.data.table(extensions[order(extensions$Index), ])
extensions <- extensions[, .(cropland, grazing, blue, green)]

# consumption_categories <- Y.codes$`Final Demand Category`[1:7]
country="EU27"
extension = names(extensions)[1]

# calculate footprints
for(extension in names(extensions)){
    footprint(country = country, extension = extension, extensions = extensions)
}



