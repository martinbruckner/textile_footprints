# Textiles footprint exiobase

library(Matrix)
library(tidyverse)
library(data.table)

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


# load data -------------------------------------------------------------------------
year <- 2020

L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_",year,"_pxp/L.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_",year,"_pxp/x.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_",year,"_pxp/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_",year,"_pxp/satellite/F.rds"))

Y_codes <- readRDS(file="/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/Y.codes.rds")
Q_codes <- readRDS(file="/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/Q.codes.rds")
IO_codes <- readRDS(file="/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IO.codes.rds")
exio_item_categories <- fread("input/exio_item_categories.csv")
gwp <- as.numeric(colSums(E * Q_codes$GWP))


# calculate multipliers -------------------------------------------------------------------------
ext <- gwp / as.vector(X)
ext[!is.finite(ext)] <- 0
MP <- ext * L
rm(L)


# calculate footprints -------------------------------------------------------------------------
# country="EU-27"
fp <- rbindlist(
  lapply(c("EU-27", "world"), function(country){
    
    # prepare final demand -------------------------------------------------------------------------
    if(country=="EU-27"){ Y_country <- Y[,Y_codes$Country.Name %in% unique(Y_codes$Country.Name)[1:27]]
    } else { Y_country <- Y }
    
    FP <- t(t(MP) * rowSums(Y_country))
    colnames(FP) <- rownames(FP) <- paste0(IO_codes$Country.Code, "_", IO_codes$Product.Name)
    FP <- FP[, IO_codes$Product.Short %in% c("C_TEXT", "C_GARM", "C_LETH")]
    FP <- as(FP, "dgTMatrix")
    results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value=FP@x)
    results[,`:=`(country_origin = substr(origin,1,2),
                  item_origin_index = (FP@i + 1) %% 200,
                  item_origin = substr(origin,4,100),
                  final_product = substr(target,4,100),
                  consumer = country)]
    rm(FP)
    # add auxiliary info on origin and target commodities
    match_origin <- match(results$origin, paste0(IO_codes$Country.Code, "_", IO_codes$Product.Name))
    results[,`:=`(
      item_origin_short = IO_codes$Product.Short[match_origin],
      continent_origin = IO_codes$Region.Code[match_origin],
      item_origin_category = exio_item_categories$item_category[
        match(results$item_origin, exio_item_categories$item_origin)]
      )]
    
    # reorder and aggregate columns
    results <- results %>%
      select(-origin, -target, -country_origin) %>% 
      filter(value != 0) %>% 
      # relocate(c(consumer, continent_origin, item_origin_index, item_origin_short), .before = item_origin) %>%
      # relocate(value, .after = final_product) %>% 
      group_by(consumer, continent_origin, item_origin_index, item_origin_category, final_product) %>% 
      summarize(value = sum(value, na.rm = TRUE), .groups = "drop")
    
  })
)

animal <- c("C_CATL", "C_PIGS", "C_PLTR", "C_OMEA", "C_OANP", "C_MILK", "C_WOOL", "C_MANC")
crops <- c("C_PARI", "C_WHEA", "C_OCER", "C_FVEG", "C_OILS", "C_SUGB", "C_OTCR")

data <- fp %>%
  # mutate(item_origin = ifelse(item_origin_short %in% crops, "Agriculture, other crops", 
  #                             ifelse(item_origin_short == "C_FIBR", "Agriculture, fibre crops", 
  #                                    ifelse(item_origin_short %in% animal, "Agriculture, animal products", 
  #                                           item_origin)))) %>% 
  # mutate(item_origin_index = ifelse(item_origin_index <= 15, 1, item_origin_index)) %>% 
  group_by(consumer, continent_origin, item_origin_category, final_product) %>%
  summarise(value = round(sum(value))) %>%
  filter(value != 0) %>% 
  mutate(ghg_Gt = value * 10e-12, value = NULL)

fwrite(data, file=paste0("./output/textiles_footprints_EXIOBASE_",year,".csv"), sep=",")





# data <- results %>% 
#   group_by(final_product, item_origin) %>% 
#   summarise(value = round(sum(value))) %>% 
#   filter(value != 0) %>% 
#   spread(item_origin, value, fill = 0)
# data$index <- IO_codes$Index[IO_codes$Country.Code=="AT"][match(substr(data$final_product,1,50), 
#                                                                 substr(IO_codes$Product.Name[IO_codes$Country.Code=="AT"],1,50))]
# data <- data[,c(ncol(data),1:(ncol(data)-1))]
# data <- data[order(data$index),]
# fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_products-inputs.csv"), sep=",")
# 
# data <- results %>% 
#   group_by(item_origin, continent_origin) %>% 
#   filter(value != 0) %>% 
#   summarise(value = round(sum(value))) %>% 
#   spread(continent_origin, value, fill = 0)
# data.table::fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_inputs-continent.csv"), sep=",")
