##############################################################################################
##  FABIO Footprints
##############################################################################################

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

gwp <- as.numeric(colSums(E * Q_codes$GWP))

# calculate footprints -------------------------------------------------------------------------
country="EU27"

ext <- gwp / as.vector(X)
ext[!is.finite(ext)] <- 0
MP <- ext * L

# Prepare Final Demand -------------------------------------------------------------------------
if(country=="EU27"){
  Y_country <- Y[,Y_codes$Country.Name %in% unique(Y_codes$Country.Name)[1:27]]
  colnames(Y_country) <- Y_codes$FD.Category[Y_codes$Country.Name %in% unique(Y_codes$Country.Name)[1:27]]
  Y_country <- agg(Y_country)
} else if(country=="World"){
  Y_country <- Y
  colnames(Y_country) <- Y_codes$FD.Category
  Y_country <- agg(Y_country)
} else {
  Y_country <- Y[,Y_codes$Country.Name == country]
  colnames(Y_country) <- Y_codesFD.Category[Y_codes$Country.Name == country]
}

#-------------------------------------------------------------------------
# Calculate detailed Footprints
#-------------------------------------------------------------------------
FP <- t(t(MP) * rowSums(Y_country))
colnames(FP) <- rownames(FP) <- paste0(IO_codes$Country.Code, "_", IO_codes$Product.Name)
FP <- FP[, IO_codes$Product.Short %in% c("C_TEXT", "C_GARM", "C_LETH")]

results <- FP %>%
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(origin = paste0(IO_codes$Country.Code, "_", IO_codes$Product.Name)) %>% 
  gather(index, value, -origin) %>%
  mutate(country_origin = substr(origin,1,2)) %>% 
  mutate(item_origin = substr(origin,4,100)) %>% 
  mutate(country_target = substr(index,1,2)) %>% 
  mutate(final_product = substr(index,4,100)) %>% 
  select(-index, -origin) %>% 
  filter(value != 0) %>% 
  filter(! item_origin %in% c())

results$item_origin_short <- IO_codes$Product.Short[match(results$item_origin, IO_codes$Product.Name)]

data <- results %>%
  filter(!item_origin_short %in% c("C_PARI", "C_WHEA", "C_OCER", "C_FVEG", "C_OILS", "C_SUGB", "C_FIBR", "C_OTCR", 
                                   "C_CATL", "C_PIGS", "C_PLTR", "C_OMEA", "C_OANP", "C_MILK", "C_WOOL", "C_MANC")) %>% 
  group_by(final_product, item_origin) %>%
  summarise(value = round(sum(value))) %>%
  filter(value != 0) %>% 
  mutate(ghg_Gt = value * 10e-12, value = NULL)

fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_ghg.csv"), sep=",")




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
