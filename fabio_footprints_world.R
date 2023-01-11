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
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")
# read commodity classification
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$name, each = nrcom),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds"))

#-------------------------------------------------------------------------
# Calculate footprints
#-------------------------------------------------------------------------
year <- 2013
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

# extensions <- colnames(Ei)[c(8)]
# consumption_categories <- c("food","other","stock_addition","balancing")
# countries <- c("USA","IND","CHN","TCG","COG")
allocation = "value"
extension = "landuse"
# consumption = "food"

if(allocation=="mass") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/",year,"_L_mass.rds"))
if(allocation=="value") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/",year,"_L_value.rds"))
# Prepare Multipliers
ext <- as.vector(as.matrix(as.vector(Ei[, ..extension]) / as.vector(Xi)))
ext[!is.finite(ext)] <- 0
MP <- ext * L

FP <- as.matrix(Yi) * colSums(MP[index$item!="Grazing", ])
colnames(FP) <- paste0(Y_codes$iso3c,"_",Y_codes$fd)
rownames(FP) <- index$item
FP <- t(agg(t(FP)))
melt(FP)
results <- FP %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(item = items$item) %>% 
  gather(index, value, -item) %>% 
  mutate(iso3c = substr(index,1,3)) %>% 
  mutate(fd_category = substr(index,5,100)) %>% 
  select(-index) %>% 
  mutate(value = round(value)) %>%
  spread(fd_category, value)

results$comm_group <- items$comm_group[match(results$item,items$item)]
results$group <- items$group[match(results$item,items$item)]
results$country <- regions$name[match(results$iso3c, regions$iso3c)]
results$continent <- regions$continent[match(results$iso3c, regions$iso3c)]

data <- results %>% 
  group_by(iso3c, country, continent, group, comm_group) %>% 
  summarize(food=sum(food),losses=sum(losses),stock_addition=sum(stock_addition),
            other=sum(other),balancing=sum(balancing),unspecified=sum(unspecified)) %>% 
  mutate(total = food + losses + stock_addition + other + balancing + unspecified) %>% 
  filter(total != 0)

data.table::fwrite(data, file=paste0("./output/FABIO_WORLD_",year,"_",extension,"_",allocation,"-alloc.csv"), sep=",")

data <- data %>% 
  ungroup %>% 
  mutate(group = if_else(group=="Livestock products", "Animal-based", "Plant-based")) %>% 
  group_by(iso3c, country, continent, group) %>% 
  summarize(food=sum(food),losses=sum(losses),stock_addition=sum(stock_addition),
            other=sum(other),balancing=sum(balancing),unspecified=sum(unspecified)) %>% 
  mutate(total = food + losses + stock_addition + other + balancing + unspecified) %>% 
  filter(total != 0)

data.table::fwrite(data, file=paste0("./output/FABIO_WORLD_",year,"_",extension,"_",allocation,"-alloc_totals.csv"), sep=",")

