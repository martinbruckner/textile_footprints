# Textiles footprint

library(data.table)
library(Matrix)
library(dplyr)
library(ggplot2)
library(patchwork)

source("R/footprint_functions.R")

year = 2020

conc <- fread("input/fabio_exio_conc.csv")
fabio_index <- readRDS("input/fabio_index.rds")
exio_index <- readRDS("input/exio_index.rds")

# prepare data ---------------------------------------------------------------------------------------

Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/pxp/IOT_",year,"_pxp/Y.rds"))
Y_codes <- readRDS(paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.8.2/Y.codes.rds"))
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds"))[,as.character(year)]
X[X<0] <- 0

# prepare extensions ---
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E.rds"))
E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_value.rds"))
E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_value.rds"))
items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/gwp_names.csv"))
items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/luh_names.csv")) 
E_biodiv <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_biodiv.rds"))
items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/biodiv_codes.csv"))

E_ghg_agg <- lapply(E_ghg, colSums)
E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("5 years", items_luh$Element),])})
E_ghg_energy <- lapply(E_ghg, function(x){colSums(x[grepl("Energy use", items_ghg$Element),])})
E_ghg_live <- lapply(E_ghg, function(x){colSums(x[grepl("Manure|Enteric", items_ghg$Element),])})
E_ghg_other <- lapply(E_ghg, function(x){colSums(x[!grepl("Energy use|Manure|Enteric", items_ghg$Element),])})

items_biodiv <- items_biodiv[items_biodiv$land %in% c("cropland", "pasture"),]
E_biodiv <- lapply(E_biodiv, function(x){
  x <- x[, paste0(items_biodiv$species,"_", items_biodiv$land), with = FALSE]
  x <- t(t(x) / 100) 
  colnames(x) <- items_biodiv$land
  x <- agg(x)
})

E_all <- Map(function(e, e_biodiv, e_ghg, e_luh, e_energy, e_live, e_other){
  cbind(e[,1:7],
        e[, "biomass"],
        e[, "landuse"] * 1e-2 * 1e-6, # ha to mio km2
        e[,"blue"] * 1e-9, # m3 to km3
        "biodiv" = e_biodiv[,"cropland"]+e_biodiv[,"pasture"], 
        e[, "n_application"] * 1e-9, # from kg to Tg
        e[, "p_application"] * 1e-9, # from kg to Tg
        "ghg" = e_ghg *1e-6 ,    # kt to Gt
        "luh" = e_luh *1e-6, 
        "ghg_energy" = e_energy *1e-6, 
        "ghg_live" = e_live *1e-6, 
        "ghg_other" = e_other *1e-6, 
        "ghg_all" = (e_ghg + e_luh) *1e-6)
}, E, E_biodiv, E_ghg_agg, E_luh2_agg, E_ghg_energy, E_ghg_live, E_ghg_other)

rm(E, E_biodiv, E_ghg_agg, E_luh2_agg, E_ghg_energy, E_ghg, E_luh)
E <- E_all[[as.character(year)]]


E_int <- E[,c("landuse","biomass","blue", "biodiv","n_application","p_application", "ghg_all", "ghg", "luh", "ghg_energy", "ghg_live", "ghg_other")]
E_int <- E_int/X
E_int[!is.finite(E_int)] <- 0
E_int <- cbind(E[,1:7], E_int)


# prepare Y ---
exio_cont <- unique(conc[, .(Country.Name = EXIOBASE_desire, continent)])
exio_cont <- exio_cont[!duplicated(Country.Name),]
Y_codes <- merge(Y_codes, exio_cont, by = "Country.Name", all.x = TRUE, sort = FALSE)
Y_EU <- rowSums(Y[,Y_codes$continent=="EU"])
Y_world <- rowSums(Y)
Y <- cbind(Y_EU, Y_world)
colnames(Y) <- c("EU", "world")

# prepare L ---
L <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/hybrid/losses/",year,"_B_inv_value.rds"))


# compute footprints ---------------------------------------------------------------------------------------------

fp <- rbindlist(
  lapply(c("EU", "world"), function(reg){
    tex_footprint(Y = Y[,reg], consumer = reg, L = L, E_int = E_int, 
                  fabio_index = fabio_index, exio_index = exio_index, 
                  agg_by = c("consumer", "continent_origin", "item_origin", "group_origin", "item_target"))
  })
)

# some adaptions for graphs
fp$landuse[fp$item_origin=="Grazing"] <- 0
animal <- unique(fabio_index[comm_group %in% c("Live animals", "Milk", "Eggs", "Hides, skins, wool", 
                                               "Meat", "Animal fats", "Honey", "Fish"), item])
fp[, category := "other crops"]
fp[item_origin %in% animal, category := "animal-based"]
fp[group_origin == "Fibre crops", category := "fibre crops"]
fp[, consumer := ifelse(consumer == "EU", "EU-27", "World")]

pc_factors = c("landuse" = 1e12, "blue" = 1e9, "ghg_all" = 1e9, "biodiv" = 1, "n_application" = 1e9, "p_application" = 1e9)

fp_pc <- copy(fp)
fp_pc[consumer == "EU-27", names(pc_factors)] <- as.data.table(t(t(fp_pc[consumer == "EU-27", names(pc_factors), with = FALSE])/447485231*pc_factors))
fp_pc[consumer == "World", names(pc_factors)] <- as.data.table(t(t(fp_pc[consumer == "World", names(pc_factors), with = FALSE])/7794798739*pc_factors))

data <- fp[, .(consumer, origin = continent_origin, input=item_origin, input_group=group_origin, product=item_target,
               input_category=category, cropland_km2=landuse, blue_water_km3=blue, ghg_Gt=ghg_all, biodiv, 
               n_application_Tg=n_application, p_application_Tg=p_application)]
data_pc <- fp_pc[, .(consumer, origin = continent_origin, input=item_origin, input_group=group_origin, product=item_target,
                     input_category=category, cropland_m2=landuse, blue_water_m3=blue, ghg_kg=ghg_all, biodiv, 
                     n_application_kg=n_application, p_application_kg=p_application)]

# save data --------------------------------------------------------------------------------------------
saveRDS(fp, "output/textiles_footprints_2020.rds")
saveRDS(fp_pc, "output/textiles_footprints_percap_2020.rds")
fwrite(data, "output/textiles_footprints_2020.csv")
fwrite(data_pc, "output/textiles_footprints_percap_2020.csv")



# load results --------------------------------------------------------------------------------------------
#fp <- readRDS("output/textiles_footprints_2020.rds")
#fp_pc <- readRDS("output/textiles_footprints_percap_2020.rds")

# aggregate 
fp_agg_cat <- fp_aggregate(fp, aggregate_by = c("consumer", "category"))
fp_agg <- fp_aggregate(fp, aggregate_by = c("consumer"))



# create plots --------------------------------------------------------------------------------------------


## define boundaries -----

pbs_global <- rbind(
  #"boundary" = c("lower", "upper"),
  "landuse" = c(11, 13, 15),
  "blue" = c(1000, 2500, 4000),
  "ghg_all" = c(4.7, 5, 5.4),
  "biodiv" = c(1, 10, 80) * (sum(unique(items_biodiv$number))/1e6),
  "n_application" = c(65, 90, 130),
  "p_application" = c(6, 8, 16)
)
colnames(pbs_global) <- c("lower", "boundary",  "upper")
pbs_eu <- pbs_global * (447485231 / 7794798739)
fwrite(as.data.table(pbs_global, keep.rownames = TRUE), "output/boundaries_world.csv")
fwrite(as.data.table(pbs_eu, keep.rownames = TRUE), "output/boundaries_EU27.csv")

pbs_pc <- pbs_global* (1 / 10e9) * pc_factors

pbs_global <- cbind(pbs_global, "range" = pbs_global[,"upper"] - pbs_global[,"lower"])
pbs_eu <- cbind(pbs_eu, "range" = pbs_eu[,"upper"] - pbs_eu[,"lower"])
pbs_pc <- cbind(pbs_pc, "range" = pbs_pc[,"upper"] - pbs_pc[,"lower"])



## stack plot PER CAPITA ------

indicators <- c("landuse", "blue", "ghg_all", "biodiv", "n_application", "p_application")
cols_vect <- c("fibre crops" =  "#72BE7A", "animal-based" = "#f28f93", "other crops" = "#6f4369", "unspecified" = "#cccccc")


indicator_labs <- c(landuse = "Cropland  in m<sup>2</sup>",
                      ghg_all = "GHG emissions in t CO<sub>2</sub>-eq.",
                      blue = "Freshwater use in m<sup>3</sup>",
                      biodiv = "Biodiversity loss in species / year",
                      p_application =  "Phosphorous application in kg",
                      n_application = "Nitrogen use application kg",
                      ghg_pb = "GHG emissions (excl. energy & LUC) in t CO<sub>2</sub>-eq.", 
                      luh = "GHG emissions from land use change in t CO<sub>2</sub>-eq.")


pb_stack_list <- sapply(indicators, function(ind){
  stacked_bars_general(fp_list = list("EU-27" = fp_pc[consumer == "EU-27",], "World" = fp_pc[consumer == "World",]), # , 
               target = "consumer", aggregate_by = c("consumer", "category"), 
               bounds = pbs_pc, reverse_legend = TRUE, 
               leg_name_bound = "Per-capita boundaries", leg_name_group = "Final consumption type",
               indicator = ind, axis_lab = indicator_labs[ind], lang = "en")
}, simplify = FALSE, USE.NAMES = TRUE)

(pb_stack <- wrap_plots(pb_stack_list, nrow = 2, nocl = 3, guides = "collect")
   & theme(legend.position = "bottom", title = element_text(hjust = 0.5))) #  plot_annotation(title = "Per-capita footprints") 

ggsave(filename="output/pb_stack_per_capita.png", pb_stack, width = 30, height = 25, units = "cm") 



