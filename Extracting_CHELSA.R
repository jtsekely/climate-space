#####################################################
##### Extracting bioclim data for exploratories #####
#####################################################

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/media/jtsekely/LocalAdaptHD/Exploratories")

## vector of targeted species
target.species <- c("Acer platanoides", "Acer pseudoplatanus", "Betula pendula", "Carpinus betulus", "Fraxinus excelsior",
                    "Prunus avium", "Sorbus aucuparia", "Tilia cordata")

target.species.under <- c("Acer_platanoides", "Acer_pseudoplatanus", "Betula_pendula", "Carpinus_betulus", "Fraxinus_excelsior",
"Prunus_avium", "Sorbus_aucuparia", "Tilia_cordata")

## Read in full CHELSA layers - EPSG 4326 (wgs84)
climRasts.list <- list.files(pattern = "CHELSA_", 
                             full.names = T) 
climRasts <- terra::rast(climRasts.list) # read in the rasters

## shortening names
names <- as.data.frame(names(climRasts))
names.new <- separate_wider_delim(names, "names(climRasts)", delim="_", names=c("ch", "env", "yr", "v"))
names(climRasts) <- names.new$env
plot(climRasts[[2]]) # sanity check

## metadata for Exploratory plots and tree species presence
plot.meta <- read.csv("./Plot_information/1000_10_data.csv")
tree.meta <- read.csv("./Plot_information/31404_11_data.csv")

## Read in a species distribution shapefiles
spList <- list.files("./chosen", pattern = ".shp", 
                             full.names = T) 
spVect <- vect(spList)

## extract values
extr.every.sp <- list()
for (i in 1:length(target.species)){
  sp <- target.species[i]
  spVect <- vect(paste0("./chosen/", sp, ".shp")) # read in the distribution range shapefile
  extr.range <- terra::extract(climRasts, spVect) # extract values
  extr.range$loc <- "range" # add range column
  extr.range$ID <- "range" # change to character vector
  
  ## subset the tree plot information to the target species
  under <- target.species.under[i] # get species name with underscore
  target.tree.meta <- tree.meta %>%
    filter(Species %in% under)
  
  ## keep only "presence" points
  target.tree.meta <- target.tree.meta %>%
    filter(Cover > 0)
  
  ## keep only 1 row per combination of plot ID - species
  tree.meta.unique <- target.tree.meta  %>%
    distinct(EP_PlotID, Species, .keep_all = TRUE)
  
  ## rename the column for left_join
  tree.meta.unique <- tree.meta.unique %>%
    rename(EP_Plot_ID = EP_PlotID)
  
  ## left_join
  metadata <- left_join(tree.meta.unique, plot.meta)
  
  ## subset to target species
  meta.target <- metadata %>%
    filter(Species == under)
  
  ## latitude-longitude dataframe
  meta.target.ll <- meta.target[,c(12,11)]
  
  ## Extract plot information
  extr.target <- terra::extract(climRasts, meta.target.ll) # extract values
  
  ## rename the plot IDs with their actual IDs
  extr.target$ID <- meta.target$EP_Plot_ID
  
  extr.target <- extr.target %>%
    mutate(loc = case_when(
      grepl("AEW", ID) ~ "ALB",
      grepl("SEW", ID) ~ "SCH",
      grepl("HEW", ID) ~ "HAI",
      TRUE ~ NA_character_  # Keeps other values as NA or change to a default value
    ))
  
  ## bind together for plotting
  extr.all <- bind_rows(extr.range, extr.target)
  extr.all$bio1 <- extr.all$bio1*10
  extr.all$bio12 <- extr.all$bio12*.1
  extr.all$species <- sp
  
  ## write into list
  extr.every.sp[[i]] <- extr.all
  
  # write out dataframe
  write.csv(extr.all, paste0("Extracted_CHELSA_v21", under, ".csv"), row.names = FALSE)
}

# concatenate into one dataframe
end <- do.call(rbind.data.frame, extr.every.sp)

## controlling plot aspects
color_vector <- c("range" = "lightgrey", "ALB" = "#191970", "SCH" = "#1A936F", "HAI" = "#d74e4e")
size_vector <- c("range" = 2, "ALB" = 3, "SCH" = 3, "HAI" = 3)
shape_vector <- c("range" = 20, "ALB" = 15, "SCH" = 15, "HAI" = 15)

## plot
ggplot(end, aes(x=bio1, y=bio12, color=loc, shape=loc, size=loc))+
  geom_point()+
  scale_size_manual(values=size_vector)+
  scale_color_manual(values=color_vector)+
  scale_shape_manual(values=shape_vector)+
  theme_bw()+
  labs(x="Mean Annual Temperature (°C)", y="Total Annual Precipitation (mm)")+
  facet_wrap(~species)

ggsave("climate_space_GenTreeDiv.png", width=10, height=9, dpi=300, units=c("in"))
