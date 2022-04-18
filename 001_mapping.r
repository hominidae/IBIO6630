# Load library
library(tidyverse)
library(ggmap)
library(skimr)

# Load the data sets. Change these to where you have the files.
britishcolumbia_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/001_British_Columbia.txt")
alberta_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/002_Alberta.txt")
saskatchewan_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/003_Saskatchewan.txt")
manitoba_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/004_Manitoba.txt")
ontario_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/005_Ontario.txt")
quebec_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/006_Quebec.txt")
newbrunswick_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/007_NewBrunswick.txt")
novascotia_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/008_Nova_Scotia.txt")
newflab_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/009_Newfoundland_Labradour.txt")
yukon_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/010_Yukon.txt")
nwt_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/011_NWT.txt")
nunavut_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/012_Nunavut.txt")

# Let's look at the data.
britishcolumbia_data %>% skim()
alberta_data %>% skim()
saskatchewan_data %>% skim()
manitoba_data %>% skim()
ontario_data %>% skim()
quebec_data %>% skim()
newbrunswick_data %>% skim()
novascotia_data %>% skim()
newflab_data %>% skim()
yukon_data %>% skim()
nwt_data %>% skim()
nunavut_data %>% skim()

# That's a lot of information.
# But let's do something crazy. Let's combine them all to compare against the GBIF data set.
canada_data <- rbind(britishcolumbia_data, alberta_data)
canada_data1 <- rbind(canada_data, saskatchewan_data)
canada_data2 <- rbind(canada_data1, manitoba_data)
canada_data3 <- rbind(canada_data2, ontario_data)
canada_data4 <- rbind(canada_data3, quebec_data)
canada_data5 <- rbind(canada_data4, newbrunswick_data)
canada_data6 <- rbind(canada_data5, novascotia_data)
canada_data7 <- rbind(canada_data6, newflab_data)
canada_data8 <- rbind(canada_data7, yukon_data)
canada_data9 <- rbind(canada_data8, nwt_data)
canada_data10 <- rbind(canada_data9, nunavut_data)
rm(canada_data1,canada_data2,canada_data3,canada_data4,canada_data5,canada_data6,canada_data7,canada_data8,canada_data9)
canada_data <- canada_data10
rm(canada_data10)

# Let's write a save it so we don't need to do that again.
write_csv(x = canada_data, "E:/2021_UoG/IBIO 6630/src/Data/canada_data.csv")

# Let's do some clean-up. We only want canada_data and nunavut_data going forward.
rm(alberta_data,britishcolumbia_data,manitoba_data,newbrunswick_data,newflab_data,novascotia_data,nwt_data,ontario_data,quebec_data,saskatchewan_data,yukon_data)

# Use this to read it again later
canada_data <- read_csv("E:/2021_UoG/IBIO 6630/src/Data/canada_data.csv")

# Load the GBIF data
canada_gbif_data <- read_tsv("E:/2021_UoG/IBIO 6630/src/Data/Canada_GBIF_data.csv")

# Select from nunavut_data our working set of data
nunavut_specimens <- nunavut_data %>%
  select(recordID,phylum_name,class_name,order_name,family_name,subfamily_name,genus_name,lat,lon)

# Let's map it!
canada_specimens <- canada_data %>%
  select(recordID,phylum_name,class_name,order_name,family_name,subfamily_name,genus_name,lat,lon)

# Let's map the GBIF data now too
canada_gbif_specimens <- canada_gbif_data %>%
  select(gbifID,phylum,class,order,family,genus,species,decimalLatitude,decimalLongitude)

# Remove anything that is NA from lat and long coords for nunavut data
latnotdistinct <- data.frame(nunavut_specimens$lat)
latnotdistinct <- latnotdistinct[!is.na(latnotdistinct)]
longnotdistinct <- data.frame(nunavut_specimens$lon)
longnotdistinct <- longnotdistinct[!is.na(longnotdistinct)]

# Let's do the same for Canada
latcannotdistinct <- data.frame(canada_specimens$lat)
latcannotdistinct <- latcannotdistinct[!is.na(latcannotdistinct)]
longcannotdistinct <- data.frame(canada_specimens$lon)
longcannotdistinct <- longcannotdistinct[!is.na(longcannotdistinct)]

# Let's do the same for the GBIF data
latcangbifnotdistinct <- data.frame(canada_gbif_specimens$decimalLatitude)
latcangbifnotdistinct <- latcangbifnotdistinct[!is.na(latcangbifnotdistinct)]
longcangbifnotdistinct <- data.frame(canada_gbif_specimens$decimalLongitude)
longcangbifnotdistinct <- longcangbifnotdistinct[!is.na(longcangbifnotdistinct)]

# Only keep insects using match for Arthropoda in Nunauvt
nunavut_arthropoda <- nunavut_specimens %>%
  filter(phylum_name == "Arthropoda")

# Only keep insects using match for Arthropoda in Canada
canada_arthropoda <- canada_specimens %>%
  filter(phylum_name == "Arthropoda")

# Remove NA's from Nunavut
latnotdistinctarth <- data.frame(nunavut_arthropoda$lat)
latnotdistinctarth <- latnotdistinctarth[!is.na(latnotdistinctarth)]
longnotdistinctarth <- data.frame(nunavut_arthropoda$lon)
longnotdistinctarth <- longnotdistinctarth[!is.na(longnotdistinctarth)]

# Let's remove NA's from Canada too
latcannotdistinctarth <- data.frame(canada_arthropoda$lat)
latcannotdistinctarth <- latcannotdistinctarth[!is.na(latcannotdistinctarth)]
longcannotdistinctarth <- data.frame(canada_arthropoda$lon)
longcannotdistinctarth <- longcannotdistinct[!is.na(longcannotdistinctarth)]

# Only keep insecta from Nunavut
nunavut_insects <- nunavut_specimens %>%
  filter(class_name == "Insecta")

# Let's do the same for insecta
canada_insects <- canada_specimens %>%
  filter(class_name == "Insecta")

# Only keep insects using match for Arthropoda in Canada from GBIF as we've already selected all Arthropoda from Canada from GBIF
canada_gbif_insects <- canada_gbif_specimens %>%
  filter(class == "Insecta")

# Change the names of canada_gbif_specimens though because I am not typing decimalLatitude and decimalLongitude multiple times.
names(canada_gbif_insects) <- c("gbifID","phylum","class","order","family","genus","species","lat","lon")

# Remove NA's from Insecta from Nunavut
latnotdistinctins <- data.frame(nunavut_insects$lat)
latnotdistinctins <- latnotdistinctins[!is.na(latnotdistinctins)]
longnotdistinctins <- data.frame(nunavut_insects$lon)
longnotdistinctins <- longnotdistinctins[!is.na(longnotdistinctins)]

# Remove NA's from Insect from Canada
latcannotdistinctins <- data.frame(canada_insects$lat)
latcannotdistinctins <- latcannotdistinctins[!is.na(latcannotdistinctins)]
longcannotdistinctins <- data.frame(canada_insects$lon)
longcannotdistinctins <- longcannotdistinctins[!is.na(longcannotdistinctins)]

# Remove NA's from Insecta from GBIF's Canada data
latcangbifnotdistinctins <- data.frame(canada_gbif_insects$lat)
latcangbifnotdistinctins <- latcangbifnotdistinctins[!is.na(latcangbifnotdistinctins)]
longcangbifnotdistinctins <- data.frame(canada_gbif_insects$lon)
longcangbifnotdistinctins <- longcangbifnotdistinctins[!is.na(longcangbifnotdistinctins)]

# Populate all Nunavut coordinates
mapnu.x <- longnotdistinct
mapnu.y <- latnotdistinct

# Populate all Canada coordinates
mapcan.x <- longcannotdistinct
mapcan.y <- latcannotdistinct

# Populate only Arthropoda collected in Nunavut
mapnuarth.x <- longnotdistinctarth
mapnuarth.y <- latnotdistinctarth

# Populate only Arthropoda collected in Canada
mapcanarth.x <- longcannotdistinctarth
mapcanarth.y <- latcannotdistinctarth

# Populate only Insecta collected in Nunavut
mapnuins.x <- longnotdistinctins
mapnuins.y <- latnotdistinctins

# Populate only Insecta collected in Canada
mapcanins.x <- longcannotdistinctins
mapcanins.y <- latcannotdistinctins

# Populate only Insecta collected in Canada GBIF
mapcangbifins.x <- longcangbifnotdistinctins
mapcangbifins.y <- latcangbifnotdistinctins

# Create maps, map everything from Nunavut
mp <- NULL # Initalize mp
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld # Call ggplot and MapWorld over it
mp <- mp+ geom_point(aes(x=mapnu.x, y=mapnu.y), color="blue", size=1)
mp

############# Skip the GGPlot stuff for all of Canada for now.
# Let's do the same with everything from Canada. WARNING: LONG!
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot() + mapWorld
mp <- mp+ geom_point(aes(x=mapcan.x, y=mapcan.y), color="blue", size=1)
mp

# Redraw with just Arthropoda
mapnuarth.x <- longnotdistinctarth
mapnuarth.y <- latnotdistinctarth
mp <- mp+ geom_point(aes(x=mapnuarth.x, y=mapnuarth.y), color="green", size=1)
mp

# Redraw with just Insecta
mp <- mp+ geom_point(aes(x=mapnuins.x, y=mapnuins.y), color="orange", size=1)
mp

# Let's try something else, using the Google Maps API
# Set centre of map to centre of Canada
# 49.77152683630754, -96.81643342186659
# Alternative, centre on the arctic
# 65.37890150264174, -103.89915490418755
# Alt, cambridge bay
# 69.11673387824136, -105.06173296903886
can_centre <- c(long = -105.061732, lat = 69.116733)
# Retrieve map of Canada from Google Maps
mapcan <- get_googlemap(center = can_centre, zoom = 3, maptype="terrain")

######################### This is for Nunavut
# Unfortunately, we need a dataframe rather than vector lists
# So let's fix that
nu_ins <- data.frame(latnotdistinctins,longnotdistinctins)
# Change the column names
names(nu_ins) <- c("lat", "long")

ggmap(mapcan) +
  geom_point(
    data = nu_ins,
    mapping = aes(
      x = long,
      y = lat),
    color="red",
    size=5,
    alpha = 0.01)

###################### This is for Canada
can_ins <- data.frame(latcannotdistinctins,longcannotdistinctins)
# Change the column names
names(can_ins) <- c("lat", "long")

# Map with ggmap and Google Maps API
ggmap(mapcan) +
  geom_point(
    data = can_ins,
    mapping = aes(
      x = long,
      y = lat),
    color="red",
    size=5,
    alpha = 0.01)

# Let's try a heatmap
library(RColorBrewer)

# This is for Nunavut insects
ggmap(mapcan) +
  stat_density_2d(
    data = nu_ins,
    aes(
      x = long,
      y = lat,
      fill = stat(level)
    ),
    alpha = .2,
    bins = 25,
    geom = "polygon"
  ) +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))

######################### The same code as above, but for Canada insects
ggmap(mapcan) +
  stat_density_2d(
    data = can_ins,
    aes(
      x = long,
      y = lat,
      fill = stat(level)
    ),
    alpha = .5,
    bins = 75,
    geom = "polygon"
  ) +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))

######################### The same code as above, but for Canada GBIF insects
can_gbif_ins <- data.frame(latcangbifnotdistinctins,longcangbifnotdistinctins)
# Change the column names
names(can_gbif_ins) <- c("lat", "long")

ggmap(mapcan) +
  stat_density_2d(
    data = can_gbif_ins,
    aes(
      x = long,
      y = lat,
      fill = stat(level)
    ),
    alpha = .5,
    bins = 75,
    geom = "polygon"
  ) +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))
