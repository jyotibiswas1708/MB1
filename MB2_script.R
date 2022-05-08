#---------------------------------------------------------------------------------------------------
#                            CHANGE IN TREE PERCENT COVER 
#                           ================================
#--------------------------------------------------------------------------------------------------

# University of Wuerzburg, EAGLE M.sc 
# Department of Remote Sensing, Dr. Martin Wegmann
# Project on : Introduction to Programming and Geostatistics ( MB2)
# Submission for : Winter Semester 2021

# Submitted By : Jyoti Biswas
# Email : jyoti.biswas@stud-mail.uni-wuerzburg.de



# Project idea -------------------------------------------------------------------------------------

# The idea of the project is to work with the spatraster data(Vegetation Continuous 
# Fields (VCF) data,getting the data,reading,processing it according to the area of interest. 


# Aim  ---------------------------------------------------------------------------------------------
# To see/visualize the tree cover change within five years of interval. 
# The area of interest is a state,Assam in India.

# Requirements -------------------------------------------------------------------------------------
# VCF data can be downloaded through the script(for data download,you will need 
# a username and password.Please register on NASA Earth Data if you haven't done so.
# also  data is provided additionally as it takes longer time for the data to get 
# downloaded depending on the interested time frame. 
# The shapefile for aoi can be downloaded through geodata package

# Result ------------------------------------------------------------------------------------------
# will be a plot showing tree cover changes for different time frame


# Project Overview --------------------------------------------------------------------------------
#   - 1.Introduction (About the data)
#   - 2.Installation/loading of required package
#           2.1  Loading of the package
#           2.2  Acessing VCF data,aoi shapefile
#   - 3.Basic operation 
#           3.1  Merginng,cropping,masking
#           3.2  Extracting values and computing results
#   - 4 Visualizing the changes
#   - 5 References


#===================================================================================================
# 1.Introduction (About the data )------------------------------------------------------------------

# Vegetation Continuous Fields (VCF) data provided by the Land Processes Distri-
# buted Active Archive Center (LP DAAC), a component of NASA's Earth Observing 
# system Data and Information System (EOSDIS). The MOD44B Version 6 VCF is a ye-
# arly representation of surface vegetation from 2000 to 2020 at 250 m resolution. 
# Each pixel stores a percentage of three ground cover components: percent tree 
# cover, percent non-tree cover, and percent bare
# The VCF data utilize thermal signatures and other correlates to distinguish
# forest and non-forest plantation





#====================================================================================================
# 2. Installation /loading required package ---------------------------------------------------------


# Required Package ----------------------------------------------------------------------------------
# The require package are installed and loaded automatically if they are not al-
# ready present.
# The packages are : pacman,terra,luna,getPass,gdalUtils,sf,tidyverse,geodata,d-
# plyr, rlang,purrr,readr,data.table,ggplot2,reshape2


# 2.1 Loading of the package
# pacman will install and load all needed packages if they are not already present
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dependencies = TRUE)
}

# needed packages:
p_load(terra, luna, getPass, gdalUtils, sf,tidyverse, geodata,dplyr,ggplot2,
       reshape2,rlang, purrr, install = TRUE)
# incase you have trouble installing "luna package",try installing through
# install.packages("remotes")
# remotes::install_github("rspatial/luna")

# 2.2 Accessing VCF data,aoi shapefile

# lists all products that are currently searchable
prod <- getProducts()
head(prod)

# listing the MODIS products
modis <- getProducts("^MOD|^MYD|^MCD")
head(modis)
product = "MOD44B"    #  interested product id
path  <- "/data/" # select the path for the MODIS data 
start.date <- "2010-01-01"#setting parameters for data download
end.date <- "2020-01-01"



# aoi shapefile
# interested in India at district level,we have downloaded map of India and its  
# level 2 administrative areas with the following code,data downloaded in the 
# specified path a/c to arguements
india = geodata::gadm("India", level=2, path= path)

# downloaded data through gadm () will be PackedSpatVector class,converted to sf.
# sf class,easier to work with in R,first we read it usinng readRDS(),then converted to Spatvector via vect()

india = readRDS("./data/gadm36_IND_2_pk.rds") %>% vect() %>% st_as_sf(india)

assam_aoi  = india %>% filter(NAME_1 == "Assam")
head(assam_aoi )

# plotting aoi interes shapefile
ggplot(data = assam_aoi) +
  geom_sf()


# using the area of interest and time frame ,we can filter VCF data and downlaod  
# the available product

setwd(path) # setting working directory to the path

#if the path exist,data will get downloaded
if (dir.exists(path)){
  #path = path
  Assam <- luna::getModis(product = product,
                          start_date = start.date, end_date = end.date,
                          aoi = assam_aoi, download = TRUE, path=path,username = getPass(msg = "Enter your username:"),password = getPass(msg = "Enter your password:"))
  
} else{ # if not,it will create the path,then download the data
  dir.create(path)
  #path = path
  Assam <- luna::getModis(product = product,
                          start_date = start.date, end_date = end.date,
                          aoi = assam_aoi, download = TRUE, path=path,username = getPass(msg = "Enter your username:"),password = getPass(msg = "Enter your password:"))
}

# downloading data may take some time,we can also use the data already provided 
# if interested in working with same time frame.

files = list.files(path= "./data/", pattern = ".hdf")# returns list of files of pattern .hdf from the path
print(files)
start_year  <- 2009 # download data starts from the year 2009,ends at 2019
end_year  <- 2020


#========================================================================================================

# 3.Basic Operation -------------------------------------------------------------------------------------

years  <- seq(start_year, end_year, by = 1) #creating a seq from start to end year

# filter the files based on year and 
finish_years  <- list()
vcf_raster_all <- list()


# 3.1  Merginng,cropping,masking -----------------------------------------------------------------------

for (file in files){
  year = substr(file, 9,12)#grab year from each file name 
  
  if (year %in% years){
    specific_year_files <-  grep(year, files, value = T)#
    
    
    if(year %in% finish_years){
      print("data for this year already extracted")
    } else{
      # read the data  for each year and select only the Percent tree cover layer.
      
      vcf_raster <-  lapply(specific_year_files, function(x) rast(x)[[1]])
      
      #  (since each year has more than one file, merge
      # merge the data from each file into a single raster.
      
      vcf_raster_per_year_merged <- do.call(merge, vcf_raster)
      
      vcf_raster_all <- append(vcf_raster_all, vcf_raster_per_year_merged)
      
      print(specific_year_files)
      finish_years <- append(finish_years, year)
    }
    
  }
  
}


nlyr(vcf_raster_all) # number of layers



# align coordinate reference systems
assam_aoi = assam_aoi %>% 
  st_transform(crs = crs(vcf_raster_all))

# crop and mask raster data
vcf_raster_croped_masked = terra::crop(vcf_raster_all, assam_aoi) %>% 
  mask(vect(assam_aoi))


# plotting new raster file
plot(vcf_raster_croped_masked[[8]])
plot(st_geometry(assam_aoi), add = TRUE)# add aoi on top of raster plot


# 3.2  Extracting values and computing results --------------------------------------------------------

# extract values for each county and name the colums by year. Each column represents each layer which represents each year.
raster_values_per_polygon <-  terra::extract(vcf_raster_croped_masked, vect(assam_aoi))
colnames(raster_values_per_polygon) <-  c("ID", unlist(finish_years))
head(raster_values_per_polygon)


# compute the per country stats
# keep only rows where the value is less than 200 as 200 is water and rivers.
raster_values_per_polygon <- raster_values_per_polygon %>%
  filter_all(all_vars(. != 200))%>%
  dplyr::group_by(ID)

for(year in unlist(finish_years)){
  x <- raster_values_per_polygon %>% 
    summarise(Mean    = mean(`year`),
              Median  = median(`year`),
              Max     = max(`year`),
              Min     = min(`year`),
              `Positive_Percent` = sum(`year` > 0)/length(`year`) * 100)
  
}


summary_stats <- raster_values_per_polygon %>%
  summarise_all(funs(mean,median,max,min))


#  calculate stats for year 1 , mid year and last year the compare.
# you can save the csv file by uncomment the write_csv code
year1_stats <- raster_values_per_polygon %>% 
  summarise(Mean.2009    = mean(`2009`),
            Median.2009  = median(`2009`),
            Max.2009    = max(`2009`),
            Min.2009     = min(`2009`),
            `Positive Percent.2009` = sum(`2009` > 0)/length(`2009`) * 100)
year1_stats = year1_stats %>% mutate(ID = assam_aoi$NAME_2) %>% rename(County = ID)
knitr::kable(year1_stats, digits = 2)
# write_csv(year1_stats, paste0(path,"/year2009_stats.csv"))# save .csv to dir



year2_stats <- raster_values_per_polygon %>% 
  summarise(Mean.2014    = mean(`2014`),
            Median.2014  = median(`2014`),
            Max.2014     = max(`2014`),
            Min.2014     = min(`2014`),
            `Positive Percent.2014` = sum(`2014` > 0)/length(`2014`) * 100)

year2_stats = year2_stats %>% mutate(ID = assam_aoi$NAME_2) %>% rename(County = ID)
knitr::kable(year2_stats, digits = 2)
#write_csv(year2_stats, paste0(path,"/year2014_stats.csv"))# save .csv to dir

year3_stats <- raster_values_per_polygon %>% 
  summarise(Mean.2019    = mean(`2019`),
            Median.2019   = median(`2019`),
            Max.2019      = max(`2019`),
            Min.2019     = min(`2019`),
            `Positive Percent.2019 ` = sum(`2019` > 0)/length(`2019`) * 100)
year3_stats = year3_stats %>% mutate(ID = assam_aoi$NAME_2) %>% rename(County = ID)
knitr::kable(year3_stats, digits = 2)
#write_csv(year3_stats, paste0(path,"/year2019_stats.csv"))# save .csv to dir

# =======================================================================================================
# 4 Visualizing the changes -----------------------------------------------------------------------------

#merge three csv files 
Merged_file <- list(year1_stats,year2_stats,year3_stats) %>% reduce(full_join,by = "County")

# creating a test df by extracting only mean and also shifting the position of values from column to row 
test_df <- data.frame(County = Merged_file$County, Mean.2009 = Merged_file$Mean.2009, Mean.2014 = Merged_file$Mean.2014, Mean.2019 = Merged_file$Mean.2019)
test_df <- reshape2::melt(test_df, id.var = "County")

# we can remove all the other data which are no longer required
rm(modis,pro,raster_values_per_polygon, summary_stats, vcf_raster, vcf_raster_croped_masked, vcf_raster_per_year_merged,
   x, vcf_raster_all, year1_stats, year2_stats, year3_stats, finish_years, assam_aoi, india, Merged_file)


#plotting
ggplot(test_df, aes(x = County, y = value,fill = variable)) + 
  geom_col(width = 0.7,position = "dodge") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_fill_manual(values = c("yellow green","lime green","dark green"),name = "Year",
                    labels = c("2009", "2014", "2019")
  ) +
  labs(title = "Tree Cover Change (Assam)",
       subtitle = "year (2009,2014,2019)",
       caption = "Contains: MOD44B MODIS data.",hjust =2,
       
       x = "Name of District", y = "Annual mean tree cover [%]") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5))+
  theme(legend.position = c(.9,.9))

# ======================================================================================================
#5 Reference -------------------------------------------------------------------------------------------
# DiMiceli, C., Carroll, M., Sohlberg, R., Kim, D., Kelly, M., Townshend, J. 
# (2015). <i>MOD44B MODIS/Terra Vegetation Continuous Fields Yearly L3 Global 250m 
# SIN Grid V006</i> [Data set]. NASA EOSDIS Land Processes DAAC. Accessed 2022-05
# -03 from https://doi.org/10.5067/MODIS/MOD44B.006

# https://learn.geo4.dev/Deforestation.html Deforestation Learning Module






























