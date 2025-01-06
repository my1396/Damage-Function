## Climate projection until 2100 
##  Goal: calculate country average tmp and pre weighted by population;

##  Data source: https://esgf-node.llnl.gov/search/cmip6/ ;
##  experiment id: `ssp585`, 
##  naming convention SSPx-y: SSP-x + RCP-y;
##  var: `tas` (temperaure) and `pr` (precipitation);
##  Note that the unit for `tas` is K; `pr` unit: kg/m2/s;

##  General procedure:
##      1. resample population and country shapefile to match the resolution of GCMs;
##          ** need to specify resample factor manually; need to be INTEGER only. **
##          ** only need to resample once per GCM; if you run `tas` first and 
##          resample pop and country, then when you run `pr`, only need to 
##          start from the merge iteration;
##      2. merge data together;
##      3. loop through periods to calculate country averages;

##  Code comments:
##      1. need to specify SSP scenario (`ssp`); 
##          variable being processed (`var_name`);
##          GCM name being processed (`gcm_name`);
##      2. put climate nc files in "tas" and "pr" folders; these are input;
##          - download data from ESGF website as aforementioned (data source);
##      3. output folders are "CC_tas" and "CC_pr" containing 
##          country averages for each GCM;

library(tidyverse)
library(lubridate)
library(mapview)
library(sp)
library(raster)
library(ncdf4)
library(latex2exp)
library(scales)
## map data
library(rworldmap)
library(rnaturalearth)

source("fun_script.R")

## specify global variables
ssp <- "ssp585"   # SSP scenario
var_name <- "tas" # temperature
# var_name <- "pr"  # precipitation

CC_folder <- sprintf("data/SSP585/CC_%s/", var_name) # output folder containing country averages
CC_folder

if (!dir.exists(CC_folder)){
    dir.create(CC_folder)
    cat ("create new folder...")
} else{
    cat ("dir exists!")
}

nc_folder <- sprintf("data/SSP585/%s/", var_name) # GCM data input dir
gcm_name <- "BCC-CSM2-MR" # specify which GCM
f_name <- paste0(nc_folder, sprintf("%s_Amon_%s_%s_r1i1p1f1_gn_201501-210012.nc", var_name, gcm_name, ssp) )
f_name
cru <- brick(f_name) # read GCM climate data
cru
res(cru) # check resolution and dimension
dim(cru) 

## ========================================================================== ##
## population and country boundary data needs to be resampled 
##      to conform with the resolution of GCMs;
##      only need to process **ONCE** per GCM; (usually tas and pr share same res)
## Process population data -----------------------------------------------------
## population res: 0.5*0.5 deg
pop <- raster("./data/population/gpw_v4_population_density_rev11_2000_30_min.asc")
pop
dim(pop)[-3]/dim(cru)[-3] # need to factor by 2.25, 1/4*9
factor1 <- 4 # specify disaggregate factor!
factor2 <- 9 # specify aggregate factor!

# interpolate by 'nearest neighbor', disaggregate to 0.125 deg
template <- raster(extent(pop), 
                   crs=crs(pop),
                   resolution=res(pop)[1]/factor1)
template
detailedRas <- projectRaster(pop, to=template, method="ngb") 
detailedRas
# aggregate to 1.125 deg by sum, conform to GCM resolution
aggRas <- aggregate(detailedRas, fact=factor2, fun=sum)
aggRas <- aggRas/factor1^2 # adjust for the disaggregate operation, divide by the square of the disaggregate factor
aggRas # make sure the resolution equal to that of GCM!

## ========================================================================== ##
## Process country boundary data -----------------------------------------------
## CC boundary res: 0.5*0.5 deg
# country code data, -99: NA
cntry_code <- read_csv("./data/cntry_code.csv")
countryRas <- raster(gridCountriesDegreesHalf)
countryRas

# interpolate by 'nearest neighbor', disaggregate to 0.125 deg
detailedRas_CC <- projectRaster(countryRas, to=template, method="ngb") 
# aggregate to 1.125 deg by average
aggRas_CC <- aggregate(detailedRas_CC, fact=factor2, fun=mean)
# remove values not in the ISO code list, 169 CC remain
values(aggRas_CC)[getValues(aggRas_CC) %ni% cntry_code$ISO_N3] <- NA
getValues(aggRas_CC) %>% na.omit() %>% unique() %>% length() # 169
aggRas_CC # check resolution


## ========================================================================== ##
## Merge climate, population, and country code ---------------------------------
pop_sgdf <- as(aggRas, "SpatialGridDataFrame") # population
CC_sgdf <- as(aggRas_CC, "SpatialGridDataFrame") # country boundary

end_id <- nlayers(cru) # total number of periods: 1032 mon
end_id
## merge iteration
for (i in 1:end_id){
    if (i==1 & exists("climate_df") ) rm(climate_df) # remove to initialize
    ## loop through period to get country climate averages
    one_layer <- cru[[i]] # get current climate layer
    one_layer <- rotate(one_layer) # convert to 180 longitude CRS
    the_period <- format(getZ(one_layer), "%Y-%m-%d")

    DNE <- tryCatch(colnames(climate_df), error=function(e) e)    
    if (inherits(DNE, "error")) {
        print ("Initializing climate_df ...")
    } else if (the_period %in% colnames(climate_df)) {
        # already calculated, skip to next period
        print (sprintf("%s already exited. Skip to next.", the_period))
        next  
    }
    if (i%%12==0) cat(sprintf("Current period: %s \n", the_period))
    
    ## convert to SpatialGridDataFrame
    cru_sgdf <- as(one_layer, "SpatialGridDataFrame")
    ## merge data together
    cru_sgdf[['cntry.code']] <- CC_sgdf@data
    cru_sgdf[['popu']] <- pop_sgdf@data
    
    ## calculate country averages
    climate_CC <- agg_cntry(cru_sgdf@data[,1],
                            id=cru_sgdf@data$cntry.code, 
                            w=cru_sgdf@data$popu)
    colnames(climate_CC) <- the_period
    climate_CC <- climate_CC %>% 
        as_tibble(rownames = "ISO_N3") %>% 
        mutate(ISO_N3=as.numeric(ISO_N3)) 
    
    if (!exists("climate_df")) {
        climate_df <- climate_CC
    } else {
        climate_df <- climate_df %>% 
            left_join(climate_CC, by="ISO_N3")
    }
} # end loop through periods

climate_df <- climate_df %>% 
    left_join(cntry_code, by="ISO_N3") %>% 
    select(ISO_N3, ISO_C3, cntry.name, everything() )
climate_df
tail(colnames(climate_df))

f_name <- sprintf("data/SSP585/CC_%2$s/%1$s_%2$s-2015-01-16to2100-12-16.csv", gcm_name, var_name)
f_name # output file name

if (var_name=="tas"){
    ## write to local 
    # convert tas unit to Celsius
    climate_df_Celsius <- do.call(cbind, lapply(climate_df[,-1:-3], function(col) col-273.15))
    climate_df_Celsius <- bind_cols(climate_df[,1:3], climate_df_Celsius)
    climate_df_Celsius
    write_excel_csv(climate_df_Celsius, f_name)
} else if (var_name=="pr") {
    # convert pr unit to mm/month
    climate_df_mm <- do.call(cbind, lapply(climate_df[,-1:-3], function(col) col*86400*30))
    climate_df_mm <- bind_cols(climate_df[,1:3], climate_df_mm)
    climate_df_mm
    write_excel_csv(climate_df_mm, f_name)
}
#


