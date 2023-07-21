## Climate projection until 2100 
##  Goal: calculate country weighted average by population;

##  data source: https://esgf-node.llnl.gov/search/cmip6/ ;
##  experiment id: `ssp585`, var: `tas` and `pr`;
##  naming convention SSPx-y: SSP-x + RCP-y;
##  Note that the unit for `tas` is K; `pr` unit: kg/m2/s


##  General procedure:
##      1. resample population and country shapefile to match the resolution of GCMs;
##          ** need to specify resample factor manually; need to be integer. **
##      2. merge data together;
#       3. calculate country averages;


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

# set plot parameters
par(oma=c(0.5,0.5,0,2), mar=c(2.5, 3.5, 2, 1), mgp=c(2.4,0.8,0), mfrow=c(1,1), xpd=TRUE)
theme_set(theme_bw())
c <- 10 * .Machine$double.eps

setwd("~/Documents/postdoc/GDP")

## save current workspace ##
f_name <- "RImage/gdp_2023-07-21.RData"
f_name
# save.image(f_name)
# load(f_name)


## specify global variables
var_name <- "tas" # temperature
var_name <- "pr" # precipitation
ssp <- "ssp585"

CC_folder <- sprintf("data/SSP585/CC_%s/", var_name) # output folder containing country averages
CC_folder

if (!dir.exists(CC_folder)){
    dir.create(CC_folder)
    cat ("create new folder...")
} else{
    cat ("dir exists!")
}

nc_folder <- sprintf("data/SSP585/%s/", var_name) # GCM data dir
gcm_name <- "BCC-CSM2-MR"
f_name <- paste0(nc_folder, sprintf("%s_Amon_%s_%s_r1i1p1f1_gn_201501-210012.nc", var_name, gcm_name, ssp) )
f_name
cru <- brick(f_name) # read GCM climate data
cru
res(cru) # 160*320
dim(cru)
# 1032 months, from 2015-01-16 to 2100-12-16
time_idx <- getZ(cru) 
length(time_idx)
time_idx %>% str()
time_idx[1:5]
time_idx %>% tail()

cru[[1]]
getValues(cru[[1]]) %>% summary()
rotate(cru[[1]]) # convert to 180 longitude CRS
mapview(rotate(cru[[1]]))



## ========================================================================== ##
## population and country boundary data needs to be resampled 
##      to conform with the resolution of GCMs;
##      only need to process once per GCN;
## Process population data -----------------------------------------------------
## population res: 0.5*0.5 deg
pop <- raster("./data/population/gpw_v4_population_density_rev11_2000_30_min.asc")
pop
dim(pop)[-3]/dim(cru)[-3] # need to factor by 2.25, 1/4*9
factor1 <- 4 # specify disaggregate factor!
factor2 <- 9 # specify aggregate factor!
mapview(pop)
getValues(pop)
plot(pop, main="0.5 deg")
pop
getValues(pop) %>% summary()
getValues(pop) %>% na.omit() %>% length()
getValues(pop) %>% na.omit() %>% quantile(probs=seq(0.5, 1, 0.05))
# visualize areas with pop density >200
pop_filter <- pop
values(pop_filter) <- as.numeric(values(pop_filter)>200)
values(pop_filter) <- as.factor(values(pop_filter)>200)
values(pop_filter) <- values(pop_filter)>200
values(pop_filter) %>% summary()
table(values(pop_filter) %>% na.omit())

values(pop_filter) %>% na.omit() %>% summary()

# categorical legend, with pop density greater than 200
myColorRamp <- colorRampPalette(c("red", "blue")) 
f_name <- "figures/pop_density/pop_more-than200.png"
png(f_name, width=12*ppi, height=6*ppi, res=ppi)
par(oma=c(0.5,0.5,0,2), mar=c(2.5, 3.5, 2, 5), mgp=c(2.4,0.8,0), mfrow=c(1,1), xpd=TRUE)
plot(pop_filter, main="more than 200 pop density", col=myColorRamp(2), legend=FALSE)
par(xpd=TRUE)
legend(x="bottomright", legend=c("<200", ">200"), fill=myColorRamp(2), inset=c(-0.1, 0))
dev.off()

# interpolate by 'nearest neighbor', disaggregate to 0.125 deg
template <- raster(extent(pop), 
                   crs=crs(pop),
                   resolution=res(pop)[1]/factor1)
template
detailedRas <- projectRaster(pop, to=template, method="ngb") 
detailedRas
getValues(detailedRas) %>% summary()
# aggregate to 1.125 deg, conform to GCM resolution
aggRas <- aggregate(detailedRas, fact=factor2, fun=sum)
aggRas <- aggRas/factor1^2 # adjust for the disaggregate operation, divide by the square of the disaggregate factor
aggRas
getValues(aggRas) %>% summary()
getValues(aggRas) %>% na.omit() %>% quantile(probs=seq(0.5, 1, 0.05))

aggRas500 <- aggRas
values(aggRas500)[values(aggRas500)>500] <- 500
getValues(aggRas500) %>% summary()
getValues(aggRas500) %>% na.omit() %>% quantile(probs=seq(0.5, 1, 0.05))

## visualize population density
f_name <- "figures/pop_density/pop_agg_1p125_cap500.png"
png(f_name, width=10*ppi, height=7*ppi, res=ppi)
plot(aggRas500, main="1.125 deg, cap by 500")
dev.off()


x11()
plot(aggRas, main="1.125 deg")
f_name <- "figures/pop_density/pop_agg_1p125.png"
ppi <- 300
png(f_name, width=10*ppi, height=7*ppi, res=ppi)
plot(aggRas, main="1.125 deg")
dev.off()

## ========================================================================== ##
## Process country boundary data -----------------------------------------------
## CC boundary res: 0.5*0.5 deg
# country code data, -99: NA
cntry_code <- read_csv("./data/cntry_code.csv")
cntry_code$ISO_N3
cntry_code$ISO_N3 %>% length() # 185

countryRas <- raster(gridCountriesDegreesHalf)
countryRas
res(countryRas)
getValues(countryRas) %>% str()
getValues(countryRas) %>% na.omit() %>% str()
getValues(countryRas) %>% na.omit() %>% unique()
getValues(countryRas) %>% na.omit() %>% unique() %>% length()
# interpolate by 'nearest neighbor', disaggregate to 0.125 deg
template
detailedRas_CC <- projectRaster(countryRas, to=template, method="ngb") 
detailedRas_CC
getValues(detailedRas_CC) %>% summary()

getValues(detailedRas_CC) %>% na.omit() %>% unique()
getValues(detailedRas_CC) %>% na.omit() %>% unique() %>% length()

# aggregate to 1.125 deg
aggRas_CC <- aggregate(detailedRas_CC, fact=factor2, fun=mean)
aggRas_CC
getValues(aggRas_CC) %>% na.omit() %>% unique()
values(aggRas_CC)[getValues(aggRas_CC) %ni% cntry_code$ISO_N3] <- NA
getValues(aggRas_CC) %>% na.omit() %>% unique()
getValues(aggRas_CC) %>% na.omit() %>% unique() %>% length()
plot(aggRas_CC)
## remove values not in the ISO code list


# 186 country
na.omit(gridCountriesDegreesHalf@data$ISO_N3) %>% unique() %>% length()

plot(gridCountriesDegreesHalf)

## ========================================================================== ##
## Merge climate, population, and country code ---------------------------------
agg_cntry <- function(x, id, w=1) {
    ## Calculate weighted averages grouped by `id`, weighted by `w`
    # aggregate(x, by=id, FUN='mean', na.rm=TRUE)
    result <- by(cbind(x,w), id, function(d) weighted_mean(d[,1], d[,2], na.rm=TRUE))
    as.data.frame(cbind(result))
}
pop_sgdf <- as(aggRas, "SpatialGridDataFrame") # population
pop_sgdf@data %>% str()
CC_sgdf <- as(aggRas_CC, "SpatialGridDataFrame") # country boundary, 169 countries in total
CC_sgdf@data %>% str()

end_id <- nlayers(cru) # total number of periods
end_id

rm(climate_df) # remove to initialize

for (i in 1:end_id){
    ## loop through period to get country climate averages
    one_layer <- cru[[i]] # get current climate layer
    one_layer <- rotate(one_layer) # convert to 180 longitude CRS
    one_layer 
    getZ(one_layer)
    the_period <- format(getZ(one_layer), "%Y-%m-%d")
    the_period

    DNE <- tryCatch(colnames(climate_df), error=function(e) e)    
    if (inherits(DNE, "error")) {
        print ("Initializing climate_df ...")
    } else if (the_period %in% colnames(climate_df)) {
        # already calculated, skip to next period
        print (sprintf("%s already exited. Skip to next.", the_period))
        next  
    }
    if (end_id%%12==0) cat(sprintf("Current period: %s \n", the_period))
    
    ## convert to SpatialGridDataFrame
    cru_sgdf <- as(one_layer, "SpatialGridDataFrame")
    cru_sgdf
    # summary(cru_sgdf)
    # cru_sgdf@data %>% str()
    
    ## merge data together
    cru_sgdf[['cntry.code']] <- CC_sgdf@data
    cru_sgdf[['popu']] <- pop_sgdf@data
    # str(cru_sgdf)
    
    ## calculate country averages
    climate_CC <- agg_cntry(cru_sgdf@data[,1],
                            id=cru_sgdf@data$cntry.code, 
                            w=cru_sgdf@data$popu)
    colnames(climate_CC) <- the_period
    climate_CC <- climate_CC %>% 
        as_tibble(rownames = "ISO_N3") %>% 
        mutate(ISO_N3=as.numeric(ISO_N3)) 
    climate_CC
    
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
head(colnames(climate_df))
tail(colnames(climate_df))

f_name <- sprintf("data/SSP585/CC_%2$s/%1$s_%2$s-2015-01-16to2100-12-16.csv", gcm_name, var_name)
f_name # output file name
if (var_name=="tas"){
    ## convert tas unit to Celsius
    climate_df_Celsius <- do.call(cbind, lapply(climate_df[,-1:-3], function(col) col-273.15))
    climate_df_Celsius <- bind_cols(climate_df[,1:3], climate_df_Celsius)
    climate_df_Celsius
    write_excel_csv(climate_df_Celsius, f_name)
} else if (var_name=="pr") {
    ## convert pr unit to mm/month
    climate_df_mm <- do.call(cbind, lapply(climate_df[,-1:-3], function(col) col*86400*30))
    climate_df_mm <- bind_cols(climate_df[,1:3], climate_df_mm)
    climate_df_mm
    write_excel_csv(climate_df_mm, f_name)
}
#


## ========================================================================== ##
## plot climate map ------------------------------------------------------------
## tmp distribution
summary(climate_CC[2])
quantile(climate_CC[2], c(0.05, 0.95), na.rm=TRUE)
legend_low <- -10
legend_high <- 30
step <- 5
climate_CC <- climate_CC %>% 
    mutate(result_c = squish(result, range=c(legend_low+c, legend_high)) )
## historgram
ggplot(data=climate_CC, aes(result_c)) +
    geom_histogram(binwidth = 5, boundary=0, fill="#BDBCBC", color="black") +
    labs(x="Average temperature [ºC]")
## map for one period
world.map <- ne_countries(scale = "medium", returnclass = "sf")
world.map <- world.map %>% left_join(climate_CC, by=c("iso_a3"="ISO_C3"))
world.map %>% str()
world.map$result
climate_CC$ISO_C3 %in% world.map$iso_a3 %>% sum()

title <- "Average temperature"
unit <- "ºC"
myColors <- colorRampPalette(c("#4D1811", "#6F1F16", "#94261A", "#BE3425", "#E75040", "#EC8E85", "#F1CDC9", "#F2E2E0"))(7) # dark to light, tmp

p_map_level <- ggplot(data = world.map %>% filter(continent!="Antarctica") ) +
    geom_sf(aes(fill=result_c), colour='gray50', lwd=0.3 ) +
    scale_fill_stepsn(limits = c(legend_low, legend_high), 
                      breaks = c(legend_low-c, seq(legend_low+step, legend_high-step, step), legend_high+c ), 
                      labels = function(x) {x},
                      show.limits = TRUE, 
                      right = FALSE, # include right bin, (low, up]
                      colours = rev(myColors),
                      name = TeX(unit)
    ) +
    coord_sf(datum = NA) +
    guides(pattern = guide_legend(title=element_blank()) ) +
    labs(title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.1),
          legend.title = element_text(hjust=0.9) )
p_map_level












