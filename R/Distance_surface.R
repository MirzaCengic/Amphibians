# Functions in this script:
#########################
# - get_distance_surface
# - create_absence_points
# - load_mask
# - get_presence_points

# Functions for distance calculation

# Distance calculation
# This function takes in species range and mask raster, and creates distance surface
# for species' range and continent (corresponding to absence cretion regions)

#' Calculate distance surface
#'
#' Take species range and raster mask, and create distance surface
#' that will encompass same realm and continent that species itself occupies (absence creation regions).
#' This function will call SAGA GIS from the terminal, so SAGA has to be installed, and able to
#'
#' @param species_range Species range. Should be class \code{sf}.
#' @param output_name Output name of created SAGA grid.
#' @param processing_resolution_data csv with species names and processing resolutions.
#'
#' @return Raster
#' @export
#'
#' @examples None.
#' library(Rahat); library(raster); library(tidyverse)
#'
#' proc_res <- "Projects/Amphibians/Output/csv/Species_processing_resolution.csv" %>%
#'  milkunize2() %>%
#'  read_csv()
#'
#'
#' @importFrom raster raster writeRaster mask getValues setValues unique Which
#' @importFrom Rahat milkunize2
#' @importFrom fasterize fasterize
#' @importFrom fs file_temp path_ext_set file_delete
#' @importFrom dplyr filter select pull
#'
get_distance_surface <- function(species_range, output_name, processing_resolution_data, keep = FALSE)
{
  proc_resolution <- processing_resolution_data %>%
    dplyr::filter(species_name == species_range$binomial) %>%
    dplyr::select(proc_resolution) %>%
    dplyr::pull()

  "%notin%" <- Negate("%in%")

  ## Load realm and continents raster, to constrain creation to the realms and continents
  ## in which the species is found
  # Realm raster
  realm_raster <- load_mask("Realm", resolution = proc_resolution)

  # Continents raster
  continent_raster <- load_mask("Continent", resolution = proc_resolution)
  # Climate raster (raster mask)
  raster_mask <- load_mask("Climate", resolution = proc_resolution)

  ## Load realm and continent rasters

  species_range_raster <- fasterize::fasterize(species_range, raster_mask)

  # Create temp file
  temp_file <- paste0(fs::file_temp(), ".tif")
  # Add check for file extension (oath_ext())
  saga_output_name <- fs::path_ext_set(output_name, "sdat")

  raster::writeRaster(species_range_raster, temp_file, options = "COMPRESS=LZW")
  temp_file_raster <- raster::raster(temp_file)

  saga_call <- paste0("saga_cmd grid_tools 26 -FEATURES:", "\'",
                      temp_file,"\'", " ", "-DISTANCE:",
                      "\'", fs::path_ext_set(saga_output_name, "sgrd"), "\'")
  system(saga_call)
  gdal_call <- paste0("gdal_translate", " ", saga_output_name,
                      " ", output_name, " ", "-co COMPRESS=LZW")
  system(gdal_call)

  dist_raster <- raster::raster(output_name)
  #### Mask out continent and realm - Absence points are created within
  #### the same continent and realm that the species occupies.
  # Mask out continent
  species_continent_mask <- raster::mask(continent_raster, temp_file_raster) #select continent code overlaping with range
  species_continents <- raster::unique(species_continent_mask)
  #codes of continents for species range
  # print("Calculating continents")

  conti_vals <- raster::getValues(continent_raster)

  conti_vals[conti_vals %notin% species_continents] <- NA
  conti_vals[conti_vals %in% species_continents] <- 1

  continent_raster2 <- raster::setValues(continent_raster, conti_vals)

  # #extract raster for pseudo-absences (realm in which the species is present)
  # print("Calculating realms")
  species_realms_mask <- raster::mask(realm_raster, temp_file_raster) #select realm code overlaping with range
  species_realms <- raster::unique(species_realms_mask)


  realm_vals <- raster::getValues(realm_raster)

  realm_vals[realm_vals %notin% species_realms] <- NA
  realm_vals[realm_vals %in% species_realms] <- 1

  realm_raster2 <- raster::setValues(realm_raster, realm_vals)


  # #combination realm-continent where species is present
  realmcont <- raster::mask(realm_raster2, continent_raster2)
  realmcont[realmcont == 1] <- 0
  # Resample for testing
  # print("Calculating PAs")
  PA_ras <- raster::mask(dist_raster, realmcont)

  #### Clean up residuals
  unlink(temp_file)
  unlink(fs::path_ext_set(output_name, "mgrd"))
  unlink(fs::path_ext_set(output_name, "prj"))
  unlink(fs::path_ext_set(output_name, "sdat"))
  unlink(fs::path_ext_set(output_name, "sdat.aux.xml"))
  unlink(fs::path_ext_set(output_name, "sgrd"))

  return(PA_ras)
}

#####
# Create absence points
# Creates spatially biased set of points based on a surface.
# In the case of amphibian project, surface represent distance from the current range.

# Species distance surface - (ouput of get_distance surface)
# species_range - Range of species processed. Input should be sf class polygon.
# return - Type of object to return. Value return = "df" returns data.frame (default),
# "sp" returns sp object, "sf" returns sf object.
# points_number - number of absence points to create.

#' Create absence points based on distance surface
#'
#' @param species_distance_surface Distance surface. Output of \code{get_distance_surface}.
#' @param species_range Species range. Should be of class \code{sf}.
#' @param return Type of object to return. One of \code{c("df", "sp", "sf")}. Default is "df".
#' @param points_number Number of absence points to sample.
#'
#' @return Depends on \code{return} argument.
#' @export
#'
#' @examples None.
#' @importFrom fasterize fasterize
#' @importFrom raster rasterToPoints
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sf st_as_sf


create_absence_points <- function(species_range, species_distance_surface, return = "df",
                                  # raster_mask,
                                  points_number = 10000)
{

species_range_raster <- fasterize::fasterize(species_range, species_distance_surface)

species_range_raster[species_range_raster == 1, ] <- 0
species_range_raster[is.na(species_range_raster), ] <- 1
# Create exponential range (+1 is to remove 0 values)
dist_raster_rev <- (1 / (species_distance_surface + 1))
# Remove species range from distance surface
species_distance_decay <- species_range_raster * dist_raster_rev


raster_points <- raster::rasterToPoints(species_distance_decay, spatial = FALSE, fun = function(x) {x > 0}) #(Potential) PseudoAbsences are created here

#Now to sample for Pseuooabsences Presence Data
species_absences <- raster_points[sample(seq(1:nrow(raster_points)), size = points_number,
                           replace = TRUE, prob = raster_points[, 3]), 1:2]

species_absences_pts <- sp::SpatialPointsDataFrame(species_absences, proj4string = raster::crs(species_distance_surface),
                                                   data = as.data.frame(rep(0, points_number)))

species_absences_coords <- as.data.frame(species_absences_pts@coords)
species_absences_coords$PA <- rep(0, nrow(species_absences_coords))
# Return as
if (return == "df")
{
  return(species_absences_coords)
}
if (return == "sp")
{
  return(species_absences_pts)
}
if (return == "sf")
{
  species_absences_pts_sf <- sf::st_as_sf(species_absences_pts)
  return(species_absences_pts_sf)
}
}

#' Load realm or continent mask
#'
#' @param type Type of data to load. One in c("Realm", "Continent", "Climate")
#' @param resolution Data resolution. One in c("10m", "5m", "2.5m", "30s")
#' @param path Folder path. Default is milkunize2("Projects/Amphibians/data_raw/Rasters")
#'
#' @return Raster*
#' @export
#'
#' @examples None
#' @importFrom raster raster
#' @importFrom Rahat milkunize2
#' @importFrom stringr str_subset
load_mask <- function(type, resolution, path = "Projects/Amphibians/data_raw/Rasters")
{
  stopifnot(type %in% c("Continent", "Realm", "Climate"),
            resolution %in% c("10m", "5m", "2.5m", "30s"))

  if (type == "Climate")
  {
    r <- "WorldClim-1.4/current/" %>%
      milkunize2("data") %>%
      paste0(resolution) %>%
      list.files(pattern = "tif$", full.names = TRUE) %>%
      head(1) %>%
      raster::raster()

    return(r)
  } else {
    r <- path %>%
      milkunize2() %>%
      list.files(recursive = TRUE, pattern = "tif$", full.names = TRUE) %>%
      str_subset(paste0(type, "_", resolution)) %>%
      raster()
    return(r)
  }
}


#' Get presence points
#'
#' Get presence points from species ranges. This function will take species range,
#' and retrieve processing resolution (resolution at which there are min 30 points),
#' and create presence points at given resolution. Maximum number of points is 1000.
#' If there are more than 1000 points, they will be sampled so there is max 1000.
#'
#' @param species_range
#' @param processing_resolution_data
#'
#' @return dataframe
#' @export
#'
#'
#' @examples
get_presence_points <- function(species_range, processing_resolution_data)
{
  # Get the name of species being processed
  sp_name <- species_range$binomial
  # Find what is the processing resolution
  res <- processing_resolution_data %>%
    dplyr::filter(species_name == sp_name) %>%
    dplyr::select(proc_resolution) %>%
    dplyr::pull()
  # Load climate mask
  raster_mask <- load_mask(type = "Climate", resolution = res)
  # Convert to raster
  range_sp <- fasterize::fasterize(species_range, raster_mask)
  # Reclassify
  range_vals <- raster::getValues(range_sp)
  range_vals[range_vals >= 1] <- 1
  range_sp <- raster::setValues(range_sp, range_vals)
  # Extract points
  occ_pts <- range_sp %>%
    raster::rasterToPoints() %>%
    sp::coordinates() %>%
    as.data.frame() %>%
    dplyr::rename(PA = layer)

  if (nrow(occ_pts) > 1000)
  {
    occ_pts <- occ_pts[sample(nrow(occ_pts), 1000, replace = FALSE), ]
  }

  return(occ_pts)

}
