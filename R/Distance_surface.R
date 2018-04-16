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
#' @param raster_mask Raster mask that provides information for rasterizing the species range.
#' @param output_name Output name of created SAGA grid.
#'
#' @return Raster
#' @export
#'
#' @examples None
#' @importFrom raster raster writeRaster mask getValues setValues
#' @importFrom Rahat milkunize
#' @importFrom fasterize fasterize
#' @importFrom fs file_temp path_ext_set file_delete
get_distance_surface <- function(species_range, raster_mask, output_name)
{
  ## Load realm and continent rasters
  # TODO - Add resolution check for species

  if (!file.exists(output_name))
  {
      realm_raster <- raster::raster(paste0(Rahat::milkunize("Amphibians_project_folder/Data/Raster/Realm_", "m5"),
                                "10m", ".tif"))

  # Continents rasters
  continent_raster <- raster::raster(paste0(Rahat::milkunize("Amphibians_project_folder/Data/Raster/Continent_", "m5"),
                                    "10m", ".tif"))

  species_range_raster <- fasterize::fasterize(species_range, raster_mask)
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

  }

  dist_raster <- raster::raster(output_name)
  #### Mask out continent and realm - Absence points are created within
  #### the same continent and realm that the species occupies.
  # Mask out continent
  species_continent_mask <- raster::mask(continent_raster, temp_file_raster) #select continent code overlaping with range
  mask_continent <- unique(species_continent_mask)
  #codes of continents for species range
  # print("Calculating continents")
  continent_raster2 <- Which(continent_raster %in% mask_continent)
  conti_vals <- raster::getValues(continent_raster2)
  conti_vals[conti_vals == 0] <- NA
  continent_raster2 <- raster::setValues(continent_raster2, conti_vals)
  # #extract raster for pseudo-absences (realm in which the species is present)
  # print("Calculating realms")
  mask2 <- raster::mask(realm_raster, temp_file_raster) #select realm code overlaping with range
  mask2_v <- unique(mask2)
  realm2 <- Which(realm_raster %in% mask2_v)

  realm2_vals <- raster::getValues(realm2)
  realm2_vals[realm2_vals == 0] <- NA
  realm2 <- raster::setValues(realm2, realm2_vals)
  # #combination realm-continent where species is present
  realmcont <- raster::mask(realm2, continent_raster2)
  realmcont[realmcont == 1] <- 0
  # Resample for testing
  # print("Calculating PAs")
  PA_ras <- raster::mask(dist_raster, realmcont)



  #### Clean up residuals
  fs::file_delete(temp_file)
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
#' @examples
#' @importFrom fasterize fasterize
#' @importFrom raster rasterToPoints
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sf st_as_sf


create_absence_points <- function(species_distance_surface, species_range, return = "df",
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


raster_points <- raster::rasterToPoints(species_distance_decay, spatial = F, fun=function(x){x>0}) #(Potential) PseudoAbsences are created here

#Now to sample for Pseuooabsences Presence Data
species_absences <- raster_points[sample(seq(1:nrow(raster_points)), size = points_number,
                           replace = TRUE, prob = raster_points[, 3]), 1:2]
species_absences_pts <- sp::SpatialPointsDataFrame(species_absences, proj4string = raster::crs(species_distance_surface), data = as.data.frame(rep(0, points_number)))

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