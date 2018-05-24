##%######################################################%##
#                                                          #
####             Check processing resolution              ####
#                                                          #
##%######################################################%##


# This script contains helper functions that assist with obtaining the proper
# processing resolution for model fitting of amphibian species

# It contains the following function:
# - get_cell_number()
# - get_processing_resolution()

#########################################

#' Get cell number
#' Function takes in species range and a raster with
#' specified resolution, and returns the number of cells
#' that exist within the species range
#'
#' @param input_species sp class species spatial data.
#' @param resolution Resolution to check. One in c("10m", "5m", "2.5m", "30s").
#'
#' @return Integer
#' @export
#'
#' @examples None
#'
get_cell_number <- function(input_species, resolution, dir_path = "WorldClim-1.4/current")
{

  cells_no <- dir_path %>%
    milkunize2("data") %>%
    list.files(pattern = paste0("_", resolution, ".*.tif$"),
               recursive = TRUE, full.names = TRUE) %>%
    head(1) %>%
    raster() %>%
    raster::extract(input_species, df = TRUE) %>%
    nrow()
  return(cells_no)
}



#' Get processing resolution for species data
#'
#' This function will test the get_cell_number function against rasters of 4 different
#' spatial resolutions. Checks with which processing resolution the species has 30 or more points.
#'
#' @param species Species range of the species for which the processing resolution is calculated. Class sp.
#'
#' @return String.
#' @export
#'
#' @examples None.
get_processing_resolution <- function(species)
{
  my_res <- get_cell_number(species, resolution = "10m")
  if (my_res > 30)
  {
    processing_resolution <- "10m"
    print("10 minutes resolution")
    return(processing_resolution)
    # break()
  } else {
    # 5 minute resolution
    my_res <- get_cell_number(species, resolution = "5m")
    # print("Downscaling to 5m")
  }
  if (my_res > 30)
  {
    processing_resolution <- "5m"
    print("5 minutes resolution")
    return(processing_resolution)
    break()
  } else {
    # 2.5 minute resolution
    my_res <- get_cell_number(species, resolution = "2.5m")
    # print("Downscaling to 2.5m")
  }
  if (my_res > 30)
  {
    processing_resolution <- "2.5m"
    print("2.5 minutes resolution")
    return(processing_resolution)
    break()
  } else {
    # 30 seconds resolution
    my_res <- get_cell_number(species, resolution = "30s")
    # print("Downscaling to 30s")
  }
  if (my_res > 30)
  {
    processing_resolution <- "30s"
    print("30 second resolution")
  } else {
    processing_resolution <- "00"
    print("Can't be done")
  }
  return(processing_resolution)
}
