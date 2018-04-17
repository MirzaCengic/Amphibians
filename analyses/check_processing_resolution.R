#' Check processing resolution of species.
#'
#' Checks with which processing resolution the species has 30 or more points.
#'
#' @param in_species Shapefile of the species being checked for.
#'
#' @return A string with the processing resolution
#' @export
#'
#' @examples
#'
check_processing_resolution <- function(input_species)
{
proc_resolution <- "10m"

raster_files <- list.files(paste0(.basepath_milkun1, "Data_RAW/WorldClim/bioclim"),
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
raster_mask <- raster(raster_files[1])

species_range <- fasterize(input_species, raster_mask)
species_range_points <- rasterToPoints(species_range)

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "5m"

	raster_files <- list.files(paste0(.basepath_milkun1, "Data_RAW/WorldClim/bioclim"),
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster(raster_files[1])


	species_range <- fasterize(input_species, raster_mask)
    species_range_points <- rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "2.5m"

	raster_files <- list.files(paste0(.basepath_milkun1, "Data_RAW/WorldClim/bioclim"),
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster(raster_files[1])


	species_range <- fasterize(input_species, raster_mask)
    species_range_points <- rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "30s"

	raster_files <- list.files(paste0(.basepath_milkun1, "Data_RAW/WorldClim/bioclim"),
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster(raster_files[1])


	species_range <- fasterize(input_species, raster_mask)
    species_range_points <- rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30)
{
cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")
proc_resolution <- "Insufficient"
}


outdir <- paste0(milkunize("Projects/Amphibians/R/Output/Modeling_resolution/"), proc_resolution)

if (!dir.exists(outdir))
{
  dir.create(outdir)
  message("Output directory created")
}

outfile <- paste0(outdir, "/", unique(input_species$binomial))

if (!dir.exists(outfile))
{
file.create(outfile)
  message("Output file created")
}


return(proc_resolution)
}
