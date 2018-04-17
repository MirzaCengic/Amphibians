#' Check processing resolution of species.
#'
#' Checks with which processing resolution the species has 30 or more points.
#'
#' @param input_species Species range of the species for which the processing resolution is calculated. Class sf.
#' @param raster_folder Folder that contains bioclim data. eg milkunize("Data_RAW/WorldClim/bioclim")
#' @param output_dir Directory in which to store the results. eg. milkunize("Projects/Amphibians/R/Output/Modeling_resolution/")
#'
#' @return A string with the processing resolution
#' @export
#'
#' @examples None.
#' @importFrom fasterize fasterize
#' @importFrom raster rasterToPoints raster
#' @importFrom fs dir_create
check_processing_resolution <- function(input_species, raster_folder, output_dir)
{

  stopifnot(exists(output_dir))

proc_resolution <- "10m"

raster_files <- list.files(raster_folder,
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
raster_mask <- raster::raster(raster_files[1])

species_range <- fasterize::fasterize(input_species, raster_mask)
species_range_points <- raster::rasterToPoints(species_range)

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "5m"

	raster_files <- list.files(raster_folder,
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster::raster(raster_files[1])


	species_range <- fasterize::fasterize(input_species, raster_mask)
    species_range_points <- raster::rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "2.5m"

	raster_files <- list.files(raster_folder,
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster::raster(raster_files[1])


	species_range <- fasterize::fasterize(input_species, raster_mask)
    species_range_points <- raster::rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30) {
    cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")

	proc_resolution <- "30s"

	raster_files <- list.files(raster_folder,
                           pattern = paste0(proc_resolution, ".*.tif"), full.names = TRUE, recursive = TRUE)
    raster_mask <- raster::raster(raster_files[1])


	species_range <- fasterize::fasterize(input_species, raster_mask)
    species_range_points <- raster::rasterToPoints(species_range)
	}

if (nrow(species_range_points) <= 30)
{
cat(paste0("Less than 30 points for resolution ", proc_resolution), "\n")
proc_resolution <- "Insufficient"
}


outdir <- paste0(output_dir, proc_resolution)

fs::dir_create(outdir)


outfile <- paste0(outdir, "/", unique(input_species$binomial))

if (!dir.exists(outfile))
{
file.create(outfile)
  message("Output file created")
}


return(proc_resolution)
}
