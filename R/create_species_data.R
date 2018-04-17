#' Create species data for amphibians
#'
#' Create species data (presences and absences) based on processing resolution.
#'
#' @param processing_species Species name.
#' @param species_resolution_list File with species resolution.
#' @param amphibians_shp Shapefile with all of amphibians data.
#'
#' @return
#' @export
#'
#' @examples
create_species_data <- function(processing_species, species_resolution_list, amphibians_shp)
{
  species_row_index <- which(species_resolution_list %>% str_split("/", simplify = TRUE) %>% .[, 2] %in% processing_species)
  processing_resolution <- species_resolution_list %>%  str_split("/", simplify = TRUE) %>% .[species_row_index, 1]
  if (processing_resolution == "00")
  {warning("Resolution insufficient to model species.");next()}
  outfolder <- paste0(milkunize("Amphibians_project_folder/Data/Species_data/PAs/", "m5"), processing_resolution)
  dir.create(outfolder)
  outfile <- paste0(outfolder, "/", processing_species, "_", processing_resolution, ".csv")
  realm_raster <- raster(paste0(milkunize("Amphibians_project_folder/Data/Raster/Realm_", "m5"), processing_resolution, ".tif"))
  continent_raster <- raster(paste0(milkunize("Amphibians_project_folder/Data/Raster/Continent_", "m5"), processing_resolution, ".tif"))
  amphibians_shp$binomial <- gsub(" ", "_", amphibians_shp$binomial)
  # Get species range
  input_species_range <- amphibians_shp %>% filter(binomial == processing_species)

  range_sp <- fasterize(input_species_range, continent_raster)

  range_vals <- getValues(range_sp)
  range_vals[range_vals >= 1] <- 1
  range_sp <- setValues(range_sp, range_vals)
  # Extract points
  range_pts <- rasterToPoints(range_sp)
  occ_pts <- as.data.frame(coordinates(range_pts))
  names(occ_pts)[3] <- "PA"

  if (nrow(occ_pts > 1000))
  { occ_pts <- occ_pts[sample(nrow(occ_pts), 1000, replace = FALSE), ]}
  #### Create pseudoabsences ####
  sp_mask <- mask(continent_raster, range_sp) #select continent code overlaping with range
  mask_v <- unique(sp_mask)
  #codes of continents for species range
  continent_raster2 <- Which(continent_raster %in% mask_v)
  conti_vals <- getValues(continent_raster2)
  conti_vals[conti_vals == 0] <- NA
  continent_raster2 <- setValues(continent_raster2, conti_vals)
  # #extract raster for pseudo-absences (realm in which the species is present)
  mask2 <- mask(realm_raster, range_sp) #select realm code overlaping with range
  mask2_v <- unique(mask2)
  realm2 <- Which(realm_raster %in% mask2_v)
    realm2_vals <- getValues(realm2)
  realm2_vals[realm2_vals == 0] <- NA
  realm2 <- setValues(realm2, realm2_vals)
  # #combination realm-continent where species is present
  realmcont <- mask(realm2, continent_raster2)
  realmcont[realmcont == 1] <- 0
  # Resample for testing
  PA_ras <- merge(range_sp, realmcont) #raster with 1 for cells outside range and 0 for within range
  #
  PA_vals <- getValues(PA_ras)
  PA_vals[PA_vals == 1] <- NA
  PA_vals[PA_vals == 0] <- 1
  PA_ras <- setValues(PA_ras, PA_vals)
  # Points might fall outside of the land area
  pas <- sampleRandom(PA_ras, 1000, na.rm = TRUE, sp = TRUE)
  pas <- as.data.frame(coordinates(pas))
  pas$PA <- rep(0, nrow(pas))
  sp_presabs <- rbind(occ_pts, pas)
  return(sp_presabs)
}
