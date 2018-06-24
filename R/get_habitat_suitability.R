##%######################################################%##
#                                                          #
####              Function to get suitable              ####
####         and unsuitable data for amphibians         ####
#                                                          #
##%######################################################%##

# TODO:
# - implement additional lc data (using conversion matrices)
# - harmonize argument names and output column names
# - read in species_name argument from passed arguments (species_habitat or conversion_matrix)
# - make sure that species_habitat and conversion_matrix arguments talk to each other
# - add input checks and argument checks



#' Get habitat suitability
#'
#' Pass IUCN habitat classification preferences, and habitat classification matrix, and the function
#' will return a dataframe that contains information whether the specific habitat is suitable or unsuitable.
#' Currently the function is intended for use in the amphibians project, and some options that a general user
#' might want to change are hardcoded.
#'
#' Use \code{rredlist::rl_habitats()} function to retrieve species habitat preference data. Currently only
#' IUCN -> ESA CCI land cover conversion matrix is supported.
#'
#' @param species_habitat Habitat classification data. Output of \code{rredlist::rl_habitats()} function
#' @param conversion_matrix Conversion matrix between the habitat categories. Should contain column named "IUCN_code".
#' @param species_name Species name for which the suitability is derived.
#' @param col_name Name of the land cover column to reclassify.
#' 
#' @return dataframe
#'
#' @importFrom tidyr separate
#' @importFrom dplyr left_join transmute case_when
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#'
#' @examples None.
#'
#' library(rredlist)
#' library(Rahat)
#' library(tidyverse)
#'
#' habitat_conversion_matrix <- "Projects/Amphibians/Documents/Notes/Habitat_suitability/Habitats_conversion.csv" %>%
#' milkunize2() %>%
#' read_csv() %>%
#' mutate(IUCN_code = as.character(IUCN))
#'
#' my_sp <- "Salamandra salamandra"
#' # Get IUCN species habitat data preferences
#' # Originally stored in individual csv's, here everything is stored in one rds file
#' species_habitat_data <- "Projects/Amphibians/data/IUCN_habitat_data/Species_habitat_data.rds" %>%
#'   milkunize2() %>%
#' read_rds()
#'
#' sp_data <- species_habitat_data %>%
#' filter(Species == my_sp)
#'
#' habitat_data <- rl_habitats(my_sp)$results
#' get_habitat_suitability(species_habitat = sp_data,
#' conversion_matrix = habitat_conversion_matrix, species_name = my_sp)

get_habitat_suitability <- function(species_habitat, conversion_matrix, col_name, species_name)
{
  if (missing(col_name))
  {
    stop("Argument col_name is missing. Set name of land cover column.")
  }
  species_habitat <- species_habitat %>%
    separate(col = code, sep = "[.]", into = c("IUCN_code", "IUCN_code_subcategory"))

    species_habitat_suitability <- conversion_matrix %>%
    left_join(species_habitat, by = "IUCN_code") %>%
    transmute(
      Species = str_replace(species_name, " ", "_"),
      Habitat = habitat,
      IUCN_code,
      lc = col_name,
      Suitability = case_when(
        suitability == "Marginal" ~ "Unsuitable",
        is.na(suitability) ~ "Unsuitable",
        TRUE ~ as.character(suitability)),
      Suit_binary = case_when(
        Suitability == "Suitable" ~ 1,
        Suitability == "Unsuitable" ~ 0),
      Season = season,
      Major_importance = majorimportance)

  return(species_habitat_suitability)
}


# Reclassify land cover raster


#' Reclassify suitable/unsuitable land cover
#'
#' Function takes in land cover data (right now only ESA CCI LC works),
#' and habitat suitability/classification data retrieved from IUCN,
#' and returns reclassified raster where value 1 will be "suitable" habitat,
#' while value 0 is "unsuitable" habitat (as defined by IUCN habitat classification).
#'
#' @param lc_raster Raster to reclassify. Right now it should be ESA CCI land cover raster.
#' @param habitat_suitability Habitat classification data. Output of \code{get_habitat_suitability()} function.
#'
#' @return Raster*
#' @export
#'
#' @examples None
#'
#' @importFrom raster getValues setValues
#' @importFrom dplyr filter select pull
reclassify_lc <- function(lc_raster, habitat_suitability)
{
  lc_vals <- raster::getValues(lc_raster)

  suitable <- habitat_suitability %>%
    dplyr::filter(Suit_binary == 1) %>%
    dplyr::select(lc) %>%
    dplyr::pull()

  lc_vals <- ifelse(lc_vals %in% suitable, 1, 0)
  lc_rcl <- raster::setValues(lc_raster, lc_vals)

  return(lc_rcl)
}
