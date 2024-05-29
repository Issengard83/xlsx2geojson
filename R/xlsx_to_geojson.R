#' This function reads an Excel file, cleans the column names,
#' converts a specified GeoJSON geometry column to an sf object, and returns it.
#'
#' @param data A string specifying the path to the Excel file.
#' @return An sf object containing the transformed spatial data.
#' @importFrom rio import
#' @importFrom dplyr select mutate rename_with starts_with
#' @importFrom janitor clean_names
#' @importFrom  sf st_as_sfc st_as_sf
#' @export

excel_to_geojson <- function(data){
  # Read excel file
  df <- rio::import(data)

  # Clean and transform the data
  sf_df <- df |>
    # clean column names
    dplyr::rename_with(~ make.unique(.)) |>
    janitor::clean_names() |>
    dplyr::rename_with(.cols = dplyr::contains("geom"), .fn = ~ "geometry") |>
    # transform to spatial object
    dplyr::mutate(geometry = sf::st_as_sfc(geometry, GeoJSON = TRUE)) |>
    sf::st_as_sf(sf_column_name = "geometry")

  return(sf_df)
}
