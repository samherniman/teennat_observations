download_ppw_observations <- function(ppw_sf, project_name = "teennat-at-pepperwood-santa-rosa-ca") {
  # Function that downloads all inat observations in a project and cleans them
  
  # Find all observations in the project
  teennat <- get_inat_obs_project(
    project_name, 
    type = "info", 
    raw = FALSE
  )
  
  # Download observations 
  teennat_obs <- get_inat_obs_project(teennat$id, type = "observations")
  
  # Quickly filter out observations not near Pepperwood
  if(project_name == "teennat-at-pepperwood-santa-rosa-ca"){
  teennat_obs <- teennat_obs %>% 
    dplyr::filter(
      as.numeric(latitude) > 38.56,
      as.numeric(latitude) < 38.7,
      as.numeric(longitude) < -122.68,
      as.numeric(longitude) > -122.75
    )
  }
  
  # Transform observations to UTM coordinate system
  tn_sf <- st_as_sf(teennat_obs, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
    st_transform(crs = st_crs(ppw_sf))
  
  # Add the longitude and latitude cols back in
  latlong <- teennat_obs %>% 
    dplyr::select(latitude, longitude)
  tn_sf <- bind_cols(tn_sf, latlong)
  
  # Return all observations in the Pepperwood boundary
  st_intersection(tn_sf, st_buffer(ppw_sf, 0))
}
