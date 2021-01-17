download_ppw_observations <- function(ppw_sf, project_name = "teennat-at-pepperwood-santa-rosa-ca") {
  
  teennat <- get_inat_obs_project(
    project_name, 
    type = "info", 
    raw = FALSE
  )
  teennat_obs <- get_inat_obs_project(teennat$id, type = "observations")
  
  if(project_name == "teennat-at-pepperwood-santa-rosa-ca"){
  teennat_obs <- teennat_obs %>% 
    dplyr::filter(
      as.numeric(latitude) > 38.56,
      as.numeric(latitude) < 38.7,
      as.numeric(longitude) < -122.68,
      as.numeric(longitude) > -122.75
    )
  }
  
  tn_sf <- st_as_sf(teennat_obs, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
    st_transform(crs = st_crs(ppw_sf))
  
  latlong <- teennat_obs %>% 
    dplyr::select(latitude, longitude)
  
  tn_sf <- bind_cols(tn_sf, latlong)
  
  
  st_intersection(tn_sf, st_buffer(ppw_sf, 0))
}
