#0_functions

# setup -------------------------------------------------------------------

library(magrittr)


# shp_extract_load --------------------------------------------------------

shp_extract_read <- function(path, shape, crs, opt.st) {
  if(missing(opt.st)){
    temp <- tempfile()
    unzip(path, exdir = temp, junkpaths = TRUE)
    listfiles <- list.files(temp, pattern = shape, full.names = TRUE)
    shp <- st_read(dsn = listfiles, crs = crs)
    unlink(temp, recursive = TRUE)
    return(shp)
  } else{
    temp <- tempfile()
    unzip(path, exdir = temp, junkpaths = TRUE)
    listfiles <- list.files(temp, pattern = shape, full.names = TRUE)
    shp <- st_read(dsn = listfiles, crs = crs, options = opt.st)
    unlink(temp, recursive = TRUE)
    return(shp)
  }
}

#test
#foo <- shp_extract_read("data/shp/distritos-sp-master.zip", "Distritos_Policiais_SHP.shp", opt.st = "ENCODING=WINDOWS-1252")



# dist_nearest ------------------------------------------------------------

dist_nearest <- function(data1, data2, varname, unit) {
  closest <- sf::st_nearest_feature(data1, data2) %>% 
    tibble::tibble(id = .) %>% 
    dplyr::left_join(tibble::rowid_to_column(data2, "id")) %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(crs = sf::st_crs(data2))
  
  data1 %>%
    dplyr::mutate(!!as.name(varname) := st_distance(closest, data1, by_element = TRUE)) %>% 
    units::drop_units() %>% 
    return()
}

#test
#df <- distance_to_nearest(df, shp_estacoes_cptm, "dist_cptm")



# count_features ------------------------------------------------------------

## buffer
count_features <- function(data1, data2, buffer, varname) {
  if(missing(buffer)){
    st_intersects(data1, data2) %>% 
      lengths() %>% 
      tibble::tibble() %>% 
      rename(!!as.name(varname) := .) %>% 
      cbind(data1) %>% 
      st_as_sf() %>% 
      return()
  } else {
    sf::st_buffer(data1, buffer) %>% 
      st_intersects(data2) %>% 
      lengths() %>% 
      tibble::tibble() %>% 
      rename(!!as.name(varname) := .) %>% 
      cbind(data1) %>% 
      st_as_sf() %>% 
      return() 
  }
}



#foo3 <- shp_distritos %>% 
#  st_join(shp_educ, join = st_contains) %>% 
#  rename_at(
#    vars(ends_with(".x")),
#    ~str_replace(., "\\..$","")
#  ) %>% 
#  select_at(
#    vars(-ends_with(".y"))
#  ) %>% 
#  arrange(eq_id) %>% 
#  count(cd_subp, eq_classe)



# palma_ratio ------------------------------------------------------------

## credits for palma ratio's base formula:
## https://ipeagit.github.io/aopdata/articles/access_inequality.html 

palma_ratio <- function(data, population, income, access) {
  # average access of the wealthiest 10%
  avg_access_10p_wealthiest <- data %>% 
    filter(!!enquo(income) == 10) %>% 
    summarise(
      d10 = weighted.mean(x = !!enquo(access), w = !!enquo(population), na.rm=T) 
    ) %>% 
    st_drop_geometry()
  
  # average access of the poorest 40%
  avg_access_40p_poorest <- data %>% 
    filter(!!enquo(income) <= 4) %>% 
    summarise(
      d4 = weighted.mean(x = !!enquo(access), w = !!enquo(population), na.rm=T) 
    ) %>% 
    st_drop_geometry()
  
  # Palma ratio
  palma_ratio <- avg_access_10p_wealthiest / avg_access_40p_poorest                
  return(palma_ratio)   
}