#################################
## DATA MINING - MTAT.03.183
## 5. JAN 2018
## MÄRTEN VESKIMÄE
#################################

library(tidyverse)
library(osmar)

#################################
## Spatial extent of
## OSM road data
#################################

# tln_box = center_bbox(center_lon=24.7536,center_lat=59.4370,width=32000,height=16000)
tln_box = center_bbox(center_lon=24.64714,center_lat=59.42753,width=12000,height=10000)
tln = get_osm(tln_box, source = osmsource_osmosis("estonia-latest.osm.bz2"))

tln_road = find(tln, way(tags(k%in%c("highway"))))
tln_roads = subset(tln, ids=find_down(tln, way(tln_road)))
save(tln_roads,file="tln_roads.Rda")

tln_lines = as_sp(tln_roads, "lines")

#################################
## Joining roads with
## starship data
#################################

# Random data for testing
# sample_data = data.frame(lon = seq(24.47165,25.03556,0.003))
# sample_data$lat = seq(59.36519,59.50880,length.out=nrow(sample_data))
# sample_data = expand.grid(sample_data)
# sample_data$quality = rnorm(nrow(sample_data),5)

load("road_quality.Rda")
load("tln_roads.Rda")

add_weights = function(lon_ss,lat_ss,weight,ways=F){
  id = find(tln_roads, node(attrs(lon > (lon_ss-0.001) & lon < (lon_ss+0.001) &
                                  lat > (lat_ss-0.001) & lat < (lat_ss+0.001))))
  id_table = tln_roads$nodes$attrs[tln_roads$nodes$attrs$id %in% id,c("id","lon","lat")]
  order = ((lon_ss - id_table$lon)^2) + ((lat_ss - id_table$lat)^2)
  id = id[order(order)][1]
  if(ways==T){
    id = find_up(tln_roads, node(id))
    # if(length(id$way_ids)==1){
      obj = subset(tln_roads, ids = find_down(tln_roads, way(id$way_ids)))
      if(length(obj$ways$attrs$id) > 0) return(data.frame(id = obj$ways$attrs$id,w = weight))
    # }
  } else return(data.frame(from_node_id = id,w = weight))
}

# Node weights
tln_nodes = lapply(1:nrow(road_quality), function(x){
  add_weights(road_quality$lon[x],
              road_quality$lat[x],
              road_quality$quality[x])
  }) %>% do.call(rbind.data.frame,.) %>%
  group_by(from_node_id) %>%
  summarise(w = mean(w))

# Way weights
ways_weights = lapply(1:nrow(road_quality), function(x){
  add_weights(road_quality$lon[x],
              road_quality$lat[x],
              road_quality$quality[x],T)
  }) %>% do.call(rbind.data.frame,.) %>%
  group_by(id) %>%
  summarise(w = round(mean(w),2))

## Adding weight data for map application
load("tln_lines.Rda")
tln_lines@data$timestamp = NULL
tln_lines@data$w = NULL
tln_lines@data = tln_lines@data %>%
  left_join(ways_weights, "id")

save(tln_lines,file="tln_lines.Rda")
save(tln_nodes,file="tln_nodes.Rda")
