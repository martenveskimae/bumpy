#################################
## DATA MINING - MTAT.03.183
## 5. JAN 2018
## MÄRTEN VESKIMÄE
#################################

library(shiny)
library(tidyverse)
library(osmar)
library(maptools)
library(rgeos)
library(mapview)
library(leaflet)
library(igraph)
Sys.setlocale("LC_ALL", "UTF-8")

# From osmar package
merge_ways_nodes <- function(ways, nodes) {
  colnames(ways) <- sprintf("w%s", colnames(ways))
  colnames(nodes) <- sprintf("n%s", colnames(nodes))
  m <- match(ways$wref, nodes$nid)
  dat <- cbind(ways, nodes[m, ])
  dat <- dat[!is.na(dat$nlat), ]
  dat$nid <- NULL
  colnames(dat) <- substring(colnames(dat), 2)
  dat
}

# From osmar package. Modified for road quality weights
as_igraph_mod = function(obj,weights = F){
  dat <- merge_ways_nodes(obj$ways[[3]], obj$nodes[[1]])
  dat <- split(dat, dat$id)
  dat <- dat[sapply(dat, nrow) >= 2]
  edges <- lapply(dat, function(x) {
    n <- nrow(x)
    from <- 1:(n - 1)
    to <- 2:n
    weights <- distHaversine(x[from, c("lon", "lat")], x[to,c("lon","lat")])
    cbind(from_node_id = x[from, "ref"],
          to_node_id = x[to,"ref"],
          way_id = x[1, "id"],
          weights = weights)
  })
  edges <- do.call(rbind, edges)
  
  if(weights==T){
    edges = edges %>%
      as.data.frame() %>%
      left_join(tln_nodes,"from_node_id") %>%
      mutate(w = ifelse(is.na(w),1,w),
             weights = weights * w)
  }

  weights <- edges[, "weights"]
  names <- edges[, "way_id"]
  edges <- cbind(as.character(edges[, "from_node_id"]), as.character(edges[,"to_node_id"]))
  graph <- graph.edgelist(edges)
  E(graph)$weight <- weights
  E(graph)$name <- names
  graph
}

load("tln_roads.Rda")
load("tln_lines.Rda")
load("tln_nodes.Rda")
tln_lines$w = round(log(tln_lines$w+1),2)

ui = bootstrapPage(
  title = "Bumpy",
  HTML("<style>
       html, body{
           width:100%;
           height:100%
       }
       .menuContainer{
           background-color:rgba(245,245,245,0.9);
           text-align:left;
           vertical-align: middle;
           padding:10px;
           margin:10px;
           border-radius: 5px;
           border: 1px solid rgba(245,245,245,1);
       }
       </style>"),
  leafletOutput("streetsOfTallinn", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                class = "menuContainer",
                width = 330, draggable = T,
                h4("Pavement quality measured by Starship delivery robots"),
                HTML("<a href='https://github.com/martenveskimae' target='_blank'>Märten Veskimäe</a><br>
                     <a href='https://github.com/Kennu76' target='_blank'>Kevin Kanarbik</a><br>
                     <a href='https://github.com/TKasekamp' target='_blank'>Tõnis Kasekamp</a>"),
                hr(),
                textInput("from",NULL,"24.61358-59.44556",placeholder="lon-lat"),
                textInput("to",NULL,"24.66345-59.42753",placeholder="lon-lat"),
                checkboxInput("quality", c("Pavement quality"),T),
                actionButton("submit","Route!",icon("gears")),
                hr(),
                HTML("About the project: <a href='https://github.com/martenveskimae/bumpy' target='_blank'>GitHub</a>")
  )
)

server = function(input,output,session){
  route_data = eventReactive(input$submit,{
    from = as.numeric(strsplit(input$from,"-")[[1]])
    to = as.numeric(strsplit(input$to,"-")[[1]])
    hway_start_node = local({
      id = find(tln_roads, node(attrs(lon > (from[1]-0.0025) & lon < (from[1]+0.0025) &
                                      lat > (from[2]-0.0025) & lat < (from[2]+0.0025))))
      id_table = tln_roads$nodes$attrs[tln_roads$nodes$attrs$id %in% id,c("id","lon","lat")]
      order = ((from[1] - id_table$lon)^2) + ((from[2] - id_table$lat)^2)
      id[order(order)][1]
    })
    hway_end_node = local({
      id = find(tln_roads, node(attrs(lon > (to[1]-0.002) & lon < (to[1]+0.002) &
                                      lat > (to[2]-0.002) & lat < (to[2]+0.002))))
      id_table = tln_roads$nodes$attrs[tln_roads$nodes$attrs$id %in% id,c("id","lon","lat")]
      order = ((to[1] - id_table$lon)^2) + ((to[2] - id_table$lat)^2)
      id = id[order(order)][1]
    })
    hway_start = subset(tln_roads, node(hway_start_node))
    hway_end = subset(tln_roads, node(hway_end_node))
    
    gr_tln = as_igraph_mod(tln_roads,input$quality)
    
    route = shortest_paths(gr_tln,
                           from = as.character(hway_start_node),
                           to = as.character(hway_end_node),
                           mode="all")[[1]]
    route_nodes = as.numeric(V(gr_tln)[route[[1]]]$name)
    route_ids = find_up(tln_roads, node(route_nodes))
    route_tln = subset(tln_roads, ids = route_ids)
    return(route_tln)
  })
  output$streetsOfTallinn = renderLeaflet({
    mapview(tln_lines,trim=T,cex=NULL,popup=NULL,
            maxpixels = 1000,
            zcol = "w")@map %>%
      addTiles(options=tileOptions(minZoom=13, maxZoom=16)) %>%
      setView(24.64714,59.42753,13) %>%
      addLegend("bottomleft",
                colors = rev(mapviewPalette(name = "mapviewVectorColors")(6)),
                values = ~get("w"),
                labels = 5:0,
                title = "Vibration level (%)<br> log transformed",
                opacity = 1)
  })
  
  observe({
      leafletProxy("streetsOfTallinn") %>%
        addPolylines(data = as_sp(route_data(),"lines"))
  })
}

shinyApp(ui = ui, server = server)