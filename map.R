works_with_R("3.2.2",
             data.table="1.9.6",
             leaflet="1.0.0")

load("places.RData")
places.dt <- data.table(places)
prices <- fread("products/hemp_seeds.csv")

## https://rstudio.github.io/leaflet/shapes.html

store.manu <- unique(prices[!grepl("web", store),.(store, manufacturer)])
setkey(places.dt, place.name)

price.place.vec <- unique(store.manu[, c(store, manufacturer)])
price.place.dt <- places.dt[price.place.vec]
stopifnot(is.finite(price.place.dt$lon))

m <- leaflet() %>%
  addTiles() %>%
  addLayersControl(
    overlayGroups=unique(places.dt$place.type),
    options = layersControlOptions(collapsed = FALSE)
    )
setkey(places.dt, place.type)
color.code <- c(
  manufacture="#1B9E77",
  consumer="#D95F02",
  store="#7570B3")
places.dt[, popup := sprintf("%s (%s)", place.name, place.type)]
for(place.type in unique(places.dt$place.type)){
  one.place <- places.dt[place.type]
  m <- addCircleMarkers(
    m, data=one.place,
    color=color.code[[place.type]],
    group=place.type,
    popup=one.place$popup)
}

setkey(places.dt, place.name)
for(path.i in 1:nrow(store.manu)){
  one.path.row <- store.manu[path.i,]
  one.path <- unique(c("Chez nous", unlist(one.path.row)))
  path.dt <- places.dt[one.path]
  m <-
    path.dt[, {
    addPolylines(m, lon, lat)
    }]
}

## TODO:
## https://rstudio.github.io/leaflet/markers.html#customizing-marker-icons
## for question marks where we don't know.
