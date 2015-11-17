works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             ggmap="2.5.2",
             leaflet="1.0.0")

load("places.RData")
places.dt <- data.table(places)
prices <- fread("products/hemp_seeds.csv")

## https://rstudio.github.io/leaflet/shapes.html

store.manu <- unique(prices[!grepl("web", store),.(store, manufacturer)])
places.dt[, place.type := NA_character_]
setkey(places.dt, place.name)

price.place.vec <- unique(store.manu[, c(store, manufacturer)])
price.place.dt <- places.dt[price.place.vec]
stopifnot(is.finite(price.place.dt$lon))

for(ptype in c("store", "manufacturer")){
  place.vec <- store.manu[[ptype]]
  places.dt[place.vec, place.type := ptype]
}
  

addPolylines
