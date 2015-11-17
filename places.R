works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/ggplot2@a8b06ddb680acdcdbd927773b1011c562134e4d2",
             ggmap="2.5.2")

## Cache geocodes for places.tsv in places.RData.

if(file.exists("places.RData")){
  load("places.RData")
}else{
  places.list <- list()
}

places.in.tsv <- fread("places.tsv")

## geocoding http://zevross.com/blog/2014/03/19/geocoding-with-rs-ggmap-package/

new.places <- places.in.tsv[! name %in% names(places.list),]
for(new.i in seq_along(new.places$name)){
  place <- new.places[new.i,]
  geocode.result <- geocode(place$address, source="google", output="more")
  places.list[[place$name]] <- geocode.result
}

some.cols.list <- list()
setkey(places.in.tsv, name)
for(place.name in names(places.list)){
  geocode.result <- places.list[[place.name]]
  place <- places.in.tsv[place.name, ]
  wide <- data.frame(geocode.result, place)
  some.cols <- wide[, c("lon", "lat", "place.type", "type", "loctype")]
  some.cols.list[[place.name]] <- data.frame(place.name, some.cols)
}
places <- do.call(rbind, some.cols.list)

save(places, places.list, file="places.RData")
