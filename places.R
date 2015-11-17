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

new.places <- places.in.tsv[! name %in% names(places.list),]
for(new.i in seq_along(new.places$name)){
  place <- new.places[new.i,]
  geocode.result <- geocode(place$address, output = "more")
  places.list[[place$name]] <- geocode.result
}

places <- do.call(rbind, places.list)

save(places, places.list, file="places.RData")
