works_with_R("3.2.2",
             data.table="1.9.6")

org.file.vec <- Sys.glob("places/*.org")
RData.file.vec <- sub("org$", "RData", org.file.vec)

mtime.dt <- data.table(
  org.file=org.file.vec,
  RData.file=RData.file.vec,
  org.mtime=file.mtime(org.file.vec),
  RData.mtime=file.mtime(RData.file.vec))
to.process <- mtime.dt[is.na(RData.mtime) | RData.mtime < org.mtime, ]

for(org.file in to.process$org.file){
  cmd <- paste(
    R.home(file.path("bin", "Rscript")),
    "place2RData.R",
    org.file)
  system(cmd)
}

org.places <- list()
for(file.i in 1:nrow(mtime.dt)){
  mtime.row <- mtime.dt[file.i, ]
  objs <- load(mtime.row$RData.file)
  if(length(data.by.section)){
    org.file <- paste(mtime.row$org.file)
    org.places[[org.file]] <- data.by.section
  }
}
table(unlist(sapply(org.places, names))) 
table(unlist(lapply(org.places, function(L)names(L$prices$table))))
table(unlist(lapply(org.places, function(L)L$prices$table$product)))
table(unlist(lapply(org.places, function(L)L$prices$table$product.file)))

save(org.places, file="org.places.RData")
