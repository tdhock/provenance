works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264",
             RJSONIO="1.3.0",
             orgR="0.9.0")

## Parse .org myself using regular expressions?
lines.vec <- readLines("products/Hemp_seeds.org")
place.org.vec <- Sys.glob("places/*.org")
place.vec <- sapply(place.org.vec, function(place.org){
  paste(readLines(place.org), collapse="\n")
})

table.pattern <- paste0(
  "[*]+ +",
  "(?<section>.*?)",
  "\n",
  "(?:.*?\n)+?",
  "(?<table>",
  "(?:",
  "[|].*?\n",
  ")+",
  ")"
)
list.of.tables <- str_match_all_named(place.vec, table.pattern)

description.pattern <- paste0(
  "-",
  " +",
  "(?<name>.*?)",
  " +:: +",
  "(?<value>.*?)",
  "\n")
list.of.described <- str_match_all_named(place.vec, description.pattern)

str2dt <- function(table.str){
  no.dividers <- gsub("[|]-.*?\n", "", table.str)
  full.dt <- fread(no.dividers, sep="|")
  full.dt[, -c(1, ncol(full.dt)), with=FALSE]
}

sapply(list.of.tables, nrow)
crepuscule <- list.of.tables[["places/Ferme_La_Crepuscule.org"]]
list.of.dt <- lapply(crepuscule[, "table"], str2dt)

data.by.place <- list()
for(file.i in seq_along(place.vec)){
  base.org <- basename(place.org.vec[[file.i]])
  place.name <- gsub("_", " ", sub(".org$", "", base.org))
  table.mat <- list.of.tables[[file.i]]
  tables.list <- list()
  if(nrow(table.mat))for(table.i in 1:nrow(table.mat)){
    table.data <- table.mat[table.i,]
    section <- table.data[["section"]]
    tables.list[[section]] <- str2dt(table.data[["table"]])
  }
  data.by.place[[place.name]] <- list(
    tables=tables.list,
    values=list.of.described[[file.i]])
}

## Or first convert to .json files and then parse using RJSONIO?
system("pandoc --version") #pandoc 1.15.2.1
system("pandoc products/Hemp_seeds.org -o products/Hemp_seeds.json")
fromJSON("products/Hemp_seeds.json")
