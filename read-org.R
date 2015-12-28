works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264",
             RJSONIO="1.3.0",
             orgR="0.9.0")

link.subject.vec <- c(
  "[[file:Le_Tournesol.org][Le Tournesol]]",
  "[[../products/Hemp_seeds.org][Hemp seeds]]",
  "[[file:../products/Hemp_seeds.org][Hemp seeds]]",
  "[[file:../products/Hemp_seeds.org]]",
  "not a link")
no.link.vec <- c("not a link", "also not a link")
link.pattern <- paste0(
  "\\[\\[",
  "(?:file:)?",
  "(?:",
  "[.][.]/",
  "(?<directory>[^/]+)",
  "/",
  ")?",
  "(?<file>[^]]+)",
  "\\]",
  "(?:",
  "\\[",
  "(?<description>",
  "[^]]+",
  ")",
  ")?",
  "\\]")
link.mat <- str_match_named(link.subject.vec, link.pattern)
stopifnot(identical(link.mat[, "directory"], c(
  "",
  "products",
  "products",
  "products",
  NA)))
stopifnot(identical(link.mat[, "file"], c(
  "Le_Tournesol.org",
  "Hemp_seeds.org",
  "Hemp_seeds.org",
  "Hemp_seeds.org",
  NA)))
stopifnot(identical(link.mat[, "description"], c(
  "Le Tournesol",
  "Hemp seeds",
  "Hemp seeds",
  "",
  NA)))
file.or.null <- function(maybe.link.vec, default.dir){
  if(is.character(maybe.link.vec)){
    maybe.link.mat <- str_match_named(maybe.link.vec, link.pattern)
    dir.vec <- ifelse(
      maybe.link.mat[, "directory"] == "",
      default.dir,
      maybe.link.mat[, "directory"])
    maybe.file <- maybe.link.mat[, "file"]
    result.vec <- ifelse(
      is.na(maybe.file), NA,
      paste0(dir.vec, "/", maybe.file))
    if(!all(is.na(result.vec))){
      result.vec
    }
  }
}
link.result <- file.or.null(link.subject.vec, "places")
stopifnot(identical(link.result, c(
  "places/Le_Tournesol.org",
  "products/Hemp_seeds.org",
  "products/Hemp_seeds.org",
  "products/Hemp_seeds.org",
  NA)))
no.link.result <- file.or.null(no.link.vec, "places")
stopifnot(is.null(no.link.result))

## Parse .org myself using regular expressions?
lines.vec <- readLines("products/Hemp_seeds.org")
place.org.vec <- Sys.glob("places/*.org")
place.vec <- sapply(place.org.vec, function(place.org){
  paste(readLines(place.org), collapse="\n")
})

section.pattern <- paste0(
  "[*]+ +",
  "(?<name>.*?)",
  "\n",
  "(?<content>",
  "(?:", 
  "\n",
  "|",
  "[^*][^\n]*",
  "\n",
  ")+",
  ")")
sections.by.place <- str_match_all_named(place.vec, section.pattern)
crepuscule <- sections.by.place[["places/Ferme_Le_Crepuscule.org"]]
stopifnot("Public prices" %in% rownames(crepuscule))
table(sapply(sections.by.place, nrow))

table.pattern <- paste0(
  "\n",
  "(?<table>",
  "(?:",
  "[|][^\n]+\n",
  ")+",
  ")")
content.vec <- crepuscule[,"content"]
all.tables.by.section <- str_match_all_named(content.vec, table.pattern)
first.tables.mat <- str_match_named(content.vec, table.pattern)

description.pattern <- paste0(
  "-",
  " +",
  "(?<name>.*?)",
  " +:: +",
  "(?<value>.*?)",
  "\n")
list.of.described <- str_match_all_named(content.vec, description.pattern)

str2dt <- function(table.str){
  no.dividers <- gsub("[|]-.*?\n", "", table.str)
  full.dt <- fread(no.dividers, sep="|")
  full.dt[, -c(1, ncol(full.dt)), with=FALSE]
}

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
  if(place.name %in% names(data.by.place)){
    stop("duplicate place name ", place.name)
  }
  data.by.place[[place.name]] <- list(
    tables=tables.list,
    values=list.of.described[[file.i]])
}

## Or first convert to .json files and then parse using RJSONIO?
system("pandoc --version") #pandoc 1.15.2.1
system("pandoc products/Hemp_seeds.org -o products/Hemp_seeds.json")
fromJSON("products/Hemp_seeds.json")
