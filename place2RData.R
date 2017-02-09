works_with_R("3.2.2",
             data.table="1.9.6",
             "tdhock/namedCapture@05175927a45c301a18e8c6ebae67ea39a842d264")

print.character <- function(x, ...){
  row.vec <- rownames(x)
  row.chars <- if(is.null(row.vec)){
    0
  }else{
    max(nchar(row.vec))
  }
  max.chars <- getOption("width") - row.chars
  is.big <- max.chars < nchar(x)
  if(any(is.big)){
    n <- max.chars - 10
    cat("... = only printed first", n, "characters.\n")
    first <- ifelse(is.big, paste0(substr(x, 1, n), "..."), x)
    print.default(first, ...)
  }else{
    print.default(x, ...)
  }
}

arg.vec <- "places/Ferme_Le_Crepuscule.org"
arg.vec <- "places/Vrac_en_Folie.org"
arg.vec <- commandArgs(trailingOnly=TRUE)

if(length(arg.vec) != 1){
  print(arg.vec)
  stop("Usage: org2RData.R file.org")
}

file.org <- arg.vec[1]

org.lines <- readLines(file.org)

org.txt <- paste(org.lines, collapse="\n")

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
section.mat <- str_match_all_named(org.txt, section.pattern)[[1]]
content.vec <- section.mat[, "content"]
names(content.vec) <- rownames(section.mat)

table.pattern <- paste0(
  "\n",
  "(?<table>",
  "(?:",
  "[|][^\n]+\n",
  ")+",
  ")")
##all.tables.by.section <- str_match_all_named(content.vec, table.pattern)
first.table.mat <- str_match_named(content.vec, table.pattern)
print(first.table.mat)

description.pattern <- paste0(
  "-",
  " +",
  "(?<name>.*?)",
  " +:: +",
  "(?<value>.*?)",
  "\n")
list.of.described <- str_match_all_named(content.vec, description.pattern)

## detect if columns in tables are links.
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
file.or.null <- function(maybe.link.vec, default.dir=basename(dirname(file.org))){
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

section.names <- c(
  Prices="prices",
  "Public prices"="prices")
table.names <- c(
  bio="organic",
  what="product")
rename.some <- function(name.vec, code.vec){
  stopifnot(is.character(name.vec))
  stopifnot(is.character(code.vec))
  stopifnot(!is.null(names(code.vec)))
  to.rename <- name.vec %in% names(code.vec)
  old.names <- name.vec[to.rename]
  name.vec[to.rename] <- code.vec[old.names]
  name.vec
}

str2dt <- function(table.str){
  no.dividers <- gsub("[|]-.*?\n", "", table.str)
  full.dt <- fread(no.dividers, sep="|")
  small.dt <- full.dt[, -c(1, ncol(full.dt)), with=FALSE]
  new.name.vec <- rename.some(names(small.dt), table.names)
  setnames(small.dt, new.name.vec)
  for(col.name in names(small.dt)){
    maybe.file.vec <- file.or.null(small.dt[[col.name]])
    if(is.character(maybe.file.vec)){
      new.name <- paste0(col.name, ".file")
      small.dt[[new.name]] <- sub("[.]org$", "", basename(maybe.file.vec))
    }
  }
  small.dt
}

data.by.section <- list()
for(section.i in seq_along(content.vec)){
  orig.section <- names(content.vec)[[section.i]]
  section <- rename.some(orig.section, section.names)
  table.txt <- first.table.mat[section.i, "table"]
  result.list <- list()
  if(!is.na(table.txt)){
    result.list$table <- str2dt(table.txt)
  }
  described.mat <- list.of.described[[section.i]]
  if(nrow(described.mat)){
    result.list$values <- described.mat
  }
  if(length(result.list)){
    data.by.section[[section]] <- result.list
  }
}

file.RData <- sub("org$", "RData", file.org)
cat("Writing ", file.RData, "\n", sep="")
save(data.by.section, file=file.RData)
