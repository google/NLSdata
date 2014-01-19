
double.me <- function(x) {
  return(2 * x)
}

# All missing data are clearly flagged in the NLSY97 data set with five negative values:
# (-1) refusal, (-2) don't know, (-3) invalid skip, (-4) valid skip, and (-5) noninterview. 
# Think about how these still might be MCAR

# roster: like a repeated element
#  roster may be thought of as a list--for example, a list of household 
# members, a list of employers, or a list of children. 

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

rename <- function(df, old, new, pos = NULL) {
  df[, new] <- df[, old]
  df[, old] <- NULL;
  if (!is.null(pos)) { # This is a swap
    current.pos <- which(names(df) == new)
    val.at.new.pos <- df[, pos]
    name.at.new.pos <- names(df)[pos]
    df[, pos] <- df[, new]
    names(df)[pos] <- new
    df[, current.pos] <- val.at.new.pos
    names(df)[current.pos] <- name.at.new.pos 
  }
  return(df)
}

GetChunkListFromContents <- function(contents, 
			     separator = paste(rep("-", 80), collapse = "")) {

  chunks <- list()
  chunk <- c()
  for (i in 1:length(contents)) {
    if(contents[i] != separator){
      chunk <- c(chunk, contents[i])
    } else {
     chunks[[length(chunks) + 1]] <- chunk
     chunk <- c() 
    }
  }
  return(chunks)
}

CreateNLSdata <- function(codebook, csv.extract, filedir,
                          na.strings = c("-1", "-2", "-3", "-4", "-5")) {

  contents <- readLines(file.path(filedir, codebook))
  chunks <- GetChunkListFromContents(contents)
  my.df <- read.csv(file.path(filedir, csv.extract), na.strings = na.strings)
  metadata <- list()
  i = 0
  for (chunk in chunks) {
    i = i + 1
    cat(" Working on Chunk ", i, "\n")
    r.id <- sub("([A-Z][0-9\\.]+).*", "\\1",  chunk[1]) 
    r.id <- sub("\\.", "", r.id)
    name <- sub(".*\\[(.*)\\].*", "\\1", chunk[1])
    year <- sub(".*Survey Year: (\\d{4})", "\\1", chunk[1])
    
    summary.index <- 2 + min(grep("[A-Z]", chunk[3:length(chunk)]))
    summary <- trim(chunk[summary.index])
    
    # Figuring out values from summary histograms 
    number.lines <- grep("^\\s*\\d", chunk)
    dash.index <- grep("^\\s+-", chunk)
    distr.lines <- chunk[min(number.lines):(max(dash.index) - 1)]
    distr.lines <- gsub(":", "", distr.lines)
    char.map <- sub("^\\s+\\d+\\s+(\\d+)\\s(.*)$", "\\1\t\\2", distr.lines)
    char.map <- char.map[grepl("\t", char.map)]
    cont.evidence <- (length(grep("\tTO\\s", char.map)) > 0) | 
                     (length(char.map) == 0)
    new.name <- paste(name, year, sep = ".")
    if (cont.evidence) {
      my.df <- rename(my.df, r.id, new.name) 
      } else {
      mapping <- data.frame()
      for (elem in strsplit(char.map, "\t")) {
        mapping <- rbind(mapping, data.frame(var1 = as.numeric(elem[[1]]),
      				       var2 = elem[[2]]))
      }
      names(mapping) <- c(r.id, new.name)
      print(names(mapping))  
      my.df <- merge(my.df, mapping, all.x = TRUE)
      print(ncol(my.df))
      if(ncol(my.df) < 501) stop("You're about to lose a column")
      my.df[, r.id] <- NULL
      print(ncol(my.df))
    }
    if (ncol(my.df) < 500) stop("You lost a column")  
    metadata[[name]] <- list(summary = summary, year = year, r.id = r.id, 
  			   chunk = chunk) 
    cat("After chunk ", i, ",", nrow(my.df), "rows remain, ", 
        ncol(my.df), "cols remain, is numeric:", cont.evidence, "\n")
  }
  obj <- list(metadata = metadata, data = my.df)
  class(obj) <- "NLSdata"
  return(obj)
}
