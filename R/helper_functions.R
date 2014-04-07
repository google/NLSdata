
all.logical <- function(var) {
  # Checks whether a vector contains only logical constants, perhaps as chars
  #
  # Args:
  #  var: a vector of any type
  #
  # Returns:
  #  A logical scalar indicating whether the input can be treated as logical
  unique.vals <- setdiff(unique(var), NA)
  # Non-bolean values will be turned in to NA, stacked at the front
  vals.as.logical <- sort(as.logical(unique.vals), na.last = FALSE)
  return(identical(vals.as.logical, c(FALSE, TRUE)))
}

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

CreateNLSdata <- function(codebook, csv.extract, 
			  na.strings = c("-1", "-2", "-3", "-4", "-5")) {
  write("\n", file = codebook, append = TRUE)
  contents <- readLines(codebook)
  chunks <- GetChunkListFromContents(contents)
  my.df <- read.csv(csv.extract, na.strings = na.strings)
  metadata <- list()
  i = 0
  for (chunk in chunks) {
    i = i + 1
    cat(" Working on Chunk ", i, "\n")
    r.id <- sub("([A-Z][0-9\\.]+).*", "\\1",  chunk[1]) 
    r.id <- sub("\\.", "", r.id)
    name <- sub(".*\\[(.*)\\].*", "\\1", chunk[1])
    year <- sub(".*Survey Year: (\\d{4}|XRND)", "\\1", chunk[1])
    
    summary.index <- 2 + min(grep("[A-Z]", chunk[3:length(chunk)]))
    summary <- trim(chunk[summary.index])
    
    # Figuring out values from summary histograms 
    number.lines <- grep("^\\s*\\d", chunk)
    dash.index <- grep("^\\s+-", chunk)
    distr.lines <- ""
    if ((length(number.lines) > 0) & (length(dash.index) > 0)) {
      distr.lines <- chunk[min(number.lines):(max(dash.index) - 1)]
    }
    distr.lines <- gsub(":", "", distr.lines)
    char.map <- sub("^\\s+\\d+\\s+(\\d+)\\s(.*)$", "\\1\t\\2", distr.lines)
    char.map <- char.map[grepl("\t", char.map)]
    cont.evidence <- (length(grep("\tTO\\s", char.map)) > 0) | 
                     (length(char.map) == 0)
    new.name <- gsub("-", "_", paste(name, year, sep = "."))
    if (cont.evidence | length(distr.lines) == 0) {
      my.df <- rename(my.df, r.id, new.name) 
      } else {
      mapping <- data.frame()
      label.map.list <- strsplit(char.map, "\t")
      for (k in 1:length(label.map.list)) {
        mapping <- rbind(mapping,
                         data.frame(var1 = as.numeric(label.map.list[[k]][1]),
                                    var2 = label.map.list[[k]][2]))
      }
      names(mapping) <- c(r.id, new.name)
      print(names(mapping))  
      my.df <- merge(my.df, mapping, all.x = TRUE)
      my.df[, r.id] <- NULL
      if (all.logical(my.df[, new.name])) {
        my.df[, new.name] <- as.logical(my.df[, new.name])
      }
    }
    metadata[[new.name]] <- list(name = new.name, summary = summary,
				 year = year, r.id = r.id, chunk = chunk) 
    cat("After chunk ", i, ",", nrow(my.df), "rows remain, ", 
        ncol(my.df), "cols remain, is numeric:", cont.evidence, "\n")
  }
  obj <- list(metadata = metadata, data = my.df)
  class(obj) <- "NLSdata"
  return(obj)
}

summary.NLSdata <- function(obj) {
  vars <- names(obj$data)
  for (var in vars) {
    cat("------------------------------------------------------\n")
    cat("*** Variable:", var, "***\n")
    cat("------------------------------------------------------\n")
    cat("    Year:", obj$metadata[[var]]$year, "\n")
    cat("    Summary:", obj$metadata[[var]]$summary, "\n")
    cat("    Distribution:\n")
    print(summary(obj$data[, var]))
  }
}

CreateTimeSeriesDf <- function(obj, variable.base) {
  require(reshape2)
  var.vec <- sort(grep(variable.base, names(obj$data), value = TRUE))
  year.vec <- as.numeric(sub(".*(\\d{4}.*)", "\\1", var.vec))
  var.type <- class(obj$data[, var.vec[1]])
       
  melted.df <- melt(obj$data[, c("PUBID.1997", var.vec)], id = "PUBID.1997")
  if (var.type == "logical") {
    melted.df[, "value"] <- as.logical(melted.df[, "value"])
  }
  melted.df$year <- as.numeric(sub(".*(\\d{4}.*)", "\\1", melted.df$variable))
  melted.df$variable <- NULL                                       
  names(melted.df) <- c("PUBID.1997", variable.base, "year")
  return(melted.df)
}

RosterToLongDf <- function(data, roster.base, id = "PUBID.1997") {
  # Creates a long data frame from a wide roster variable set
  #
  # Args:
  #  data: a data frame
  #  roster.base: the stable character prefix of the roster variable
  #  id: character name representing the unit identifier
  #
  # Returns:
  #  A data frame in long format with the roster base as the outcome name
  roster.vars <- grep(paste0(roster.base, "\\.\\d{2}\\.\\d{4}"),
                      names(data), value = TRUE)
  if (length(roster.vars) == 0){
    stop("No rosters. Ensure that roster variables in form [base].dd.dddd")
  }
  roster.subset <- data[, c(id, roster.vars)]
  roster.long <- melt(roster.subset, id.var = id)
  roster.long <- roster.long[!is.na(roster.long$value), ]
  roster.long$year <- sub(paste0(roster.base, "\\.\\d{2}\\.(\\d{4})"), "\\1",
                          roster.long$variable)
  roster.long[, roster.base] <- roster.long$value
  roster.long$variable <- NULL
  roster.long$value <- NULL
  roster.long <- roster.long[order(roster.long[, id], roster.long$year),
                             c(id, "year", roster.base)]
  return(roster.long)
}

ShowAcronyms <- function() {
  print("R : Respondant")
  print("RS: Round Specific")
  print("CV: Created Variable")
  print("CVC: Cumulative Version of Created Variable")
  print("XRND: accross rounds")
  print("E: Event history")
}

ThrowAwayDataForBalance <- function(df, var.name) {
  n.df <- aggregate(df[, var.name], by = list(df$PUBID.1997),
	            FUN = function(x) sum(!is.na(x)))
  new.df <- df[df$PUBID.1997 %in% n.df[n.df$x == max(n.df$x), "Group.1"], ]
  return(new.df)
}
