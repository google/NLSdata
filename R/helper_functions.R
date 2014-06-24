# Copyright 2014 Google Inc. All rights reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
  # Renames R.* variable names to NLS versions with metadata built in
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

EncodeFactorAsNumeric <- function(var, new.levels, data, verbose = TRUE) {
  # TODO: Needs documentation and a quick test
  if (class(data[, var]) != "factor") {
    stop("This function converts factors only")
  }
  old.levels <- levels(data[, var])
  levels(data[, var]) <- new.levels
  data[, var] <- as.numeric(as.character(data[, var]))
  if (verbose) {
    cat("mapping of", var, "\n")
    for (i in 1:length(old.levels)) {
      cat("Old level:", old.levels[i], ", New level:", new.levels[i], "\n")
    }
  }
  return(data)
}

GetChunkListFromContents <- function(contents, 
			     separator = paste(rep("-", 80), collapse = "")) {
  # Returns a list of distinct text chunks from the NLS codebook
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
  # Converts raw NLS Investigator output to an NLSdata object
  #
  # Args:
  #  codebook: the .cdb file from the NLS Investigator export
  #  csv.extract: the .csv file from the NLS Investigator export
  #  na.strings: which of the missing value situations to represent as NA
  # Returns:
  #  an NLSdata object containing a data frame and metadata dictionary
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

summary.NLSdata <- function(object, ...) {
  # Prints summary information for NLSdata object
  # 
  # Args:
  #  object: An NLSdata object
  #  ...: Not used currently
  vars <- names(object$data)
  for (var in vars) {
    cat("------------------------------------------------------\n")
    cat("    ", var, ", measured in",  object$metadata[[var]]$year, "\n")
    cat("    ", object$metadata[[var]]$summary, "\n")
    cat("------------------------------------------------------\n")
    print(summary(object$data[, var]))
  }
}

KeywordSearch <- function(term, nlsdata) {
  #TODO: documentation
  summary.vec <- c()
  n <- length(nlsdata$metadata)
  for (i in 1:n) {
    #summary.vec <- c(summary.vec, nlsdata$metadata[[i]]$summary)
    text.to.search <- paste(nlsdata$metadata[[i]]$summary,
                            paste(nlsdata$metadata[[i]]$chunk, collapse = " "))
    summary.vec <- c(summary.vec, text.to.search)
  }
  found <- grep(term, summary.vec, ignore.case = TRUE)
  for (i in found) {
    cat("*FOUND* : ", nlsdata$metadata[[i]]$name, ", index:", i, "\n")
    cat(" SUMMARY: ", nlsdata$metadata[[i]]$summary, "\n\n")
  }
}

CreateTimeSeriesDf <- function(obj, variable.base) {
  # Converts a repeated element into a long format data frame
  #
  # Args:
  #  obj: an NLSdata object
  #  variable.base: character string common prefix for the repeated element
  #
  # Returns:
  #  A data frame in long format
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

ThrowAwayDataForBalance <- function(data, var.name, id = "PUBID.1997") {
  # Achieve Balance over Time Period and Respondent by Crude Subsetting
  #
  # Args:
  #  data: a data frame
  #  var.name: variable to achieve balance with respect to
  #  id: id variable, also to achieve balance with respect to
  #
  # Returns: a balanced data data frame
  n.df <- aggregate(data[, var.name], by = list(data[[id]]),
	            FUN = function(x) sum(!is.na(x)))
  new.df <- data[data$PUBID.1997 %in% n.df[n.df$x == max(n.df$x), "Group.1"], ]
  return(new.df)
}

