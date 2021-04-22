######################################################################
# A collection of custom R functions
######################################################################
# source('~/GitHub/Packages/CodeAndRoll/CodeAndRoll.R')
# source('https://raw.githubusercontent.com/vertesy/CodeAndRoll/master/CodeAndRoll.R')

## If something is not found:
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)

# try(source("~/Github/TheCorvinas/R/RNA_seq_specific_functions.r"), silent = T)
## For Plotting From Clipboard or Files
# source("~/Github/TheCorvinas/R/Plotting.From.Clipboard.And.Files.r")
# # Load sequence length and base distribution check
# source("~/Github/TheCorvinas/R/Gene.Stats.mm10.R")
suppressMessages(try(require(clipr), silent = T))
try(require(ggplot2),silent = T)


### CHAPTERS:
# -  Generic functions
# -  File handling, export, import [read & write]
#   - Clipboard interaction (OS X)
#   - Reading files in
#   - Writing files out
# - Create and check variables
# -  Vector operations
#   - Vector filtering
# -  String operations
# -  Matrix operations
#   - Matrix filtering
# -  List operations
# -  Set operations
# -  Math and stats
# -  Plotting and Graphics
# -  Clustering heatmap tools
# -  Search query links
# -  Biology


# -  Generic
# -  New additions

wA4 = 8.27 # A4 inches
hA4 = 11.69


## Setup   -------------------------------------------------------------------------------------------------
# pdf.options(title = paste0('Copyright Abel Vertesy ', Sys.Date())) # Setup to your own name
debuggingState(on = FALSE)
# "gtools", "readr", "gdata", "colorRamps", "grDevices", "plyr"
print("Depends on MarkdownReports, gtools, readr, gdata, clipr. Some functions depend on other libraries.")

### Load the MarkdownReports Library -------------------------------------------------------------------------------------------------
# source("~/Github/MarkdownReports/MarkdownReports/R/MarkdownReports.R")
# try(require("MarkdownReports"))
# try(require("gtools"))
# try(ggplot2::theme_set( theme_bw()), silent = TRUE)



# Alisases ----------------
sort.natural = gtools::mixedsort
p0 = paste0
l = length

ppp <- function(...) { paste(..., sep = '.') } # Paste by point
pps <- function(...) { paste(..., sep = '/') } # Paste by (forward) slash
ppu <- function(...) { paste(..., sep = '_') } # Paste by underscore
ppd <- function(...) { paste(..., sep = '-') } # Paste by dash

kpp <- function(...) { paste(..., sep = '.', collapse = '.') } # kollapse by point
kppu <- function(...) { paste(..., sep = '_',  collapse = '_') } # kollapse by underscore
kpps <- function(...) { paste(..., sep = '/', collapse = '/') } # kollapse by (forward) slash
kppd <- function(...) { paste(..., sep = '-', collapse = '-') } # kollapse by dash

stry <- function(...) {try(..., silent = T)} # Silent try



## Generic -------------------------------------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))


stopif2 <- function(condition, ...) { if (condition) {iprint(...); stop()} } # Stop script if the condition is met. You can parse anything (e.g. variables) in the message


say <- function(...) { # Use system voice to notify (after a long task is done)
  sys <- Sys.info()["sysname"]
  if (sys == "Darwin") system("say Ready")
  if (sys == "Linux") system("echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'")  # For UNIX servers.
}
sayy <- function(...) {system("say Ready to roll")} # Use system voice to notify (after a long task is done)



grepv <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE  # grep returning the value
                  , invert = FALSE, ...) grep(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed
                                              , useBytes = useBytes, invert = invert, ..., value = TRUE)

oo <- function() { # Open current working directory.
  system("open .")
}

# detach_package <-
unload <- function(pkg, character.only = FALSE) { # Unload a package. Source: https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
  if (!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while (search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

most_frequent_elements <- function(thingy, topN = 10) { # Show the most frequent elements of a table
  tail(sort(table(thingy, useNA = "ifany")), topN)
}

top_indices <- function(x, n = 3, top = TRUE) { # Returns the position / index of the n highest values. For equal values, it maintains the original order
  head( order(x, decreasing = top), n )
}

percentile2value <- function(distribution, percentile = 0.95, FirstValOverPercentile = TRUE) { # Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.
  index = percentile * length(distribution)
  if (FirstValOverPercentile) { index = ceiling(index)
  } else {index = floor(index) }
  value = sort(distribution)[index]
  return(value)
}

printEveryN <- function(i, N = 1000) { if ((i %% N) == 0 ) iprint(i) } # Report at every e.g. 1000

irequire <- function(package) { package_ = as.character(substitute(package)); print(package_); # Load a package. If it does not exist, try to install it from CRAN.
if (!require(package = package_,  character.only = TRUE)) {
  print("Not Installed yet.");install.packages(pkgs = package_);
  Sys.sleep(1)
  print("Loading package:")
  require(package = package_, character.only = TRUE)
}
}  # install package if cannot be loaded

idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) { format(Sys.time(), format = Format ) } # Parse current date, dot separated.

view.head <- function(matrix, enn = 10) { matrix[1:min(NROW(matrix), enn), 1:min(NCOL(matrix), enn)] } # view the head of an object by console.
view.head2 <- function(matrix, enn = 10) { View(head(matrix, n = min(NROW(matrix), NCOL(matrix), enn))) } # view the head of an object by View().

iidentical.names <- function(v1, v2) { # Test if names of two objects for being exactly equal
  nv1 = names(v1)
  nv2 = names(v2)
  len.eq = (length(nv1) == length(nv2))
  if (!len.eq) iprint("Lenghts differ by:", (length(nv1) - length(nv2)) )
  Check = identical(nv1, nv2)
  if (!Check) {
    diff = setdiff(nv1, nv2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

iidentical <- function(v1, v2) { # Test if two objects for being exactly equal
  len.eq = (length(v1) == length(v2))
  if (!len.eq) iprint("Lenghts differ by:", (length(v1) - length(v2)) )
  Check = identical(v1,v2)
  if (!Check) {
    diff = setdiff(v1, v2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

iidentical.all <- function(li) all(sapply(li, identical, li[[1]])) # Test if two objects for being exactly equal.

#' IfExistsAndTrue
#'
#' Internal function. Checks if a variable is defined, and its value is TRUE.
#' @param name Name of the varaible
#' @export
#' @examples IfExistsAndTrue()

IfExistsAndTrue <- function(name = "pi" ) { # Internal function. Checks if a variable is defined, and its value is TRUE.
  x = FALSE
  if (exists(name)) {
    if (isTRUE(get(name)))  {x = TRUE} else {x = FALSE; iprint(name, " exists, but != TRUE; ", get(name))}
  }
  return(x)
}

memory.biggest.objects <- function(n = 5, saveplot = F) { # Show distribution of the largest objects and return their names. # https://stackoverflow.com/questions/17218404/should-i-get-a-habit-of-removing-unused-variables-in-r
  try.dev.off()
  gc()
  ls.mem <- ls( envir = .GlobalEnv)
  ls.obj <- lapply(ls.mem, get)
  Sizes.of.objects.in.mem <- unlapply(ls.obj, object.size)
  names(Sizes.of.objects.in.mem) <- ls.mem
  topX = sort(Sizes.of.objects.in.mem,decreasing = TRUE)[1:n]

  Memorty.usage.stat = c(topX, 'Other' = sum(sort(Sizes.of.objects.in.mem,decreasing = TRUE)[-(1:n)]))
  pie(Memorty.usage.stat, cex = .5, sub = make.names(date()))
  try(qpie(Memorty.usage.stat, w = 7,  ), silent = T)
  # Use wpie if you have MarkdownReports, from https://github.com/vertesy/MarkdownReports
  dput(names(topX))
  iprint("rm(list = c( 'objectA',  'objectB'))")
  # inline_vec.char(names(topX))
  # Use inline_vec.char if you have DataInCode, from https://github.com/vertesy/DataInCode
}
# memory.biggest.objects()


## File handling, export, import [read & write] -------------------------------------------------------------------------------------------------

### Reading files in -------------------------------------------------------------------------------------------------
read.simple.vec <- function(...) { # Read each line of a file to an element of a vector (read in new-line separated values, no header!).
  pfn = kollapse(...) # merge path and filename
  read_in = as.vector(unlist(read.table( pfn , stringsAsFactors = FALSE, sep = "\n" )) )
  iprint(length(read_in), "elements")
  return(read_in);
}

read.simple <- function(...) { # It is essentially read.table() with file/path parsing.
  pfn = kollapse(...) # merge path and filename
  read_in = read.table( pfn , stringsAsFactors = FALSE)
  return(read_in)
}

read.simple_char_list <- function(...) { # Read in a file.
  pfn = kollapse(...) # merge path and filename
  read_in = unlist(read.table( pfn , stringsAsFactors = FALSE ) )
  iprint("New variable head: ", what(read_in))
  return(read_in)
}

read.simple.table <- function(..., colnames = TRUE, coltypes = NULL) { # Read in a file. default: header defines colnames, no rownames. For rownames give the col nr. with rownames, eg. 1 The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.table( pfn , stringsAsFactors = FALSE, sep = "\t", header = colnames )
  read_in = readr::read_tsv( pfn, col_names = colnames, col_types = coltypes )
  iprint("New variable dim: ", dim(read_in))
  read_in = as.data.frame(gtools::na.replace(data.matrix(read_in), replace = 0))
  return(read_in)
}

FirstCol2RowNames <- function(Tibble, rownamecol = 1, make_names = FALSE) { # Set First Col to Row Names
  Tibble = as.data.frame(Tibble)
  NN = Tibble[[rownamecol]]
  rownames(Tibble) = if (make_names) make.names(NN, unique = TRUE) else NN
  return(Tibble[, -rownamecol, drop = F])
}

read.simple.tsv <- function(..., sep_ = "\t", colnames = TRUE, wRownames = TRUE, coltypes = NULL, NaReplace = TRUE) { # Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = , sep_, row.names = 1, header = TRUE )
  read_in = suppressWarnings(readr::read_tsv( pfn, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


read.simple.csv <- function(...,  colnames = TRUE, coltypes = NULL, wRownames = TRUE, NaReplace = TRUE, nmax = Inf) { # Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_csv( pfn, col_names = colnames, col_types = coltypes, n_max = nmax ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}

read.simple.ssv <- function(..., sep_ = " ", colnames = TRUE, wRownames = TRUE, NaReplace = TRUE, coltypes = NULL) { # Space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_delim( pfn, delim = sep_, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


read.simple.tsv.named.vector <- function(...) { # Read in a file with excel style named vectors, names in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = sep_, row.names = 1, header = TRUE )
  read_in = readr::read_tsv( pfn )
  vect = read_in[[2]]
  names(vect) = read_in[[1]]
  iprint("New vectors length is: ", length(vect))
  return(vect)
}

convert.tsv.data <- function(df_by_read.simple.tsv = x, digitz = 2, na_rep = 0 ) { # Fix NA issue in dataframes imported by the new read.simple.tsv. Set na_rep to NA if you want to keep NA-s
  DAT = data.matrix(df_by_read.simple.tsv)
  SNA = sum(is.na(DAT))
  try(iprint("Replaced NA values:", SNA, "or", percentage_formatter(SNA/length(DAT))), silent = TRUE)
  gtools::na.replace(round(DAT, digits = digitz), replace = na_rep)
}



read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) { # Read multi-sheet excel files. row_namePos = NULL for automatic names Look into: http://readxl.tidyverse.org/.
  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '/Users/abel.vertesy/'.")
    pfn = gsub(pattern = "^~/", replacement = "/Users/abel.vertesy/", x = pfn)
  } else {print(pfn)}

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  # merge path and filename
  TheSheetNames = sheetNames(pfn, verbose = FALSE);
  NrSheets = length(TheSheetNames)
  iprint(NrSheets, "sheets in the file.")
  ExpData = list.fromNames(TheSheetNames)
  RangeOfSheets = if (missing(WhichSheets)) 1:NrSheets else WhichSheets
  for (i in RangeOfSheets ) {
    iprint("sheet", i)
    ExpData[[i]] = gdata::read.xls(pfn, sheet = i, row.names = row_namePos, header = header_)
  } #for
  lapply(ExpData, function(x) print(dimnames(x)) )
  return(ExpData);
}

sourcePartial <- function(fn,startTag = '#1', endTag = '#/1') { # Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st + 1):(en - 1)])
  source(tc)
  close(tc)
}



### Writing files out -------------------------------------------------------------------------------------------------

write.simple <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ...  ) { # Write out a matrix-like R-object to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP = ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  iprint("Length: ", length(input_df))
} # fun

write.simple.vec <- function(input_vec, extension = 'vec', ManualName = "", o = FALSE, ... ) { # Write out a vector-like R-object to a file with as newline separated values (.vec). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_vec, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE  )
  iprint("Length: ", length(input_vec))
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun

write.simple.xlsx <- function(named_list, ManualName = "", o = FALSE,  ..., TabColor = "darkgoldenrod1", Creator = "Vertesy",# Write out a list of matrices/ data frames WITH ROW- AND COLUMN- NAMES to a file with as an Excel (.xslx) file. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
                              HeaderCex = 12, HeaderLineColor = "darkolivegreen3", HeaderCharStyle = c("bold", "italic", "underline")[1]  ) {
  irequire(openxlsx)
  fname = if (nchar(ManualName) < 2 ) { fname = substitute(named_list) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, "xlsx") }

  hs <- createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex, fgFill = HeaderLineColor)
  setwd(OutDir)
  openxlsx::write.xlsx(named_list, file = ppp(fname,"xlsx"), rowNames = TRUE, firstRow = TRUE, firstCol = TRUE, colWidths = "auto"
                       , headerStyle = hs, tabColour = TabColor, creator = Creator) #

  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


write.simple.append <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ... ) { # Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of the same number of columns. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_df) }
  if (nchar(ManualName)) { FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE  )
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


jjpegA4 <- function(filename, r = 225, q = 90) { # Setup an A4 size jpeg
  jpeg(file = filename,width = wA4, height = hA4, units = 'in', quality = q,res = r)
}

extPDF <- function(vec) ppp(vec, "pdf") # add pdf as extension to a file name

extPNG <- function(vec) ppp(vec, "png") # add png as extension to a file name


### Clipboard interaction -------------------------------------------------------------------------------------------------
# https://github.com/vertesy/DataInCode
# try(source("~/Github/TheCorvinas/R/DataInCode/DataInCode.R"), silent = FALSE)

clip2clip.vector <- function() { # Copy from clipboard (e.g. excel) to a R-formatted vector to the  clipboard
  x = dput(clipr::read_clip() )
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}


clip2clip.commaSepString <- function() { # Read a comma separated string (e.g. list of gene names) and properly format it for R.
  x = unlist(strsplit(clipr::read_clip(), split = ','))
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}

write_clip.replace.dot <- function(var = df.markers, decimal_mark = ',') { # Clipboard export for da wonderful countries with where "," is the decimal
  write_clip(format(var, decimal.mark = decimal_mark) )
}
# write_clip.replace.dot(df_markers)

## Create and check variables -------------------------------------------------------------------------------------------------

vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) { # create a vector from a vector of names
  v = numeric(length(name_vec))
  if (length(fill) == 1) {v = rep(fill, length(name_vec))}
  else if (length(fill == length(name_vec))) {v = fill}
  names(v) = name_vec
  return(v)
}

list.fromNames <- function(name_vec = LETTERS[1:5], fill = NaN) { # create list from a vector with the names of the elements
  liszt = as.list(rep(fill, length(name_vec)))
  names(liszt) = name_vec
  return(liszt)
}

matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) { # Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
  mx = matrix(data = fill, nrow = length(rowname_vec), ncol = length(colname_vec), dimnames = list(rowname_vec, colname_vec))
  iprint("Dimensions:", dim(mx))
  return(mx)
}


matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) { # Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
  matt = matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if ( !IsItARow ) {matt = t(matt)}
  return(matt)
}


array.fromNames <- function(rowname_vec = 1:3, colname_vec = letters[1:2], z_name_vec = LETTERS[4:6], fill = NA) { # create an N-dimensional array from N vectors defining the row-, column, etc names of the array
  DimNames = list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ = lapply(DimNames, length)
  mx = array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}


what <- function(x, printme = 0) { # A better version of is(). It can print the first "printme" elements.
  iprint(is(x), "; nr. of elements:", length(x))
  if ( is.numeric(x) )    { iprint("min&max:", range(x) ) } else {print("Not numeric")}
  if ( length(dim(x) ) > 0 )  { iprint("Dim:", dim(x) ) }
  if ( printme > 0)       { iprint("Elements:", x[0:printme] ) }
  head(x)
}

idim <- function(any_object) { # A dim() function that can handle if you pass on a vector: then, it gives the length.
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) { print("list") } #if
    print(length(any_object))
  }
  else { print(dim(any_object))  }
}

idimnames <- function(any_object) { # A dimnames() function that can handle if you pass on a vector: it gives back the names.
  if (!is.null(dimnames(any_object)))   { print(dimnames(any_object)) }
  else if (!is.null(colnames(any_object))) { iprint("colnames:", colnames(any_object))  }
  else if (!is.null(rownames(any_object))) { iprint("rownames:", rownames(any_object))  }
  else if (!is.null(names(any_object))) { iprint("names:", names(any_object)) }
}

table_fixed_categories <- function(vector, categories_vec) { # generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
  if ( !is.vector(vector)) {print(is(vector[]))}
  table(factor(unlist(vector), levels = categories_vec))
}

## Vector operations -------------------------------------------------------------------------------------------------

trail <- function(vec, N = 10) c(head(vec, n = N), tail(vec, n = N) ) # A combination of head() and tail() to see both ends.

sort.decreasing <- function(vec) sort(vec, decreasing = TRUE) # Sort in decreasing order.

sstrsplit <- function(string, pattern = "_", n = 2) { stringr::str_split_fixed(string, pattern = pattern, n = n) } # Alias for str_split_fixed in the stringr package

topN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5)   { head(sort(df_Col, decreasing = TRUE), n = n) } # Find the n highest values in a named vector
bottomN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5) { head(sort(df_Col, decreasing = FALSE), n = n) } # Find the n lowest values in a named vector


as.named.vector <- function(df_col, WhichDimNames = 1) { # Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
  # use RowNames: WhichDimNames = 1 , 2: use ColNames
  # !!! might require drop = FALSE in subsetting!!! eg: df_col[, 3, drop = FALSE]
  # df_col[which(unlist(lapply(df_col, is.null)))] = "NULL" # replace NULLs - they would fall out of vectors - DOES not work yet
  namez = dimnames(df_col)[[WhichDimNames]]
  if (is.list(df_col) & !is.data.frame(df_col)) {namez = names(df_col)}
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

col2named.vector <- function(df_col) { # Convert a dataframe column into a vector, keeping the corresponding dimension name.
  namez = rownames(df_col)
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

row2named.vector <- function(df_row) { # Convert a dataframe row into a vector, keeping the corresponding dimension name.
  namez = colnames(df_row)
  vecc = as.vector(unlist(df_row))
  names(vecc) = namez
  return(vecc)
}

as_tibble_from_named_vec <- function(vec.w.names =  c("a" = 1, "b" = 2), transpose = T) { # Convert a vector with names into a tibble, keeping the names as rownames.
  stopif(is_null(names(vec.w.names)))
  tbl <- bind_rows(vec.w.names)
  if (transpose) t(tbl) else tbl
}
# as_tibble_from_named_vec()


as.numeric.wNames <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

as.numeric.wNames.old <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec))
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

as.character.wNames <- function(vec) { # Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
  char_vec = as.character(vec)
  if (!is.null(names(vec))) {names(char_vec) = names(vec)}
  return(char_vec)
}

rescale <- function(vec, from = 0, upto = 100) { # linear transformation to a given range of values
  vec = vec - min(vec, na.rm = TRUE)
  vec = vec * ((upto - from)/max(vec, na.rm = TRUE))
  vec = vec + from
  return(vec)
} # fun

flip_value2name <- function(named_vector, NumericNames = FALSE, silent = F) { # Flip the values and the names of a vector with names
  if (!is.null(names(named_vector))) {
    newvec = names(named_vector)
    if (NumericNames) { newvec = as.numeric(names(named_vector))     }
    names(newvec) = named_vector
  } else {llprint("Vector without names!", head(named_vector))}
  if (!silent) {
    if (any(duplicated(named_vector))) {iprint("New names contain duplicated elements", head(named_vector[which(duplicated(named_vector))])) }
    if (any(duplicated(newvec))) {iprint("Old names contained duplicated elements", head(newvec[which(duplicated(newvec))])) }
  }
  return(newvec)
}

value2name_flip = flip_value2name

sortbyitsnames <- function(vec_or_list, decreasing = FALSE, ...) { # Sort a vector by the alphanumeric order of its names(instead of its values).
  xx = names(vec_or_list)
  names(xx) = 1:length(vec_or_list)
  order = as.numeric(names(gtools::mixedsort(xx, decreasing = decreasing, ...)))
  vec_or_list[order]
}

any.duplicated <- function(vec, summarize = TRUE) { # How many entries are duplicated
  y = sum(duplicated(vec))
  if (summarize & y) {
    x = table(vec); x = x[x > 1] - 1;
    print("The following elements have  > 1 extra copies:")
    print(x) # table formatting requires a separate entry
  }
  return(y)
}

which.duplicated <- function(vec, orig = F) { # which values are duplicated?
  DPL = vec[which(duplicated(vec))]; iprint(length(DPL), "Duplicated entries: ", DPL)
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(DPL)
}

which.NA <- function(vec, orig = F) { # which values are NA?
  NANs = vec[which(is.na(vec))]; iprint(length(NANs), "NaN entries: ", NANs)
  NAs = vec[which(is.na(vec))]; iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(NAs)
}

pad.na <- function(x, len) { c(x, rep(NA, len - length(x))) } # Fill up with a vector to a given length with NA-values at the end.


clip.values <- function(valz, high = TRUE, thr = 3) { # Signal clipping. Cut values above or below a threshold.
  if (high) { valz[valz > thr] = thr
  } else {    valz[valz < thr] = thr }
  valz
}

clip.outliers <- function(valz, high = TRUE, probs = c(.01, .99), na.rm = TRUE, showhist = FALSE, ...) { # Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
  qnt <- quantile(valz, probs = probs, na.rm = na.rm)
  if (showhist) { whist(unlist(valz), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- valz
  y[valz < qnt[1]] <- qnt[1]
  y[valz > qnt[2]] <- qnt[2]
  y
}

#' as.logical.wNames
#'
#' Converts your input vector into a logical vector, and puts the original character values
#' into the names of the new vector, unless it already has names.
#' @param x A vector with names that will be converted to a logical vector
#' @param ... Pass any other argument.
#' @export
#' @examples x = -1:2; names(x) = LETTERS[1:4]; as.logical.wNames(x)

as.logical.wNames <- function(x, ...) { # Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
  numerified_vec = as.logical(x, ...)
  if (!is.null(names(x))) {names(numerified_vec) = names(x)}
  return(numerified_vec)
}

col2named.vec.tbl <- function(tbl.2col) { # Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
  nvec = tbl.2col[[2]]
  names(nvec) = tbl.2col[[1]]
  nvec
}


iterBy.over <- function(yourvec, by = 9) { # Iterate over a vector by every N-th element.
  steps = ceiling(length(yourvec)/by)
  lsX = split(yourvec, sort(rank(yourvec) %% steps))
  names(lsX) = 1:length(lsX)
  lsX
} # for (i in iterBy.over(yourvec = x)) { print(i) }

zigzagger <- function(vec = 1:9) { # mix entries so that they differ
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}

numerate <- function(x = 1, y = 100, zeropadding = TRUE, pad_length = floor( log10( max(abs(x), abs(y)) ) ) + 1) { # numerate from x to y with additonal zeropadding
  z = x:y
  if (zeropadding) { z = stringr::str_pad(z, pad = 0, width = pad_length)   }
  return(z)
}
# (numerate(1, 122))


MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) { # find second (third…) highest/lowest value in vector
  topN = topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column


cumsubtract <- function(numericV = blanks) { # Cumulative subtraction, opposite of cumsum()
  DiffZ = numericV[-1] - numericV[-length(numericV)]
  print(table(DiffZ))
  DiffZ
}


### Vector filtering  -------------------------------------------------------------------------------------------------

which_names <- function(named_Vec) { # Return the names where the input vector is TRUE. The input vector is converted to logical.
  return(names(which(as.logical.wNames(named_Vec)))) }

which_names_grep <- function(named_Vec, pattern) { # Return the vector elements whose names are partially matched
  idx = grepv(x = names(named_Vec),pattern = pattern)
  return(named_Vec[idx])
}

na.omit.strip <- function(vec, silent = FALSE) { # Calls na.omit() and returns a clean vector
  if (is.data.frame(vec)) {
    if (min(dim(vec)) > 1 & silent == FALSE) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = na.omit(vec)
  attributes(clean)$na.action <- NULL
  return(clean)
}

inf.omit <- function(vec) { # Omit infinite values from a vector.
  if (is.data.frame(vec)) {
    if ( min(dim(vec)) > 1 ) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}

zero.omit <- function(vec) { # Omit zero values from a vector.
  v2 = vec[vec != 0]
  iprint("range: ", range(v2))
  if ( !is.null(names(vec)) ) {names(v2) = names(vec)[vec != 0]}
  return(v2)
}

pc_TRUE <- function(logical_vector, percentify = TRUE, NumberAndPC = FALSE, NArm = TRUE) { # Percentage of true values in a logical vector, parsed as text (useful for reports.)
  SUM = sum(logical_vector, na.rm = NArm)
  LEN = length(logical_vector)
  out = SUM / LEN
  if (percentify) {out = percentage_formatter(out) }
  if (NumberAndPC) { out = paste0(out, " or " , SUM, " of ", LEN) }
  return(out)
}

# deprecated :
NrAndPc <- function(logical_vec = idx_localised, total = TRUE, NArm = TRUE) { # Summary stat. text formatting for logical vectors (%, length)
  x = paste0(pc_TRUE(logical_vec), " or ", sum(logical_vec, na.rm = NArm))
  if (total) paste0(x, " of ", length(logical_vec))
}


pc_in_total_of_match <- function(vec_or_table, category, NA_omit = TRUE) { # Percentage of a certain value within a vector or table.
  if (is.table(vec_or_table)) { vec_or_table[category]/sum(vec_or_table, na.rm = NA_omit) }
  else {# if (is.vector(vec_or_table))
    if (NA_omit) {
      if (sum(is.na(vec_or_table))) { vec_or_table = na.omit(vec_or_table); iprint(sum(is.na(vec_or_table)), 'NA are omitted from the vec_or_table of:', length(vec_or_table))}
      "Not wokring complelety : if NaN is stored as string, it does not detect it"
    }
    sum(vec_or_table == category) /  length(vec_or_table)
  } # else: is vector
} # fun

filter_survival_length <- function(length_new, length_old, prepend = "") { # Parse a sentence reporting the % of filter survival.
  pc = percentage_formatter(length_new/length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}

remove_outliers <- function(x, na.rm = TRUE, ..., probs = c(.05, .95)) { # Remove values that fall outside the trailing N % of the distribution.
  print("Deprecated. Use clip.outliers()")
  qnt <- quantile(x, probs = probs, na.rm = na.rm, ...)
  # H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  # y[x < (qnt[1] - H)] <- NA ## Add IQR dependence
  # y[x > (qnt[2] + H)] <- NA
  y[x < qnt[1]] <- NA ## Add IQR dependence
  y[x > qnt[2]] <- NA
  y
}

simplify_categories <- function(category_vec, replaceit , to ) { # Replace every entry that is found in "replaceit", by a single value provided by "to"
  matches = which(category_vec %in% replaceit); iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] = to
  return(category_vec)
}

lookup <- function(needle, haystack, exact = TRUE, report = FALSE) { # Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
  ls_out = as.list( c(ln_needle = length(needle), ln_haystack = length(haystack), ln_hits = "",  hit_poz = "", hits = "") )
  Findings = numeric(0)
  ln_needle = length(needle)
  if (exact) {
    for (i in 1:ln_needle) {      Findings = c(Findings, which(haystack == needle[i]) )    } # for
  } else {
    for (i in 1:ln_needle) {      Findings = c(Findings, grep(needle[i], haystack,  ignore.case = TRUE, perl = FALSE))    } # for
  } # exact or partial match
  ls_out$'hit_poz' = Findings
  ls_out$'ln_hits' = length(Findings)
  ls_out$'hits' = haystack[Findings]
  if (length(Findings)) { ls_out$'nonhits' = haystack[-Findings]
  } else {      ls_out$'nonhits' = haystack }
  if (report) {
    llprint(length(Findings), "/", ln_needle, '(', percentage_formatter(length(Findings)/ln_needle)
            , ") of", substitute(needle), "were found among", length(haystack), substitute(haystack), "." )
    if (length(Findings)) { llprint( substitute(needle), "findings: ", paste( haystack[Findings], sep = " " ) ) }
  } else { iprint(length(Findings), "Hits:", haystack[Findings]) } # if (report)
  return(ls_out)
}



## String operations  -------------------------------------------------------------------------------------------------
parsepvalue <- function(pvalue = 0.01) paste0("(p<",pvalue,")"); # Parse p-value from a number to a string.

eval_parse_kollapse <- function(...) { # evaluate and parse (dyn_var_caller)
  substitute(eval(parse(text = kollapse( ... , print = FALSE))))
}


param.list.2.fname <- function(ls.of.params = p) { # Take a list of parameters and parse a string from their names and values.
  paste(names(ls.of.params), ls.of.params, sep = ".", collapse = "_")
}


PasteDirNameFromFlags <- function(...) { # Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).
  flagList <- c(...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  return(CleanDirName)
}
# PasteDirNameFromFlags("HCAB"
#                       , flag.nameiftrue(p$'premRNA')
#                       , flag.nameiftrue(p$"dSample.Organoids")
#                       , flag.names_list(p$'variables.2.regress')
#                       ,  flag.nameiftrue(p$'Man.Int.Order') )


### File name and path parsing ------------------------------------------------------------------------------------------------
PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) { # Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).
  flagList <- c(path, ...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  # pastedOutDir <- kpps(path, CleanDirName, "/")
  pastedOutDir <- p0(CleanDirName, "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = '[//] + ',replacement = '/' )
  return(CleanDirName)
}
# PasteOutdirFromFlags("~/Dropbox/Abel.IMBA/AnalysisD/HCAB"
#                      , flag.nameiftrue(p$'premRNA')
#                      , flag.nameiftrue(p$"dSample.Organoids")
#                      , flag.names_list(p$'variables.2.regress')
#                      ,  flag.nameiftrue(p$'Man.Int.Order') )

flag.name_value <- function(toggle, Separator = "_") { # Returns the name and its value, if its not FALSE.
  if (!isFALSE(toggle)) {
    output = paste(substitute(toggle), toggle, sep = Separator)
    if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
    return(output)
  }
}
# Xseed = p$'seed' = F; flag.name_value(Xseed); flag.name_value(p$'seed')

flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") { # Returns the name and its value, if its TRUE.
  output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
  } else {paste0(prefix, name.if.not, suffix)}
  if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true
nameiftrue = flag.nameiftrue # backward compatible

flag.names_list <- function(par = p$'umap.min_dist') { # Returns the name and value of each element in a list of parameters.
  if (length(par)) paste(substitute(par), kppu(par) , sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


flag.names_list.all.new <- function(pl = p.hm) { # Returns the name and value of each element in a list of parameters.
  # if (length(pl)) paste(kppu(names(pl)), kppu(pl) , sep = "_")
  if (length(pl)) kppd(paste(names(pl), pl, sep = "_"))
}


param.list.flag <- function(par = p$'umap.min_dist') { # Returns the name and value of each element in a list of parameters.
  paste(substitute(par), par, sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


## Matrix operations -------------------------------------------------------------------------------------------------

### Matrix calculations  -------------------------------------------------------------------------------------------------
rowMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, median, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.
colMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, median, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.

rowGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, geomean, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.
colGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, geomean, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.

# depend on cv in MarkdownReports
rowCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, cv, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.
colCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, cv, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.

rowVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, var, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.
colVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, var, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.

rowMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, min, na.rm = na.rm) # Calculates the minimum of each row of a numeric matrix / data frame.
colMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, min, na.rm = na.rm) # Calculates the minimum of each column of a numeric matrix / data frame.

rowMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, max, na.rm = na.rm) # Calculates the maximum of each row of a numeric matrix / data frame.
colMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, max, na.rm = na.rm) # Calculates the maximum of each column of a numeric matrix / data frame.

rowSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sem, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
colSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sem, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

rowSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sd, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
colSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sd, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

rowIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, IQR, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
colIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, IQR, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

rowquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 1, quantile, ..., na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
colquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 2, quantile, ..., na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

colDivide <- function(mat, vec) { # divide by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat / vec[col(mat)] # fastest
}

colMutliply <- function(mat, vec) { # Mutliply by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat * vec[col(mat)] # fastest
}


rowDivide <- function(mat, vec) { # divide by row
  stopifnot(NROW(mat) == length(vec))
  mat / vec[row(mat)] # fastest
}

rowMutliply <- function(mat, vec) { # Mutliply by row
  stopifnot(NROW(mat) == length(vec))
  mat * vec[row(mat)] # fastest
}

row.Zscore <- function(DF) t(scale(t(DF))) # Calculate Z-score over rows of data frame.

TPM_normalize <- function(mat, SUM = 1e6) { # normalize each column to 1 million
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * SUM
  return(norm_mat)
}

median_normalize <- function(mat) { # normalize each column to the median of all the column-sums
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * median(cs)
  iprint("colMedians: ", head(signif(colMedians(norm_mat), digits = 3)))
  return(norm_mat)
}

mean_normalize <- function(mat) { # normalize each column to the median of the columns
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * mean(cs)
  iprint("colMeans: ", head(signif(colMeans(norm_mat))))
  return(norm_mat)
}

### Distance and correlation calculations --------------
eucl.dist.pairwise <- function(df2col) { # Calculate pairwise euclidean distance
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

sign.dist.pairwise <- function(df2col) { # Calculate absolute value of the pairwise euclidean distance
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

# Auto correlation functions
rowACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 1, acf, na.action = na_pass,  plot = plot, ...)} # RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.
colACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 2, acf, na.action = na_pass,  plot = plot, ...)} # RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

acf.exactLag <- function(x, lag = 1, na_pass = na.pass, plot = FALSE, ... ) { # Autocorrelation with exact lag
  x = acf(x, na.action = na_pass,  plot = plot, ...)
  x[['acf']][(lag + 1)]
}

rowACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) { # RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.
  signif(apply(x, 1, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}

colACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) { # RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.
  signif(apply(x, 2, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}


### Matrix manipulations -------------------------------------------------------------------------------------------------
rotate <- function(x, clockwise = TRUE) { # rotate a matrix 90 degrees.
  if (clockwise) { t( apply(x, 2, rev))  #first reverse, then transpose, it's the same as rotate 90 degrees
  } else {apply( t(x), 2, rev)}  #first transpose, then reverse, it's the same as rotate -90 degrees:
}

sortEachColumn <- function(data, ...) sapply(data, sort, ...) # Sort each column of a numeric matrix / data frame.

sort.mat <- function(df, colname_in_df = 1, decrease = FALSE, na_last = TRUE) { # Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
  if (length(colname_in_df) > 1) { print("cannot handle multi column sort") }
  else {df[ order(df[, colname_in_df], decreasing = decrease, na.last = na_last), ]}
}

rowNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames) ), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))
}

colNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).
  x = rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames) )
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}

rownames.trimws <- function(matrix1) { # trim whitespaces from the rownames
  rownames(matrix1) = trimws(rownames(matrix1))
  return(matrix1)
}

colsplit <- function(df, f = colnames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ , which(f == levelz[i]) ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}
splitByCol = colsplit

rowsplit <- function(df, f = rownames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ which(f == levelz[i]), ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}


select.rows.and.columns <- function(df, RowIDs = NULL, ColIDs = NULL ) { # Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.
  if (length(RowIDs)) {
    true_rownames = intersect(rownames(df), RowIDs)
    NotFound = setdiff(RowIDs, rownames(df))
    if (length(NotFound)) { iprint(length(NotFound), "Row IDs Not Found:", head(NotFound), "...     Rows found:", length(true_rownames)) } else {iprint("All row IDs found")} #if
    df = df[ true_rownames, ]
  } #if
  if (length(ColIDs)) {
    true_colnames = intersect(colnames(df), ColIDs)
    NotFound = setdiff(ColIDs, colnames(df))
    if (length(NotFound)) { iprint(length(NotFound), "Column IDs Not Found:", head(NotFound), "...     Rows found:", length(true_colnames)) } else {iprint("All column IDs found")}
    df = df[ , true_colnames ]
  } #if
  iprint(dim(df))
  return(df)
}

getRows <- function(mat, rownamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of rows with existing rownames, report how much it could not find.
  idx = intersect(rownamez, row.names(mat))
  if (removeNAonly) { idx = which_names(rowSums(!is.na(mat[ idx, ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(rowSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(rownamez), "are found. Missing: ", length(setdiff(row.names(mat), rownamez))  ) }
  mat[ idx, ]
}

getCols <- function(mat, colnamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of cols with existing colnames, report how much it could not find.
  idx = intersect(colnamez, colnames(mat))
  print(symdiff(colnamez, colnames(mat)))
  if (removeNAonly) {   idx = which_names(colSums(!is.na(mat[ ,idx ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(colSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(colnamez), "are found. Missing: ", length(setdiff(colnames(mat), colnamez))  ) }
  mat[ ,idx ]
}

get.oddoreven <- function(df_ = NULL, rows = FALSE, odd = TRUE) { # Get odd or even columns or rows of a data frame
  counter = if (rows) NROW(df_) else NCOL(df_)
  IDX = if (odd) seq(1, to = counter, by = 2) else seq(2, to = counter, by = 2)
  df_out = if (rows) df_[IDX, ] else df_[, IDX]
  return(df_out)
}

combine.matrices.intersect <- function(matrix1, matrix2, k = 2) { # combine matrices by rownames intersect
  rn1 = rownames(matrix1); rn2 = rownames(matrix2);
  idx = intersect(rn1, rn2)
  llprint(length(idx), "out of", substitute(matrix1), length(rn1), "and", length(rn2), substitute(matrix2), "rownames are merged")
  merged = cbind(matrix1[idx, ], matrix2[idx, ])
  diffz = symdiff(rn1, rn2)
  print("Missing Rows 1, 2")
  x1 = rowSums( matrix1[diffz[[1]], ] )
  x2 = rowSums( matrix2[diffz[[2]], ] ); print("")
  iprint("Values lost 1: ", round(sum(x1)), "or", percentage_formatter(sum(x1)/sum(merged)))
  print(tail(sort(x1), n = 10));print("")
  iprint("Values lost 2: ", round(sum(x2)), "or", percentage_formatter(sum(x2)/sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("dim:", dim(merged)); return(merged)
}


merge_dfs_by_rn <- function(list_of_dfs) { # Merge any data frames by rownames. Required plyr package
  for (i in names(list_of_dfs) ) { colnames(list_of_dfs[[i]]) <- paste0(i,'.',colnames(list_of_dfs[[i]])) } # make unique column names
  for (i in names(list_of_dfs) ) { list_of_dfs[[i]]$rn <- rownames(list_of_dfs[[i]]) } #for
  COMBINED <- plyr::join_all(list_of_dfs, by = 'rn', type = 'full');   idim(COMBINED)
  rownames(COMBINED) = COMBINED$rn
  COMBINED$rn = NULL
  return(COMBINED)
}

merge_numeric_df_by_rn <- function(x, y) { # Merge 2 numeric data frames by rownames
  rn1 = rownames(x); rn2 = rownames(y);
  diffz = symdiff(rn1, rn2)
  merged = merge(x , y, by = "row.names", all = TRUE)  # merge by row names(by = 0 or by = "row.names")
  rownames(merged) = merged$Row.names
  merged = merged[ , -1] # remove row names
  merged[is.na(merged)] <- 0

  print("Uniq Rows (top 10 by sum)")
  x1 = rowSums( x[diffz[[1]], ] )
  x2 = rowSums( y[diffz[[2]], ] ); print("")
  iprint("Values specific to 1: ", round(sum(x1)), "or", percentage_formatter(sum(x1)/sum(merged)))
  print(tail(sort(x1), n = 10));print("")
  iprint("Values specific to 2: ", round(sum(x2)), "or", percentage_formatter(sum(x2)/sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("Dimensions of merged DF:", dim(merged))

  return(merged)
}


remove.na.rows <- function(mat, cols = 1:NCOL(mat)) { # cols have to be a vector of numbers corresponding to columns
  mat2 = mat[ , cols]
  idxOK = which(rowSums(!apply(mat2, 2, is.na)) == NCOL(mat)  )
  mat[idxOK, ]
}

remove.na.cols <- function(mat) { # cols have to be a vector of numbers corresponding to columns
  idxOK = !is.na(colSums(mat))
  return(mat[, idxOK])
}

na.omit.mat <- function(mat, any = TRUE) { # Omit rows with NA values from a matrix. Rows with any, or full of NA-s
  mat = as.matrix(mat)
  stopifnot(length(dim(mat)) == 2)
  if (any) outMat = mat[ !is.na(rowSums(mat)), ]
  else outMat = mat[ (rowSums(is.na(mat)) <= ncol(mat)), ] # keep rows not full with NA
  outMat
}





## List operations -------------------------------------------------------------------------------------------------
any.duplicated.rownames.ls.of.df <- function(ls) any.duplicated(rownames(ls)) # Check if there are any duplocated rownames in a list of dataframes.

intersect.ls <- function(ls, ...) { Reduce(intersect, ls) } # Intersect any number of lists.

union.ls <- function(ls, ...) { sort(unique(do.call(c,ls))) } # Intersect any number of list elements. Faster than reduce.

unlapply <- function(...) { unlist(lapply(...)) } # lapply, then unlist

list.wNames <- function(...) { # create a list with names from ALL variables you pass on to the function
  lst = list(...)
  names(lst) = as.character(match.call()[-1])
  return(lst)
}

as.list.df.by.row <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(as.data.frame(t( dtf ) ) )
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

as.list.df.by.col <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # Split a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(dtf)
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

reorder.list <- function(L, namesOrdered = mixedsort(names(L))) { # reorder elements of lists in your custom order of names / indices.
  Lout = list(NA)
  for (x in 1:length(namesOrdered)) { Lout[[x]] = L[[namesOrdered[x] ]]  }
  if (length(names(L))) { names(Lout) = namesOrdered }
  return(Lout)
}

range.list <- function(L, namesOrdered) { # range of values in whole list
  return(range(unlist(L), na.rm = TRUE))
}

intermingle2lists <- function(L1, L2) { # Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
  stopifnot(length(L1) == length(L2) )
  Lout = list(NA)
  for (x in 1:(2*length(L1)) ) {
    if (x  %% 2) {  Lout[[x]] = L1[[((x + 1) / 2)]]; names(Lout)[x] = names(L1)[((x + 1) / 2)]
    } else {    Lout[[x]] = L2[[x / 2]]; names(Lout)[x] = names(L2)[x / 2]      }
  } # for
  return(Lout)
}

as.listalike <- function(vec, list_wannabe) { # convert a vector to a list with certain dimensions, taken from the list it wanna resemble
  stopifnot(length(vec) == length(unlist(list_wannabe)))
  list_return = list_wannabe
  past = 0
  for (v in 1:length(list_wannabe)) {
    lv = length(list_wannabe[[v]])
    list_return[[v]] = vec[(past + 1):(past + lv)]
    past = past + lv
  } # for
  return(list_return)
}



reverse.list.hierarchy <- function(ll) { # reverse list hierarchy
  ## https://stackoverflow.com/a/15263737
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}


#' list2fullDF.byNames
#' # Convert a list to a full matrix.  Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).
#'
#' @param your.list List of vectors with names elements (see example).
#' @param byRow Transpose output matrix if TRUE.
#' @param FILL Fill missing entries in the matrix with this value. Default: NA.
#' @export
#' @examples list2fullDF.byNames()

list2fullDF.byNames <- function(your.list = list(
  "set.1" = vec.fromNames(LETTERS[1:5], fill = 1),  # Convert a list to a full matrix. Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).
  "set.2" = vec.fromNames(LETTERS[3:9], fill = 2)
), byRow = TRUE, FILL = NA) {
  length.list = length(your.list)
  list.names = names(your.list)
  list.element.names = sort(unique(unlist(lapply(your.list, names))))

  mat = matrix.fromNames(rowname_vec = list.element.names, colname_vec = list.names, fill = FILL)
  for (i in 1:length.list) {
    element = list.names[i]
    mat[ names(your.list[[element]]), element] = your.list[[element]]
  }
  if (!byRow) {mat = t(mat)}
  return(mat)
}

#' list2fullDF.presence
#' # Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
#'
#' @param your.list  List of vector with categorical data (see example).
#' @param byRow Transpose output matrix if TRUE.
#' @param FILL Fill missing entries in the matrix with this value. Default: 0.
#' @export
#' @examples list2fullDF.presence()

list2fullDF.presence <- function(your.list = list("set.1" = LETTERS[1:5]  # Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
                                                  , "set.2" = LETTERS[3:9]), byRow = TRUE, FILL = 0) {
  length.list = length(your.list)
  list.names = names(your.list)
  list.elements = sort(Reduce(f = union, your.list))

  mat = matrix.fromNames(rowname_vec = list.elements, colname_vec = list.names, fill = FILL)
  for (i in 1:length.list) {
    element = list.names[i]
    mat[ your.list[[element]], element] = 1
  }
  if (!byRow) {mat = t(mat)}
  return(mat)
}

splitbyitsnames <- function(namedVec) { # split a list by its names
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(namedVec, f = names(namedVec))
}

splititsnames_byValues <- function(namedVec) { # split a list by its names
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(names(namedVec), f = namedVec)
}

intermingle2vec <- function(V1, V2, wNames = TRUE) { # Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
  stopifnot(length(V1) == length(V2) )
  Vout = c(rbind(V1, V2))
  if (wNames) {names(Vout) = c(rbind(names(V1), names(V2)))}
  return(Vout)
}



intermingle.cbind <- function(df1, df2) { # Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
  stopifnot(ncol(df1) == ncol(df2) )
  if (nrow(df1) != nrow(df2) ) { # not equal rows: subset
    print(symdiff(rownames(df2), rownames(df1)))
    CommonGenes = intersect(rownames(df2), rownames(df1)); print(length(CommonGenes))
    df1 = df1[CommonGenes, ]
    df2 = df2[CommonGenes, ]
  } else { CommonGenes = rownames(df1) }

  # Create New column names
  if (length(colnames(df1)) == ncol(df1) & length(colnames(df2)) == ncol(df2) ) {
    NewColNames = intermingle2vec(paste0("df1.", colnames(df1) ), paste0("df2.", colnames(df2) ))
  } else {
    NewColNames = intermingle2vec(paste0("df1.", 1:ncol(df1) ), paste0("df2.", 1:ncol(df2) ))
  }
  NewMatr = matrix.fromNames(rowname_vec = CommonGenes, colname_vec = NewColNames)
  for (x in 1:(2*length(df1)) ) {
    if (x  %% 2) {  NewMatr[ , x ] = df1[ , (x + 1)/2 ]
    } else {        NewMatr[ , x ] = df2[ , (x)/2 ]      }
  } # for
  print(idim(NewMatr))
  return(NewMatr)
}

ls2categvec <- function(your_list ) { # Convert a list to a vector repeating list-element names, while vector names are the list elements
  VEC = rep(names(your_list),unlapply(your_list, length))
  names(VEC) = unlist(your_list, use.names = TRUE)
  return(VEC)
}


list.2.replicated.name.vec <- function(ListWithNames = Sections.ls.Final) { # Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.
  NZ = names(ListWithNames)
  LZ = unlapply(ListWithNames, length)
  replicated.name.vec = rep(NZ, LZ)
  names(replicated.name.vec) = unlist(ListWithNames)
  return(replicated.name.vec)
}

### Work with multi dimensional lists --------------------------------

copy.dimension.and.dimnames <- function(list.1D, obj.2D) { # copy dimension and dimnames
  dim(list.1D) <- dim(obj.2D)
  dimnames(list.1D) <- dimnames(obj.2D)
  list.1D
}

mdlapply <- function(list_2D, ...) { #  lapply for multidimensional arrays
  x = lapply(list_2D, ...)
  copy.dimension.and.dimnames(x,list_2D)
}


arr.of.lists.2.df <- function(two.dim.arr.of.lists) { # simplify 2D-list-array to a DF
  list.1D = unlist(two.dim.arr.of.lists)
  dim(list.1D) <- dim(two.dim.arr.of.lists)
  dimnames(list.1D) <- dimnames(two.dim.arr.of.lists)
  list.1D
}


mdlapply2df <- function(list_2D, ...) { # multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF)
  x = lapply(list_2D, ...)
  z = copy.dimension.and.dimnames(x,list_2D)
  arr.of.lists.2.df(z)
}



## Set operations -------------------------------------------------------------------------------------------------

symdiff <- function(x, y, ...) { # Quasy symmetric difference of any number of vectors
  big.vec <- c(x, y, ...)
  ls = list(x, y, ...); if ( length(ls) > 2) {print("# Not Mathematically correct, but logical for n>2 vectors: https://en.wikipedia.org/wiki/Symmetric_difference#Properties")}
  names(ls) = paste("Only in", as.character(match.call()[-1]))
  duplicates <- big.vec[duplicated(big.vec)]
  lapply(ls, function(x) setdiff(x, duplicates))
}

## Math & stats -------------------------------------------------------------------------------------------------

sem <- function(x, na.rm = TRUE) sd(unlist(x), na.rm = na.rm)/sqrt(length(na.omit.strip(as.numeric(x))))  # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)

fano <- function(x, na.rm = TRUE, USE = "na.or.complete") var(x, na.rm = na.rm, use = USE )/mean(x, na.rm = na.rm) # Calculates the fano factor on a numeric vector (it excludes NA-s by default)

geomean <- function(x, na.rm = TRUE) { # Calculates the geometric mean of a numeric vector (it excludes NA-s by default)
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x)) }
gm_mean = geomean

mean_of_log <- function(x, k = 2, na.rm = TRUE) { # Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default)
  negs = sum(x < 0);  zeros = sum(x == 0)
  if (negs | zeros) { iprint("The input vector has", negs, "negative values and", zeros, "zeros." ) }
  mean(log(x, base = k), na.rm = na.rm) }

movingAve <- function(x, oneSide = 5) { # Calculates the moving / rolling average of a numeric vector.
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = mean( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}


movingAve2 <- function(x,n = 5) {filter(x,rep(1/n,n), sides = 2)}

movingSEM <- function(x, oneSide = 5) { # Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = sem( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}

imovingSEM <- function(x, oneSide = 5) { # Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
  y = NULL
  for (i in 1:length(x)) {
    oneSideDynamic = min(i - 1, oneSide, length(x) - i); oneSideDynamic
    indexx = (i - oneSideDynamic):(i + oneSideDynamic);indexx
    y[i] = sem( x[ indexx ] )
  };  return(y)
}

shannon.entropy <- function(p) { # Calculate shannon entropy
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p.norm <- p[p > 0]/sum(p) - sum(log2(p.norm)*p.norm)
}


## Plotting and Graphics -----------------------------------------------------------------------------------------------------

legend.col <- function(col, lev) { # Legend color. # Source: https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)

  par(xpd = TRUE)
  for (i in 1:n) {
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
  par(xpd = FALSE)
  # print("You might need to set par('mar' = c( 5.1, 4.1, 4.1, 2.1)) to higher values.")
}


#' val2col
#'
#' This function converts a vector of values("yourdata") to a vector of color levels.
#' One must define the number of colors. The limits of the color scale("zlim") or
#' the break points for the color changes("breaks") can also be defined.
#' When breaks and zlim are defined, breaks overrides zlim.
#' Source: http://menugget.blogspot.nl/2011/09/converting-values-to-color-levels.html
#' @param yourdata The data, to what the colors will be scaled to.
#' @param zlim Limits.
#' @param col Color of the plot.
#' @param breaks Number of bins.
#' @param rename The returned color vector will be named with its previous values
#' @export
#' @examples val2col (yourdata = rpois(200, 20), zlim = c(0,5),col = rev(heat.colors(100)), breaks = 101  )


### CONTAINS A QUICK FIX FOR THE NUMBER OF COLOR LEVELS. See #59 on GitHub ###
val2col <- function(yourdata, # This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. When breaks and zlim are defined, breaks overrides zlim.
                    zlim,
                    col = rev(heat.colors(max(12, 3 * length(unique(yourdata))))),
                    breaks,
                    rename = FALSE) {
  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than color")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(yourdata, na.rm = TRUE)
    zlim[2] <- zlim[2] + c(zlim[2] - zlim[1]) * (0.001)
    zlim[1] <- zlim[1] - c(zlim[2] - zlim[1]) * (0.001)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  colorlevels <- col[((as.vector(yourdata) - breaks[1]) /
                        (range(breaks)[2] - range(breaks)[1])) * (length(breaks) - 1) + 1]
  if (length(names(yourdata))) {
    names(colorlevels) = yourdata
  }

  if (rename) {
    names(colorlevels) = yourdata
  } # works on vectors only"
  colorlevels
}


### Colors -----------------------------------------------------------------------------------------------------
richColors <- function(n = 3) { gplots::rich.colors(n) } # Alias for rich.colors in gplots


Color_Check <- function(..., incrBottMarginBy = 0, savefile = FALSE ) { # Display the colors encoded by the numbers / color-ID-s you pass on to this function
  if (incrBottMarginBy) { .ParMarDefault <- par("mar");   par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]) ) }  # Tune the margin
  Numbers = c(...)
  if (length(names(Numbers)) == length(Numbers)) {labelz = names(Numbers)} else {labelz = Numbers}
  barplot(rep(10, length(Numbers)), col = Numbers, names.arg = labelz, las = 2 )
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}

  fname = substitute(...)
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "ColorCheck.pdf")) }
}

HeatMapCol_BGR <- grDevices::colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1)
# HeatMapCol_BWR <- grDevices::colorRampPalette(c("blue", "white", "red"), bias = 1)
HeatMapCol_RedBlackGreen <- grDevices::colorRampPalette(c("red", "black", "green"), bias = 1)


colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) } # Draw a barplot from ColSums of a matrix.

lm_equation_formatter <- function(lm) { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients);
  kollapse("Intercept: ", eq[1], " Slope: ", eq[2]);
}

lm_equation_formatter2 <- function(lm) { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients, digits = 3);
  kollapse("y = ", eq[2], "* x + ", eq[1]);
}

lm_equation_formatter3 <- function(lm, y.var.name = "y", x.var.name = "x") { # Renders the lm() function's output into a human readable text. (e.g. for subtitles)
  eq = signif(lm$coefficients, digits = 3);
  plusSign = if (sign(eq[1] == 1)) "" else "-"
  kollapse(y.var.name, " = ", eq[2], "*",x.var.name," ",plusSign,"", eq[1]);
}

hist.XbyY <- function(dfw2col = NULL, toSplit = 1:100, splitby = rnorm(100), breaks_ = 20 ) { # Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.
  # http://stackoverflow.com/questions/8853735/get-index-of-the-histogram-bin-in-r
  if (NCOL(dfw2col) == 2) { toSplit = dfw2col[ , 1]; splitby = dfw2col[ , 2]; print(11) }
  xx = hist(splitby, breaks = breaks_, plot = TRUE)
  IDX = findInterval(x = splitby, vec = xx$breaks)
  ls = split(toSplit, IDX)
  iprint("Range of data:", range(xx$breaks))
  names(ls) = xx$breaks[-1]
  return(ls)
}#  ll = hist.XbyY(); wbarplot(unlapply(ll, length))

### Functions for pairs() plots  -----------------------------------------------------------------------------------------------------
panel.cor.pearson <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "pearson") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex,  col = 2)
}

panel.cor.spearman <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "spearman") { # A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex, col = 2)
}


quantile_breaks <- function(xs, n = 10, na.Rm = FALSE) { # Quantile breakpoints in any data vector http://slowkow.com/notes/heatmap-tutorial/
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n), na.rm = na.Rm)
  breaks[!duplicated(breaks)]
}




## Clustering heatmap tools -----------------------------------------------------------------------------------------------------

hclust.getOrder.row <- function(pheatmapObject) pheatmapObject$tree_row$labels[pheatmapObject$tree_row$order] # Extract ROW order from a pheatmap object.
hclust.getOrder.col <- function(pheatmapObject) pheatmapObject$tree_col$labels[pheatmapObject$tree_col$order] # Extract COLUMN order from a pheatmap object.

hclust.getClusterID.row <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_row, k = k) # Extract cluster ID's for ROWS of a pheatmap object.
hclust.getClusterID.col <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_col, k = k) # Extract cluster ID's for COLUMNS of a pheatmap object.

hclust.ClusterSeparatingLines.row <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_row, k = k)[pheatmapObject$tree_row$order])[-1]) # Calculate the position of ROW separating lines between clusters in a pheatmap object.
hclust.ClusterSeparatingLines.col <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_col, k = k)[pheatmapObject$tree_col$order])[-1]) # Calculate the position of COLUMN separating lines between clusters in a pheatmap object.

Gap.Postions.calc.pheatmap <- function(annot.vec.of.categories) { # calculate gap positions for pheatmap, based a sorted annotation vector of categories
  NAZ = sum(is.na(annot.vec.of.categories))
  if (NAZ) iprint("There are", NAZ, "NA values in your vector. They should be last and they are omitted.")
  consecutive.lengthes = rle( na.omit.strip(annot.vec.of.categories))$lengths
  cumsum(consecutive.lengthes) # return abs.positions
}

matlabColors.pheatmap <- function(matrixx, nr = 50) {colorRamps::matlab.like(length(quantile_breaks(matrixx, n = nr)) - 1)} # Create a Matlab-like color gradient using "colorRamps".

"FOR VECTOR. it works"
annot_col.create.pheatmap.vec <- function(data, annot_vec, annot_names = "Annot") { # For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopifnot( length(annot_vec) == dim(data)[2] )
  namez = as.character(if (is.null(annot_names)) substitute(annot_vec) else annot_names)

  df = data.frame(x = annot_vec); df[, 1] = as.character(df[, 1])
  names(df) = namez # colnames but more flexible
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  tt = table(annot_vec); nz = names(tt)
  if (is.numeric(annot_vec)) {
    coll = val2col(annot_vec[!duplicated(annot_vec)]); names(coll) = nz
  } else {
    coll = gplots::rich.colors(length(tt)); names(coll) = nz
  }
  col_list = list(annot_vec = coll)
  names(col_list) = namez
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}


annot_col.create.pheatmap.df <- function(data, annot_df_per_column, annot_names = NULL) { # For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopif( dim(annot_df_per_column)[1] != dim(data)[2] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_column)
  if (any(rownames(df) != colnames(data))) { print("The rownames of annot_df_per_column are not the same as the colnames of data:")
    print(cbind("rownames(df)" = rownames(df) , "colnames(data)" = colnames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_column) else annot_names)

  colnames(df) = namez
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}

annot_col.fix.numeric <- function(ListOfColnames) { # fix class and color annotation in pheatmap annotation data frame's and lists.
  for (i in 1:length(ListOfColnames) ) {
    j = ListOfColnames[i]
    annot[[j]] = as.numeric(annot[[j]])
    annot_col[[j]] = NULL # remove fixed colors -> auto determine by pheatmap
  } #for
  assign(x = "annot_col", value = annot_col, envir = .GlobalEnv)
  iprint("Columns in annot are as.numeric(), list elements in annot_col are removed")
}


annot_row.create.pheatmap.df <- function(data, annot_df_per_row, annot_names = NULL) { # For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap
  stopif( dim(annot_df_per_row)[1] != dim(data)[1] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_row)
  if (any(rownames(df) != rownames(data))) { print("The rownames of annot_df_per_row are not the same as the rownames of data:")
    print(cbind("rownames(df)" = rownames(df) , "rownames(data)" = rownames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_row) else annot_names)

  colnames(df) = namez
  rownames(df) = rownames(data)
  assign(x = "annot_rows", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_rows.col", value = col_list, envir = .GlobalEnv)

  print("annot_rows [data frame] and annot_rows.col [list] variables are created. Use: pheatmap(..., annotation_row = annot_rows, annotation_colors = annot_rows.col)")
}



# Search query links ------------------------------------------------------------------------

# Google search URL / search query links
# b.dbl.writeOut = F
# b.dbl.Open = F

link_google <- function(vector_of_gene_symbols #  Parse google search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).
                        , google = "http://www.google.com/search?as_q = ", prefix = "", suffix = ""
                        , writeOut = b.dbl.writeOut, Open = b.dbl.Open, sleep = 0) {
  links = paste0( google, prefix," ", vector_of_gene_symbols," ", suffix)
  if (writeOut) {
    bash_commands = paste0("open '", links, "'")
    if (sleep > 0) { bash_commands = paste0(bash_commands, ' ; sleep ', sleep) } # if wait
    write.simple.append("", ManualName = BashScriptLocation)
    write.simple.append(bash_commands, ManualName = BashScriptLocation)
  } else if (Open) { for (linkX in links) Sys.sleep(0.3 + runif(1)); browseURL(linkX, encodeIfNeeded = T) } else {return(links)}
}


# link.google.clipboard = clipr::write_clip(link_google(clipr::read_clip()))


# Bing search URL / search query links
link_bing <- function(vector_of_gene_symbols #  Parse bing search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).
                      , bing = "https://www.bing.com/search?q = ", prefix = "", suffix = ""
                      , writeOut = b.dbl.writeOut, Open = b.dbl.Open, sleep = 0) {
  links = paste0( bing, prefix," ", vector_of_gene_symbols," ", suffix)
  if (writeOut) {
    bash_commands = paste0("open '", links, "'")
    if (sleep > 0) { bash_commands = paste0(bash_commands, ' ; sleep ', sleep) } # if wait
    write.simple.append("", ManualName = BashScriptLocation)
    write.simple.append(bash_commands, ManualName = BashScriptLocation)
  } else if (Open) { for (linkX in links) Sys.sleep(0.3 + runif(1)); browseURL(linkX, encodeIfNeeded = T) } else {return(links)}
}

# Biology ------------------------------------------------------------

GC_content <- function(string, len = nchar(string), pattern = c("G","C")) { # GC-content of a string (frequency of G and C letters among all letters).
  char.list <- stringr::str_split_fixed(string, pattern = "", n = nchar(string))
  tbl = table(factor(unlist(char.list), levels = c("A", "T", "G", "C")))
  sum(tbl[  pattern ]) / sum(tbl)
}


# Temporary  ------------------------------------------------------------

# TMP ------------------------------------------------------------------------------------------------



sourceGitHub <- function(script = "Cell.cycle.scoring.R"
                         , repo = "Seurat.Pipeline"
                         , folder = "elements"
                         , user = "vertesy"
                         , rawpath = "https://raw.githubusercontent.com"
                         , suffix = "master"
                         , token = NULL, ...) { # Source from GitHub. Example https://raw.githubusercontent.com/vertesy/Seurat.Pipeline/main/elements/Cell.cycle.scoring.R
  path.part = FixPath(kpps(user, repo, suffix, folder, script))
  fullpath = RemoveFinalSlash(kpps(rawpath, path.part))
  if (!is.null(token)) fullpath = p0(fullpath, token)
  print(fullpath)
  source(fullpath)
}
# sourceGitHub(script = , repo = "Seurat.Pipeline", folder = "elements" )
#
# RemoveFinalSlash <- function(string = "stairway/to/heaven/") { #
#   gsub(x = string, pattern = '/$', replacement = '')
# }
#



#' ww.set.OutDir
#'
#' Checks if global variable OutDir is defined. If not,
#' it returns the current working directory
#' @export
#'
#' @examples ww.set.OutDir()

ww.set.OutDir <- function() {
  if (exists("OutDir"))   iprint("OutDir not defined !!! Saving in working directory.")
  if (dir.exists(OutDir)) iprint("OutDir defined, but folder does not exist!!! Saving in working directory.")
  NewOutDir =
    if (exists("OutDir")  & dir.exists(OutDir)) { OutDir
    } else {     paste0(getwd(),"/", collapse = "")}
  FixPath(NewOutDir)
}



backup <- function(obj) { # make a backup of an object into global env. Scheme: obj > obj.bac
  varname <- as.character(substitute(obj))
  bac.varname <- ppp(varname, "bac")
  if (exists(bac.varname)) {
    print(" Backup already exists.")
  } else {
    iprint(varname, "is backep up into:", bac.varname)
    assign(x = bac.varname, value = obj, envir= as.environment(1) )
  }
}
# backup(combined.obj)



list.dirs.depth.n <- function(dir = '.' , depth = 2) { # list dirs recursive up to a certain level in R https://stackoverflow.com/questions/48297440/list-files-recursive-up-to-a-certain-level-in-r
  iprint("Scanning directories. Depth:", depth)
  res <- list.dirs(dir, recursive = FALSE)
  if (depth > 1) {
    add <- list.dirs.depth.n(res, depth - 1)
    c(res, add)
  } else {
    res
  }
}
# list.dirs.depth.n(depth = 3)
