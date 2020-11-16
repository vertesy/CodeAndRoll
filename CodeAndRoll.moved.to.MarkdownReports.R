# Moved to MarkdownReports
# modus <- function(x) { # Calculates the modus of a numeric vector (it excludes NA-s by default)
#   x= unlist(na.exclude(x))
#   ux <- unique(x)
#   tab <- tabulate(match(x, ux));
#   ux[tab == max(tab)]
# }

# Moved to MarkdownReports
# cv <- function(x, na.rm=TRUE) sd( x, na.rm=na.rm)/mean(x, na.rm=na.rm) # Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default)

# Moved to MarkdownReports
# substrRight <- function(x, n) substr(x, nchar(x)-n+1, nchar(x)) # Take the right substring of a string

# Moved to MarkdownReports
# write.simple.tsv <- function(input_df, extension='tsv', ManualName ="", o = FALSE, gzip = FALSE ,...  ) { # Write out a matrix-like R-object WITH ROW- AND COLUMN- NAMES to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
#   fname = kollapse (..., print = FALSE); if (nchar (fname) < 2 ) { fname = substitute(input_df) }
#   if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  { FnP = ww.FnP_parser (fname, extension) }
#   write.table (input_df, file = FnP, sep = "\t", row.names = TRUE, col.names = NA, quote=FALSE  )
#   printme = if (length(dim(input_df))) paste0("Dim: ", dim(input_df) ) else paste0("Length (of your vector): ", length(input_df) )
#   iprint (printme)
#   if (o) { system(paste0("open ", FnP), wait = FALSE) }
#   if (gzip) { system(paste0("gzip ", FnP), wait = FALSE) }
# } # fun
# # If col.names = NA and row.names = TRUE a blank column name is added, which is the convention used for CSV files to be read by spreadsheets.


# Moved to MarkdownReports
# as.factor.numeric <- function(vec, rename=FALSE) { # Turn any vector into numeric categories as.numeric(as.factor(vec))
#   vec2 = as.numeric(as.factor(vec)) ;
#   names (vec2) =  if ( !rename & !is.null(names(vec) ) ) names (vec) else vec; return(vec2)
#   }

# Moved to MarkdownReports
# as.logical.wNames <- function(vec) { # Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
#   numerified_vec = as.logical(vec)
#   if (!is.null(names(vec))) {names (numerified_vec) = names (vec)}
#   return(numerified_vec)
# }

# Moved to MarkdownReports
# unique.wNames <- function(vec_w_names) {  vec_w_names[!duplicated(vec_w_names)] } # Get the unique elements from a vector keeping names (of the first elements of each category).



# ### 2017.12.12 FIX:   if (WriteOut) { write.simple.tsv(df, ManualName = paste0(substitute(df),".tsv")) }
#
# md.tableWriter.DF.w.dimnames <- function(df, FullPath = path_of_report, percentify = FALSE, title_of_table = NA, print2screen=FALSE, WriteOut =FALSE) {
#   if (is.na(title_of_table)) {    t = paste0(substitute(df), collapse = " ")
#   } else {                        t = title_of_table  }
#
#   title_of_table = paste("\n#### ", t)
#   # if (file.exists(FullPath)) {
#   write(title_of_table, FullPath, append = TRUE)
#   # } else { print("NOT LOGGED: Log path and filename is not defined in FullPath")  }
#   h = paste(colnames(df), collapse = " \t| ")
#   h = paste("\n| |", h, " |", collapse = "")
#   ncolz = dim(df)[2] + 1
#   nrows = dim(df)[1]
#   rn = rownames(df)
#   sep = kollapse(rep("| ---", ncolz), " |", print = FALSE)
#
#   write(h, FullPath, append = TRUE)
#   write(sep, FullPath, append = TRUE)
#   for (r in 1:nrows) {
#     if (is.numeric(unlist(df[r, ]))) {
#       b = iround(df[r, ])
#       if (percentify) {  b = percentage_formatter(b)  }
#     } else {  b = df[r, ] }
#
#     b = paste(b, collapse = " \t| ")
#     b = paste("|", rn[r], "\t|", b, " |", collapse = "")
#     write(b, FullPath, append = TRUE)
#   }
#
#   if (WriteOut) { write.simple.tsv(df, ManualName = paste0(substitute(df),".tsv")) }
#   if (print2screen) { print(b) }
# }
#

# Moved to Markdownreports dev
# md.List2Table <- function(parameterlist,
#                            title="List elements",
#                            colname2="Value",
#                            maxlen = 20) {
#   LZ = unlist(lapply(parameterlist, length)) # collapse paramters with multiple entires
#   LNG = names(which(LZ > 1))
#   for (i in LNG) {
#     if (length(parameterlist[[i]]) > maxlen)
#       parameterlist[[i]] = parameterlist[[i]][1:maxlen]
#     parameterlist[[i]] = paste(parameterlist[[i]], collapse = ", ")
#   } #for
#   DF = t(as.data.frame(parameterlist))
#   colnames(DF) = colname2
#   md.tableWriter.DF.w.dimnames(DF, title_of_table = title)
# }

#'
#' #' md.LinkTable
#' #'
#' #' Take a dataframe where every entry is a string containing an html link, parse and write out
#' #'  a properly formatted markdown table
#' #' @param tableOfLinkswRownames a dataframe where every entry is a string containing an html link
#' #' @export
#' #'
#' #' @examples tableOfLinkswRownames(tableOfLinkswRownames = df_of_LinksParsedByDatabaseLinkeR)
#'
#' md.LinkTable <- function(tableOfLinkswRownames) { # Take a dataframe where every entry is a string containing an html link, parse and write out
#'   TBL = tableOfLinkswRownames
#'   RN = rownames(tableOfLinkswRownames)
#'   for (i in 1:ncol(tableOfLinkswRownames)) {
#'     x = tableOfLinkswRownames[, i]
#'     TBL[, i] = paste0("[", RN, "]", "(", x, ")")
#'   } #for
#'   md.tableWriter.DF.w.dimnames(TBL,
#'                                FullPath = paste0(OutDir, substitute(tableOfLinkswRownames), ".tsv.md"))
#' }
