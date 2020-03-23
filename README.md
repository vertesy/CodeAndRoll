# CodeAndRoll
A collection of custom R functions. Works with MarkdownReports, but also as standalone. Many other repos/libraries of mine may have dependency on these functions. Source: own work + web (source referenced in description and/or source code). Intended for my personal use, shared because others may find (parts of it) useful.



## List of Functions

Note that this library is under continous development. Thus not all functions here may be still in CodeAndRoll.R, and vice versa, new functions in CodeAndRoll.R may not be listed here.
Backward compatibility is most often, but not always taken care of. See other files in the repo if you are missing a function.



1. #### `ppp()`:

   Paste by point

1. #### `pps()`:

   Paste by (forward) slash

1. #### `ppu()`:

   Paste by underscore

1. #### `ppd()`:

   Paste by dash

1. #### `kpp()`:

   kollapse by point

1. #### `kppu()`:

   kollapse by underscore

1. #### `kppd()`:

   kollapse by dash

1. #### `stry()`:

   Silent try

1. #### `say()`:

   Use system voice to notify (after a long task is done)

1. #### `sayy()`:

   Use system voice to notify (after a long task is done)

1. #### `grepv()`:

   grep returning the value.

1. #### `unload()`:

   Unload a package. Source: https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r

1. #### `clip2clip.vector()`:

   Copy from clipboard (e.g. excel) to a R-formatted vector to the  clipboard

1. #### `clip2clip.commaSepString()`:

   read a comma separated string (e.g. list of gene names) and properly format it for R.

1. #### `read.simple.vec()`:

   Read each line of a file to an element of a vector (read in new-line separated values, no header!).

1. #### `read.simple()`:

   It is essentially read.table() with file/path parsing.

1. #### `read.simple_char_list()`:

   Read in a file.

1. #### `read.simple.table()`:

   Read in a file. default: header defines colnames, no rownames. For rownames give the col nr. with rownames, eg. 1 The header should start with a TAB / First column name should be empty.

1. #### `FirstCol2RowNames()`:

   Set First Col to Row Names

1. #### `read.simple.tsv()`:

   Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

1. #### `read.simple.csv()`:

   Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

1. #### `read.simple.ssv()`:

   Space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

1. #### `read.simple.tsv.named.vector()`:

   Read in a file with excel style named vectors, names in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.

1. #### `convert.tsv.data()`:

   Fix NA issue in dataframes imported by the new read.simple.tsv. Set na_rep to NA if you want to keep NA-s

1. #### `read.simple.xls()`:

   Read multi-sheet excel files. row_namePos = NULL for automatic names

1. #### `write.simple()`:

   Write out a matrix-like R-object to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

1. #### `write.simple.vec()`:

   Write out a vector-like R-object to a file with as newline separated values (.vec). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

1. #### `write.simple.xlsx()`:

   Write out a list of matrices/ data frames WITH ROW- AND COLUMN- NAMES to a file with as an Excel (.xslx) file. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

1. #### `write.simple.append()`:

   Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of the same number of columns. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

1. #### `sstrsplit()`:

   Alias for str_split_fixed in the stringr package

1. #### `topN.dfCol()`:

   Find the n highest values in a named vector

1. #### `bottomN.dfCol()`:

   Find the n lowest values in a named vector

1. #### `as.named.vector()`:

   Convert a dataframe column or row into a vector, keeping the corresponding dimension name.

1. #### `col2named.vector()`:

   Convert a dataframe column into a vector, keeping the corresponding dimension name.

1. #### `row2named.vector()`:

   Convert a dataframe row into a vector, keeping the corresponding dimension name.

1. #### `as.numeric.wNames()`:

   Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

1. #### `as.numeric.wNames.old()`:

   Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

1. #### `as.character.wNames()`:

   Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.

1. #### `rescale()`:

   linear transformation to a given range of values

1. #### `flip_value2name()`:

   Flip the values and the names of a vector with names

1. #### `sortbyitsnames()`:

   Sort a vector by the alphanumeric order of its names (instead of its values).

1. #### `any.duplicated()`:

   How many entries are duplicated

1. #### `which.duplicated()`:

   orig =rownames(sc@expdata)

1. #### `which.NA()`:

   orig =rownames(sc@expdata)

1. #### `which_names()`:

   Return the names where the input vector is TRUE. The input vector is converted to logical.

1. #### `which_names_grep()`:

   Return the vector elements whose names are partially matched

1. #### `na.omit.mat()`:

   Omit rows with NA values from a matrix. Rows with any, or full of NA-s

1. #### `inf.omit()`:

   Omit infinite values from a vector.

1. #### `zero.omit()`:

   Omit zero values from a vector.

1. #### `pc_TRUE()`:

   Percentage of true values in a logical vector, parsed as text (useful for reports.)

1. #### `NrAndPc()`:

   Summary stat. text formatting for logical vectors (%, length)

1. #### `pc_in_total_of_match()`:

   Percentage of a certain value within a vector or table.

1. #### `filter_survival_length()`:

   Parse a sentence reporting the % of filter survival.

1. #### `remove_outliers()`:

   Remove values that fall outside the trailing N % of the distribution.

1. #### `simplify_categories()`:

   Replace every entry that is found in "replaceit", by a single value provided by "to"

1. #### `rotate()`:

   rotate a matrix 90 degrees.

1. #### `sortEachColumn()`:

   Sort each column of a numeric matrix / data frame.

1. #### `rowMedians()`:

   Calculates the median of each row of a numeric matrix / data frame.

1. #### `colMedians()`:

   Calculates the median of each column of a numeric matrix / data frame.

1. #### `rowGeoMeans()`:

   Calculates the median of each row of a numeric matrix / data frame.

1. #### `colGeoMeans()`:

   Calculates the median of each column of a numeric matrix / data frame.

1. #### `rowCV()`:

   Calculates the CV of each ROW of a numeric matrix / data frame.

1. #### `colCV()`:

   Calculates the CV of each column of a numeric matrix / data frame.

1. #### `rowVariance()`:

   Calculates the CV of each ROW of a numeric matrix / data frame.

1. #### `colVariance()`:

   Calculates the CV of each column of a numeric matrix / data frame.

1. #### `rowMin()`:

   Calculates the minimum of each row of a numeric matrix / data frame.

1. #### `colMin()`:

   Calculates the minimum of each column of a numeric matrix / data frame.

1. #### `rowMax()`:

   Calculates the maximum of each row of a numeric matrix / data frame.

1. #### `colMax()`:

   Calculates the maximum of each column of a numeric matrix / data frame.

1. #### `rowSEM()`:

   Calculates the SEM of each row of a numeric matrix / data frame.

1. #### `colSEM()`:

   Calculates the SEM of each column of a numeric matrix / data frame.

1. #### `rowSD()`:

   Calculates the SEM of each row of a numeric matrix / data frame.

1. #### `colSD()`:

   Calculates the SEM of each column of a numeric matrix / data frame.

1. #### `rowIQR()`:

   Calculates the SEM of each row of a numeric matrix / data frame.

1. #### `colIQR()`:

   Calculates the SEM of each column of a numeric matrix / data frame.

1. #### `rowquantile()`:

   Calculates the SEM of each row of a numeric matrix / data frame.

1. #### `colquantile()`:

   Calculates the SEM of each column of a numeric matrix / data frame.

1. #### `row.Zscore()`:

   Calculate Z-score over rows of data frame.

1. #### `rowACF()`:

   RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

1. #### `colACF()`:

   RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

1. #### `acf.exactLag()`:

   Autocorrelation with exact lag

1. #### `rowACF.exactLag()`:

   RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

1. #### `colACF.exactLag()`:

   RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

1. #### `colDivide()`:

   divide by column

1. #### `rowDivide()`:

   divide by row

1. #### `sort.mat()`:

   Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r

1. #### `rowNameMatrix()`:

   Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).

1. #### `colNameMatrix()`:

   Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).

1. #### `colsplit()`:

   split a data frame by a factor corresponding to columns.

1. #### `rowsplit()`:

   split a data frame by a factor corresponding to columns.

1. #### `TPM_normalize()`:

   normalize each column to 1 million

1. #### `median_normalize()`:

   normalize each column to the median of all the column-sums

1. #### `mean_normalize()`:

   normalize each column to the median of the columns

1. #### `rownames.trimws()`:

   trim whitespaces from the rownames

1. #### `select.rows.and.columns()`:

   Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

1. #### `getRows()`:

   Get the subset of rows with existing rownames, report how much it could not find.

1. #### `getCols()`:

   Get the subset of cols with existing colnames, report how much it could not find.

1. #### `get.oddoreven()`:

   Get odd or even columns or rows of a data frame

1. #### `combine.matrices.intersect()`:

   combine matrices by rownames intersect

1. #### `merge_dfs_by_rn()`:

   Merge any data frames by rownames. Required plyr package

1. #### `merge_numeric_df_by_rn()`:

   Merge 2 numeric data frames by rownames

1. #### `attach_w_rownames()`:

   Take a data frame (of e.g. metadata) from your memory space, split it into vectors so you can directly use them. E.g.: Instead of metadata$color[blabla] use color[blabla]

1. #### `panel.cor.pearson()`:

   A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".

1. #### `panel.cor.spearman()`:

   A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".

1. #### `remove.na.rows()`:

   cols have to be a vector of numbers corresponding to columns

1. #### `remove.na.cols()`:

   cols have to be a vector of numbers corresponding to columns

1. #### `intersect.ls()`:

   Intersect any number of lists.

1. #### `union.ls()`:

   Intersect any number of list elements. Faster than reduce.

1. #### `unlapply()`:

   lapply, then unlist

1. #### `list.wNames()`:

   create a list with names from ALL variables you pass on to the function

1. #### `as.list.df.by.row()`:

   Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.

1. #### `as.list.df.by.col()`:

   oSplit a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.

1. #### `reorder.list()`:

   reorder elements of lists in your custom order of names / indices.

1. #### `range.list()`:

   range of values in whole list

1. #### `intermingle2lists()`:

   Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

1. #### `as.listalike()`:

   convert a vector to a list with certain dimensions, taken from the list it wanna resemble

1. #### `list2fullDF.byNames()`:

   Convert a list to a full matrix. Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).

1. #### `list2fullDF.presence()`:

   Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).

1. #### `splitbyitsnames()`:

   split a list by its names

1. #### `splititsnames_byValues()`:

   split a list by its names

1. #### `intermingle2vec()`:

   Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

1. #### `intermingle.cbind()`:

   Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

1. #### `pad.na()`:

   Fill up with a vector to a given length with NA-values at the end.

1. #### `clip.values()`:

   Signal clipping. Cut values above or below a threshold.

1. #### `clip.outliers()`:

   Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.

1. #### `ls2categvec()`:

   Convert a list to a vector repeating list-element names, while vector names are the list elements

1. #### `symdiff()`:

   Quasy symmetric difference of any number of vectors

1. #### `sem()`:

   Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)

1. #### `fano()`:

   Calculates the fano factor on a numeric vector (it excludes NA-s by default)

1. #### `geomean()`:

   Calculates the geometric mean of a numeric vector (it excludes NA-s by default)

1. #### `mean_of_log()`:

   Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default)

1. #### `movingAve()`:

   Calculates the moving / rolling average of a numeric vector.

1. #### `movingAve2()`:

1. #### `movingSEM()`:

   Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

1. #### `imovingSEM()`:

   Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.

1. #### `eval_parse_kollapse()`:

   evaluate and parse (dyn_var_caller)

1. #### `lookup()`:

   Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.

1. #### `richColors()`:

   Alias for rich.colors in gplots

1. #### `Color_Check()`:

   Display the colors encoded by the numbers / color-ID-s you pass on to this function

1. #### `colSums.barplot()`:

   Draw a barplot from ColSums of a matrix.

1. #### `lm_equation_formatter()`:

   Renders the lm() function's output into a human readable text. (e.g. for subtitles)

1. #### `lm_equation_formatter2()`:

   Renders the lm() function's output into a human readable text. (e.g. for subtitles)

1. #### `lm_equation_formatter3()`:

   Renders the lm() function's output into a human readable text. (e.g. for subtitles)

1. #### `hist.XbyY()`:

   Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.

1. #### `flag.name_value()`:

   returns the name and its value, if its not FALSE.

1. #### `flag.nameiftrue()`:

   Returns the name and its value, if its TRUE.

1. #### `flag.names_list()`:

   Returns the name and value of each element in a list of parameters.

1. #### `param.list.flag()`:

   Returns the name and value of each element in a list of parameters.

1. #### `quantile_breaks()`:

   Quantile breakpoints in any data vector http://slowkow.com/notes/heatmap-tutorial/

1. #### `vec.fromNames()`:

   create a vector from a vector of names

1. #### `list.fromNames()`:

   create list from a vector with the names of the elements

1. #### `matrix.fromNames()`:

   Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

1. #### `matrix.fromVector()`:

   Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.

1. #### `array.fromNames()`:

   create an N-dimensional array from N vectors defining the row-, column, etc names of the array

1. #### `what()`:

   A better version of is(). It can print the first "printme" elements.

1. #### `idim()`:

   A dim() function that can handle if you pass on a vector: then, it gives the length.

1. #### `idimnames()`:

   A dimnames() function that can handle if you pass on a vector: it gives back the names.

1. #### `table_fixed_categories()`:

   generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.

1. #### `stopif2()`:

   Stop script if the condition is met. You can parse anything (e.g. variables) in the message

1. #### `most_frequent_elements()`:

   Show the most frequent elements of a table

1. #### `top_indices()`:

   Returns the position / index of the n highest values. For equal values, it maintains the original order

1. #### `percentile2value()`:

   Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.

1. #### `MaxN()`:

   find second (third…) highest/lowest value in vector

1. #### `hclust.getOrder.row()`:

   Extract ROW order from a pheatmap object.

1. #### `hclust.getOrder.col()`:

   Extract COLUMN order from a pheatmap object.

1. #### `hclust.getClusterID.row()`:

   Extract cluster ID's for ROWS of a pheatmap object.

1. #### `hclust.getClusterID.col()`:

   Extract cluster ID's for COLUMNS of a pheatmap object.

1. #### `hclust.ClusterSeparatingLines.row()`:

   Calculate the position of ROW separating lines between clusters in a pheatmap object.

1. #### `hclust.ClusterSeparatingLines.col()`:

   Calculate the position of COLUMN separating lines between clusters in a pheatmap object.

1. #### `Gap.Postions.calc.pheatmap()`:

   calculate gap positions for pheatmap, based a sorted annotation vector of categories

1. #### `matlabColors.pheatmap()`:

   Create a Matlab-like color gradient using "colorRamps".

1. #### `annot_col.create.pheatmap.vec()`:

   For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

1. #### `annot_col.create.pheatmap.df()`:

   For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

1. #### `annot_col.fix.numeric()`:

   fix class and color annotation in pheatmap annotation data frame's and lists.

1. #### `annot_row.create.pheatmap.df()`:

   For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

1. #### `wPairConnector()`:

   Connect Pairs of datapoints with a line on a plot.

1. #### `numerate()`:

   numerate from x to y with additonal zeropadding

1. #### `printEveryN()`:

   Report at every e.g. 1000

1. #### `zigzagger()`:

   mix entries so that they differ

1. #### `irequire()`:

   Load a package. If it does not exist, try to install it from CRAN.

1. #### `IfExistsAndTrue()`:

   Internal function. Checks if a variable is defined, and its value is TRUE.

1. #### `filter_InCircle()`:

   Find points in/out-side of a circle.

1. #### `cumsubtract()`:

   Cumulative subtraction, opposite of cumsum()

1. #### `trail()`:

   A combination of head() and tail() to see both ends.

1. #### `sort.decreasing()`:

   Sort in decreasing order.

1. #### `list.2.replicated.name.vec()`:

   Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.

1. #### `idate()`:

   Parse current date, dot separated.

1. #### `view.head()`:

   view the head of an object by console.

1. #### `view.head2()`:

   view the head of an object by View().

1. #### `iidentical.names()`:

   Test if names of two objects for being exactly equal

1. #### `iidentical()`:

   Test if two objects for being exactly equal

1. #### `iidentical.all()`:

   Test if two objects for being exactly equal.

1. #### `parsepvalue()`:

   Parse p-value from a number to a string.

1. #### `shannon.entropy()`:

   Calculate shannon entropy

1. #### `id2titlecaseitalic()`:

   Convert a gene ID to title case italic

1. #### `id2titlecaseitalic.sp()`:

   Convert a gene ID to italic

1. #### `id2name()`:

   Convert a gene ID to a gene name (symbol). From / for RaceID.

1. #### `id2chr()`:

   Convert a gene ID to the chromosome. From / for RaceID.

1. #### `name2id()`:

   Convert an name to gene ID. From / for RaceID.

1. #### `name2id.toClipboard()`:

   Convert an name to gene ID, anc copy to clipboard. From / for RaceID.

1. #### `name2id.fast()`:

   Convert an name to gene ID. From / for RaceID.

1. #### `legend.col()`:

   Legend color. # Source: https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/

1. #### `copy.dimension.and.dimnames()`:

   copy dimension and dimnames

1. #### `mdlapply()`:

   lapply for multidimensional arrays

1. #### `arr.of.lists.2.df()`:

   simplify 2D-list-array to a DF

1. #### `mdlapply2df()`:

   multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF)

1. #### `memory.biggest.objects()`:

   Show distribution of the largest objects and return their names

1. #### `na.omit.strip()`:

   Calls na.omit() and returns a clean vector

1. #### `md.LinkTable()`:

   Take a dataframe where every entry is a string containing an html link, parse and write out

1. #### `link_google()`:

   Parse wormbase database links to your list of gene symbols. "additional_terms" can be any vector of strings that will be searched for together with each gene.

1. #### `link_bing()`:

   Parse wormbase database links to your list of gene symbols. "additional_terms" can be any vector of strings that will be searched for together with each gene.

1. #### `val2col()`:

   This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. When breaks and zlim are defined, breaks overrides zlim.

1. #### `as.logical.wNames()`:

   Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

1. #### `iterBy.over()`:

   Iterate over a vector by every N-th element.

1. #### `sourcePartial()`:

   Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file

1. #### `oo()`:

   Open current working directory.

1. #### `jjpegA4()`:

   Setup an A4 size jpeg

1. #### `param.list.2.fname()`:

   Take a list of parameters and parse a string from their names and values.

1. #### `GC_content()`:

   GC-content of a string (frequency of G and C letters among all letters).

1. #### `eucl.dist.pairwise()`:

   Calculate pairwise euclidean distance

1. #### `sign.dist.pairwise()`:

   Calculate absolute value of the pairwise euclidean distance

1. #### `reverse.list.hierarchy()`:

   reverse list hierarchy

1. #### `extPDF()`:

   add pdf as extension to a file name

1. #### `extPNG()`:

   add png as extension to a file name

1. #### `col2named.vec.tbl()`:

   Convert a 2-column table (data frame) into a named vector. 1st column will be used as names.