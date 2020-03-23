######################################################################
# Less often used R functions from CodeAndRoll.R
######################################################################
# source ('~/GitHub/CodeAndRoll/CodeAndRoll.other.functions.R')

wA4 = 8.27 # A4 inches
hA4 =11.69

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

my_render <- function(input, encoding) { # For Rmarkdown to keep the markdown file after rendering. Source: https://github.com/rstudio/rmarkdown/issues/107
  rmarkdown::render(input, clean = FALSE, encoding = encoding)
}


# Then add this to your metadata:
# output: html_document
# knit: my_render



# ------------------------------------------------------------------------

#' whist.back2back
#'
#' Two back-to-back histograms from a list. The X-axis is only correct if  breaks1 ==breaks2.
#' Undeveloped function, contains graphical bugs, no support for this function.
#' @param ListOf2 List of 2 numeric vectors
#' @param breaks1 break parameter for histogram function for the 2st list element.
#' @param breaks2 break parameter for histogram function for the 2st list element.
#' @param ... Pass any other parameter of the corresponding
#' plotting function (most of them should work).
#' @param plotname The name of the file saved.
#' @param main The title of the plot.
#' @param ylab Y-axis label
#' @param col  Color of the 2 histograms
#' @param incrBottMarginBy Increase the blank space at the bottom of the plot. Use if labels do not
#'   fit on the plot.
#' @param savefile Save plot as pdf in OutDir, TRUE by default.
#' @param w Width of the saved pdf image, in inches.
#' @param h Height of the saved pdf image, in inches.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report,
#' set by "path_of_report".
#' @param PNG Set to true if you want to save the plot as PNG instead of the default PDF.
#' @export
#' @examples  try(dev.off(), silent = TRUE)
#' ListOf2 = list("A"  = rnorm(100), "B"=rnorm(100))
#' ls_of_hists = whist.back2back(ListOf2)


whist.back2back <-
  function(ListOf2 = list("A"  = rnorm(10000), "B" = rnorm(10000)),
           breaks1 = 20,
           breaks2 = breaks1,
           col = c("green", "blue"),
           plotname = substitute(variable),
           main = plotname,
           ylab = "Frequency",
           savefile = UnlessSpec("b.save.wplots"),
           incrBottMarginBy = 0,
           w = UnlessSpec("b.defSize", 7),
           h = w,
           mdlink = ww.set.mdlink(),
           PNG = UnlessSpec("b.usepng"),
           ...) {
    print("Does not always work - experimental. Problem is the separate binning.")
    fname = kollapse(plotname, ".hist.btb")
    if (incrBottMarginBy) {
      .ParMarDefault <- par("mar")
      par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]))
    }   # Tune the margin
    lsNm = if (!is.null(names(ListOf2)))
      names(ListOf2)
    else
      1:2

    lng = length(ListOf2)
    if (lng != 2) {
      iprint("length(List): ", lng, " First two elements used")
    } #if
    x = NULL
    x[[1]] = h1 = hist(ListOf2[[1]], plot = FALSE, breaks = breaks1)
    x[[2]] = h2 = hist(ListOf2[[2]], plot = FALSE, breaks = breaks2)
    names(h2$counts) = h2$breaks[-1]
    names(h1$counts) = h1$breaks[-1]
    h2$counts =  -h2$counts

    AllBreaks = sort(union(h1$breaks, h2$breaks))[-1]
    ct1 = h1$counts[as.character(AllBreaks)]
    ct2 = h2$counts[as.character(AllBreaks)]

    hmax = max(h1$counts, na.rm = TRUE)
    hmin = min(h2$counts, na.rm = TRUE)
    xlimm = range(unlist(ListOf2), na.rm = TRUE)
    xlimm = c(1, max(length(h2$counts), length(h1$counts)) + 3)

    colorz = col # to avoid circular reference in the inside function argument
    main_ = main
    barplot(
      ct1,
      ylim = c(hmin, hmax),
      xlim = xlimm,
      col = colorz[1],
      names.arg = AllBreaks,
      las = 3,
      main = main_,
      ylab = ylab,
      ...
    )
    barplot(ct2,
            col = colorz[2],
            add = TRUE,
            names.arg = "")
    legend("topright", lsNm[1], bty = "n")
    legend("bottomright", lsNm[2], bty = "n")

    if (savefile) {
      ww.dev.copy(
        PNG_ = PNG,
        fname_ = fname,
        w_ = w,
        h_ = h
      )
    }
    if (incrBottMarginBy) {
      par("mar" = .ParMarDefault)
    }
    assign("plotnameLastPlot", fname, envir = .GlobalEnv)
    if (mdlink & savefile) {
      ww.MarkDown_Img_Logger_PDF_and_PNG(fname_wo_ext = fname)
    }
    x
  }


# ------------------------------------------------------------------------


#' corner.label.w
#'
#' Add Legends to the corners. From the Plotrix package.
#' @param label Text to display
#' @param cex font size
#' @param x an integer value: -1 for the left side of the plot, 1 for the right side
#' @param y an integer value: -1 for the bottom side of the plot, 1 for the top side
#' @param `xoff,yoff` Horizontal and vertical text offsets. If NA,
#' it defaults to one half of the width and height of "m" respectively.
#' @param figcorner Whether to find/display at the corner of the plot or figure.
#' @export
#' @examples plot(2); corner.label.w("A")

corner.label.w <- function (label = "A", # Add Legends to the corners. From the Plotrix package.
            cex = 3,
            x = -1,
            y = 1,
            xoff = 1,
            yoff = 1
            ,
            figcorner = TRUE,
            ...) {
    if (is.na(xoff))
      xoff <- strwidth("m") / 2
    if (is.na(yoff))
      yoff <- strheight("m") / 2
    par.usr <- par("usr")
    xpos <- par.usr[(3 + x) / 2]
    ypos <- par.usr[(3 + y) / 2 + 2]
    if (figcorner) {
      par.pin <- par("pin")
      xplotrange <- par.usr[2] - par.usr[1]
      yplotrange <- par.usr[4] - par.usr[3]
      par.mai <- par("mai")
      xmar <- xplotrange * par.mai[3 + x] / par.pin[1]
      ymar <- yplotrange * par.mai[2 + y] / par.pin[2]
      xpos <- xpos + x * xmar
      ypos <- ypos + y * ymar
    }
    if (!is.null(label)) {
      if (figcorner)
        par(xpd = TRUE)
      text(xpos - x * xoff, ypos - y * yoff, label, adj = c((1 + x) / 2, (1 + y) /
                                                              2), cex, ...)
      if (figcorner)
        par(xpd = FALSE)
    }
    return(list(x = xpos, y = ypos))
  }



# unrequire <- function(string='MarkdownReportsDev') detach(name = paste0("package:",string, collapse = ""), unload=TRUE)
# unrequire()
# detach("package:MarkdownReportsDev", unload=TRUE)
# require('MarkdownReportsDev' )
# unrequire('MarkdownReportsDev' )



# TMP

# create_set_OutDir <- function (..., setDir = TRUE) { # create set OutDir TMP for markdownreports
#   OutDir = kollapse(..., print = FALSE)
#   if (!substrRight(OutDir, 1) == "/")
#     OutDir = paste0(OutDir, "/") # add '/' if necessary
#   OutDir = gsub(x = OutDir,
#                 pattern = '//',
#                 replacement = '/')
#   iprint("All files will be saved under 'OutDir': ", OutDir)
#   if (!exists(OutDir)) {
#     dir.create(OutDir, showWarnings = FALSE)
#   }
#   if (setDir) {
#     setwd(OutDir)
#   }
#   ww.assign_to_global("OutDir", OutDir, 1)
# }
