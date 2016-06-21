# function to generate a histogram from a chm raster
# use the function with make_pdf

library(raster)

chm <- raster("C://Users/sjgraves/Documents/data/NEONDI-2016/NEONdata/D17-California/TEAK/2013/lidar/TEAK_lidarCHM.tif")


plot_chm_hist <- function(chm,title){
  
  hist(chm,
       main=title,
       xlab="height (m)")
  
}



make_pdf <- function(expr, filename, ..., verbose = TRUE) {
  if (verbose) {
    message("Creating: ", filename)
  }
  pdf(file = filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))}


plot_chm_hist(chm,"TEAK CHM")

make_pdf(plot_chm_hist(chm,"TEAK CHM"),filename="testCHM2.pdf")