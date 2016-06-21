#### FUNCTION - CREATE HEIGHT CLASS MATRIX ####
# function to reclassify raster from provided set of breaks
# input is vector of break values, where each number is the maximum of that break
# output is a matrix object with columns as the min, max, and class number

create_ht_class_matrix <- function(breaks){
  
  # get length of breaks vector to figure out how many classes to have
  brk_len <- length(breaks)
  
  # initialize ht class vector
  ht.class.m <- c(0)
  
  # loop through each break and set the lower and upper bound, and class number
  for(i in 1:brk_len){
    ht.class.m <- c(ht.class.m,breaks[i-1], breaks[i], i)
  }
  
  # create matrix from vector
  reclass.ht.m <- matrix(ht.class.m,
                         ncol=3,
                         byrow = T)
  
  return(reclass.ht.m)
}

#### FUNCTION - PLOT DENSITY WITH BREAKS ####
# plot density of chm with chosen breaks
# expects a chm, title, and vector of breaks

plot_chm_dens <- function(chm,title,breaks){
  density(chm,
          main=title,
          xlab="Height (m)")
  abline(v=breaks,col="red")
  
}


#### PLOT PDF ####
# function to save pdf from a function that produces a figure
make_pdf <- function(expr, filename, ..., verbose = TRUE) {
  if (verbose) {
    message("Creating: ", filename)
  }
  pdf(file = filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))}

#### FUNCTION - PLOT RECLASSIFIED RASTER ####
plot_reclassified_raster <- function(rast.in, site.name, breaks){
  # this is a tricky bit because we need to out the legend
  # outside of the plot region
  
  # Get colors for plotting
  bin.colors <- rev(terrain.colors(length(breaks)))
  
  # make room for a legend
  par(xpd = FALSE, mar = c(5.1, 4.1, 4.1, 4.5))
  
  # plot
  plot(rast.in,
       col = bin.colors,
       main = paste("Canopy height classes \n", site.name),
       legend = FALSE)
  
  # allow legend to plot outside of bounds
  par(xpd = TRUE)
  
  # legend x
  leg.x <- par()$usr[2] + 20
  
  # legend y
  leg.y <- par()$usr[4] + 50 - (abs(par()$usr[3] - par()$usr[4]) / 2) 
  
  # create legend text
  height.mat <- create_ht_class_matrix(breaks)
  
  # initialize legend text
  legend.text <- c()
  
  for (i in 1:length(breaks)) {
    
    legend.text <- c(legend.text, 
                     paste0(height.mat[i, 1], "-", 
                            height.mat[i, 2], " m"))
  }
  
  # create the legend
  legend(leg.x, leg.y,  # set x,y legend location
         legend = legend.text,  # make sure the order matches colors
         fill = bin.colors,
         bty = "n") # turn off border
  
  # turn off plotting outside of bounds
  par(xpd = FALSE)
}