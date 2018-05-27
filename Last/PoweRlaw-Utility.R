######################################################################
# Utility functions for defining and initializing distribution objects
# from the poweRlaw package. 
# Dan Suthers, Sept. 26, 2016; Added plot utility Dec. 17, 2016
# Last edit Feburary 16, 2010 (simplifications; added bty)
######################################################################

library(igraph)
library(poweRlaw)

######################################################################
# Get degrees, indegrees, or outdegrees without isolate nodes. 

nonzero_degrees <- function(g, mode="total") {
	d <- degree(g, mode=mode)
	return(d[d != 0])
}

######################################################################
# We often have to do the same sequence of operations to 
# initialize the distribution object.  This function
# modifies distribution object dis to have parameters set. 
# The given object is modified so there is no returned value. 

initialize_parameter_estimates <- function(dis) {
  dis$setXmin(estimate_xmin(dis))
  dis$setPars(estimate_pars(dis)) 
}

######################################################################
# Helpers that given the degrees (use one of the above helpers), 
# will return the indicated distribution (save it in a variable). 

initialize_disexp <- function(degrees) {
	dis <- disexp$new(degrees)
  initialize_parameter_estimates(dis)
	return(dis)
}

initialize_dislnorm <- function(degrees) {
	dis <- dislnorm$new(degrees)
	initialize_parameter_estimates(dis)
	return(dis)
}

initialize_displ <- function(degrees) {
	dis <- displ$new(degrees)
  initialize_parameter_estimates(dis)
	return(dis)
}

initialize_dispois <- function(degrees) {
	dis <- dispois$new(degrees)
  initialize_parameter_estimates(dis)
	return(dis)
}

######################################################################
# Utility Plotter 
# Uses poweRlaw package plotter for cumulative distribution functions. 
# Requires all four distributions. 
# lwd controls width of lines for distributions in legend
# lcex controls legend size 
# lpos controls legend position
# bty controls whether there is a box around the legend

plot_discrete_distributions <- function(title, disexp, dislnorm, displ, dispois,
                                        lwd=3, lcex=1.0, lpos="topright", bty="n") {
    plot(disexp, main=title, xlab="k", ylab="p(k)")
    lines(dispois, col="orange", lwd=lwd) # Rarely matches: draw first 
    lines(disexp, col="green", lwd=lwd) 
    lines(dislnorm, col="red", lwd=lwd) 
    lines(displ, col="blue", lwd=lwd)     # Draw last so we can see it
    legend(lpos, bty=bty, 
           c(paste("exp, xmin =", disexp$getXmin()),
             paste("lnorm, xmin =", dislnorm$getXmin()),
             paste("pl, xmin =", displ$getXmin()),
             paste("pois, xmin =", dispois$getXmin())),
           col=c("green", "red", "blue", "orange"), 
           lty=1, lwd=lwd, cex=lcex)
}


#######################################################################
# Pau