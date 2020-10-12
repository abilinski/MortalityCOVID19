#*********************************** GLOBAL OPTIONS ***************************************#
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#### KNITR GLOBAL OPTIONS ####
knitr::opts_chunk$set(echo = F, message = F, warning = F)

#### LIBRARIES ####
library(tidyverse)
library(scales)
library(zoo)
library(knitr)
library(kableExtra)
library(regtools)
library(lubridate)
library(openxlsx)
library(sars2pack)

#### ADDITIONAL PLOT OPTIONS ####

# multiplot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ggplot theme and color options
theme_opts = theme_minimal() + theme(text = element_text(family = "sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "darkgrey"))
pal = c( "#449187", "#52bdd3", "#bce4bc", "#204393")