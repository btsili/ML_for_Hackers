# File-Name:       outlining_drivers_for_UFO_sightings.R           
# Date:            2014-01-31                                
# Author:          Bill Tsiligkiridis
# Purpose:         Part 2 expands on Drew Conway's code (Part 1) by clustering the mass 
#                  sighting events in categories to conduct further analysis.
#                  The focus will be on the 'Short description' or Shape parameter  
#                  of the data with the analysis aiding to determine its relation
#                  to the actual driver of the UFO sighting event.			 
# Data Used:       http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
# Packages Used:   ggplot2, plyr, scales, gridExtra


#########################################################################################
# Part 1 ################################################################################
#########################################################################################



# File-Name:       ufo_sightings.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu)
# Purpose:         Code for Chapter 1.  In this case we will review some of the basic
#                   R functions and coding paradigms we will use throughout this book.
#                   This includes loading, viewing, and cleaning raw data; as well as
#                   some basic visualization.  This specific case we will use data from
#                   reported UFO sightings to investigate what, if any, seasonal trends
#                   exists in the data.
# Data Used:       http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
# Packages Used:   ggplot2, plyr, scales

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# Load libraries and data
library(ggplot2)    # We'll use ggplot2 for all of our visualizations
library(plyr)       # For data manipulation
library(scales)     # We'll need to fix date formats in plots
library(gridExtra)  # For graph alignments


# This is a tab-delimited file, so we use 'read.delim' and set the separator as a tab character.
# We also have to alter two defaults; first, we want the strings to not be converted to
# factor types; and, this data has does not have header labels in the first row, so
# we want to keep the first row as data.
ufo <- read.delim(file.path("data", "ufo", "ufo_awesome.tsv"),
                  sep = "\t",
                  stringsAsFactors = FALSE,
                  header = FALSE, 
                  na.strings = "")
# This is a large text file (75MB), so this may take a moment

# Inspect the data frame
summary(ufo)
head(ufo)

# From the data's description file, we will set the column names accordingly using 
# the 'names' function
names(ufo) <- c("DateOccurred", "DateReported",
                "Location", "ShortDescription",
                "Duration", "LongDescription")

# To work with the dates, we will need to convert the YYYYMMDD string to an R Date
# type using the 'strptime' function

# But, something has gone wrong with the data. For now, we'll just ignore the errata
# by removing those entries that have not parsed correctly.  We know that the date 
# strings are always 8 characters long, and any deviation from this would indicate
# a row to ignore.  We will use the 'ifelse' function to construct a vector of
# Booleans indicating the problem rows
good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 |
                    nchar(ufo$DateReported) != 8,
                    FALSE,
                    TRUE)
length(which(!good.rows))      # While 731 rows may seem like a lot, out of over 60K
ufo <- ufo[good.rows, ]        # it is only about 0.6% of the total number of records.

# Now we can convert the strings to Date objects and work with them properly
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")

# It will be useful to create separate columns for both town and state from the Location 
# column.  To do so we will use the 'strsplit' function to perform the regex.
# Note: not every entry in Location is of the form 'City, State'.  We use the
# 'tryCatch' function to simply return [NA, NA] when this is the case.  Next,
# we remove the leading white-space from both the city and state strings with 'gsub'
get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ","",split.location)
  if (length(clean.location) > 2)
  {
    return(c(NA,NA))
  }
  else
  {
    return(clean.location)
  }
}

# We use 'lapply' to return a list with [City, State] vector as each element
city.state <- lapply(ufo$Location, get.location)

# We use 'do.call' to collapse the list to an N-by-2 matrix
location.matrix <- do.call(rbind, city.state)

# Add the city and state data to ufo data frame. We can do this using the 'transform'
# function.
ufo <- transform(ufo,
                 USCity = location.matrix[, 1],
                 USState = location.matrix[, 2],
                 stringsAsFactors = FALSE)

# Next step, we will strip out non-US incidents

# Insert NA's where there are non-US cities
ufo$USState <- state.abb[match(ufo$USState, state.abb)]

# Finally, we'll use 'subset' to examine only events in the United States and convert 
# states to factors, i.e., a categorical variable.
ufo.us <- subset(ufo, !is.na(USState))

# Now, we are ready to do some analysis!  First, take a look at the post-processed data
summary(ufo.us)
head(ufo.us)

# The summary functions shows us that the data actually go back a very long time (1440!).  So, 
# we will want to take a quick look at the date to see where the majority of the data exists.
# We can do this by creating a histogram of frequencies for UFO sightings over time
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram() + 
  scale_x_date(breaks = "50 years")
  
ggsave(plot = quick.hist,
       filename = file.path("images", "quick_hist.pdf"),
       height = 6,
       width = 8)

# First, we notice that there are many very old entries in the data.  For our purposes, we will only look
# at incidents that occurred from 1990 to the most recent
ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))

# Let's look at the histogram now
new.hist <- ggplot(ufo.us, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(breaks = "50 years")

ggsave(plot = new.hist,
       filename = file.path("images", "new_hist.pdf"),
       height = 6,
       width = 8)

# Now that we have the data we want, let's look at some aggregations.  We will use
# the 'ddply' funtion in the plyr package. But first, we create a column of just
# the Year-Month of each incident.
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format = "%Y-%m")

# This will return the number of sightings of UFO by Year-Month and state for the whole time-series
sightings.counts <- ddply(ufo.us, .(USState,YearMonth), nrow)

# As we might expect, there are several Year-Month and state combinations for which there are no 
# UFO sightings.  We need to count these as zero so we can go back and fill those in.
# First, we will create a new vector that has all of the Year-Month dates in it that span the 
# range of our time-series (1990-2010)
date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by = "month")
date.strings <- strftime(date.range, "%Y-%m")

# To fill in the missing dates from the 'sightings.counts' data frame we will need to create a separate data
# frame with a column of states and Year-Months.
states.dates <- lapply(state.abb, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates),
                           stringsAsFactors = FALSE)

# We use 'merge' to take the counts we have and merge them with the missing dates.  Note, we have to specify
# the columns from each data frame we are using to do the merge, and set 'all' to TRUE, which will fill in 
# this missing dates from the original data frame with NA.
all.sightings <- merge(states.dates,
                       sightings.counts,
                       by.x = c("s", "date.strings"),
                       by.y = c("USState", "YearMonth"),
                       all = TRUE)

# Now we just need to clean up the merged data frame a bit
# Set the column names to something meaningful
names(all.sightings) <- c("State", "YearMonth", "Sightings")

# Covert the NAs to 0's, what we really wanted
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0

# Reset the character Year-Month to a Date objects
all.sightings$YearMonth <- as.Date(rep(date.range, length(state.abb)))

# Capitalize the State abbreviation and set as factor
all.sightings$State <- as.factor(all.sightings$State)

# There are lots of ways we could test the seasonality of of these sightings, but one basic method is to 
# inspect the trends visually.  We now construct a plot that will show these trends for all 50 U.S. states
# over the time-series.

# First we have to create a ggplot2 object and then create a geom layer, which in this case is a line.
# Additional points of note:
# (1) facet_wrap() will create separate plots for each state on a 10x5 grid.
# (2) theme_bw() changes the default ggplot2 style from grey to white (personal preference).
# (3) scale_color_manual() sets the line color to dark blue.
# (4) scale_x_date() scales the x-axis as a date, with major lines every 5 years.
# (5) xlab() and ylab() set axis labels.
# (6) opts() sets a title for the plot

state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")

# Save the plot as a PDF
ggsave(plot = state.plot,
       filename = file.path("images", "ufo_sightings.pdf"),
       width = 14,
       height = 8.5)


# Create a new graph where the number of signtings is normailzed by the state population
state.pop <- read.csv(file.path('data/census.csv'), stringsAsFactors=FALSE)

state.pop$abbs <- sapply(state.pop$State, function(x) state.abb[grep(paste('^', x, sep=''), state.name)])
all.sightings$Sightings.Norm <- sapply(1:nrow(all.sightings), 
    function(i) all.sightings$Sightings[i] / state.pop$X2000[which(state.pop$abbs== all.sightings$State[i])])
    
    
state.plot.norm <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings.Norm)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
  
  
# Save the plot as a PDF
ggsave(plot = state.plot.norm,
     filename = file.path("images", "ufo_sightings_norm.pdf"),
     width = 14,
     height = 8.5)
     
     

#########################################################################################
# Part 2 ################################################################################
#########################################################################################

# Taking a closer look at the peaks of the plots.

# We need to set thresholds both for the sightings and the normalised Sightings to 
# determine which peaks on the data we will be working with. Let's choose the 
# thresholds to be 30 UFO Sightings/per month and 3e-06/per month.
peak.sightings <- subset(all.sightings, all.sightings$Sightings >= 40 &
  all.sightings$Sightings.Norm >= 2.5e-06)


# Extracting information from the short description (shape).
triming.lead <- function (x) sub("^\\s+", "", x)
shape <- sapply(ufo.us$ShortDescription, triming.lead)

# Add the shape data to ufo.us data frame. We can do this using the 'transform'
# function.
ufo.us <- transform(ufo.us,
                 Shape = shape,
                 stringsAsFactors = FALSE)

# Remove all NAs
ufo.us.shape <- subset(ufo.us, !is.na(ufo.us$Shape))

# This will return the number of sightings of UFO by Year-Month,state and shape for 
the whole time-series
sightings.shape.counts <- ddply(ufo.us.shape, .(USState,YearMonth,Shape), nrow)

date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by = "month")
date.strings <- strftime(date.range, "%Y-%m")

ufo.us.shapes <- levels(factor(sightings.shape.counts$Shape))

# To fill in the missing dates from the 'sightings.counts.shapes' data frame we will 
# need to create a separate data frame with a column of states, Year-Months and possible
# shapes.
states.dates.shapes <- lapply(state.abb, function(s) cbind(s, date.strings, ufo.us.shapes))
states.dates.shapes <- data.frame(do.call(rbind, states.dates.shapes),
                           stringsAsFactors = FALSE)

# We use 'merge' to take the counts we have and merge them with the missing dates and shapes.  Note, we have to specify
# the columns from each data frame we are using to do the merge, and set 'all' to TRUE, which will fill in 
# this missing dates from the original data frame with NA.
all.sightings.shape <- merge(states.dates.shapes,
                       sightings.shape.counts,
                       by.x = c("s", "date.strings", "ufo.us.shapes"),
                       by.y = c("USState", "YearMonth", "Shape"),
                       all = TRUE)

# Now we just need to clean up the merged data frame a bit.
# Set the column names to something meaningful
names(all.sightings.shape) <- c("State", "YearMonth", "Shape", "Sightings") 

# Convert the NAs to 0's, what we really wanted
all.sightings.shape$Sightings[is.na(all.sightings.shape$Sightings)] <- 0

# Reset the character Year-Month to a Date objects
add.day <- function(x)
{
	first.day <- '-01'
	paste(x, first.day, sep = '', collapse = '')
}
all.sightings.shape$YearMonth <- sapply(all.sightings.shape$YearMonth, add.day) 
all.sightings.shape$YearMonth <- as.Date(all.sightings.shape$YearMonth)

# Set Shape as factor
all.sightings.shape$Shape <- as.factor(all.sightings.shape$Shape)


# Create a plot to visualise thresholds for both the number and normalised number of 
# sightings for the State of AZ.

az.sightings <- subset(all.sightings, all.sightings$State == 'AZ')

az.state.plot <- ggplot(az.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  geom_vline(yintercept=c(40), linetype="dotted")
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year in AZ (1990-2010)")
   
az.state.plot.norm <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings.Norm)) +
  geom_line(aes(color = "darkblue")) +
  geom_vline(yintercept=c(2.5e-06), linetype="dotted")
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Normalised number of UFO sightings by Month-Year in AZ (1990-2010)")
  
az.combo.plot <- grid.arrange(az.state.plot, az.state.plot.norm)


png("images/az_combo_plot.png")
dev.off() 


# Clustering the sighting events in a total of 3 categories based on:
# 1. The number of States where observed	
# 2. The frequency of occurrence in a State(s) in the selected time period

# Category 1 : Single occurence in one State
single.peak.sightings <- subset(peak.sightings, (peak.sightings$State == 'AZ' | 
  peak.sightings$State == 'CA' | peak.sightings$State == 'FL' | 
  peak.sightings$State == 'PA' | peak.sightings$State == 'TX')) 

single.peak.sightings$YearMonth <- as.character(single.peak.sightings$YearMonth)

single.peak.sightings$YearMonth <- gsub("1997-03-01", "Mar - 1997", 
  single.peak.sightings$YearMonth)
single.peak.sightings$YearMonth <- gsub("2008-04-01", "Apr - 2008", 
  single.peak.sightings$YearMonth)
single.peak.sightings$YearMonth <- gsub("2008-10-01", "Oct - 2008", 
  single.peak.sightings$YearMonth)
single.peak.sightings$YearMonth <- gsub("2010-07-01", "Jul - 2010", 
  single.peak.sightings$YearMonth)
single.peak.sightings$YearMonth <- gsub("2009-01-01", "Jan - 2009", 
  single.peak.sightings$YearMonth)


single.peak.sightings$Sightings <- as.numeric(single.peak.sightings$Sightings)
single.peak.sightings$Sightings.Norm <- as.numeric(single.peak.sightings$Sightings.Norm) 

# Plot single occurence in one State Number of sightings and Normalised number of sightings
# by Month-Year. 
se.all <- ggplot(single.peak.sightings, aes(x = YearMonth, y = Sightings, 
  fill = State)) +
  geom_bar(width=0.9, stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#CC6666", "#FF9966", "#669999", "#66CC99", "#CC9999", "#CCCCCC")) +
  scale_x_discrete(limits=c('Mar - 1997','Apr - 2008','Oct - 2008','Jan - 2009','Jul - 2010')) +
  coord_flip() +
  theme_bw() + 
  xlab("Month - Year") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings for the 'Single occurence in one State' cases")

se.all.norm <- ggplot(single.peak.sightings, aes(x = YearMonth, y = Sightings.Norm, 
  fill = State)) +
  geom_bar(width=0.9, stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#CC6666", "#FF9966", "#669999", "#66CC99", "#CC9999", "#CCCCCC")) +
  scale_x_discrete(limits=c('Mar - 1997','Apr - 2008','Oct - 2008','Jan - 2009','Jul - 2010')) +
  coord_flip() +
  theme_bw() + 
  xlab("Month - Year") +
  ylab("Per Capita Number of Sightings (2000 Census)") +
  ggtitle("Normalised number of UFO sightings for the 'Single occurence in one State' cases")

 
se.all.plot <- grid.arrange(se.all, se.all.norm)

# Save the plot as a PDF
ggsave(plot = se.all.plot,
     filename = file.path("images", "se_all_plot.pdf"),
     width = 14,
     height = 8.5)


# Bringing Shape in the analysis

az.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'AZ' & 
  all.sightings.shape$YearMonth == '1997-03-01')
ca.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'CA' & 
  all.sightings.shape$YearMonth == '2009-01-01')
fl.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'FL' & 
  all.sightings.shape$YearMonth == '2008-04-01')
pa.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'PA' & 
  all.sightings.shape$YearMonth == '2010-07-01')
tx.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'TX' & 
  all.sightings.shape$YearMonth == '2008-10-01')

se.sightings <- rbind(az.sightings,ca.sightings,fl.sightings,pa.sightings,
  tx.sightings)

se.sightings <- subset(se.sightings,!(se.sightings$Shape == 'pyramid') &
  !(se.sightings$Shape == 'delta'))

# Plot single occurence in one State Number of sightings and Normalised number of sightings
# by Shape and Month-Year. 
se.shape.allinone <- ggplot(se.sightings, aes(x = State, y = Sightings, fill = State)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#669999", "#66CC99", "#FF9966", "#CC9999", "#CC6666")) +
  coord_flip() +
  facet_wrap(~Shape, nrow = 6, ncol = 5) + 
  theme_bw() + 
  xlab("US State") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Shape and US State for the 'Single occurence in one State' cases")
  
  
# Save the plot as a PDF
ggsave(plot = se.shape.allinone,
     filename = file.path("images", "se_allinone.pdf"),
     width = 14,
     height = 8.5)



# Category 2 : Single occurence in multiple States
ohmi.sightings <- subset(all.sightings.shape, (all.sightings.shape$State == 'OH' | 
  all.sightings.shape$State == 'MI' | all.sightings.shape$State == 'IN' |
  all.sightings.shape$State == 'KY') & all.sightings.shape$YearMonth == '1999-11-01')

# Plot single occurence in in multiple States Number of sightings and Normalised number of sightings
# by shape and Month-Year. 
ohmi_shape.plot <- ggplot(ohmi.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~Shape, nrow = 10, ncol = 3) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "1 month", labels = date_format('%b')) +
  xlab("Months") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings in OH and MI by Shape and Month")

# Save the plot as a PDF
ggsave(plot = ohmi_shape.plot,
       filename = file.path("images", "ohmi_shape_ufo_sightings.pdf"),
       width = 14,
       height = 8.5)

# 5 most dominant shapes on the same plot
Dominant.Shape <- c('light', 'formation', 'fireball', 'cigar', 'circle')
ohmi.sightings.dom.shape <- subset(ohmi.sightings, ohmi.sightings$Shape %in% Dominant.Shape)  

# To make the bars equal in width
dat <- rbind(wa.sightings.dom.shape,
  c('KY','1999-11-01','circle',NA,NA),c('IN','1999-11-01','circle',NA,NA))

dat$Sightings <- as.numeric(dat$Sightings)
dat$Sightings.Norm <- as.numeric(dat$Sightings.Norm) 

# Plot only dominant shapes
ohmi.shape.allinone <- ggplot(ohmi.sightings.dom.shape, aes(x = Shape, y = Sightings, 
  fill = State)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#669999", "#CC9999")) +
  coord_flip() +
  theme_bw() + 
  xlab("Dominant Shapes") +
  ylab("Number of Sightings") +
  ggtitle("Number of Dominant Shape UFO sightings in OH, MI, IN and KY in November 1999")
  
  
# Save the plot as a PDF
ggsave(plot = ohmi.shape.allinone,
     filename = file.path("images", "ohmi_shape_allinone.pdf"),
     width = 14,
     height = 8.5)


# Category 3.1 : Recurring in one State IL
date.range.il <- seq.Date(from = as.Date('2004-01-01'),
                       to = as.Date('2006-12-01'),
                       by = "month")
il.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'IL' & 
  all.sightings.shape$YearMonth %in% date.range.il)

# Plot recurring in one State Number of sightings and Normalised number of sightings
# by Shape and Month-Year. 
il_shape.plot <- ggplot(il.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~Shape, nrow = 10, ncol = 3) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "6 months", labels = date_format('%Y-%b')) +
  xlab("Months") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings in IL by Shape and Month (2004 - 2006)")

# Save the plot as a PDF
ggsave(plot = il_shape.plot,
       filename = file.path("images", "il_shape_ufo_sightings.pdf"),
       width = 14,
       height = 8.5)


# Plot Dominant Shapes only
Dominant.Shape <- c('triangle', 'light', 'formation', 'circle')
Dominant.Dates <- as.Date(c('2004-08-01', '2004-10-01', '2005-09-01'))

il.sightings.dom.shape <- subset(il.sightings, 
  il.sightings$Shape %in% Dominant.Shape & il.sightings$YearMonth %in% Dominant.Dates)  

il.sightings.dom.shape$YearMonth <- as.character(il.sightings.dom.shape$YearMonth)

il.sightings.dom.shape$YearMonth <- gsub("2004-08-01", "Aug - 2004", 
  il.sightings.dom.shape$YearMonth)
il.sightings.dom.shape$YearMonth <- gsub("2004-10-01", "Oct - 2004", 
  il.sightings.dom.shape$YearMonth)
il.sightings.dom.shape$YearMonth <-gsub("2005-09-01", "Sep - 2005", 
  il.sightings.dom.shape$YearMonth)

il.shape.allinone <- ggplot(il.sightings.dom.shape, aes(x = YearMonth, y = Sightings, 
  fill = Shape)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#CC6666", "#66CC99", "#669999", "#CC9999")) +
  coord_flip() +
  theme_bw() + 
  xlab("Month - Year") +
  ylab("Number of Sightings") +
  ggtitle("Number of Dominant Shape UFO sightings for three events in IL")
  
# Save the plot as a PDF
ggsave(plot = il.shape.allinone,
     filename = file.path("images", "il_shape_allinone.pdf"),
     width = 14,
     height = 8.5) 


# Category 3.2 : Recurring in one State WA
date.range.wa <- seq.Date(from = as.Date('1995-02-01'),
                       to = as.Date('2010-07-01'),
                       by = "month")
wa.sightings <- subset(all.sightings.shape, all.sightings.shape$State == 'WA' & 
  all.sightings.shape$YearMonth %in% date.range.wa)

# Plot recurring in one State Number of sightings and Normalised number of sightings
# by Shape and Month-Year. 
wa_shape.plot <- ggplot(wa.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~Shape, nrow = 10, ncol = 3) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "6 months", labels = date_format('%Y-%b')) +
  xlab("Months") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings in WA by Shape and Month (1995 - 2010)")

# Save the plot as a PDF
ggsave(plot = wa_shape.plot,
       filename = file.path("images", "wa_shape_ufo_sightings.pdf"),
       width = 14,
       height = 8.5)


Dominant.Dates <- as.Date(c("1999-09-01", "2003-07-01",
  "2003-09-01", "2009-07-01", "2010-07-01"))


wa.sightings.dom <- subset(wa.sightings, wa.sightings$YearMonth %in% Dominant.Dates)  

wa.sightings.dom$YearMonth <- as.character(wa.sightings.dom$YearMonth)

wa.sightings.dom$YearMonth <- gsub("1999-09-01", "Sep - 1999", 
  wa.sightings.dom$YearMonth)
wa.sightings.dom$YearMonth <- gsub("2003-07-01", "Jul - 2003", 
  wa.sightings.dom$YearMonth)
wa.sightings.dom$YearMonth <- gsub("2003-09-01", "Sep - 2003", 
  wa.sightings.dom$YearMonth)
wa.sightings.dom$YearMonth <-gsub("2009-07-01", "Jul - 2009", 
  wa.sightings.dom$YearMonth)
wa.sightings.dom$YearMonth <-gsub("2010-07-01", "Jul - 2010", 
  wa.sightings.dom$YearMonth)

# Most dominant shapes (more than 5 obs) on the same plot
wa.sightings.dom.shape <- subset(wa.sightings.dom, wa.sightings.dom$Sightings > 5)


# To make the bars equal in width
dat <- rbind(wa.sightings.dom.shape,
  c('WA','Sep - 1999','circle',0,0),c('WA','Sep - 1999','unknown',NA,NA),
  c('WA','Sep - 1999','fireball',0,0),c('WA','Sep - 1999','sphere',NA,NA),
  c('WA','Jul - 2003','circle',0,0),c('WA','Jul - 2003','unknown',NA,NA),
  c('WA','Jul - 2003','fireball',0,0),c('WA','Jul - 2003','sphere',NA,NA),
  c('WA','Jul - 2009','circle',0,0),c('WA','Jul - 2009','triangle',NA,NA),
  c('WA','Jul - 2009','fireball',0,0),c('WA','Jul - 2010','unknown',NA,NA),
  c('WA','Sep - 2003','circle',0,0),c('WA','Sep - 2003','unknown',NA,NA),
  c('WA','Sep - 2003','fireball',0,0),c('WA','Sep - 2003','sphere',NA,NA),
  c('WA','Sep - 2003','triangle',NA,NA))

dat$Sightings <- as.numeric(dat$Sightings)
dat$Sightings.Norm <- as.numeric(dat$Sightings.Norm) 


# Plot Dominant Shapes only
wa.shape.allinone <- ggplot(dat, aes(x = YearMonth, y = Sightings, 
  fill = Shape)) +
  geom_bar(width=0.9, stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#CC6666", "#FF9966", "#669999", "#66CC99", "#CC9999", "#CCCCCC")) +
  scale_x_discrete(limits=c('Sep - 1999','Jul - 2003','Sep - 2003','Jul - 2009','Jul - 2010')) +
  coord_flip() +
  theme_bw() + 
  xlab("Month - Year") +
  ylab("Number of Sightings") +
  ggtitle("Number of Dominant Shape UFO sightings for five events in WA")
  
# Save the plot as a PDF
ggsave(plot = wa.shape.allinone,
     filename = file.path("images", "wa_shape_allinone.pdf"),
     width = 14,
     height = 8.5)  



