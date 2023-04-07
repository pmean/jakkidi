# annotate-maps.R
# author: "Steve Simon"
# date: "Created 2023-04-06"

# This program provides a generic 
# function for adding text to a map.
# One portion of the map is drawn with
# a green background. A second is 
# drawn with red borders. Then a short
# text string is added inside the red
# boundaries.

### Load relevant files

library(glue)
library(magrittr)
library(sf)
library(tidyverse)
library(dplyr)
path_name <- "../data/"
if (!exists("terse")) terse <- FALSE
if (!exists("run_tests")) run_tests <- TRUE

### Provide an option to glimpse
conditional_glimpse <- function(x) {
  if (terse) return(invisible())
  cat("\n\n")
  cat(deparse(substitute(x)))
  cat("\n\n")
  glimpse(x)
  cat("\n\n")
  return(invisible())
}

# Test of conditional_glimpse
if (run_tests) {
  test1 <- data.frame(x=1:3, y=4:6)
  conditional_glimpse(test1)
}

# The plot_green function takes simple
# features from two separate files, and 
# maps the first in green, and the 
# second with a red border. Then 
# annotation text is added inside each
# red border.
plot_green <- function(sf1, sf2, sf_text) {

  # draw sf1 in green
  ggplot(sf1)                           +
    xlab("Longitude")                   +
    ylab("Latitude")                    +
      geom_sf(
        aes(),
        fill="lightgreen",
        color=NA)                       -> m1
  if (is.null(sf2)) return(m1)
  
  # draw sf2 with red border
  m1                                    +
    geom_sf(
      data=sf2,
      aes(),
      fill=NA,
      color="darkred")                  -> m2

  # add text from sf_text
  m2                                    +
    geom_sf_text(
      data=sf2,
      aes(label=sf_text),
      size=2)                           -> m3
  return(m3)
}

# Get some data to test this function.
if (run_tests) {
  load(glue("{path_name}co.RData"))
  co %>%
    filter(GEOID=="29095") -> jackson
  conditional_glimpse(jackson)
  test_plot <- plot_green(jackson, jackson, "NAME")
  plot(test_plot)
}

### Find intersecting blocks for a given cd_id

# The find_intersecting_bg function
# takes the bg_cd_intersection file
# and extracts all the bg_id values
# associated with a particular cd_id.
# It can exclude bg_id values where
# the proportion of area inside is
# less than one threshold (lo) or 
# greater than a second threshold 
# (hi).
find_intersecting_bg <- function(
    bg_cd_intersections, 
    i_cd, 
    lo=0,
    hi=1) {

  bg_cd_intersections                 %>%
    filter(cd_id==i_cd)               %>%
    filter(bg_prop_in >= lo)          %>%
    filter(bg_prop_in <= hi)          %>%
    pull(bg_id)
}

if (run_tests) {
  load(glue("{path_name}cd-intersections.RData"))
  find_intersecting_bg(bg_cd_intersection, 117, lo=0.1)
}
