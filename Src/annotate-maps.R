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
  cat("\n\nTesting conditional_glimpse\n\n")
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
      fill="white",
      alpha=0.5,
      color="darkred")                  -> m2

  # add text from sf_text
  m2                                    +
    geom_sf_text(
      data=sf2,
      aes(label=sf_text),
      size=3)                           -> m3
  return(m3)
}

# Get some data to test this function.
if (run_tests) {
  cat("\n\nTesting plot_green\n\n")
  load(glue("{path_name}co.RData"))
  co %>%
    filter(GEOID=="29095") -> jackson
  conditional_glimpse(jackson)
  test_plot <- plot_green(jackson, jackson, "Jackson")
  plot(test_plot)
}

# The find_bg function takes the 
# bg_cd_intersection file and extracts 
# all the bg_id values associated with
# a particular cd_id. It can exclude 
# bg_id values where the proportion of
# area inside is less than one 
# threshold (lo) or greater than a
# second threshold (hi). Then it 
# creates a subset of bg that is 
# suitable for plotting.
find_bg <- function(i_cd, lo=0, hi=1.01) {
  # Note the default of 1.01 for hi is
  # needed to account for rounding 
  # errors.
  bg_cd_intersection                  %>%
    filter(cd_id==i_cd)               %>%
    filter(bg_prop_in >= lo)          %>%
    filter(bg_prop_in <= hi)          %>%
    pull(bg_id)                        -> bg_subset
  bg %>%
    filter(bg_id %in% bg_subset)      %>%
    arrange(bg_id)
}
if (run_tests) {
  cat("\n\nTesting find_bg\n\n")
  load(glue("{path_name}bg.RData"))
  load(glue("{path_name}cd-intersections.RData"))
  find_bg(117, lo=0.1) %>% print
}



# The find_bl function takes the 
# bl_cd_intersection file and extracts 
# all the bl_id values associated with
# a particular cd_id and bl_id. It can
# exclude bl_id values where the
# proportion of area inside is less
# than one threshold (lo) or greater
# than a second threshold (hi). Then
# it creates a subset of bg that is 
# suitable for plotting.
find_bl <- function(i_cd, i_bg, lo=0, hi=1.01) {
  # Note the default of 1.01 for hi is
  # needed to account for rounding 
  # errors.
  bl_cd_intersection                  %>%
    filter(cd_id==i_cd)               %>%
    filter(bg_id==i_bg)               %>%
    filter(bl_prop_in >= lo)          %>%
    filter(bl_prop_in <= hi)          %>%
    pull(bl_id)                        -> bl_subset
  bl %>%
    filter(bl_id %in% bl_subset)      %>%
    arrange(bl_id)
}

if (run_tests) {
  cat("\n\nTesting find_bl\n\n")
  load(glue("{path_name}bg.RData"))
  load(glue("{path_name}bl.RData"))
  load(glue("{path_name}cd-intersections.RData"))
  find_bg("117", "290470222003") %>% print
}

# The align_tx function takes a file
# with text data and subsets it by
# a particular cd_id and the bg_id 
# values found in bg0. This creates
# a file that you can use as labels
# for individual block groups.
align_tx <- function(bg0, tx, i_cd) {
  tx                                  %>%
    filter(cd_id==i_cd)               %>%
    filter(bg_id %in% bg0$bg_id)      %>%
    arrange(bg_id)
}

if (run_tests) {
  cat("\n\nTesting align_tx\n\n")
  load(glue("{path_name}bg.RData"))
  load(glue("{path_name}cd-intersections.RData"))
  load(glue("{path_name}cd-weights.RData"))
  i_cd <- 117
  bg0 <- find_bg(117, lo=0.1)
  align_tx(bg0, bg_counts, 117) %>% print
}

