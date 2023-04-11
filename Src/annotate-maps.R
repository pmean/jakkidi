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

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(glue)))
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(sf)))
suppressMessages(suppressWarnings(library(tidycensus)))
suppressMessages(suppressWarnings(library(tidyverse)))

path_name <- "../data/"

if (!exists("terse")) terse <- FALSE
if (!exists("run_tests")) run_tests <- TRUE

### Provide an option to glimpse or print
conditional_glimpse <- function(x) {
  if (terse) return(invisible())
  cat("\n\n")
  cat(deparse(substitute(x)))
  cat("\n\n")
  glimpse(x)
  cat("\n\n")
  return(invisible())
}

conditional_print <- function(x) {
  if (terse) return(invisible())
  cat("\n\n")
  cat(deparse(substitute(x)))
  cat("\n\n")
  print(x)
  cat("\n\n")
  return(invisible())
}

# Test of conditional_glimpse
if (run_tests) {
  cat("\n\nTesting conditional_glimpse\n\n")
  test1 <- data.frame(x=1:3, y=4:6)
  conditional_glimpse(test1)
}
# Test of conditional_print
if (run_tests) {
  cat("\n\nTesting conditional_print\n\n")
  test1 <- data.frame(x=1:3, y=4:6)
  conditional_print(test1)
}


### Load key data

# Key RData files are:
# + bg: shape files for census block groups
# + cd: shape files for community districts
# + cd-intersections: percentage area overlapping
# + cd-weights: estimated people/housing units in overlap

### bg

path_name <- "../data/"
load(glue("{path_name}bg.RData"))
# convert from square meters to square kilometers
bg <- mutate(bg, bg_area=round(bg_area/(10^6), 2))
conditional_glimpse(bg)

### bl

path_name <- "../data/"
load(glue("{path_name}bl.RData"))
# convert from square meters to square kilometers
bl <- mutate(bl, bl_area=round(bl_area/(10^6), 2))
conditional_glimpse(bl)

### cd

path_name <- "../data/"
load(glue("{path_name}cd.RData"))
conditional_glimpse(cd)

### cd-intersections

path_name <- "../data/"
load(glue("{path_name}cd-intersections.RData"))
conditional_glimpse(bg_cd_intersection)
# cd-intersections.RData also contains
# information at the block and tract
# levels

### cd-weights

path_name <- "../data/"
load(glue("{path_name}cd-weights.RData"))
# merge with bg_cd_intersection to get
# bg_prop_in. Also convert units, compute
# weights and create shorter names.
#
# Note: we are adding a small constant to
# the denominator for weight calculations
# to avoid computing 0/0 for some weights.
bg_counts                             %>%
  inner_join(
    bg_cd_intersection, 
    by=c("bg_id", "cd_id"))           %>%
  mutate(
    people_weights=
      round(
        people_in/(people+0.001), 2)) %>%
  mutate(
    unit_weights=
      round(
        units_in/(units+0.01), 2))    %>%
  mutate(
    bg_prop_in=round(100*bg_prop_in)) %>%
  mutate(
    short_id=str_sub(bg_id, 6, 12))    -> bg_counts
conditional_glimpse(bg_counts)
# cd-weights.RData also contains bl_counts

### co

path_name <- "../data/"
load(glue("{path_name}co.RData"))
conditional_glimpse(co)

### List seven counties and their states

county_list <- c(
  "Johnson",
  "Leavenworth",
  "Wyandotte",
  "Cass",
  "Clay",
  "Jackson",
  "Platte")
state_list <- rep(c("KS", "MO"), c(3,4))

### Select interesting cds

name_list <- c(
  "Brookside",
  "Midtown",
  "Swope Park",
  "Waldo",
  "Ward Parkway",
  "Greater Downtown",
  "Antioch",
  "Little Blue Valley",
  "Rosedale",
  "Argentine")

cd                                     %>%
  filter(cd_name %in% name_list)       %>%
  pull(cd_id)                           -> interesting_cd

created_objects <- sort(ls())

if(!terse) {
  cat("\n\n")
  print("Annotate maps creates the following objects")
  print(paste0("+ ", created_objects))
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
  i_cd <- "117"
  i_bg <- "290470222003"
  find_bl(i_cd, i_bg) %>% print
}

# The download_acs function uses the 
# tidycensus package to get ACS data
# for a specified list of variables.
download_acs <- function(vlist) {
  load_variables(2020, "acs5", cache = TRUE) %>%
    filter(name %in% vlist)                  %>%
    print
  acs_vars <- NULL
  for (i in 1:7) {
    acs_i <- get_acs(
      geography = "cbg", 
      variables = vlist, 
      state=state_list[i],
      county=county_list[i],
      year = 2020)
    acs_vars <- rbind(acs_vars, acs_i)
  }
  return(acs_vars)
}

if (run_tests) {
  cat("\n\nTesting download_acs\n\n")
  vlist <- c("B01001_003", "B01001_027")
  conditional_print(download_acs(vlist))
}

if(!terse) {
  cat("\n\n")
  print("Annotate maps creates the following functions")
  print(lsf.str())
  # I found this function at
  # https://stackoverflow.com/questions/12455503/finding-all-functions-in-current-workspace
}

