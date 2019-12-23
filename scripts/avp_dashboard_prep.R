
# Reference ---------------------------------------------------------------

# Building Waffle Charts
# https://rud.is/rpubs/building-waffle-charts.html

# Hexbin Maps
# # https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# For mapping
library(geojsonio)
library(rgdal)
library(broom)
library(viridis)

# For plotting
library(hrbrthemes)
library(waffle)
library(knitr)
library(kableExtra)
library(scales)

# Load Data ---------------------------------------------------------------

df_interactions <- read_csv(here::here("data - raw", "2019-12-18_avp_dashboard_interactions.csv"), 
                            col_types = cols(QUERYRECID = col_skip(), 
                                             actual_date = col_date(format = "%m/%d/%Y"), 
                                             pm_start = col_date(format = "%m/%d/%Y")))

df_constituents <- read_csv(here::here("data - raw", "2019-12-18_avp_dashboard_constituents.csv"), 
                            col_types = cols(fips_state = col_number(),
                                             fips_county = col_number(),
                                             campaign_2_recognition = col_number(), 
                                             campaign_3_recognition = col_number(), 
                                             largest_recognition_commitment_amount = col_number(), 
                                             largest_recognition_commitment_date = col_date(format = "%m/%d/%Y"), 
                                             last_recognition_cash_amount = col_number(), 
                                             last_recognition_cash_date = col_date(format = "%m/%d/%Y"), 
                                             last_recognition_commitment_amount = col_number(), 
                                             last_recognition_commitment_date = col_date(format = "%m/%d/%Y"), 
                                             lifetime_recognition_commitments = col_number()))

df_constituents <- select(df_constituents, -c(38:40))

# Load translation table for rating
translate_rating_to_value <- read_csv(here::here("data - raw", "translate_rating_to_value.csv"))

# Merge & Wrangle Ratings -------------------------------------------------

# Merge rating values into df
df_constituents <- left_join(df_constituents, 
                             translate_rating_to_value, 
                             by = c("research_rating" = "rating"))

df_constituents$rating_value[is.na(df_constituents$rating_value)] <- 0

# Define rating bin levels
rating_bin_levels <- c("Not Rated",
                       "Less than $50K",
                       "$50-99K",
                       "$100-249K",
                       "$250-999K",
                       "$1M+")

# Create rating buckets to bin ratings
df_constituents$rating_bucket <- cut(
  df_constituents$rating_value,
  breaks = c(0,
             1,
             49999,
             99999,
             249999,
             999999,
             Inf),
  include.lowest = TRUE,
  labels = rating_bin_levels
)

# Create householded list
clean_df_constituents <- df_constituents %>% 
  group_by(spouse_hash) %>% 
  arrange(spouse_hash, household_position) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# Merge & Wrangle Interactions --------------------------------------------

# Find most recent personal visit
pm_personal_visit <- df_interactions %>% 
  filter(contact_method == "Personal Visit",
         owner_full == pm_full) %>% 
  group_by(spouse_hash, actual_date) %>% 
  arrange(spouse_hash, household_position, desc(actual_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(spouse_hash) %>% 
  arrange(spouse_hash, desc(actual_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  arrange(sort_name) %>% 
  select(spouse_hash,
         actual_date) %>% 
  rename(pm_last_visit_date = actual_date)

# Find most recent interaction
pm_interaction <- df_interactions %>% 
  filter(owner_full == pm_full) %>% 
  group_by(spouse_hash, actual_date) %>% 
  arrange(spouse_hash, household_position, desc(actual_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(spouse_hash) %>% 
  arrange(spouse_hash, desc(actual_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  arrange(sort_name) %>% 
  select(spouse_hash,
         actual_date) %>% 
  rename(pm_last_interaction_date = actual_date)

# Merge datasets together
clean_df_constituents <- left_join(clean_df_constituents, pm_personal_visit, by = "spouse_hash")
clean_df_constituents <- left_join(clean_df_constituents, pm_interaction   , by = "spouse_hash")

# Add months since each PM interaction
clean_df_constituents <- clean_df_constituents %>%
  mutate(
    months_since_last_pm_visit =
      (interval(pm_last_visit_date, Sys.Date())) %/% months(1),
    months_since_last_pm_interaction =
      (interval(pm_last_interaction_date, Sys.Date())) %/% months(1)
    
  )

# Use mutate if_else to define buckets
clean_df_constituents <- clean_df_constituents %>%
  mutate(time_since_last_visit = if_else(
    is.na(months_since_last_pm_visit),
    "Never",
    if_else(
      months_since_last_pm_visit %in% 0:11,
      "Within a Year",
      "Over a Year"
    )),
    time_since_last_interaction = if_else(
      is.na(months_since_last_pm_interaction),
      "Never",
      if_else(
        months_since_last_pm_interaction %in% 0:11,
        "Within a Year",
        "Over a Year"
      )
    )
  )

# Define visit & interaction bin levels
interaction_bin_levels <- c("Never",
                            "Over a Year",
                            "Within a Year")

# Create rating buckets to bin ratings
clean_df_constituents$time_since_last_visit <-
  factor(clean_df_constituents$time_since_last_visit, 
         levels = interaction_bin_levels)
clean_df_constituents$time_since_last_interaction <-
  factor(clean_df_constituents$time_since_last_interaction, 
         levels = interaction_bin_levels)

# Create fundraiser list
fundraisers <- enframe(unique(clean_df_constituents$pm), name = NULL)
fundraisers <- fundraisers %>% arrange(fundraisers$value)

# Plotting Functions ------------------------------------------------------

my_colors <- c("#cc0035", alpha("#354ca1", 0.2), alpha("#f9c80e", 0.2))
names(my_colors) <-
  levels(
    factor(
      c(
        levels(clean_df_constituents$time_since_last_visit),
        levels(clean_df_constituents$time_since_last_interaction
        ))))
my_scale <- scale_fill_manual(values = my_colors)

plot_portfolio_stats <- function(x) {
  clean_df_constituents %>% 
    filter(pm == x) %>% 
    group_by(rating_bucket) %>%
    add_tally() %>% 
    ungroup() %>% 
    group_by(rating_bucket, n) %>% 
    summarise(avg_largest_gift = mean(largest_recognition_commitment_amount, na.rm = TRUE),
              sum_rating_value = sum(rating_value, na.rm = TRUE)) %>% 
    mutate_at(vars(avg_largest_gift), ~replace(., is.nan(.), 0)) %>% 
    mutate(avg_largest_gift = dollar(avg_largest_gift),
           sum_rating_value = dollar(sum_rating_value)) %>% 
    rename("Rating Bucket" = rating_bucket,
           "Total Households" = n,
           "Average Largest Recognized Gift" = avg_largest_gift,
           "Total Rating Value" = sum_rating_value) %>%
    # kable(caption = paste(x, "Portfolio Stats")) %>% 
    kable() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = FALSE)
}

plot_last_interaction <- function(x) {
  clean_df_constituents %>%
    filter(pm == x) %>%
    count(time_since_last_interaction) %>%
    ggplot(aes(fill = time_since_last_interaction, values = n)) +
    expand_limits(x = c(0, 0), y = c(0, 0)) +
    coord_equal() +
    labs(
      # title = paste(x, "Time Since Last Interaction"),
      x = "",
      y = "",
      fill = NULL,
      colour = NULL
    ) +
    theme_ipsum_rc(grid = "") +
    geom_waffle(
      n_rows = 10,
      size = 1,
      color = "white",
      make_proportional = TRUE,
      radius = unit(4, "pt"),
      height = 1,
      width = 1
    ) +
    # scale_fill_manual(values = c("#cc0035", alpha("#354ca1", 0.2), alpha("#f9c80e", 0.2))) +
    my_scale +
    theme(axis.text = element_blank()) +
    # theme(legend.position = "none") +
    NULL
}

plot_last_visit <- function(x) {
  clean_df_constituents %>%
    filter(pm == x) %>%
    count(time_since_last_visit) %>%
    ggplot(aes(fill = time_since_last_visit, values = n)) +
    expand_limits(x = c(0, 0), y = c(0, 0)) +
    coord_equal() +
    labs(
      # title = paste(x, "Time Since Last Visit"),
      x = "",
      y = "",
      fill = NULL,
      colour = NULL
    ) +
    theme_ipsum_rc(grid = "") +
    geom_waffle(
      n_rows = 10,
      size = 1,
      color = "white",
      make_proportional = TRUE,
      radius = unit(4, "pt"),
      height = 1,
      width = 1
    ) +
    # scale_fill_manual(values = c("#cc0035", alpha("#354ca1", 0.2), alpha("#f9c80e", 0.2))) +
    my_scale +
    theme(axis.text = element_blank()) +
    NULL
}

# Mapping -----------------------------------------------------------------

# Load the data
spdf <- geojson_read(here::here("data - raw", "us_states_hexgrid.geojson"), what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Prep data for fortifying
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

plot_hex_map <- function(x) {
  
  # Create clean spdf_fortified dataset
  spdf_fortified <- tidy(spdf, region = "google_name")
 
  # Create dataset of fundraiser's prospects by state
  by_state <- clean_df_constituents %>% 
    filter(pm == x) %>% 
    select(state) %>% 
    group_by(state) %>% 
    count()
  
  # Merge datasets together
  spdf_fortified <- spdf_fortified %>% 
    left_join(., by_state, by = c("id" = "state"))
  
  # Replace blank states with 0
  spdf_fortified$n[is.na(spdf_fortified$n)] <- 0
  
  # Prepare binning
  spdf_fortified$bin <-
    cut(
      spdf_fortified$n ,
      breaks = c(0,1,5,9,Inf),
      labels = c("None", "1-5", "6-10", "10+"),
      include.lowest = TRUE
    )
  
  # Prepare a color scale coming from the viridis color palette
  my_palette <- rev(magma(8))[c(-1,-8)]
  
  # plot
  ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
    geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
    theme_void() +
    scale_fill_manual( 
      values=my_palette, 
      guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
    ) +
    labs(# title = paste("Location of", x, "Assigned Prospects, by State"),
         fill = "Prospects") +
    theme(
      legend.position = c(0.5, 0.9),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )
     
}
