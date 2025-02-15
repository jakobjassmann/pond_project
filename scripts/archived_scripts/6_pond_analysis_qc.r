# Pond analysis playground
# Aim: track ponds across time-series
# Jakob J. Assmann 19 April 2024

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)

# Load mock pond time-series
ponds_reference <- read_sf("data/pond_polys/mock_time-series.gpkg")

# Remove annototations for testing
ponds <- select(ponds_reference, -pond_id) 

# Roughly arrange top-left to bottom-right an add unique id to all ponds
ponds <- ponds %>%
    # Arragne top-left to bottom-right corner
    mutate(min_x = st_bbox(.)[1], min_y = st_bbox(.)[2]) %>%
    arrange(min_y, min_y) %>%
    select(-min_y, -min_y) %>%
    mutate(id = 1:nrow(.))

# Helper function to check pond in next year
ponds_intersecting_next_year <- function(pond_id, current_year, ponds) {
    # Get pond geometry,
    pond <- filter(ponds, id == pond_id)

    # Safety check whether there is one more year in the time-series,
    # if not return NULL
    if (max(ponds$year) < current_year + 1) {
        return(NULL)
    }
    # Get ponds for next year
    ponds_next_year <- filter(ponds, year == (current_year + 1))
    # Get rows for ponds intersecting pond of interest
    ponds_intersecting <- pond %>%
        st_intersects(ponds_next_year) %>%
        unlist(.)
    # Check if for empty intersections and return NULL
    if (length(ponds_intersecting) == 0) {
        return(NULL)
    }
    # Otherwise return ponds intersecting point of interest
    return(ponds_next_year[ponds_intersecting,]$id)
}

# Helper function to check whether two ponds overlap based on id
check_intersection <- function(id_a, id_b, ponds) {
    # Get ponds
    pond_a <- filter(ponds, id %in% id_a)
    if(nrow(pond_a) > 1) pond_a <- st_union(pond_a)
    pond_b <- filter(ponds, id == id_b)
    if(length(unlist(st_intersects(pond_a, pond_b))) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# Get uniqiue years in time-series and sort ascending
years <- unique(ponds$year) %>% sort()

# Function to track ponds from any given starting year
#start_year <- 2014
track_ponds <- function(start_year, ponds) {
    # Status
    cat("Getting pond time-series with start year:", start_year, "\n")

    # Get years larger than year of interest and sort
    years <- filter(ponds, year >= start_year) %>%
        pull(year) %>%
        unique() %>%
        sort()
        
    # Assing start ponds as first ponds in time series
    time_series <- filter(ponds, year == start_year) %>%
        # Arragne top-left to bottom-right corner
        mutate(min_x = st_bbox(.)[1], min_y = st_bbox(.)[2]) %>%
        arrange(min_y, min_y) %>%
        select(-min_y, -min_y) %>%
        st_drop_geometry() %>%
        # Add identifier
        mutate(
            ts_id = paste0(year, "_", 1:nrow(.)),
            !!as.character(start_year) := id
        ) %>%
        select(ts_id, !!as.character(start_year)) %>%
        mutate(multiple_intersect = NA)
    
    # Loop through years and grow time-series
    current_year <- start_year
    while (current_year < max(years)) {
        # Status update
        cat("\tGrowing pond time-series... current year:", current_year, "\n\t")
        # Obtain overlap for next year
        time_series <- time_series %>%
            split(time_series$ts_id) %>%
            map(function(pond_ts) {
                # Status update
                cat(".")
                # Get pond id for current year
                id_current_year <- pond_ts[1, colnames(pond_ts) == current_year] %>% unlist()

                # if empyt check for ponds in pervious years
                if (is.na(id_current_year)) {
                    previous_year <- current_year - 1
                    while (is.na(id_current_year) & previous_year >= start_year) {
                        id_current_year <- pond_ts[1, colnames(pond_ts) == previous_year] %>% unlist()
                        previous_year <- previous_year - 1
                    }
                }

                # Get overlapping ids for next year
                ids_next_year <- ponds_intersecting_next_year(
                    id_current_year,
                    current_year,
                    ponds
                )

                # Check if there is more than one pond in the next year
                n_ids_next_year <- length(ids_next_year)
                if (n_ids_next_year > 1) {
                    # If yes fork the time-series into multiples
                    # Prepare mulitple intersect column
                    m_intersect <- c(pond_ts$multiple_intersect, current_year) %>%
                        na.omit() %>%
                        unlist()

                    # If this is the first year split the time-series
                    # Split tibble and update multiple intersect colum
                    pond_ts_new <- map(1:n_ids_next_year, function(x) {
                        pond_ts %>% mutate(
                            !!as.character(current_year + 1) := ids_next_year[x],
                            multiple_intersect = list(m_intersect)
                        )
                    }) %>%
                        bind_rows()
                    # IMPORTANT! Check whether newly forked time-series make sense
                    # If there never was a pond present before the split, then
                    # then any newly added ponds are not meaningful, but only
                    # for those ponds where there was not another split

                    # Go across each new time_series to check consistency
                    pond_ts_new <- pond_ts_new %>%
                        split(1:nrow(.)) %>%
                        map(function(pond_ts_new_sub) {
                            # Identify years in time-series with no previous split.
                            no_split_years <- pond_ts_new_sub$multiple_intersect %>%
                                unlist(recursive = TRUE) %>%
                                unique() %>%
                                {
                                    !(years %in% .)
                                } %>%
                                years[.] %>%
                                .[. < current_year]
                            # Check for any overlap
                            is_consistent <- sapply(no_split_years, function(x) {
                                check_intersection(
                                    unlist(pond_ts_new_sub[1, colnames(pond_ts_new_sub) == current_year + 1]),
                                    unlist(pond_ts_new_sub[1, colnames(pond_ts_new_sub) == x]),
                                    ponds
                                )
                            }) %>%
                                any()
                            # Add new reulst to tibble and return
                            pond_ts_new_sub %>%
                                mutate(is_consistent = is_consistent) %>%
                                return()
                        }) %>%
                        bind_rows()

                    # If current year is the first year, assume the time-series
                    # are consistent
                    if (current_year == start_year) pond_ts_new$is_consistent <- TRUE
                    
                    # Filter out inconsistent time_series
                    pond_ts_new <- filter(pond_ts_new, is_consistent) %>%
                        select(-is_consistent)
                    
                    # Update identifier if new rows were added
                    if (nrow(pond_ts_new) > 1) {
                        pond_ts_new <- mutate(pond_ts_new, ts_id = paste0(ts_id, "_", letters[1:nrow(pond_ts_new)]))
                    }
                    pond_ts <- pond_ts_new
                } else if (n_ids_next_year == 1) {
                    # If not, check whether there is exactly one pond overlapping
                    # If yes, assign pond id to year colum
                    pond_ts <- mutate(
                        pond_ts,
                        !!as.character(current_year + 1) := ids_next_year
                    )
                } else {
                    # If still not, then assing NA to the year in question
                    pond_ts <- mutate(
                        pond_ts,
                        !!as.character(current_year + 1) := NA
                    )
                }
                # Return updated time-series
                return(pond_ts)
            }) %>%
            bind_rows()
        # Move to next year
        current_year <- current_year + 1
        # Status update
                cat("\n")
    }
    # Status
    cat("\n")

    # Return time-series
    return(time_series)
}
# Test function
# track_ponds(2014, ponds)

# Map accross start years
time_series <- map(years, track_ponds, ponds = ponds) %>% bind_rows()

# Filter out time-series with less than three occurances
time_series <- time_series %>%
    relocate(ts_id, multiple_intersect)
time_series$n_obs <- select(time_series, 3:last_col()) %>%
    as.matrix() %>%
    apply(1, function(x) sum(!is.na(x)))
time_series <- filter(time_series, n_obs >= 3) %>%
    relocate(ts_id, multiple_intersect, n_obs)

# Concatenate combination of ponds into one colum
# time_series <- time_series %>% select(-last_col())
time_series$combination <- select(time_series, 4:last_col()) %>%
    split(1:nrow(.)) %>%
    map(function(x) {
        x <- unlist(x, recursive = T) %>%
            na.omit() %>%
            sort()
        attributes(x) <- NULL
        return(x)
    }) 


# Retain only unique combinations, including removing subsets of other combinations
time_series$is_unique <- time_series$combination %>%
    # For each combination ...
    sapply(function(x) {
        # Determine how many other combinations it is contained in
        n_combies_contained_in <- sapply(
            time_series$combination,
            function(y) all(x %in% y)
        ) %>%
            sum()
        # Check whether it is one (i.e. quniue) and return
        return(n_combies_contained_in == 1)
    })
time_series <- time_series %>%
    filter(is_unique) %>%
    select(-is_unique)

# Summarise findings
cat("Identified", nrow(time_series), "unique time-series!!!\n")

# Plot time-series
time_series %>%
    split(time_series$ts_id) %>%
    sapply(function(x) {
        # Get ponds for time_series
        ts_ponds <- filter(ponds, id %in% unlist(x$combination))
        print(ggplot(data = ts_ponds) +
            geom_sf(fill = NA, size = 2) +
            geom_sf_text(aes(label = id)) +
            facet_wrap(~year, nrow = 1) +
            coord_sf(
                xlim = st_bbox(ts_ponds)[c(1, 3)],
                ylim = st_bbox(ts_ponds)[c(2, 4)]
            ) +
            labs(title = x$ts_id) +
            theme_map() +
            theme(legend.position = "none"))
        return(NULL)
    })

# Expecting 14 pond time-series -> the algorithm seems to work well! :)