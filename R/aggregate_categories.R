#' Aggregation for variables.
#'
#' Take mean, median, or sum of all variables by ethnoracial, income, and gentrification grouping.
#'
#' @param dat Data with a column containing census tracts ("tractid10") and categorical variables to group by or numeric variables to aggregate.
#' @param compute "mean", "median", or "sum"
#' @param group_vars Optional list of variables to group_by before aggregation. Note: All non-numeric variables should either be included here or de-selected.
#' @return Aggregated data frame.
#' @export

aggregate_categories = function(
  dat,
  compute = "mean",
  group_vars = c(NULL)) {
  
  library(tidyverse)
  
  dat$tractid10 = as.numeric(dat$tractid10)
  
  # Combine gentcat, racecat, & inccat with data
  dat <- rbind(
    dat %>% mutate(cat = "Overall", facet = "All"),
    dat %>% left_join(gentcat, by = "tractid10"), 
    dat %>% left_join(racecat, by = "tractid10"),
    dat %>% left_join(inccat, by = "tractid10") 
  ) %>%
    dplyr::select(-tractid10) %>%
    filter(!is.na(facet))
  
  # modify mean, median, and sum so that if there are only NAs then it outputs NA
  compute_fn <- function(x, compute) {
    if (all(is.na(x))) {
      x[NA_integer_]
    } else if (compute == "mean") {
      mean(x, na.rm = TRUE)
    } else if (compute == "median") {
      median(x, na.rm = TRUE)
    } else if (compute == "sum") {
      sum(x, na.rm = TRUE)
    } else {
      return("Please select mean, median, or sum.")
    }
  }
  
  group_vars <- enquo(group_vars)
  dat <- dat %>%
    group_by_at(vars(cat, facet, !!group_vars)) %>%
    dplyr::summarise_all(~ compute_fn(., compute)) %>%
    ungroup()
  return(dat)
}
