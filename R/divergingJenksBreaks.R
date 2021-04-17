#' Create diverging Jenks Breaks, split at some point.
#'
#' This function takes in data and produces Jenks Breaks, split at some point.
#'
#' @param dat The dataset to use
#' @param var Name of column containing variable to plot.
#' @param bins "c(# of JenksBreaks below split_point, # JenksBreaks above split_point)". Default is c(3, 3).
#' @param split_point The point at which to create two sets of breaks. Default is 0.
#' @return a numeric vector of breaks
#' @export

divergingJenksBreaks <- function(
  dat,
  var,
  bins = c(3, 3), #c(lower_bins, upper_bins)
  split_point = 0) {

    values = data %>%
      dplyr::pull({{var}})

    # add split_point value to range of values
    values = c(split_point, values)

    lower_values = values[which(values <= split_point)]
    upper_values = values[which(values >= split_point)]

    lower_breaks = lower_values %>%
      getJenksBreaks(k = bins[1] + 1)
    upper_breaks = upper_values %>%
      getJenksBreaks(k = bins[2] + 1)

    # Removes duplicate split points in breaks
    breaks = unique(c(lower_breaks, upper_breaks))

    return(breaks)
  }

#
#





## WE CAN PROBABLY SCRAP THIS?? JENKSBREAKS SEEMS BETTER
# # Set up data in quartiles -----------------
# quartile_data <- function(dat, vars, tract_restriction = tracts_use,
#                           labels_cat = quant_labels) {
#   # save old dataset
#   old_dat <- dat
#
#   # Only map tracts with higher populations
#   # use tract_restrictions to pick tracts to map (with sufficient population)
#   dat <- dat %>%
#     #select(tractid10, vars, add_vars) %>%
#     subset(tractid10 %in% tract_restriction$trtid10)
#
#   # add tracts that are not in the data but should be mapped with 0 values
#   add <- tract_restriction %>%
#     select(trtid10) %>%
#     filter(!(trtid10 %in% dat$tractid10))
#   zeros <- data.frame(matrix(0, ncol = length(vars), nrow = nrow(add)))
#   add <- cbind.data.frame(add,zeros)
#   names(add) <- c("tractid10",vars)
#   dat <- rbind.data.frame(dat,add)
#   rm(add, zeros)
#
#   # create quartiles for each variable of interest
#   dat <- data.frame(dat)
#   quant <-
#     foreach(var = vars) %do% {
#       out <- quantile(dat[,var], c(.25, .5, .75, 1), na.rm = TRUE)
#       # add 0 for mapping ranges
#       out <- c(0, out)
#       return(out)
#     }
#   rm(var,out)
#   names(quant) <- vars
#
#   #  Convert values into Quartile Categories
#   foreach(var = vars) %do% {
#     dat[,paste0(var,"_level")] <-
#       cut(dat[,var], quant[[var]],
#           right = FALSE, labels = labels_cat)
#   }
#   rm(var)
#   return(dat)
# }
