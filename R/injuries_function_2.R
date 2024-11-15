#' Predict injuries
#'
#' Predict injuries for baseline and scenarios based on Poisson regression model fitted
#' on baseline fatality counts and distances
#'
#' This function uses the Poisson regression model built in the \code{\link{distances_for_injury_function()}} to predict fatality
#' counts for the Baseline and all the scenarios. It performs the following steps:
#'
#' \itemize{
#' \item create an injuries data frame containing all the distances travelled by mode, age, sex and scenario
#'
#' \item predict the fatalities for each strike and casualty mode combination, age and sex category
#'   and each scenario (incl Baseline). If the sample mode is set to 'constant' (and not 'sample'),
#'   we also predict upper and lower confidence interval boundaries
#'
#' \item if running in constant mode, create a whw_temp list containing the total predicted
#'   fatality counts for each casualty and strike mode pair for each scenario split into whw
#'   and nov matrices and also give the upper and lower confidence interval limit predictions.
#'   Also create a combined outcome table where NOV fatalities are added as casualty mode
#'   equals strike mode fatalities.
#'   
#'
#' \item create an injuries2 data frame containing the total predicted fatality counts
#'   for each casualty mode by age and sex for each scenario. This dataframe also
#'   contains total death per age and sex category and, for the constant mode the
#'   upper and lower total death predictions of the confidence interval.
#' }
#'
#'
#'
#' @param true_distances data frame containing population distances for each scenario
#' @param injuries_list list of dataframes set up with scenario specific information to supply to regression model for prediction
#' @param reg_model Poisson injury regression model
#' @param constant_mode whether or not we are in constant (vs sampling) mode
#'
#' @return injuries2 - dataframe containing predicted fatality counts for each casualty mode by age and sex and for each scenario, plus confidence interval limits for constant mode
#' @return whw_temp - list containing the fatality predictions for each casualty and strike mode pair split into whw and nov matrices for each scenario. Upper and lower confidence interval predictions are also included for the constant mode
#'
#' @export


injuries_function_2 <- function(true_distances, injuries_list, reg_model, constant_mode = F) {
  # create a list of all cas modes found within the whw and nov matrices
  cas_modes <- unique(as.character(injuries_list[[1]][[1]]$cas_mode))
  if (length(injuries_list[[1]]) == 2) {
    cas_modes <- unique(c(cas_modes, as.character(injuries_list[[1]]$nov$cas_mode)))
  }

  # create matrix containing all age categories for each sex
  demographic <- DEMOGRAPHIC
  demographic$dem_index <- 1:nrow(demographic)
  demographic <- demographic[, -which(colnames(demographic) == "population")]
  colnames(demographic)[which(colnames(demographic) == "age")] <- "age_cat"

  # join demographic with distance information to add demographic index
  injuries <- true_distances
  injuries <- dplyr::left_join(injuries, demographic, by = c("age_cat", "sex"))
  injuries$bus_driver <- 0
  injuries_lb <- injuries_ub <- injuries

  colnames(demographic)[which(colnames(demographic) == "sex")] <- "cas_gender"

  ############ predict fatality counts using the pre-defined Poisson regression model

  whw_temp <- list()
  for (scen in SCEN) { # loop through scenarios incl baseline
    whw_temp[[scen]] <- list()
    for (type in INJURY_TABLE_TYPES) { # loop through whw and nov matrices
      injuries_list[[scen]][[type]]$injury_reporting_rate <- 1

      # Set weight to 1 for prediction
      injuries_list[[scen]][[type]]$weight <- 1

      ## Use link type and use linkinv function to account for positive confidence interval for the poisson distr.
      # Ref: https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/

      # grad the inverse link function
      ilink <- family(reg_model[[type]])$linkinv
      # add fit and se.fit on the **link** scale
      injuries_list[[scen]][[type]] <- bind_cols(
        injuries_list[[scen]][[type]],
        setNames(
          as_tibble(predict(reg_model[[type]],
            newdata = remove_missing_levels(
              reg_model[[type]],
              injuries_list[[scen]][[type]]
            ),
            type = "link", se.fit = TRUE
          )[1:2]),
          c("fit_link", "se_link")
        )
      )

      # create the 95% confidence interval using 2 times the standard error (se) and back-transform
      injuries_list[[scen]][[type]] <- mutate(injuries_list[[scen]][[type]],
        pred = ilink(fit_link),
        pred_ub = ilink(fit_link + (2 * se_link)),
        pred_lb = ilink(fit_link - (2 * se_link))
      )

      # for constant mode aggregate by strike mode and cas mode and create table with columns
      # containing the cas mode and rows for the strike modes for the whw and nov matrices
      if (constant_mode) {
        whw_temp[[scen]][[type]] <- sapply(unique(injuries_list[[scen]][[type]]$cas_mode), function(x) {
          sapply(
            unique(injuries_list[[scen]][[type]]$strike_mode),
            function(y) {
              sum(subset(
                injuries_list[[scen]][[type]],
                cas_mode == x & strike_mode == y
              )$pred, na.rm = T)
            }
          )
        })
        if (type == "whw") {
          colnames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
          rownames(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$strike_mode)
        } else {
          names(whw_temp[[scen]][[type]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
        }
      }

      # if constant_mode add additional tables where the predicted fatalities are aggregated by cas and strike mode for the
      # upper and lower 95% confidence interval limits
      if (constant_mode) {
        for (conf in c("ub", "lb")) { # loop through upper and lower confidence interval limits
          var_name <- paste0(type, "_", conf)
          whw_temp[[scen]][[var_name]] <- sapply(unique(injuries_list[[scen]][[type]]$cas_mode), function(x) {
            sapply(unique(injuries_list[[scen]][[type]]$strike_mode), function(y) {
              sum(subset(
                injuries_list[[scen]][[type]],
                cas_mode == x & strike_mode == y
              )[[paste0("pred_", conf)]], na.rm = T)
            })
          })
          if (type == "whw") {
            colnames(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
            rownames(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$strike_mode)
          } else {
            names(whw_temp[[scen]][[var_name]]) <- unique(injuries_list[[scen]][[type]]$cas_mode)
          }
        }
      }

      # add demographic index to predicted fatality counts
      # dataframe and tibble, returns dataframe
      suppressWarnings(
        injuries_list[[scen]][[type]] <- dplyr::left_join(injuries_list[[scen]][[type]], demographic, by = c("age_cat", "cas_gender"))
      )
    }

    # set all columns values for the respective scenario and casualty mode to 0
    for (injured_mode in cas_modes) {
      for (index in unique(injuries$dem_index)) {
        injuries[
          injuries$scenario == scen & injuries$dem_index == index,
          match(injured_mode, colnames(injuries))
        ] <- 0
        injuries_lb[
          injuries_lb$scenario == scen & injuries_lb$dem_index == index,
          match(injured_mode, colnames(injuries_lb))
        ] <- 0
        injuries_ub[
          injuries_ub$scenario == scen & injuries_ub$dem_index == index,
          match(injured_mode, colnames(injuries_ub))
        ] <- 0
      }
    }

    # update values with new fatality predictions
    for (injured_mode in cas_modes) {
      for (index in unique(injuries$dem_index)) {
        for (type in INJURY_TABLE_TYPES) {
          injuries[injuries$scenario == scen & injuries$dem_index == index, match(injured_mode, colnames(injuries))] <-
            injuries[injuries$scenario == scen & injuries$dem_index == index, match(injured_mode, colnames(injuries))] +
            sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode == injured_mode &
              injuries_list[[scen]][[type]]$dem_index == index, ]$pred, na.rm = T) |> as.numeric()
        }
      }
    }

    # repeat for upper and lower confidence interval limits
    if (constant_mode) {
      for (injured_mode in cas_modes) {
        for (index in unique(injuries$dem_index)) {
          for (type in INJURY_TABLE_TYPES) {
            injuries_lb[injuries_lb$scenario == scen & injuries_lb$dem_index == index, match(injured_mode, colnames(injuries_lb))] <-
              injuries_lb[injuries_lb$scenario == scen & injuries_lb$dem_index == index, match(injured_mode, colnames(injuries_lb))] +
              sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode == injured_mode &
                injuries_list[[scen]][[type]]$dem_index == index, ]$pred_lb, na.rm = T) |> as.numeric()

            injuries_ub[injuries_ub$scenario == scen & injuries_ub$dem_index == index, match(injured_mode, colnames(injuries_ub))] <-
              injuries_ub[injuries_ub$scenario == scen & injuries_ub$dem_index == index, match(injured_mode, colnames(injuries_ub))] +
              sum(injuries_list[[scen]][[type]][injuries_list[[scen]][[type]]$cas_mode == injured_mode &
                injuries_list[[scen]][[type]]$dem_index == index, ]$pred_ub, na.rm = T) |> as.numeric()
          }
        }
      }
    }
    
    # if in constant mode add combined nov and whw fatality counts by looping through the casualty modes and adding the
    # NOV accidents to the respective casualty versus casualty accidents (not for lower and upper bounds)
    # i.e. car with NOV accidents are added to car with car accidents
    
    if (constant_mode) {
      nov_data <- data.frame(as.list(whw_temp[[scen]]$nov))
      cas_data <- data.frame(whw_temp[[scen]]$whw)
      for (c_mode in colnames(nov_data)){ # loop through NOV casualty modes
        if (!c_mode %in% row.names(cas_data)){# if the casualty mode is not a strike mode, then add a row to data
          cas_data[c_mode,] <- NA
          cas_data[c_mode,c_mode] <- nov_data[1,c_mode]
        } else if (c_mode %in% colnames(cas_data)) {
          cas_data[c_mode,c_mode] <- cas_data[c_mode,c_mode] + nov_data[1,c_mode]
        } else {
          cas_data[,c_mode] <- 0 # add new column
          cas_data[c_mode,] <- 0 # add new row
          cas_data[c_mode,c_mode] <- cas_data[c_mode,c_mode] + nov_data[1,c_mode]
        }
      }
      
      # order row and columns names alphabetically
      cas_data <- cas_data[,order(colnames(cas_data))] # column names
      cas_data <- cas_data[order(row.names(cas_data)),]
      
      # add row sums
      cas_data <- cas_data %>% mutate(rowSum = rowSums(across(where(is.numeric)),na.rm=TRUE))
      
      # create first column containing rownames
      cas_data <- data.frame(strike_mode = row.names(cas_data), cas_data)
      
      # calculate column sums
      cas_data <- cas_data %>% adorn_totals("row")
      
      # re-assign first column as rownames
      row.names(cas_data) <- cas_data[,1]
      cas_data <- cas_data %>% dplyr::select(!strike_mode)
      
      # round all values to 1 significant figure
      cas_data <- cas_data %>% mutate_if(is.numeric, round, digits = 1)
      
      whw_temp[[scen]]$combined <- cas_data
      
    }
  
    
  } # end of scen loop


  # Create a total death count by summing across all casualty modes
  # Also remove NAs
  injuries <- injuries %>%
    ungroup() %>%
    mutate(Deaths = rowSums(dplyr::select(., cas_modes %>% as.character()), na.rm = T))

  # remove columns in injuries that still contain the original distances from the true_distances input
  # i.e remove columns where the mode is not in cas_mode
  to_keep <- c("age_cat", "sex", "scenario", "sex_age", "dem_index", levels(cas_modes), "Deaths")
  injuries2 <- injuries %>% dplyr::select(c(to_keep))

  # add lower and upper confidence interval death predictions
  if (constant_mode) {
    injuries_lb <- injuries_lb %>%
      ungroup() %>%
      mutate(Deaths_lb = rowSums(dplyr::select(., cas_modes %>% as.character()), na.rm = T))
    injuries_ub <- injuries_ub %>%
      ungroup() %>%
      mutate(Deaths_ub = rowSums(dplyr::select(., cas_modes %>% as.character()), na.rm = T))

    injuries2 <- dplyr::left_join(injuries2, injuries_lb %>% dplyr::select(age_cat, sex, dem_index, scenario, Deaths_lb),
      by = c("age_cat", "sex", "dem_index", "scenario")
    )
    injuries2 <- dplyr::left_join(injuries2, injuries_ub %>% dplyr::select(age_cat, sex, dem_index, scenario, Deaths_ub),
      by = c("age_cat", "sex", "dem_index", "scenario")
    )
  }

  

  

  list(injuries2, whw_temp)
}


# @title remove_missing_levels
#
# @description Accounts for missing factor levels present only in test data
# but not in train data by setting values to NA, i.e. if the data for which the predictions
# are made contains factor levels which do not appear in the baseline data used to
# parameterize the model, then we set the predictions for those factors to NA.
# Without this function, the entire model outputs would be NA if at least one factor level was unknown
#
# @import magrittr
# @importFrom gdata unmatrix
# @importFrom stringr str_split
#
# @param fit fitted model on training data, i.e. the baseline fatality counts in this case
#
# @param test_data data to make predictions for, i.e. injuries_list with cas, strike mode, age, sex and distance information for all scenarios
#
# @return data.frame with matching factor levels to fitted model
#
# @keywords internal
#
## !! temporary fix for missing (age) factors
#
# Adapted from  https://stackoverflow.com/a/39495480/4185785
#
#' @export


remove_missing_levels <- function(fit, test_data) {
  # drop empty factor levels in test data
  test_data <- as.data.frame(droplevels(test_data))

  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub(
      "[-^0-9]|as.factor|\\(|\\)", "",
      names(unlist(fit$contrasts))
    ))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }

    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>%
      str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels

    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub(
      "[-^0-9]|as.factor|\\(|\\)", "",
      names(unlist(fit$xlevels))
    ))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }

    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }

  # Select column names in test data that are factor predictors in
  # trained model

  predictors <- names(test_data[names(test_data) %in% factors])

  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA

  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i],
    ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data <- droplevels(test_data)
      # issue warning to console
      message(sprintf(
        paste0(
          "Setting missing levels in '%s', only present",
          " in test data but missing in train data,",
          " to 'NA'."
        ),
        var
      ))
    }
  }
  return(test_data)
}
