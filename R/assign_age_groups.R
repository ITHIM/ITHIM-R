#' Assign age groups to individuals
#' 
#' Prunes dataset given max and min ages; assigns age group labels given age
#' 
#' @param dataset data frame to be edited
#' @param age_category vector of strings giving age categories
#' @param age_lower_bounds lower boundaries of age categories
#' @param max_age maximum age for model
#' @param min_age minimum age for model
#' @param age_label string label for age column
#' 
#' @return edited data frame
#' 
#' @export
assign_age_groups <- function(dataset,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE,min_age=AGE_LOWER_BOUNDS[1],age_label='age'){
  dataset <- dataset[dataset[[age_label]]<=max_age&!is.na(dataset[[age_label]])&dataset[[age_label]]>=min_age,]
  dataset$age_cat <- 0
  ##!! assuming more than one age category
  for(i in 2:length(age_lower_bounds)-1){
    dataset$age_cat[dataset[[age_label]] >= age_lower_bounds[i] & dataset[[age_label]] < age_lower_bounds[i+1]] <- age_category[i]
  }
  dataset$age_cat[dataset[[age_label]] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
  dataset
}
