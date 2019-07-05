#' @export
assign_age_groups <- function(dataset,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE,min_age=AGE_LOWER_BOUNDS[1],age_label='age'){
  dataset_age_label <- dataset[[age_label]]
  dataset <- dataset[dataset_age_label<=max_age&!is.na(dataset_age_label)&dataset_age_label>=min_age,]
  dataset_age_label <- dataset[[age_label]]
  dataset$age_cat <- age_category[length(age_lower_bounds)]#0
  ##!! assuming more than one age category
  for(i in length(age_lower_bounds):2-1){
    dataset$age_cat[dataset_age_label < age_lower_bounds[i+1]] <- age_category[i]
  } # dataset_age_label >= age_lower_bounds[i] & 
  #dataset$age_cat[dataset_age_label >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
  dataset
}