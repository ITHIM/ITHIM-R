total_mmet <- function(rd){
  rd_pa <- subset(rd,trip_mode%in%c('Bicycle','Walking','Short Walking'))
  # Convert baseline's trip duration from mins to hours
  rd_pa$trip_duration_hrs <- rd_pa$trip_duration / 60
  # Get total individual level walking and cycling and sport mmets 
  pa_ind <- setDT(rd_pa)[,.(sex = first(sex), age = first(age),  age_cat = first(age_cat), 
                            cycling_mmet_base = (sum(trip_duration_hrs[trip_mode == 'Bicycle' & scenario == 'Baseline']) ),
                            walking_mmet_base = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Baseline']) ),
                            cycling_mmet_scen1 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'  & scenario == 'Scenario 1']) ),
                            walking_mmet_scen1 = (sum(trip_duration_hrs[trip_mode %in%c('Walking','Short Walking')  & scenario == 'Scenario 1']) ),
                            cycling_mmet_scen2 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 2']) ),
                            walking_mmet_scen2 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 2']) ), 
                            cycling_mmet_scen3 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 3']) ),
                            walking_mmet_scen3 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')  & scenario == 'Scenario 3']) ),
                            cycling_mmet_scen4 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 4']) ),
                            walking_mmet_scen4 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 4']) ), 
                            cycling_mmet_scen5 = (sum(trip_duration_hrs[trip_mode == 'Bicycle'   & scenario == 'Scenario 5']) ),
                            walking_mmet_scen5 = (sum(trip_duration_hrs[trip_mode  %in%c('Walking','Short Walking')   & scenario == 'Scenario 5']) ),
                            work_ltpa_mmet = first(work_ltpa_marg_met)),by='participant_id']
  
  # Calculate MMETs
  pa_ind$base_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_base* MMET_CYCLING + pa_ind$walking_mmet_base * MMET_WALKING
  pa_ind$scen1_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen1* MMET_CYCLING + pa_ind$walking_mmet_scen1 * MMET_WALKING
  pa_ind$scen2_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen2* MMET_CYCLING + pa_ind$walking_mmet_scen2 * MMET_WALKING
  pa_ind$scen3_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen3* MMET_CYCLING + pa_ind$walking_mmet_scen3 * MMET_WALKING
  pa_ind$scen4_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen4* MMET_CYCLING + pa_ind$walking_mmet_scen4 * MMET_WALKING
  pa_ind$scen5_mmet <- pa_ind$work_ltpa_mmet * BACKGROUND_PA_SCALAR +  pa_ind$cycling_mmet_scen5* MMET_CYCLING + pa_ind$walking_mmet_scen5 * MMET_WALKING
  name_indices <- which(colnames(pa_ind)%in%c('participant_id', 'sex', 'age', 'age_cat', paste0(SCEN_SHORT_NAME,'_mmet')))
  mmets <- tbl_df(pa_ind)[,name_indices]
  mmets
  
}