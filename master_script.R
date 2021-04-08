#Code to run the model and generate results for the paper:
#Health impacts of changes in travel patterns in Greater Accra Metropolitan Area, Ghana

#Code by Leandro Garcia (l.garcia@qub.ac.uk).
#August 2020.
#R version 4.0.2

#Install and call devtools####
package.check <- if (!require(devtools)) {
    install.packages("devtools", dependencies = TRUE)
    library(devtools)
  }

#Build ITHIM package####
install(dependencies = TRUE, upgrade = "never")

#Call packages and clean global environment####
packages <- c("tidyverse", "earth", "ggpubr", "ithimr")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
    }
  }
)
rm(list = ls())

#Create ITHIM object according to parameters####
#If you just want to test the code, change the NSAMPLES parameter (line 33) in the function below to 5.
ithim_object <- run_ithim_setup(seed = 1,
                                CITY = 'accra',
                                REFERENCE_SCENARIO = 'Scenario 1',
                                NSAMPLES = 1024,
                                BUS_WALK_TIME = c(5, 1.2),
                                MMET_CYCLING = c(4.63, 1.2),
                                MMET_WALKING = c(2.53, 1.1),
                                PM_CONC_BASE = c(50, 1.3),
                                PM_TRANS_SHARE = c(1.5, 5),
                                PA_DOSE_RESPONSE_QUANTILE = T,
                                AP_DOSE_RESPONSE_QUANTILE = T,
                                BACKGROUND_PA_SCALAR = c(1, 1.2),
                                INJURY_REPORTING_RATE = c(8, 3),
                                CHRONIC_DISEASE_SCALAR = c(1, 1.2),
                                SIN_EXPONENT_SUM = c(1.7, 1.03),
                                CASUALTY_EXPONENT_FRACTION = c(15, 15))

#Calculate outcomes####
numcores <- detectCores() - 2
ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = run_ithim, ithim_object = ithim_object,
                     mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

#Gather outcomes####
parameter_names <- names(ithim_object$parameters)[!names(ithim_object$parameters) %in% c('PROPENSITY_TO_TRAVEL','EMISSION_INVENTORY',"DR_AP_LIST")]
parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
outcome_ylls <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$ylls[,(3:ncol(x$hb$ylls))])))
outcome_deaths <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,(3:ncol(x$hb$deaths))])))

#Create function to calculate EVPPI####
compute_single_evppi <- function(sourcesj,case,nscen=1){
  #Initialise vector
  voi <- rep(0,nscen)
  max_degree <- ifelse(is.vector(sourcesj),1,ncol(sourcesj))
  indices <- 1
  #Loop over all scenarios
  for(k in 1:nscen){
    #Extract scenario values and sum
    scen_case <- case[,seq(k,ncol(case),by = nscen)]
    y <- rowSums(scen_case)
    #Compute outcome variance
    vary <- var(y)
    #Model outcome as a function of inputs
    model <- earth(y ~ sourcesj, degree = min(4,max_degree))
    #Compute EVPPI as percentage
    voi[k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  }
  voi
}

#Calculate EVPPI####
  #Assume there is more than one outcome
outcomes <- list(outcome_ylls, outcome_deaths)
outcomes_names <- c("ylls", "deaths")
evppi <- matrix(0, nrow = ncol(parameter_samples), ncol = length(outcomes)*NSCEN)
colnames(evppi) <- apply(expand.grid(SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)],outcomes_names), 1,function(x) paste0(x,collapse='_'))
rownames(evppi) <- colnames(parameter_samples)
for(i in 1:length(outcomes)){
  outcome <- outcomes[[i]]
  for(j in 1:ncol(parameter_samples)){
    sourcesj <- parameter_samples[,j]
    evppi[j,(i - 1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj = sourcesj,case = outcome,nscen = NSCEN)
  }
}
  #Add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain
if(any(grepl('AP_DOSE_RESPONSE_QUANTILE_ALPHA',parameter_names)) && NSAMPLES >= 1024){
  AP_names <- sapply(colnames(parameter_samples),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]]) > 1)
  diseases <- sapply(colnames(parameter_samples)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
  sources <- list()
  for(di in diseases){
    col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x) & grepl(di,x))
    sources[[di]] <- parameter_samples[,col_names]
  }
  evppi_for_AP <- matrix(0,nrow = length(sources),ncol = length(outcomes)*NSCEN)
  rownames(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  for(i in 1:length(outcomes)){
    outcome <- outcomes[[i]]
    for(j in 1:length(sources)){
      sourcesj <- sources[[j]]
      evppi_for_AP[j,(i - 1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj = sourcesj,case = outcome,nscen = NSCEN)
    }
  }
  evppi <- rbind(evppi,evppi_for_AP)
  #Get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!any(c('ALPHA','BETA','GAMMA','TMREL') %in% strsplit(x,'_')[[1]]))
  evppi <- evppi[keep_names,]
}

#Create function to aggregate deaths and YLLS averted by scenario####
outcome_deaths <- as_tibble(outcome_deaths)
outcome_ylls <- as_tibble(outcome_ylls)
compute_aggregated_outcomes <- function(label_scenario){
    #Deaths
    r1 <- dplyr::select(outcome_deaths, starts_with(label_scenario) & !contains("inj") & !contains("_ac") & !contains("_neo")) %>%
      mutate(ncd = rowSums(.)) %>% 
      summarise(lower_bound = quantile(ncd, probs = 0.025),
                mean = mean(ncd),
                upper_bound = quantile(ncd, probs = 0.975)) %>%
      gather %>% rename(ncd_deaths = value)
    
    r2 <- dplyr::select(outcome_deaths, starts_with(label_scenario) & contains("inj")) %>% mutate(inj = rowSums(.)) %>% 
      summarise(lower_bound = quantile(inj, probs = 0.025),
                mean = mean(inj),
                upper_bound = quantile(inj, probs = 0.975)) %>% gather %>% 
      rename(inj_deaths = value)
    
    r3 <- dplyr::select(outcome_deaths, starts_with(label_scenario) & !contains("_ac") & !contains("_neo")) %>%
      mutate(total = rowSums(.)) %>% 
      summarise(lower_bound = quantile(total, probs = 0.025),
                mean = mean(total),
                upper_bound = quantile(total, probs = 0.975)) %>%
      gather %>% rename(total_deaths = value)
    
    #YLLs
    r4 <- dplyr::select(outcome_ylls, starts_with(label_scenario) & !contains("inj") & !contains("_ac") & !contains("_neo")) %>%
      mutate(ncd = rowSums(.)) %>% 
      summarise(lower_bound = quantile(ncd, probs = 0.025),
                mean = mean(ncd),
                upper_bound = quantile(ncd, probs = 0.975)) %>%
      gather %>% rename(ncd_ylls = value)
    
    r5 <- dplyr::select(outcome_ylls, starts_with(label_scenario) & contains("inj")) %>% mutate(inj = rowSums(.)) %>% 
      summarise(lower_bound = quantile(inj, probs = 0.025),
                mean = mean(inj),
                upper_bound = quantile(inj, probs = 0.975)) %>% gather %>% 
      rename(inj_ylls = value)
    
    r6 <- dplyr::select(outcome_ylls, starts_with(label_scenario) & !contains("_ac") & !contains("_neo")) %>%
      mutate(total = rowSums(.)) %>% 
      summarise(lower_bound = quantile(total, probs = 0.025),
                mean = mean(total),
                upper_bound = quantile(total, probs = 0.975)) %>%
      gather %>% rename(total_ylls = value)
    
    rf <- bind_cols(r1, r2[,2], r3[,2], r4[,2], r5[,2], r6[,2])
    rf <- add_column(rf, scenario = label_scenario, .before = 1)
  }

#Calculate deaths and YLLS averted by scenario####
rbase <- compute_aggregated_outcomes("base")
rScen2 <- compute_aggregated_outcomes("scen2")
rScen3 <- compute_aggregated_outcomes("scen3")
rScen4 <- compute_aggregated_outcomes("scen4")
rScen5 <- compute_aggregated_outcomes("scen5")
rAllScen <- bind_rows(rbase, rScen2, rScen3, rScen4, rScen5)
rAllScen_long <- rAllScen %>% gather(outputs, values, -scenario, -key) %>% spread(key, values)

#Generate distributions of uncertain parameters####
parameter_samples.m <- as_tibble(parameter_samples) %>%
dplyr::select("Walk-to-bus time" = BUS_WALK_TIME,
       "Cycling mMETs" = MMET_CYCLING,
       "Walking mMETs" = MMET_WALKING,
       "Scalar for non-travel PA" = BACKGROUND_PA_SCALAR,
       "Background PM concentration" = PM_CONC_BASE,
       "Fraction of PM attributable to transport" = PM_TRANS_SHARE,
       "Injury reporting rate" = INJURY_REPORTING_RATE,
       "Safety-in-numbers (SIN) exponents" = SIN_EXPONENT_SUM,
       "Fraction of SIN exponents due to casuality mode" = CASUALTY_EXPONENT_FRACTION,
       "Scalar for non-communicable diseases" = CHRONIC_DISEASE_SCALAR) %>% 
  reshape2::melt()

parameters_figure <- ggplot(parameter_samples.m, aes(x = value), sprintf("%0.2f", round(a, digits = 2))) +
  stat_density() + 
  facet_wrap(~variable, scales = "free", nrow = 5, ncol = 2) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
  theme_pubr() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8))

ggsave("figure parameters.pdf", plot = parameters_figure, height = 7, dpi = 600)

#Generate bar plots with deaths and YLLs averted####
temp1 <- rAllScen %>% filter(key %in% "mean")

  #NCD deaths
temp2 <- rAllScen_long %>% filter(outputs %in% "ncd_deaths")
fig_ncd_deaths <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = ncd_deaths, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(title = "NCDs", x = "", y = "Deaths averted") +
  ylim(-50, 210) +
    #Legend (for the panel plot)
  scale_fill_discrete(labels = c("2009 mode share",
                                 "Long trips by car to bus trips",
                                 "Car trips to motorcycle trips",
                                 "Short trips by car or motorcycle to cycling trips",
                                 "Short trips by car to walking trips")) +
  guides(fill = guide_legend(title = "Reference scenario: Bus and walking trips to car trips",
                             title.position = "top",
                             nrow = 2, byrow = TRUE, element_text(size = 10)))

  #Road collision deaths
temp2 <- rAllScen_long %>% filter(outputs %in% "inj_deaths")
fig_inj_deaths <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = inj_deaths, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(title = "Road collisions", x = "", y = "") +
  ylim(-1000, 1100)

  #Total deaths
temp2 <- rAllScen_long %>% filter(outputs %in% "total_deaths")
fig_total_deaths <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = total_deaths, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(title = "NCDs + road collisions", x = "", y = "") +
  ylim(-1000, 1100)

  #NCD YLLs
temp2 <- rAllScen_long %>% filter(outputs %in% "ncd_ylls")
fig_ncd_ylls <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = ncd_ylls, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(axis.title.y = element_text(size = 11, face = "bold"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(x = "", y = "YLLs averted") +
  ylim(-2000, 6100)

  #Road collision YLLs
temp2 <- rAllScen_long %>% filter(outputs %in% "inj_ylls")
fig_inj_ylls <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = inj_ylls, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(x = "", y = "") +
  ylim(-50000, 52000)

  #Total YLLs
temp2 <- rAllScen_long %>% filter(outputs %in% "total_ylls")
fig_total_ylls <- ggplot() +
    #Bars
  geom_bar(data = temp1, aes(x = scenario, weight = total_ylls, fill = scenario)) +
    #Uncertainty interval
  geom_errorbar(data = temp2, aes(x = scenario, ymin = lower_bound, ymax = upper_bound),
                size = 0.5, width = 0.3) +
    #Theme
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line()) +
  labs(x = "", y = "") +
  ylim(-50000, 52000)

  #Aggregate and save panel plot
outcomes_figure <- ggarrange(fig_ncd_deaths, fig_inj_deaths, fig_total_deaths,
                            fig_ncd_ylls, fig_inj_ylls, fig_total_ylls,
                            ncol = 3, nrow = 2,
                            common.legend = TRUE,
                            legend = "bottom")
ggsave("figure deaths and ylls.pdf", plot = outcomes_figure, dpi = 600)

#Generate EVPPI figure####
evppi.m <- reshape2::melt(evppi) %>%
  rename(variable = Var1, scenario = Var2) %>% 
  filter(variable != "PA_DOSE_RESPONSE_QUANTILE_all_cause", variable != "PA_DOSE_RESPONSE_QUANTILE_total_cancer")

levels(evppi.m$variable)[levels(evppi.m$variable) == "BUS_WALK_TIME"] <- "Walk-to-bus time"
levels(evppi.m$variable)[levels(evppi.m$variable) == "MMET_CYCLING"] <- "Cycling mMETs"
levels(evppi.m$variable)[levels(evppi.m$variable) == "MMET_WALKING"] <- "Walking mMETs"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PM_CONC_BASE"] <- "Background PM concentration"
levels(evppi.m$variable)[levels(evppi.m$variable) == "BACKGROUND_PA_SCALAR"] <- "Scalar for non-travel PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "CHRONIC_DISEASE_SCALAR"] <- "Scalar for non-communicable diseases"
levels(evppi.m$variable)[levels(evppi.m$variable) == "SIN_EXPONENT_SUM"] <- "Safety-in-numbers (SIN) exponents"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PM_TRANS_SHARE"] <- "Fraction of PM attributable to transport"
levels(evppi.m$variable)[levels(evppi.m$variable) == "INJURY_REPORTING_RATE"] <- "Injury reporting rate"
levels(evppi.m$variable)[levels(evppi.m$variable) == "CASUALTY_EXPONENT_FRACTION"] <- "Fraction of SIN exponents due to casualty mode"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_coronary_heart_disease"] <- "RR of CHD attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_lung_cancer"] <- "RR of lung cancer attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_stroke"] <- "RR of stroke attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_diabetes"] <- "RR of T2D attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_breast_cancer"] <- "RR of breast cancer attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_colon_cancer"] <- "RR of colon cancer attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "PA_DOSE_RESPONSE_QUANTILE_endometrial_cancer"] <- "RR of endometrial cancer attributable to PA"
levels(evppi.m$variable)[levels(evppi.m$variable) == "AP_DOSE_RESPONSE_QUANTILE_cvd_ihd"] <- "RR of CHD attributable to AP"
levels(evppi.m$variable)[levels(evppi.m$variable) == "AP_DOSE_RESPONSE_QUANTILE_neo_lung"] <- "RR of lung cancer attributable to AP"
levels(evppi.m$variable)[levels(evppi.m$variable) == "AP_DOSE_RESPONSE_QUANTILE_resp_copd"] <- "RR of COPD attributable to AP"
levels(evppi.m$variable)[levels(evppi.m$variable) == "AP_DOSE_RESPONSE_QUANTILE_cvd_stroke"] <- "RR of stroke attributable to AP"
levels(evppi.m$variable)[levels(evppi.m$variable) == "AP_DOSE_RESPONSE_QUANTILE_lri"] <- "RR of LRI attributable to AP"

evppi.m$variable <- factor(evppi.m$variable, levels = c("Walk-to-bus time",
                                                        "Cycling mMETs",
                                                        "Walking mMETs",
                                                        "Scalar for non-travel PA",
                                                        "Background PM concentration",
                                                        "Fraction of PM attributable to transport",
                                                        "Injury reporting rate",
                                                        "Safety-in-numbers (SIN) exponents",
                                                        "Fraction of SIN exponents due to casualty mode",
                                                        "Scalar for non-communicable diseases",
                                                        "RR of CHD attributable to PA",
                                                        "RR of stroke attributable to PA",
                                                        "RR of T2D attributable to PA",
                                                        "RR of lung cancer attributable to PA",
                                                        "RR of breast cancer attributable to PA",
                                                        "RR of colon cancer attributable to PA",
                                                        "RR of endometrial cancer attributable to PA",
                                                        "RR of CHD attributable to AP",
                                                        "RR of stroke attributable to AP",
                                                        "RR of lung cancer attributable to AP",
                                                        "RR of COPD attributable to AP",
                                                        "RR of LRI attributable to AP"))

evppi.m$variable <- factor(evppi.m$variable, levels = rev(levels(evppi.m$variable)))
 
  #Deaths
evppi.d <- filter(evppi.m, !grepl("ylls", scenario))
levels(evppi.d$scenario)[levels(evppi.d$scenario) == "scen1_deaths"] <- "2009 mode share"
levels(evppi.d$scenario)[levels(evppi.d$scenario) == "scen2_deaths"] <- "Long trips by car to bus trips"
levels(evppi.d$scenario)[levels(evppi.d$scenario) == "scen3_deaths"] <- "Car trips to motorcycle trips"
levels(evppi.d$scenario)[levels(evppi.d$scenario) == "scen4_deaths"] <- "Short trips by car or motorcycle to cycling trips"
levels(evppi.d$scenario)[levels(evppi.d$scenario) == "scen5_deaths"] <- "Short trips by car to walking trips"

evppi_deaths <- ggplot(evppi.d, aes(scenario, variable, fill = value)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 9, face = "bold"),
        panel.grid.major.y = element_blank()) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 17)) +
  geom_tile(colour = "light grey") +
  scale_fill_gradient(low = "white", high = "darkred", name = "EVPPI (%) ", limit = c(0, 60)) +
  labs(title = "Deaths", x = "", y = "")

  #YLLs
evppi.y <- filter(evppi.m, !grepl("deaths", scenario))
levels(evppi.y$scenario)[levels(evppi.y$scenario) == "scen1_ylls"] <- "2009 mode share"
levels(evppi.y$scenario)[levels(evppi.y$scenario) == "scen2_ylls"] <- "Long trips by car to bus trips"
levels(evppi.y$scenario)[levels(evppi.y$scenario) == "scen3_ylls"] <- "Car trips to motorcycle trips"
levels(evppi.y$scenario)[levels(evppi.y$scenario) == "scen4_ylls"] <- "Short trips by car or motorcycle to cycling trips"
levels(evppi.y$scenario)[levels(evppi.y$scenario) == "scen5_ylls"] <- "Short trips by car to walking trips"

evppi_ylls <- ggplot(evppi.y, aes(scenario, variable, fill = value)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank()) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 17)) +
  geom_tile(colour = "light grey") +
  scale_fill_gradient(low = "white", high = "darkred", name = "EVPPI (%) ", limit = c(0, 60)) +
  labs(title = "Years of life lost", x = "", y = "")

evppi_figure <- ggarrange(evppi_deaths, evppi_ylls, ncol = 1, nrow = 2,
                          common.legend = TRUE,
                          legend = "bottom")

ggsave("figure evppi.pdf", plot = evppi_figure, height = 14, width = 9.5, dpi = 600)

#Travel duration (min) by mode and scenario####
dur_list <- list()
position <- 0
for(i in ithim_object$outcomes) {
  position <- position + 1
  dat <- i$dur
  dat$sample <- position
  dur_list[[position]] <- dat
  }

all_durations <- bind_rows(dur_list)
all_durations <- gather(all_durations, scenario, total_dur, -sample, -stage_mode)

summ_durations <- all_durations %>%
  group_by(stage_mode, scenario) %>%
  summarise(mean_dur = total_dur / nrow(SYNTHETIC_POPULATION)) %>%
  filter(stage_mode == "bicycle" | stage_mode == "walking" | stage_mode == "bus" |
           stage_mode == "motorcycle" | stage_mode == "car" | stage_mode == "taxi") %>%
  group_by(scenario, stage_mode) %>% 
  summarise(lower_bound = round(quantile(mean_dur, probs = 0.025), digits = 1),
            mean = round(mean(mean_dur), digits = 1),
            upper_bound = round(quantile(mean_dur, probs = 0.975), digits = 1))

write.csv(summ_durations, "table 3 - mean travel duration by mode.csv", row.names = FALSE)

#Travel distance (km) by mode and scenario####
dist_list <- list()
position <- 0
for(i in ithim_object$outcomes) {
  position <- position + 1
  dat <- i$dist
  dat$sample <- position
  dist_list[[position]] <- dat
}

all_distances <- bind_rows(dist_list)
all_distances <- gather(all_distances, scenario, total_dist, -sample, -stage_mode)

summ_distances <- all_distances %>%
  group_by(stage_mode, scenario) %>%
  summarise(mean_dist = total_dist / nrow(SYNTHETIC_POPULATION)) %>%
  filter(stage_mode == "bicycle" | stage_mode == "walking" | stage_mode == "bus" |
           stage_mode == "motorcycle" | stage_mode == "car" | stage_mode == "taxi") %>%
  group_by(scenario, stage_mode) %>% 
  summarise(lower_bound = round(quantile(mean_dist, probs = 0.025), digits = 2),
            mean = round(mean(mean_dist), digits = 2),
            upper_bound = round(quantile(mean_dist, probs = 0.975), digits = 2))

write.csv(summ_distances, "table 4 - mean travel distance by mode.csv", row.names = FALSE)

#Physical activity energy expenditure by scenario####
pa_list <- list()
position <- 0
for(i in ithim_object$outcomes) {
  position <- position + 1
  dat <- i$mmets %>% dplyr::select("Baseline" = base_mmet,
                            "Scenario 1" = scen1_mmet,
                            "Scenario 2" = scen2_mmet,
                            "Scenario 3" = scen3_mmet,
                            "Scenario 4" = scen4_mmet,
                            "Scenario 5" = scen5_mmet) %>% 
    gather(scenario, mmets) %>% 
    group_by(scenario) %>% 
    summarise(min = min(mmets),
              p.10 = quantile(mmets, probs = 0.10),
              median = median(mmets),
              mean = mean(mmets),
              p.90 = quantile(mmets, probs = 0.90),
              max = max(mmets))
  dat$sample <- position
  pa_list[[position]] <- dat
}

all_pa <- bind_rows(pa_list)
all_pa <- gather(all_pa, statistic, value, -sample, -scenario)

summ_pa <- all_pa %>%
  group_by(scenario, statistic) %>%
  summarise(lower_bound = round(quantile(value, probs = 0.025), digits = 1),
            mean = round(mean(value), digits = 1),
            upper_bound = round(quantile(value, probs = 0.975), digits = 1))

write.csv(summ_pa, "table 5 - mmets.csv", row.names = FALSE)

#Air pollution exposure by scenario####
  #Background PM concentration
bpm_list <- list()
position <- 0
for(i in ithim_object$outcomes) {
  position <- position + 1
  dat <- i$scenario_pm %>% as_tibble() %>%
    add_column(scenario = c("Baseline", "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5"), .before = 1)
  dat$sample <- position
  bpm_list[[position]] <- dat
}

all_bpm <- bind_rows(bpm_list)

summ_bpm <- all_bpm %>%
  group_by(scenario) %>%
  summarise(lower_bound = round(quantile(value, probs = 0.025), digits = 1),
            mean = round(mean(value), digits = 1),
            upper_bound = round(quantile(value, probs = 0.975), digits = 1))

write.csv(summ_bpm, "table 6 - background PM concetration.csv", row.names = FALSE)

  #Personal exposure
ap_list <- list()
position <- 0
for(i in ithim_object$outcomes) {
  position <- position + 1
  dat <- i$pm_conc_pp %>% dplyr::select("Baseline" = pm_conc_base,
                            "Scenario 1" = pm_conc_scen1,
                            "Scenario 2" = pm_conc_scen2,
                            "Scenario 3" = pm_conc_scen3,
                            "Scenario 4" = pm_conc_scen4,
                            "Scenario 5" = pm_conc_scen5) %>% 
    gather(scenario, pm_conc) %>% 
    group_by(scenario) %>% 
    summarise(min = min(pm_conc),
              p.10 = quantile(pm_conc, probs = 0.10),
              median = median(pm_conc),
              mean = mean(pm_conc),
              p.90 = quantile(pm_conc, probs = 0.90),
              max = max(pm_conc))
  dat$sample <- position
  ap_list[[position]] <- dat
}

all_ap <- bind_rows(ap_list)
all_ap <- gather(all_ap, statistic, value, -sample, -scenario)

summ_ap <- all_ap %>%
  group_by(scenario, statistic) %>%
  summarise(lower_bound = round(quantile(value, probs = 0.025), digits = 1),
            mean = round(mean(value), digits = 1),
            upper_bound = round(quantile(value, probs = 0.975), digits = 1))

write.csv(summ_ap, "table 6 - personal exposure to air pollution.csv", row.names = FALSE)

#Road traffic fatalities per year by scenario####
View(ithim_object$outcomes[[1]]$whw$Baseline$whw)

rtf_base <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$Baseline$whw[,2:ncol(x$whw$Baseline$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Baseline", .before = 1) %>% mutate(total = rowSums(.[2:6]))
  
rtf_Scen1 <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$`Scenario 1`$whw[,2:ncol(x$whw$`Scenario 1`$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Scenario 1", .before = 1) %>% mutate(total = rowSums(.[2:6]))

rtf_Scen2 <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$`Scenario 2`$whw[,2:ncol(x$whw$`Scenario 2`$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Scenario 2", .before = 1) %>% mutate(total = rowSums(.[2:6]))

rtf_Scen3 <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$`Scenario 3`$whw[,2:ncol(x$whw$`Scenario 3`$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Scenario 3", .before = 1) %>% mutate(total = rowSums(.[2:6]))

rtf_Scen4 <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$`Scenario 4`$whw[,2:ncol(x$whw$`Scenario 4`$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Scenario 4", .before = 1) %>% mutate(total = rowSums(.[2:6]))

rtf_Scen5 <- t(sapply(ithim_object$outcomes, function(x) colSums(x$whw$`Scenario 5`$whw[,2:ncol(x$whw$`Scenario 5`$whw)]))) %>%
  as_tibble() %>% add_column(scenario = "Scenario 5", .before = 1) %>% mutate(total = rowSums(.[2:6]))

all_rtf <- bind_rows(rtf_base, rtf_Scen1, rtf_Scen2, rtf_Scen3, rtf_Scen4, rtf_Scen5)
all_rtf <- gather(all_rtf, mode, fatalities, -scenario)
summ_rtf <- all_rtf %>%
  group_by(scenario, mode) %>%
  summarise(lower_bound = round(quantile(fatalities, probs = 0.025), digits = 0),
            mean = round(mean(fatalities), digits = 0),
            upper_bound = round(quantile(fatalities, probs = 0.975), digits = 0))

write.csv(summ_rtf, "table 7 - road traffic fatalities.csv", row.names = FALSE)

#Sensitivity analysis (uncomment next line to run the analysis)
source("./sensitivity_analysis_script.R")

###END OF CODE###