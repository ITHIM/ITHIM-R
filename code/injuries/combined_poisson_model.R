cities <- names(io)[!names(io) %in% 'scen_prop']
injd <- list()

CAS_EXPONENT <- STR_EXPONENT <- 0.85

for (city in cities){
  io[[city]]$inj_distances$reg_model$whw$data$city <- city
  if (length(injd) == 0)
    injd <- io[[city]]$inj_distances$reg_model$whw$data
  else
    injd <- plyr::rbind.fill(injd, io[[city]]$inj_distances$reg_model$whw$data)
}

mod <- glm(formula(io[[city]]$inj_distances$reg_model$whw), data = injd %>% as.data.frame(), family = 'poisson')

whw_ls <- list()

for (city in cities){
  
  print(city)
  
  if (!city  %in% c("belo_horizonte", "medellin")){
    
    nd <- io[[city]]$inj_distances$injuries_list$Baseline$whw %>% mutate(injury_reporting_rate = 
                                                                           unique(io[[city]]$inj_distances$reg_model$whw$data$injury_reporting_rate),
                                                                         weight = 1)
    
    pred <- predict(mod, newdata = remove_missing_levels(injd, nd))
    
    ## grad the inverse link function
    ilink <- family(mod)$linkinv
    ## add fit and se.fit on the **link** scale
    nd <- bind_cols(nd, setNames(as_tibble(predict(mod, newdata = remove_missing_levels(mod, nd), type='link', se.fit = TRUE)[1:2]),
                                 c('fit_link','se_link')))
    ## create the interval and back-transform
    nd <- mutate(nd,
                 pred  = ilink(fit_link),
                 pred_ub = ilink(fit_link + (2 * se_link)),
                 pred_lb = ilink(fit_link - (2 * se_link)))
    
    #nd %>% dplyr::select(pred, pred_ub, pred_lb) %>% summary() %>% print()
    
    whw_mat <- nd %>%  dplyr::select(cas_mode, strike_mode, pred_ub, pred, pred_lb) %>% 
      group_by(strike_mode, cas_mode) %>% 
      summarise(count_range = cur_data() %>% 
                  summarise(across(.fns = ~.x %>% 
                                     sum(na.rm = TRUE) %>% round(2))) %>%
                  unlist() %>% 
                  toString())
    
    nnd <- io[[city]]$inj_distances$injuries_list$Baseline$nov %>% mutate(injury_reporting_rate = 
                                                                           unique(io[[city]]$inj_distances$reg_model$whw$data$injury_reporting_rate),
                                                                         weight = 1)
    
    nov_mod <- io[[city]]$inj_distances$reg_model$nov
    nov_df <- io[[city]]$inj_distances$reg_model$nov$data
    pred <- predict(nov_mod, newdata = nov_df)
    
    ## grad the inverse link function
    ilink <- family(nov_mod)$linkinv
    ## add fit and se.fit on the **link** scale
    nnd <- bind_cols(nnd, setNames(as_tibble(predict(nov_mod, newdata = remove_missing_levels(nov_mod, nnd), type='link', se.fit = TRUE)[1:2]),
                                 c('fit_link','se_link')))
    ## create the interval and back-transform
    nnd <- mutate(nnd,
                 pred  = ilink(fit_link),
                 pred_ub = ilink(fit_link + (2 * se_link)),
                 pred_lb = ilink(fit_link - (2 * se_link)))
    
    #nd %>% dplyr::select(pred, pred_ub, pred_lb) %>% summary() %>% print()
    nov_mat <- nnd %>% dplyr::select(cas_mode, strike_mode, pred_ub, pred, pred_lb) %>% 
      group_by(strike_mode, cas_mode) %>% 
      summarise(count_range = cur_data() %>% 
                  summarise(across(.fns = ~.x %>% 
                                     sum(na.rm = TRUE) %>% round(2))) %>%
                  unlist() %>% 
                  toString())# %>% 
      #pivot_wider(names_from = strike_mode, values_from = count_range)
    
    whw_ls[[city]] <- rbind(whw_mat, nov_mat) #%>% 
      #janitor::adorn_totals(c('row', 'col'))
    
    whw_ls[[city]] %>% print()
      
    
    
  }
}
