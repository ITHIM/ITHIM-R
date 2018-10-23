# Prints health burden

# Assumes master-script has already run

require(tidyverse)
require(plotly)

get_health_plot <- function(d, outcome, lt, index, ac, sc, show_injury = T, combined_NCDs = T){
  
  title <- "Reduction in Years of Life Lost (YLL) - compared with Ref Scenario 1"
  
  if (outcome == "Deaths"){
    title <- "Averted number of Deaths - compared with Ref Scenario 1"
  }
  
  sub_pop <- "\n"
  
  lt <- arrange(lt, GBD_name)
  
  
  env_sum <- list()
  # browser()
  
  # print(input$inAccraEnvSc)
  
  for (wi in 1:1){#(length(d$uncertain))){
    # wi <- 1
    
    scen <- names(d$not_uncertain)[wi]
    
    # scen <- input$inAccraEnvSc[wi]
    
    obj <- list()
    
    for (i in 1:length(d$uncertain[[scen]]$outcomes)){
      
      ldat <- d$uncertain[[scen]]$outcomes[[i]]$hb$ylls
      
      if (outcome == "Deaths")
        ldat <- d$uncertain[[scen]]$outcomes[[i]]$hb$deaths
      
      ldat$index <- i
      
      cat(scen, " - " , i, "\n")
      
      obj[[i]] <- ldat
      
    }
    
    
    d <-  bind_rows(obj)
    
    d <- rename(d, "age.band" = "age_cat")
    d <- rename(d, "gender" = "sex")
    
    if (ac != "All"){
      d <- filter(d, age.band == ac)
      sub_pop <- paste(sub_pop, 'age group:', ac, sep = " ")
    }
    
    if (sc != "All"){
      d <- filter(d, gender == sc)
      sub_pop <- paste(sub_pop, 'sex group:', tolower(sc), sep = " ")
    }
    
    nd <- NULL
    # browser()
    for (i in 2:nrow(lt)){
      # i <- 2
      dn1 <- select(d, age.band, gender, index, ends_with(lt$acronym[i])) 
      names(dn1)[4:ncol(dn1)] <- append('Baseline', paste('Scenario', 2:(ncol(dn1) - 3), sep = ' '))
      dn1$cause <- lt$GBD_name[i]
      
      if (is.null(nd))
        nd <- dn1
      else
        nd <- rbind(nd, dn1)
      
    }
    
    # Remove scenario 1
    nd[['Scenario 1']] <- NULL
    
    if (show_injury){
      
      dn1 <- select(d, age.band, gender, index, ends_with('inj'))
      #dn1$base_deaths_inj <- dn1$base_yll_inj <- NULL
      names(dn1)[4:ncol(dn1)] <- append('Baseline', paste('Scenario', 2:(ncol(dn1) - 3), sep = ' '))
      dn1$cause <- 'Road Injuries'
      
      # Remove scenario 1
      dn1[['Scenario 1']] <- NULL
      
      nd <- rbind(nd, dn1)
      
      # Rename total cancers
      nd$cause[nd$cause == "Neoplasms"] <- 'Total Cancers'
      
      bd <- nd
      
      nd <- bd
      
      nd <- reshape2::melt(nd, id.vars = c("age.band", "gender", "cause", "index"))
      
      nd$cause <- factor(nd$cause, levels = unique(bd$cause))
      nd <- nd[order(nd$cause),]
      
      # # Combine all cancers together
      # 
      # # Remove cancers from the master table
      # cc <- nd %>% filter(str_detect(cause, "cancer$"))
      # nd <- filter(nd,! cause %in% cc$cause)
      
      nd$value[nd$cause == "Total Cancers"] <- nd$value[nd$cause == "Total Cancers"] - 
        nd$value[nd$cause == "Tracheal, bronchus, and lung cancer"]
      
      nd <- filter(nd, cause != "Tracheal, bronchus, and lung cancer")
      
      bd <- nd
      
      
      if (combined_NCDs){
        nd1 <- filter(nd, cause == 'Road Injuries')
        d1 <- filter(nd, cause != 'Road Injuries') %>% group_by(cause, variable, index) %>% summarise(value = sum(value)) %>% as.data.frame()
        d2 <- data.frame(d1) %>% group_by(variable, index) %>% summarise(cause = "Combined NCDs", value = sum(value)) %>% as.data.frame()
        nd2 <- d2[c(3,1,4,2)]
        
        
        d1 <- nd1 %>% group_by(cause, variable, index) %>% summarise(value = sum(value)) %>% as.data.frame()
        d2 <- data.frame(d1) %>% group_by(variable, index) %>% summarise(cause = 'Road Injuries', value = sum(value)) %>% as.data.frame()
        d2 <- d2[c(3, 1, 4, 2)]
        
        nd <- NULL
        nd <- rbind(nd2, d2)
        
      }
      
      d1 <- nd %>% group_by(cause, variable, index) %>% summarise(value = sum(value)) %>% as.data.frame()
      d2 <- data.frame(d1) %>% group_by(variable, index) %>% summarise(cause = "Total", value = sum(value)) %>% as.data.frame()
      d2 <- d2[c(3, 1, 4, 2)]
      d3 <- rbind(d1, d2)
      
      d3$name <- scen
      
      
      env_sum[[wi]] <- d3
    }
  }
  
  sum_dat <- bind_rows(env_sum)
  
  
  tds <- sum_dat %>% # the names of the new data frame and the data frame to be summarised
    group_by(name, cause, variable) %>%   # the grouping variable
    summarise(ymin = min(value),
              lower = quantile(value, .25),
              middle = median(value),
              upper = quantile(value, .75),
              ymax = max(value),
              mean = mean(value)
      
      
    )
      
      
      
      
      
      # mean = round(mean(value),1), # calculates the mean of each group
      #         min = min(value), # min
      #         max = max(value), # max
      #         sd = round(sd(value),1), # calculates the standard deviation of each group
      #         nv = n(),  # calculates the sample size per group
      #         SEv = sd(value)/sqrt(n()),
      #         ymin = mean - SEv,
      #         ymax = mean + SEv)
  
  
  tds$name <- as.factor(tds$name)
  
  # tds$name  <- factor(tds$name , levels = c("now",
  #                                           "less_background_AP",
  #                                           "less_background_PA",
  #                                           "more_chronic_disease",
  #                                           "safer"))
  
  tds <- arrange(tds, cause, variable, name)
  
  tds$int <- interaction(tds$name, tds$cause, tds$variable)
  
  return(tds)
  
  
}

d <- readr::read_rds("six_by_five_scenarios_1024.Rds")

# Read lookup table
lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

loutcome <- "Deaths"

accra_cols <- c("Baseline" = "#e41a1c", 
                "Scenario 1" = "#377eb8", 
                "Scenario 2" = "#4daf4a", 
                "Scenario 3" = "#984ea3",
                "Scenario 4" = "#80b1d3", 
                "Scenario 5" = "#cc4c02")

tds <- get_health_plot(d, outcome = loutcome, lt = lt, ac = "All", sc = "All", show_injury = T, combined_NCDs = T)


fp <- ggplot(tds, aes(x = cause, y = mean, fill = variable, group = int, 
                      ymin = ymin,
                      lower = lower,
                      middle = middle,
                      upper = upper,
                      ymax = ymax,
                      mean = mean,
                      #SE = SEv,
                      #ymin = SEv - sd,
                      #ymax = SEv + sd,
                      env_name = name)) +
  geom_boxplot(aes(lower = lower, upper = upper, middle = middle, ymax = ymax, ymin = ymin), width = 0.5, alpha = 0.5, stat = "identity") +
  scale_fill_manual(values = accra_cols)  +
  labs(y = paste(loutcome, "(value ± s.d.)"), x = "") + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge2(), colour="black") +
  theme_minimal() 

plotly::ggplotly(fp, tooltip = c("x", "y", "fill", "env_name"))
