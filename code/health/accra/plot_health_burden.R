# Prints health burden

# Assumes master-script has already run

require(tidyverse)
require(plotly)

# Read lookup table
lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

get_health_plot <- function(ds, outcome, lt, index, ac, sc, show_injury = T, combined_NCDs = T){
  
  accra_cols <- c("Baseline" = "#e41a1c", 
                  "Scenario 1" = "#377eb8", 
                  "Scenario 2" = "#4daf4a", 
                  "Scenario 3" = "#984ea3",
                  "Scenario 4" = "#80b1d3", 
                  "Scenario 5" = "#cc4c02")
  
  
  # ds = ldat 
  # outcome = loutcome
  # lt = lt
  # index = i
  # ac = "All"  
  # sc = "All"
  # show_injury = T
  # combined_NCDs = T
  
  
  # outcome <- "Deaths"
  # ac <- "All"
  # sc <- "All"
  
  # lt <- read_csv("data/accra/health/disease_outcomes_lookup.csv")
  title <- paste(index, "Reduction in Years of Life Lost (YLL) - compared with Ref Scenario 1")
  
  lt <- arrange(lt, GBD_name)
  
  if (outcome == "Deaths"){
    d <- ds
    title <- paste(index, "Averted number of Deaths - compared with Ref Scenario 1")
  }else{
    d <- ds
  }
  
  sub_pop <- "\n"
  
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
    dn1 <- select(d, age.band, gender, ends_with(lt$acronym[i])) 
    names(dn1)[3:ncol(dn1)] <- append('Baseline', paste('Scenario', 2:(ncol(dn1) - 2), sep = ' '))
    dn1$cause <- lt$GBD_name[i]
    
    if (is.null(nd))
      nd <- dn1
    else
      nd <- rbind(nd, dn1)
    
  }
  
  # Remove scenario 1
  nd[['Scenario 1']] <- NULL
  
  if (show_injury){
    
    dn1 <- select(d, age.band, gender, ends_with('inj'))
    #dn1$base_deaths_inj <- dn1$base_yll_inj <- NULL
    names(dn1)[3:ncol(dn1)] <- append('Baseline', paste('Scenario', 2:(ncol(dn1) - 2), sep = ' '))
    dn1$cause <- 'Road Injuries'
    
    # Remove scenario 1
    dn1[['Scenario 1']] <- NULL
    
    
    nd <- rbind(nd, dn1)
    
  }
  
  # Rename total cancers
  nd$cause[nd$cause == "Neoplasms"] <- 'Total Cancers'
  
  bd <- nd
  
  nd <- reshape2::melt(nd)
  
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
    d1 <- filter(nd, cause != 'Road Injuries') %>% group_by(cause, variable) %>% summarise(value = sum(value)) %>% as.data.frame()
    d2 <- data.frame(d1) %>% group_by(variable) %>% summarise(cause = "Combined NCDs", value = sum(value)) %>% as.data.frame()
    nd2 <- d2[c(2,1,3)]
    
    d1 <- nd1 %>% group_by(cause, variable) %>% summarise(value = sum(value)) %>% as.data.frame()
    d2 <- data.frame(d1) %>% group_by(variable) %>% summarise(cause = 'Road Injuries', value = sum(value)) %>% as.data.frame()
    d2 <- d2[c(2,1,3)]
    
    nd <- NULL
    nd <- rbind(nd2, d2)
    
  }
  
  d1 <- nd %>% group_by(cause, variable) %>% summarise(value = sum(value)) %>% as.data.frame()
  d2 <- data.frame(d1) %>% group_by(variable) %>% summarise(cause = "Total", value = sum(value)) %>% as.data.frame()
  d2 <- d2[c(2,1,3)]
  d3 <- rbind(d1, d2)
  
  #Replace space with line break
  #d3$cause <- gsub(" ", "\n", d3$cause)
  
  
  
  d3$cause <- factor(d3$cause, levels = unique(d3$cause))
  d3 <- d3[order(d3$cause),]
  
  
  #to_download$plot_data[[isolate(input$accraConditionedPanels)]] <<- d3
  #to_download$plot_data_name[[isolate(input$accraConditionedPanels)]] <<- paste0(tolower(outcome), '-burden')
  
  p <- ggplot(data = d3, aes(x = cause, y = value,
                             fill = variable)) +
    geom_bar(stat = 'identity', position = "dodge", color = "black", alpha = 0.5) + 
    scale_fill_manual(values = accra_cols)  +
    guides(fill = guide_legend(override.aes = list(colour = NULL))) +
    guides(colour = FALSE) +
    ylim(-1 * (max(abs(d3$value)) + 5), max(abs(d3$value)) + 5) + 
    theme_minimal() + 
    theme(axis.text.x=element_text(size = rel(0.8)))
  
  p <- p + labs(title = paste0(title, sub_pop, sep = "\n")) + xlab("") + ylab('<- Harms     Benefits ->') 
  
  return(list(p, d3))
  
  
}

fdata <- list()


d <- readr::read_rds("six_by_five_scenarios_1024.Rds")

loutcome <- "Deaths"

accra_cols <- c("Baseline" = "#e41a1c", 
                "Scenario 1" = "#377eb8", 
                "Scenario 2" = "#4daf4a", 
                "Scenario 3" = "#984ea3",
                "Scenario 4" = "#80b1d3", 
                "Scenario 5" = "#cc4c02")

for (wi in 1:(length(d$uncertain))){
  
  scen <- names(d$not_uncertain)[wi]
  
  
  l <- htmltools::tagList()
  
  cd <- list()
  
  
  for (i in 1:length(d$uncertain[[scen]]$outcomes)){
    
    # i <- 1
    
    ldat <- d$uncertain[[scen]]$outcomes[[i]]$hb$ylls
    
    if (loutcome == "Deaths")
      ldat <- d$uncertain[[scen]]$outcomes[[i]]$hb$deaths
    
    
    ldat <- rename(ldat, "age.band" = "age_cat")
    ldat <- rename(ldat, "gender" = "sex")
    
    dat <- get_health_plot(ds = ldat, 
                           outcome = loutcome, lt = lt, index = i, ac = "All", 
                           sc = "All", show_injury = T, combined_NCDs = T)
    l[[i]] <- plotly::as_widget(ggplotly(dat[[1]]))
    
    cd[[i]] <- dat[[2]]
    cd[[i]]$INDEX <- i
    
    # print(l[[i]])
  }
  
  
  base_title <- scen
  
  ctitle <- paste(base_title, ifelse(loutcome == "Deaths", "Averted number of Deaths - compared with Ref Scenario 1",
                   "Reduction in Years of Life Lost (YLL) - compared with Ref Scenario 1"))
  
  
  
  
  pdata <- bind_rows(cd)
  pdata <- arrange(pdata, variable, INDEX, cause)
  pdata$value <- round(pdata$value, 2)
  pdata$int <- interaction(pdata$variable, pdata$INDEX)
  pdata$int <- factor(pdata$int, unique(pdata$int))
  
  pdata$lab <- c(1:16) #ifelse(d$INDEX == 1, "Baseline", "Halved background PA")
  
  pdata$name <- scen
  
  fdata[[wi]] <- pdata
  
  #print(plotly::ggplotly(p))
  
}

combined_data <- bind_rows(fdata)

ctitle <- ifelse(loutcome == "Deaths", "Averted number of Deaths - compared with Ref Scenario 1",
                                   "Reduction in Years of Life Lost (YLL) - compared with Ref Scenario 1")


p <- ggplot(combined_data, aes(x = cause, y = value, fill = variable, color = name)) + 
  geom_boxplot(outlier.shape = NA, position=position_dodge(width = 2)) + 
  scale_fill_manual(values = accra_cols) + 
  scale_colour_brewer(palette = "Set1") + 
  facet_grid(. ~ name, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("") + 
  ylab('<- Harms     Benefits ->')

p <- p + labs(title = ctitle) + xlab("") + ylab('<- Harms     Benefits ->') 

print(p)
#print(plotly::as_widget(ggplotly(p, tooltip = c("x", "y", "fill", "text"))))

td <- combined_data
td$int <- interaction(td$name, td$cause, td$variable)
#td$int <- factor(td$int, unique(td$int))

ggplotly(ggplot(td, aes(x = cause, y = value, fill = variable, group = interaction(name, cause, variable))) + 
           geom_boxplot() +
           scale_fill_manual(values = accra_cols) + 
           theme_minimal() + xlab("") + 
           ylab('<- Harms     Benefits ->'))

tds <- td %>% # the names of the new data frame and the data frame to be summarised
  group_by(name, cause, variable) %>%   # the grouping variable
  summarise(mv = mean(value),  # calculates the mean of each group
            sdv = sd(value), # calculates the standard deviation of each group
            nv = n(),  # calculates the sample size per group
            SEv = sd(value)/sqrt(n()),
            ymin = mv - SEv,
            ymax = mv + SEv) # calculates the standard error of each group

# tds$wi_scen  <- factor(tds$name , levels = c("now",
#   "less_background_AP",
#   "less_background_PA",
#   "more_chronic_disease",
#   "safer"))
# 
# tds <-  tds[order(tds$wi_scen), ]

# tds$cause <- as.factor(tds$cause)
# 
# tds$name <- as.factor(tds$name)
# 
tds$name <- as.factor(tds$name)

tds$int <- interaction(tds$name, tds$cause, tds$name)

tds <- arrange(tds, name, cause, variable)

#tds$lcol <- factor(tds$name, levels = unique(tds$name), labels = c("blue", "green", "yellow", "orange", "red"))
#tds$lt <- factor(tds$name, levels = unique(tds$name), labels = c("solid", "dashed", "dotted", "dotdash", "longdash"))

fp <- ggplot(tds, aes(x = cause, y = mv, fill = variable, group = int, ymin = ymin,
                ymax = ymax,
                name = name)) +
                # colt = lcol)) + 
  geom_bar(stat = 'identity', position = "dodge2", color = 'black') +
  labs(y="Value ± s.d.", x = "Scenario") + 
  geom_errorbar(aes(ymin = mv - sdv, ymax = mv + sdv), position = position_dodge(), colour="black") +
  # scale_linetype_manual(values = rep("solid", 5)) +
  theme_classic() 

plotly::ggplotly(fp, tooltip = c("x", "y", "fill", "name", "colt"))


## Code to use interaction in ggplotly
ggplotly(ggplot(td, aes (x = interaction(name, cause, variable), y = value, fill = variable,
                         text = paste0('~', name))) + geom_boxplot() + xlab (""), tooltip = c("x", "y", "fill", "text"))


