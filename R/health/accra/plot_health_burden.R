# Prints health burden

# Assumes master-script has already run

# Read lookup table
lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

get_health_plot <- function(ds, outcome, lt, index, ac, sc, show_injury = T, combined_NCDs = T){
  
  accra_cols <- c("Baseline" = "#e41a1c", 
                  "Scenario 1" = "#377eb8", 
                  "Scenario 2" = "#4daf4a", 
                  "Scenario 3" = "#984ea3",
                  "Scenario 4" = "#80b1d3", 
                  "Scenario 5" = "#cc4c02")
  
  
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
    names(dn1)[3:ncol(dn1)] <- append('Baseline', paste('Scenario', 1:(ncol(dn1) - 3), sep = ' '))
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
    names(dn1)[3:ncol(dn1)] <- append('Baseline', paste('Scenario', 1:(ncol(dn1) - 3), sep = ' '))
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
  
  p
  
  
}

l <- htmltools::tagList()

for (i in 1:10){
  l[[i]] <- plotly::as_widget(ggplotly((
    get_health_plot(ds = deaths[[i]], outcome = "Deaths", lt = lt, index = i, ac = "All", sc = "All", show_injury = T, combined_NCDs = T)
  )))
  
  print(l[[i]])
}
