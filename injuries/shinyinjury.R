setwd('/home/rob/overflow_dropbox/ITHIM-R/injuries/')
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(xlsx)
library(readODS)
library(MASS)
library(splines)
library(gtools)

if(exists('useShiny')) rm(useShiny)

## choose whether to compute or recompute
getModelFits <- function(object_store,input){
  object_store$lq <- input$lq
  object_store$uq <- input$uq
  if(is.null(object_store$scenario_tabs)){
    object_store <- processData(object_store)
  }
  if(is.null(object_store$fit_whw)&&is.null(object_store$fit_nov)){
    object_store <- fitModel(object_store)
  }else{
    if((as.numeric(object_store$model=='poisson')==as.numeric(object_store$fit_whw$family$family=='poisson'))){
      object_store$scenario_tabs <- compute_quantiles(object_store)
    }else{
      object_store <- fitModel(object_store)
    }
  }
  object_store$summary_table <- assemble_output(object_store$scenario_tabs)
  object_store
}

## function to get injury and distance dataset
processData <- function(object_store){
  injuries <- object_store$injuries
  rownames(injuries) <- injuries[,1]
  injuries <- injuries[,-1]
  
  # get distance data
  ##TODO these lines will all change when we compute distance from the synthetic population. This should be constructed to use the labels in covlabels (below). NB 'bus', 'bus passenger'
  distance <- object_store$distance
  names(distance) <- distance[1,]
  distance <- distance[-1,]
  ##TODO choose columns to change based on column names
  distance[,c(2,4:dim(distance)[2])]<-sapply(distance[,c(2,4:dim(distance)[2])],as.numeric)
  
  ## coding, assuming only two levels are provided in ICD10
  covlabels <- list()
  covlabels[[1]] <- c('pedestrian','cyclist','motorcycle','three-wheel','car','van','hgv','bus')
  covlabels[[2]] <- c('pedestrian','cyclist','motorcycle','car/van','hgv/bus','rail','nonmotor','object','noncollision','other')
  # allocate to whw and nov
  ##TODO add in option to put distance for buses and hgvs, distributed systematically across population
  whw_codes <- c()
  nov_codes <- c()
  ##TODO what to do if there are data for e.g. bus but not hgv
  for(i in 1:length(covlabels[[2]])){
    modes <- strsplit(covlabels[[2]][i],'/')[[1]]
    if(sum(modes%in%names(distance))){whw_codes <- c(whw_codes,i-1)}
    else{nov_codes <- c(nov_codes,i-1)}
  }
  ##TODO confirm we won't have 'V00' (Wxx)
  minV <- which(rownames(injuries)=='V01')
  whw <- injuries[c(1:(minV-1),which(sapply(rownames(injuries),function(x)substr(x,3,3))%in%whw_codes)),]
  nov <- injuries[c(1:(minV-1),which(sapply(rownames(injuries),function(x)substr(x,3,3))%in%nov_codes)),]
  
  # get coded covariates
  covariates <- c('Cas mode','Strike mode')
  dimsize <- c()
  for(i in 1:(minV-1))
    if(length(unique(unlist(injuries[i,])))>1){
      covariates <- c(covariates,rownames(injuries)[i])
      covlabels[[length(covlabels)+1]] <- unique(unlist(injuries[i,]))
      dimsize <- c(dimsize,length(unique(unlist(injuries[i,]))))
    }
  covariates <- sapply(covariates,function(x)paste(strsplit(x,' ')[[1]],collapse='_'))
  # tabulate
  fatal <- injuries[minV:nrow(injuries),]
  tab <- expand.grid(covlabels)
  names(tab) <- covariates
  #names(tab) <- lapply(names(tab),function(x)paste(strsplit(x,' ')[[1]],collapse='_'))
  tab$count <- 0
  ##TODO rewrite to be less specific to Mexico example
  if(exists('useShiny')){
    for(i in 1:length(tab$count)){
      tab$count[i] <- sum(as.numeric(fatal[covlabels[[1]][as.numeric(substr(rownames(fatal),2,2))+1]==as.character(tab[[covariates[1]]][i])&
          covlabels[[2]][as.numeric(substr(rownames(fatal),3,3))+1]==as.character(tab[[covariates[2]]][i]),
        injuries[minV-2,]==as.character(tab[[covariates[3]]][i])&injuries[minV-1,]==as.character(tab[[covariates[4]]][i])]))
    }
  }else{
    withProgress(message = 'Merging distance and injury data', value = 0, {
      for(i in 1:length(tab$count)){
        tab$count[i] <- sum(as.numeric(fatal[covlabels[[1]][as.numeric(substr(rownames(fatal),2,2))+1]==as.character(tab[[covariates[1]]][i])&
            covlabels[[2]][as.numeric(substr(rownames(fatal),3,3))+1]==as.character(tab[[covariates[2]]][i]),
          injuries[minV-2,]==as.character(tab[[covariates[3]]][i])&injuries[minV-1,]==as.character(tab[[covariates[4]]][i])]))
        incProgress(1/length(tab$count))
      }
    })
  }
  
  ##TODO can we assess here how well various models might work?
  # merge in distance data
  tab$Casualty_age <- as.numeric(levels(tab$Casualty_age))[tab$Casualty_age] 
  ##TODO store tab before changing it
  tab0 <- tab
  ## choose age groupings. Start with 1. 
  ##TODO Offer option to change to 2, 3 etc if: (a) very bit data set, or (b) no good fit. Will use tab0.
  tab <- group_by_cas_age(3,tab,distance)
  # separate into WHW and NOV
  ind <- tab$Strike_mode%in%covlabels[[2]][whw_codes+1]
  tab_whw <- tab[ind,]
  tab_nov <- tab[!ind,]
  # set strike distances
  tab_nov$strike_distance <- 1  
  ## assuming strike distance does not depend on age
  ##TODO this will be different if there are any other 'strike' covariates
  tab_whw$strike_distance <- 
    apply(tab_whw[2:4],1,function(x)sum(distance[,names(distance)%in%strsplit((x[1]),'/')[[1]]]))
  
  # remove any rows for which 0 distance was travelled
  tab_whw <- tab_whw[tab_whw$cas_distance>0&tab_whw$strike_distance>0,]
  tab_nov <- tab_nov[tab_nov$cas_distance>0,]
  
  ##TODO [[2]] is the scenario. The data will be obtained from somewhere TBC.
  scenario_tabs <- list()
  scenario_tabs[[1]] <- list()
  scenario_tabs[[1]][[1]] <- tab_whw
  scenario_tabs[[1]][[2]] <- tab_nov
  object_store$scenario_tabs <- scenario_tabs
  object_store$covariates <- covariates
  object_store
}

## function to fit model
fitModel <- function(object_store){
  scenario_tabs <- object_store$scenario_tabs
  covariates <- object_store$covariates
  tab_whw <- scenario_tabs[[1]][[1]]
  tab_nov <- scenario_tabs[[1]][[2]]
  ##TODO need clever way to build formula based on covariates
  ## for the small mexico example, we can try all possible models. Not recommended for higher dimensional data sets.
  ##TODO might also want a clever way to choose spline knot number for age
  
  # list all possible interations
  interaction_indices <- list()
  for(i in 2:length(covariates)) {
    comb <- combinations(n=length(covariates),r=i)
    for(j in 1:dim(comb)[1])
      interaction_indices[[length(interaction_indices)+1]] <- comb[j,]
  }
  # list all possible combinations of interations
  combi <- list()
  groups <- list()
  for(j in 1:length(interaction_indices)) groups[[length(groups)+1]] <- j
  ##TODO check max value of j for different data sets. max(j)=length(covariates)?
  for(j in 1:4) {
    combi[[j]] <- combinations(n=length(interaction_indices)-1,r=j+1)
    for(i in dim(combi[[j]])[1]:1)
      if(!contained(lapply(combi[[j]][i,],function(x)interaction_indices[[x]])))
        combi[[j]] <- combi[[j]][-i,]
      for(k in 1:dim(combi[[j]])[1]) groups[[length(groups)+1]] <- combi[[j]][k,]
  }
  # try all models and store results
  # select a different formula for each model
  results_whw <- list()
  results_nov <- list()
  if(exists('useShiny')){
    for(j in 1:length(groups)){
      formula <- 'count~offset(log(cas_distance))+offset(log(strike_distance))'
      for(i in 1:length(covariates)) formula <- paste(c(formula,covariates[i]),collapse='+')
      for(i in groups[[j]]){
        term <- paste(covariates[interaction_indices[[i]]],collapse='*')
        formula <- paste(c(formula,term),collapse='+')
      }
      if(object_store$model=='poisson'){
        form1 <- test_poisson_model(formula,tab_whw)
        form2 <- test_poisson_model(formula,tab_nov)
      }else if(object_store$model=='NB'){
        form1 <- test_model(formula,tab_whw)
        form2 <- test_model(formula,tab_nov)
      }
      results_whw[[length(results_whw)+1]] <- list(form1,formula)
      results_nov[[length(results_nov)+1]] <- list(form2,formula)
    }
  }else{
    withProgress(message = 'Testing models', value = 0, {
      for(j in 1:length(groups)){
        formula <- 'count~offset(log(cas_distance))+offset(log(strike_distance))'
        for(i in 1:length(covariates)) formula <- paste(c(formula,covariates[i]),collapse='+')
        for(i in groups[[j]]){
          term <- paste(covariates[interaction_indices[[i]]],collapse='*')
          formula <- paste(c(formula,term),collapse='+')
        }
        if(object_store$model=='poisson'){
          form1 <- test_poisson_model(formula,tab_whw)
          form2 <- test_poisson_model(formula,tab_nov)
        }else if(object_store$model=='NB'){
          form1 <- test_model(formula,tab_whw)
          form2 <- test_model(formula,tab_nov)
        }
        results_whw[[length(results_whw)+1]] <- list(form1,formula)
        results_nov[[length(results_nov)+1]] <- list(form2,formula)
        incProgress(1/length(groups), detail = paste("Trying model ", j))
      }
    })
  }
  # select best model for each data set
  if(sum(sapply(results_whw, function(x)is.null(x[[1]])))>0) results_whw <- results_whw[-which(sapply(results_whw, function(x)is.null(x[[1]])))]
  aics <- sapply(results_whw,function(x)x[[1]])
  ind1 <- which(rank(aics)==1)
  formula_whw <- results_whw[[ind1]][[2]]
  if(sum(sapply(results_nov, function(x)is.null(x[[1]])))>0) results_nov <- results_nov[-which(sapply(results_nov, function(x)is.null(x[[1]])))]
  aics <- sapply(results_nov,function(x)x[[1]])
  ind1 <- which(rank(aics)==1)
  formula_whw <- results_nov[[ind1]][[2]]
  # get fit for two best models
  if(object_store$model=='poisson'){
    suppressWarnings(fit_whw <- glm(as.formula(formula),data=tab_whw,family=poisson()))
    suppressWarnings(fit_nov <- glm(as.formula(formula),data=tab_nov,family=poisson()))
  }else if(object_store$model=='NB'){
    suppressWarnings(fit_whw <- glm.nb(as.formula(formula),data=tab_whw,init.theta=50,control=glm.control(maxit=25)))
    suppressWarnings(fit_nov <- glm.nb(as.formula(formula),data=tab_nov,init.theta=50,control=glm.control(maxit=25)))
  }
  
  # trim glm objects
  fit_whw <- trim_glm_object(fit_whw)
  fit_nov <- trim_glm_object(fit_nov)
  
  # store everything and return
  object_store$fit_whw <- fit_whw
  object_store$fit_nov <- fit_nov
  object_store$plotButton <- 1
  object_store$scenario_tabs <- compute_quantiles(object_store)
  
  object_store
}

## function to trim object to save
trim_glm_object <- function(obj){
  obj$y <- c()
  obj$model <- c()
  obj$R <- c()
  obj$residuals <- c()
  obj$fitted.values <- c()
  obj$effects <- c()
  obj$qr$qr <- c()
  obj$linear.predictors <- c()
  obj$weights <- c()
  obj$prior.weights <- c()
  obj$data <- c()
  obj$family$variance = c()
  obj$family$dev.resids = c()
  obj$family$aic = c()
  obj$family$validmu = c()
  obj$family$simulate = c()
  #attr(obj$terms,".Environment") = c()
  attr(obj$formula,".Environment") = c()
  obj
}

## function to compute quantiles and means
compute_quantiles <- function(object_store){
  scenario_tabs <- object_store$scenario_tabs
  for(j in 1:object_store$nScenarios) {
    scenario_tabs[[1+j]] <- list()
    for(i in 1:2){
      scenario_tabs[[1+j]][[i]] <- scenario_tabs[[1]][[i]]
      scenario_tabs[[1+j]][[i]]$cas_distance <- scenario_tabs[[1]][[i]]$cas_distance*1.1
    }
  }
  for(j in 1:length(scenario_tabs)){
    for(i in 1:length(scenario_tabs[[j]])){
      ##TODO get SE for NB. Error: singular matrix.
      if(i==1){fit <- object_store$fit_whw}else{fit <- object_store$fit_nov}
      lambda <- predict(fit,newdata=scenario_tabs[[j]][[i]],type='link')
      scenario_tabs[[j]][[i]]$expected_fatalities <- exp(lambda)
      if(fit$family$family=='poisson'){
        scenario_tabs[[j]][[i]]$lower_fatalities <- qpois(object_store$lq,lambda=scenario_tabs[[j]][[i]]$expected_fatalities)
        scenario_tabs[[j]][[i]]$upper_fatalities <- qpois(object_store$uq,lambda=scenario_tabs[[j]][[i]]$expected_fatalities)
      }else{
        size <- fit$theta
        scenario_tabs[[j]][[i]]$lower_fatalities <- qnbinom(object_store$lq,size=size,mu=scenario_tabs[[j]][[i]]$expected_fatalities)
        scenario_tabs[[j]][[i]]$upper_fatalities <- qnbinom(object_store$uq,size=size,mu=scenario_tabs[[j]][[i]]$expected_fatalities)
      }
    }
  }
  scenario_tabs
}

## function to fit negative binomial model
test_model <- function(formula,data,indices){
  out <- tryCatch(
    {
      glm.nb(as.formula(formula),data=data,init.theta=50,control=glm.control(maxit=25))$aic
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={
    }
  )    
  return(out)
}
## function to fit Poisson model
test_poisson_model <- function(formula,data,indices){
  out <- tryCatch(
    {
      glm(as.formula(formula),data=data,family=poisson())$aic
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={
    }
  )    
  return(out)
}
## function to prevent trying too many models
contained <- function(mylist) {
  is.contained <- TRUE
  for(i in 1:length(mylist))
    is.contained <- is.contained*prod(sapply(mylist[-i], function(x) length(x)!=sum(x %in% mylist[[i]])))
  as.logical(is.contained )                  
}

## function to group datasets into fewer age groups
group_by_cas_age <- function(ages_per_group=1,tab,distance){
  ages <- unique(tab$Casualty_age)
  # if length of ages is not a multiple of number of ages per group, pre-group final ages
  last_age_to_remove <- length(ages)%%ages_per_group
  if(last_age_to_remove>0){
    ages_to_remove <- ages[(length(ages)-last_age_to_remove+1):length(ages)]
    last_age <- ages[length(ages)-last_age_to_remove]
    tab$Casualty_age[tab$Casualty_age%in%ages_to_remove] <- last_age
    distance$Age[distance$Age%in%ages_to_remove] <- last_age
  }
  
  age_indices <- seq(0,ages_per_group-1)
  ages_to_keep <- tab$Casualty_age%in%seq(min(ages),max(ages),by=ages_per_group)
  # add up casualty distances
  tab$cas_distance[ages_to_keep] <- 
    apply(tab[ages_to_keep,1:4],1,
      function(x)sum(distance[distance$Age%in%ages[age_indices+which(ages==as.numeric(x[3]))]&distance$Gender==x[4],sapply(names(distance),function(x)strsplit(x,' ')[[1]][1])==x[1]]))
  ##TODO add up striker distances, for cases where striker age is a covariate
  # add up counts
  tab$count[ages_to_keep] <- 
    apply(tab[ages_to_keep,1:4],1,
      function(x)sum(tab$count[tab$Casualty_age%in%ages[age_indices+which(ages==as.numeric(x[3]))]&tab$Casualty_gender==x[4]&tab$Cas_mode==x[1]&tab$Strike_mode==x[2]]))
  tab <- tab[ages_to_keep,]
  return(tab)
}

## generate predictions
pred_generation <- function(tab1,over,overs){
  ##TODO what SEs do we want to plot?
  medians <- sapply(overs,function(x)sum(tab1$expected_fatalities[tab1[[over]]==x]))
  lower <- sapply(overs,function(x)sum(tab1$lower_fatalities[tab1[[over]]==x]))
  upper <- sapply(overs,function(x)sum(tab1$upper_fatalities[tab1[[over]]==x]))
  list(medians,lower,upper)
}

## function to collate output
##TODO generalise to case where we have striker covariates
##TODO add injuries to fatalities
assemble_output <- function(scenario_tabs){
  columnnames <- c('Cas_mode','Casualty_age','Casualty_gender','expected_fatalities','lower_fatalities','upper_fatalities')
  columns <- list()
  columns[[1]] <- which(names(scenario_tabs[[1]][[1]])%in%columnnames)
  columns[[2]] <- which(names(scenario_tabs[[1]][[2]])%in%columnnames)
  full_table <- rbind(scenario_tabs[[1]][[1]][,columns[[1]]],scenario_tabs[[1]][[2]][,columns[[2]]])
  for(j in 2:length(scenario_tabs)){
    for(k in 4:6){
      full_table[[paste0('scenario',j-1,columnnames[k])]] <- c(scenario_tabs[[j]][[1]][[columnnames[k]]],scenario_tabs[[j]][[2]][[columnnames[k]]])
    }
  }
  covNames <- list()
  for(k in 1:3) covNames[[k]] <- unique(full_table[[columnnames[k]]])
  names(covNames) <- columnnames[1:3]
  out <- data.frame(expand.grid(covNames))
  for(k in 4:dim(full_table)[2]){
    colname <- names(full_table)[k]
    out[[colname]] <- apply(out,1,function(x)sum(subset(full_table,Cas_mode==x[1]&Casualty_age==x[2]&Casualty_gender==x[3])[[k]]))
  }
  return(out)
}

prepPlots <- function(object_store,input){
  scenario_tabs <- object_store$scenario_tabs
  covariate <- paste(strsplit(input$group,' ')[[1]],collapse='_')
  subgroup <- input$subgroup
  over <- paste(strsplit(input$over,' ')[[1]],collapse='_')
  # how to handle the two data sets. Either: 1 prediction, summed over; 2 predictions, concatenated; or 2 predictions, appended.
  ##TODO generalise to arbitrary covariates.
  rounds <- c(1,2)
  if(covariate=='Strike_mode') rounds <- as.numeric(subgroup%in%unique(scenario_tabs[[1]][[2]][[covariate]]))+1
  medians <- lower <- upper <- list()
  ##TODO count number of scenarios
  for(i in rounds){
    overs <- unique(scenario_tabs[[1]][[i]][[over]])
    tab <- scenario_tabs[[1]][[i]][scenario_tabs[[1]][[i]][[covariate]]==subgroup,]
    pred <- pred_generation(tab,over,overs)
    medians[[i]] <- pred[[1]]
    lower[[i]] <- pred[[2]]
    upper[[i]] <- pred[[3]]
    for(j in 1:object_store$nScenarios) {
      tab <- scenario_tabs[[j+1]][[i]][scenario_tabs[[j+1]][[i]][[covariate]]==subgroup,]
      pred <- pred_generation(tab,over,overs)
      medians[[i]] <- rbind(medians[[i]],pred[[1]])
      lower[[i]] <- rbind(lower[[i]],pred[[2]])
      upper[[i]] <- rbind(upper[[i]],pred[[3]])
    }
  }
  ##TODO check whether these make sense when we have addition covariates e.g. strike age
  ##TODO there will be additional constraints, e.g. if covariate=strike age and we want to plot over strike mode, we do not need to concatenate or sum, as we won't use tab_nov
  if(covariate=='Strike_mode'){ # if covariate=strike mode, we are looking at one option from strike mode, which determines which data set we need
    medians <- medians[[rounds]]
    lower <- lower[[rounds]]
    upper <- upper[[rounds]]
    names <- unique(scenario_tabs[[1]][[rounds]][[over]])
  }else if(over=='Strike_mode'){ # if we are plotting over strike mode, we need to calculate two models and concatenate results
    medians <- cbind(medians[[1]],medians[[2]])
    lower <- cbind(lower[[1]],lower[[2]])
    upper <- cbind(upper[[1]],upper[[2]])
    names <- c(unique(as.character(scenario_tabs[[1]][[1]][[over]])),unique(as.character(scenario_tabs[[1]][[2]][[over]])))
  }else{ # otherwise, we are adding up over strike modes
    medians <- medians[[1]]+medians[[2]]
    lower <- lower[[1]]+lower[[2]]
    upper <- upper[[1]]+upper[[2]]
    names <- unique(scenario_tabs[[1]][[1]][[over]])
  }
  par(mar=c(7,5,3,1)); 
  plotBars(medians=medians,main=input$subgroup,upper=upper,lower=lower,names=names,SE=input$SE)
}

## function to plot bars
plotBars <- function(medians,main,upper,lower,names,SE){
  bar <- barplot(medians,beside=T,las=2,cex.lab=1.5,cex.axis=1.5,main=main,
    ylim=c(0,max(upper)),col=c('navyblue','darkorange2'),cex.names=1.25,names=names); 
  mtext(2,line=3.5,text='Number of injuries',cex=1.5)
  legend(x=bar[1],y=max(upper),legend=c('Baseline','Scenario 1'),fill=c('navyblue','darkorange2'),bty='n',cex=1.25)
  if(SE==T)
    suppressWarnings(arrows(x0=bar,y0=upper,y1=lower,angle=90,code=3,length=0.1))
}

#shinyApp(ui, server)


