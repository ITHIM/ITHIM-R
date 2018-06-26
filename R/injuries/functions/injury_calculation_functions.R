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
library(tools)
library(tcltk2)
library(dplyr)
library(mvtnorm)
library(distr)

if(exists('useShiny')) rm(useShiny)

## choose whether to compute or recompute
getModelFits <- function(object_store,input){
  ##TODO replicate all input objects as object_store objects. Check them against each other when computing/plotting.
  ## recompute if SIN, data, model changed
  ## could keep same formula?
  ## resummarise if lq, uq, SIN uncertainty, reporting rate changed
  
  
  ## process data if they haven't been processed yet
  if(is.null(object_store$scenario_tabs)){
    object_store <- processData(object_store)
  }
  
  ## fit models if they haven't been fit yet
  if(is.null(object_store$fit_whw)&&is.null(object_store$fit_nov)){
    object_store <- fitModel(object_store,input)
  }else{ ## re-fit models if model type or sin have changed
    if((as.numeric(object_store$model=='poisson')!=as.numeric(object_store$fit_whw[[1]]$family$family=='poisson'))||
        input$sin!=object_store$sin ){
      object_store <- fitModel(object_store,input)
    #}else{
    #  object_store$scenario_tabs <- get_scenarios(object_store)
    }##TODO}else{ if 
      #(quantiles in the model different from quantile inputs and sin T/F in the model different from sin T/F in the inputs)
      #calculateModels
    
  }
  
  object_store$summary_table <- assemble_output(object_store,input)
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
  ##TODO neaten up corrections to column names
  if(any(c('Age','age')%in%names(distance))){
    ageind <- which(names(distance)%in%c('Age','age'))
    names(distance)[ageind] <- 'age'
  }
  if(any(c('Sex','sex','Gender','gender','Gen','gen')%in%names(distance))){
    genind <- which(names(distance)%in%c('Sex','sex','Gender','gender','Gen','gen'))
    names(distance)[genind] <- 'gender'
  }
  
  ## coding, assuming only two levels are provided in ICD10
  covlabels <- list()
  v_codes <- c('pedestrian','cyclist','motorcycle','three-wheel','car','van','hgv','bus')
  covlabels[['casualty mode']] <- intersect(v_codes,sapply(names(distance),function(y)strsplit(y,'[. ]+')[[1]][1]))
  covlabels[['strike mode']] <- c('pedestrian','cyclist','motorcycle','car/van','hgv/bus','rail','nonmotor','object','noncollision','other')
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
  covariates <- names(covlabels)
  dimsize <- c()
  # start ages to use as distance ages
  useAges <- unique(distance$age)
  for(i in 1:(minV-1))
    if(length(unique(unlist(injuries[i,])))>1){
      ##TODO harmonise ages
      if(grepl('age',tolower(rownames(injuries)[i]))){
        newAges <- unique(unlist(injuries[i,]))
        if(!identical(newAges,useAges)){
          ##TODO popup? what to do if they don't intersect. What to do if there's striker age.
          cat('Keeping ages \n')
          useAges <- intersect(newAges,useAges)
          cat(useAges)
          injuries[i,] <- sapply(injuries[i,],function(x)useAges[last(which(useAges<=as.numeric(x)))])
        }
      }
      covariates <- c(covariates,tolower(rownames(injuries)[i]))
      covlabels[[length(covlabels)+1]] <- tolower(unique(unlist(injuries[i,])))
      dimsize <- c(dimsize,length(unique(unlist(injuries[i,]))))
    }
  covariates <- sapply(covariates,function(x)paste(strsplit(x,' ')[[1]],collapse='_'))
  names(covlabels) <- covariates
  
  # tabulate
  fatal <- injuries[minV:nrow(injuries),]
  fatal <- fatal[v_codes[as.numeric(substr(rownames(fatal),2,2))+1]%in%covlabels[[1]],]
  tab <- expand.grid(covlabels)
  names(tab) <- covariates
  #names(tab) <- lapply(names(tab),function(x)paste(strsplit(x,' ')[[1]],collapse='_'))
  tab$count <- 0
  ##TODO rewrite to be less specific to Mexico example
  if(exists('useShiny')){
    for(i in 1:length(tab$count)){
      tab$count[i] <- sum(as.numeric(fatal[v_codes[as.numeric(substr(rownames(fatal),2,2))+1]==as.character(tab[[covariates[1]]][i])&
          covlabels[[2]][as.numeric(substr(rownames(fatal),3,3))+1]==as.character(tab[[covariates[2]]][i]),
        injuries[minV-2,]==tolower(as.character(tab[[covariates[3]]][i]))&tolower(injuries[minV-1,])==as.character(tab[[covariates[4]]][i])]))
    }
  }else{
    withProgress(message = 'Merging distance and injury data', value = 0, {
      for(i in 1:length(tab$count)){
        tab$count[i] <- sum(as.numeric(fatal[v_codes[as.numeric(substr(rownames(fatal),2,2))+1]==as.character(tab[[covariates[1]]][i])&
            covlabels[[2]][as.numeric(substr(rownames(fatal),3,3))+1]==as.character(tab[[covariates[2]]][i]),
          injuries[minV-2,]==tolower(as.character(tab[[covariates[3]]][i]))&tolower(injuries[minV-1,])==as.character(tab[[covariates[4]]][i])]))
        incProgress(1/length(tab$count))
      }
    })
  }
  tab$count[is.na(tab$count)] <- 0
  ##TODO can we assess here how well various models might work?
  ##TODO what if we don't have casualty_age?
  # merge in distance data
  #tab$casualty_age <- as.numeric(levels(tab$casualty_age))[tab$casualty_age] 
  ##TODO store tab before changing it
  tab0 <- tab
  ## choose age groupings. Start with 1. 
  ##TODO Offer option to change to 2, 3 etc if: (a) very bit data set, or (b) no good fit. Will use tab0.
  tab <- group_by_cas_age(1,tab,distance)
  # separate into WHW and NOV
  ind <- tab$strike_mode%in%covlabels[[2]][whw_codes+1]
  tab_whw <- droplevels(tab[ind,])
  tab_nov <- droplevels(tab[!ind,])
  # set strike distances
  tab_nov$strike_distance <- 1  
  ## assuming strike distance does not depend on age
  ##TODO this will be different if there are any other 'strike' covariates
  tab_whw$strike_distance <- 
    apply(tab_whw[2:4],1,function(x)sum(distance[,names(distance)%in%strsplit((x[1]),'/')[[1]]]))
  
  # add travel totals for SIN
  ##TODO subset when there are more years / road types
  sub_tab <- tab_whw
  for(i in 1:length(covariates))
    if(grepl('strike',covariates[i],ignore.case=T))
      sub_tab <- sub_tab[sub_tab[[i]]==unique(sub_tab[[i]])[1],]
  total_cas_distances <- sapply(unique(sub_tab$casualty_mode),function(x)sum(subset(sub_tab,sub_tab$casualty_mode==x)$cas_distance,na.rm=T))
  tab_whw$total_cas_distance <- total_cas_distances[match(tab_whw$casualty_mode,unique(sub_tab$casualty_mode))]
  tab_nov$total_cas_distance <- total_cas_distances[match(tab_nov$casualty_mode,unique(sub_tab$casualty_mode))]
  sub_tab <- tab_whw
  for(i in 1:length(covariates))
    if(grepl('casualty',covariates[i],ignore.case=T))
      sub_tab <- sub_tab[sub_tab[[i]]==unique(sub_tab[[i]])[1],]
  total_strike_distances <- sapply(unique(sub_tab$strike_mode),function(x)sum(subset(sub_tab,sub_tab$strike_mode==x)$strike_distance,na.rm=T))
  tab_whw$total_strike_distance <- total_strike_distances[match(tab_whw$strike_mode,unique(sub_tab$strike_mode))]
  tab_nov$total_strike_distance <- 1
  
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

setSINvalues <- function(object_store){
  for(i in 1:2){
    ##TODO get better SIN numbers
    if(object_store$sin==F){
      object_store$scenario_tabs[[1]][[i]]$casualty_sin <- 1
      object_store$scenario_tabs[[1]][[i]]$strike_sin <- 1
    }else{
      sin <- readRDS(object_store$sinfile)
      sin_temp <- sin[[1]][match(levels(object_store$scenario_tabs[[1]][[i]]$casualty_mode),rownames(sin[[1]])),match(levels(object_store$scenario_tabs[[1]][[i]]$strike_mode),colnames(sin[[1]]))]
      object_store$scenario_tabs[[1]][[i]]$casualty_sin <- apply(cbind(object_store$scenario_tabs[[1]][[i]]$casualty_mode,object_store$scenario_tabs[[1]][[i]]$strike_mode),1,function(x)sin_temp[x[1],x[2]])
      sin_temp <- sin[[2]][match(levels(object_store$scenario_tabs[[1]][[i]]$casualty_mode),rownames(sin[[2]])),match(levels(object_store$scenario_tabs[[1]][[i]]$strike_mode),colnames(sin[[2]]))]
      object_store$scenario_tabs[[1]][[i]]$strike_sin <- apply(cbind(object_store$scenario_tabs[[1]][[i]]$casualty_mode,object_store$scenario_tabs[[1]][[i]]$strike_mode),1,function(x)sin_temp[x[1],x[2]])
    }
    object_store$scenario_tabs[[1]][[i]]$casualty_sin_uncertainty <- 0.2
    object_store$scenario_tabs[[1]][[i]]$strike_sin_uncertainty <- 0.2
  }
  object_store
}

## function to test models
fitModel <- function(object_store,input){
  
  models <- c('poisson','NB')
  object_store$model <- models[as.numeric(input$modeltoggle)+1]
  object_store$sin <- input$sin
  
  object_store <- setSINvalues(object_store)
  
  ##TODO find all numeric quantities
  scenario_tabs <- object_store$scenario_tabs
  for(i in 1:2)
    if(class(scenario_tabs[[1]][[i]]$casualty_age)=='factor')
      object_store$scenario_tabs[[1]][[i]]$casualty_age <- as.numeric(levels(object_store$scenario_tabs[[1]][[i]]$casualty_age)[as.numeric(object_store$scenario_tabs[[1]][[i]]$casualty_age)])
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
  ##TODO tidy this bit
  tabs <- list(tab_whw,tab_nov)
  formula_base <- list()
  formula_base[[1]] <- formula_base[[2]] <- 'count~offset(log(cas_distance)+log(strike_distance)+(casualty_sin-1)*log(total_cas_distance)+(strike_sin-1)*log(total_strike_distance))'
  covariateForm <- covariates
  numericInd <- which(covariates=='casualty_age')
  covariateForm[numericInd] <- 'ns(casualty_age,df=4)'
  covariateFormula <- list(covariateForm,covariateForm)
  for(k in 1:2){
    for(i in 1:length(covariateFormula[[k]])){
      formula <- paste(c(formula_base[[k]],covariateFormula[[k]][i]),collapse='+')
      form <- test_model(formula,tabs[[k]],object_store$model)
      if(form[1] < 10) { formula_base[[k]] <- formula}
      else{
        cov_subset <- c()
        for(l in 1:length(unique(tabs[[k]][[covariates[i]]]))){
          if(sum(tabs[[k]]$count[tabs[[k]][[covariates[i]]]==unique(tabs[[k]][[covariates[i]]])[l]])>6)#(form[1] < 10) 
            cov_subset <- c(cov_subset,paste0("(",covariates[i],"=='",unique(tabs[[k]][[covariates[i]]])[l],"')"))
        }
        if(length(cov_subset)>0) {
          covariateFormula[[k]][i] <- paste0('(',paste(cov_subset,collapse='+'),')')
          formula_base[[k]] <- paste(c(formula_base[[k]],covariateFormula[[k]][i]),collapse='+')
        }
      }
    }
  }
  results <- list()
  if(exists('useShiny')){
    for(k in 1:2){
      results[[k]] <- list()
      for(j in 1:length(groups)){
        formula <- formula_base[[k]]
        for(i in groups[[j]]){
          term <- paste(covariateFormula[[k]][interaction_indices[[i]]],collapse=':')
          formula <- paste(c(formula,term),collapse='+')
        }
        form <- test_model(formula,tabs[[k]],object_store$model)
        results[[k]][[length(results[[k]])+1]] <- list(form,formula)
      }
    }
  }else{
    withProgress(message = 'Testing models', value = 0, {
      for(k in 1:2){
        results[[k]] <- list()
        for(j in 1:length(groups)){
          formula <- formula_base[[k]]
          for(i in groups[[j]]){
            term <- paste(covariateFormula[[k]][interaction_indices[[i]]],collapse=':')
            formula <- paste(c(formula,term),collapse='+')
          }
          form <- test_model(formula,tabs[[k]],object_store$model)
          results[[k]][[length(results[[k]])+1]] <- list(form,formula)
          incProgress(1/length(groups)/2, detail = paste("Trying model ", length(groups)*(k-1)+j))
        }
      }
    })
  }
  
  forms <- list()
  for(i in 1:2){
    if(sum(sapply(results[[i]], function(x)is.null(x[[1]])))>0) results[[i]] <- results[[i]][-which(sapply(results[[i]], function(x)is.null(x[[1]])))]
    stdevs <- sapply(results[[i]],function(x)x[[1]][1])
    aics <- sapply(results[[i]],function(x)x[[1]][2])
    ind1 <- which(rank(stdevs)==1)
    ind2 <- which(rank(aics)==1)
    #print(c(stdevs[ind1],aics[ind1]))
    #print(c(stdevs[ind2],aics[ind2]))
    forms[[i]] <- results[[i]][[ind1]][[2]]
  }
  
  object_store <- calculateModels(forms,object_store)

}

## function to get model fits given formula
calculateModels <- function(forms,object_store){
  # select best model for each data set
  fits <- list()
  tabs <- object_store$scenario_tabs[[1]]
  quantiles <- c(0.5,object_store$lq,object_store$uq)
  #if(object_store$sinuncertainty==F) quantiles <- 0.5
  if(exists('useShiny')){
    for(i in 1:2){
      fits[[i]] <- list()
      # apply to each quantile
      if(object_store$sinuncertainty==F) quantiles <- 0.5
      tab1 <- tabs[[i]]
      for(k in 1:length(quantiles)){
        tab1$casualty_sin <- qnorm(quantiles[k],tabs[[i]]$casualty_sin,tabs[[i]]$casualty_sin_uncertainty)
        tab1$strike_sin <- qnorm(quantiles[k],tabs[[i]]$strike_sin,tabs[[i]]$casualty_sin_uncertainty)
        if(object_store$model=='poisson'){
          suppressWarnings(fit_temp <- glm(as.formula(forms[[i]]),data=tab1,family=poisson()))
        }else if(object_store$model=='NB'){
          suppressWarnings(fit_temp <- glm.nb(as.formula(forms[[i]]),data=tab1,init.theta=50,control=glm.control(maxit=25)))
        }
        # trim glm objects
        fits[[i]][[k]] <- trim_glm_object(fit_temp)
      }
    }
  }else{
    withProgress(message = 'Fitting models', value = 0, {
      for(i in 1:2){
        fits[[i]] <- list()
        # apply to each quantile
        tab1 <- tabs[[i]]
        for(k in 1:length(quantiles)){
          tab1$casualty_sin <- qnorm(quantiles[k],tabs[[i]]$casualty_sin,tabs[[i]]$casualty_sin_uncertainty)
          tab1$strike_sin <- qnorm(quantiles[k],tabs[[i]]$strike_sin,tabs[[i]]$casualty_sin_uncertainty)
          if(object_store$model=='poisson'){
            suppressWarnings(fit_temp <- glm(as.formula(forms[[i]]),data=tab1,family=poisson()))
          }else if(object_store$model=='NB'){
            suppressWarnings(fit_temp <- glm.nb(as.formula(forms[[i]]),data=tab1,init.theta=50,control=glm.control(maxit=25)))
          }
          # trim glm objects
          fits[[i]][[k]] <- trim_glm_object(fit_temp)
          incProgress(1/length(quantiles)/2, detail = paste(length(quantiles)*(i-1)+k,' of ',length(quantiles)*2))
        }
      }
    })
  }
  
  # store everything and return
  object_store$fit_whw <- fits[[1]]
  object_store$fit_nov <- fits[[2]]
  object_store$plotButton <- 1
  object_store$scenario_tabs <- get_scenarios(object_store)
  
  object_store
}

## function to collate output
##TODO generalise to case where we have striker covariates
assemble_output <- function(object_store,input){
  
  if(!is.null(input$sinuncertainty))object_store$sinuncertainty <- input$sinuncertainty
  object_store$rr <- input$rr
  object_store$rrdistribution <- object_store$temp_rrdistribution
  object_store$rrdist <- object_store$temp_rrdist
  object_store$rrp <- object_store$temp_rrp
  
  scenario_tabs <- object_store$scenario_tabs
  lq <- object_store$lq <- input$lq
  uq <- object_store$uq <- input$uq
  
  object_store <- setSINvalues(object_store)
  
  fits <- list(object_store$fit_whw,object_store$fit_nov)
  full_table <- expand.grid(casualty_mode=unique(scenario_tabs[[1]][[1]]$casualty_mode),casualty_age=unique(scenario_tabs[[1]][[1]]$casualty_age),casualty_gender=unique(scenario_tabs[[1]][[1]]$casualty_gender))
  n <- 1000
  quantiles <- c(0.5,lq,uq)
  if(object_store$sinuncertainty==F) quantiles <- 0.5
  print(c(length(fits),length(scenario_tabs)))
  print(lapply(fits,length))
  print(lapply(scenario_tabs,length))
  if(exists('useShiny')){
    for(j in 1:(1+object_store$nScenarios)){
      ##TODO count number of scenarios
      samples <- list()
      for(q in 1:length(quantiles)){
        samples[[q]] <- matrix(0,nrow=dim(full_table)[1],ncol=n)
        for(k in 1:dim(full_table)[1]){
          meanvec <- covmat <- offsets <- list()
          for(i in 1:2){
            fit <-  fits[[i]][[q]]; tab <- scenario_tabs[[j]][[i]]
            tab$casualty_sin <- qnorm(quantiles[q],tab$casualty_sin,tab$casualty_sin_uncertainty)
            tab$strike_sin <- qnorm(quantiles[q],tab$strike_sin,tab$casualty_sin_uncertainty)
            indices <- tab$casualty_mode==full_table$casualty_mode[k]&tab$casualty_age==full_table$casualty_age[k]&tab$casualty_gender==full_table$casualty_gender[k]
            not.na <- !is.na(coef(fit))
            Xp <- model.matrix(formula(fit$terms),data=tab)[,not.na]
            Xp_temp <- Xp[indices,]
            tab_temp <- tab[indices,]
            offsets[[i]] <-  log(tab_temp$cas_distance) + log(tab_temp$strike_distance) + (tab_temp$casualty_sin-1)*log(tab_temp$total_cas_distance) + (tab_temp$strike_sin-1)*log(tab_temp$total_strike_distance)
            meanvec[[i]] <- drop(Xp_temp %*% coef(fit)[not.na])
            covmat[[i]] <- Xp_temp %*% vcov(fit) %*% t(Xp_temp) # diag(sqrt((model.matrix(fit)) %*% vcov(fit) %*% t(model.matrix(fit))))
          }
          if(sum(sapply(meanvec,length))>0)
            samples[[q]][k,] <- rowSums(exp(cbind(rmvnorm(n,meanvec[[1]],covmat[[1]]) + t(replicate(n,offsets[[1]])),rmvnorm(n,meanvec[[2]],covmat[[2]]) + t(replicate(n,offsets[[2]])))))
        }
      }
    }
  }else{
    withProgress(message = 'Assembling output', value = 0, {
      for(j in 1:(1+object_store$nScenarios)){
        ##TODO count number of scenarios
        samples <- list()
        for(q in 1:length(quantiles)){
          samples[[q]] <- matrix(0,nrow=dim(full_table)[1],ncol=n)
          for(k in 1:dim(full_table)[1]){
            meanvec <- covmat <- offsets <- list()
            for(i in 1:2){
              fit <-  fits[[i]][[q]]; tab <- scenario_tabs[[j]][[i]]
              tab$casualty_sin <- qnorm(quantiles[q],tab$casualty_sin,tab$casualty_sin_uncertainty)
              tab$strike_sin <- qnorm(quantiles[q],tab$strike_sin,tab$casualty_sin_uncertainty)
              indices <- tab$casualty_mode==full_table$casualty_mode[k]&tab$casualty_age==full_table$casualty_age[k]&tab$casualty_gender==full_table$casualty_gender[k]
              not.na <- !is.na(coef(fit))
              Xp <- model.matrix(formula(fit$terms),data=tab)[,not.na]
              Xp_temp <- Xp[indices,]
              tab_temp <- tab[indices,]
              offsets[[i]] <-  log(tab_temp$cas_distance) + log(tab_temp$strike_distance) + (tab_temp$casualty_sin-1)*log(tab_temp$total_cas_distance) + (tab_temp$strike_sin-1)*log(tab_temp$total_strike_distance)
              meanvec[[i]] <- drop(Xp_temp %*% coef(fit)[not.na])
              covmat[[i]] <- Xp_temp %*% vcov(fit) %*% t(Xp_temp) # diag(sqrt((model.matrix(fit)) %*% vcov(fit) %*% t(model.matrix(fit))))
            }
            
            incProgress(1/((1+object_store$nScenarios)*length(quantiles)*dim(full_table)[1]), detail = paste((j-1)*length(quantiles)*dim(full_table)[1]+(q-1)*dim(full_table)[1]+k,'/',(1+object_store$nScenarios)*length(quantiles)*dim(full_table)[1]))
            if(sum(sapply(meanvec,length))>0)
              samples[[q]][k,] <- rowSums(exp(cbind(rmvnorm(n,meanvec[[1]],covmat[[1]]) + t(replicate(n,offsets[[1]])),rmvnorm(n,meanvec[[2]],covmat[[2]]) + t(replicate(n,offsets[[2]])))))
          }
        }
      }
    })
    gamma_mat <- matrix(1,nrow=nrow(samples[[1]]),ncol=ncol(samples[[1]]))
    if(object_store$rr){
      gamma_vec <- r(object_store$rrdist)(dim(samples[[1]])[1])
      gamma_mat <- replicate(dim(samples[[1]])[2],gamma_vec)
    }
    full_table[[paste0('scenario_',j-1,'_mean')]] <- apply(samples[[1]]/gamma_mat,1,mean)
    full_table[[paste0('scenario_',j-1,'_median')]] <- apply(samples[[1]]/gamma_mat,1,median)
    full_table[[paste0('scenario_',j-1,'_lq_',lq)]] <- apply(samples[[1]]/gamma_mat,1,quantile,lq)
    full_table[[paste0('scenario_',j-1,'_uq_',uq)]] <- apply(samples[[1]]/gamma_mat,1,quantile,uq)
    #lower <- t(sapply(all_samples[[1]],function(x)apply(x/gamma_mat,2,quantile,lq)))
    #upper <- t(sapply(all_samples[[1]],function(x)apply(x/gamma_mat,2,quantile,uq)))
    if(length(quantiles)>1){
      full_set <- cbind(samples[[2]]/gamma_mat,samples[[3]]/gamma_mat)
      full_table[[paste0('scenario_',j-1,'_lq_',lq)]] <- apply(full_set,1,quantile,lq/2)
      full_table[[paste0('scenario_',j-1,'_uq_',uq)]] <- apply(full_set,1,quantile,1-(1-uq)/2)
    }
  }
  return(full_table)
}

## function to trim object to save
trim_glm_object <- function(obj){
  obj$y <- c()
  obj$model <- c()
  obj$R <- c()
  obj$residuals <- c()
  obj$fitted.values <- c()
  obj$effects <- c()
  #obj$linear.predictors <- c()
  obj$weights <- c()
  obj$prior.weights <- c()
  obj$data <- c()
  obj$family$variance = c()
  #obj$family$dev.resids = c()
  obj$family$aic = c()
  obj$family$validmu = c()
  obj$family$simulate = c()
  #attr(obj$terms,".Environment") = c()
  attr(obj$formula,".Environment") = c()
  obj
}

## function to get scenario data
get_scenarios <- function(object_store){
  scenario_tabs <- object_store$scenario_tabs
  for(j in 1:object_store$nScenarios) {
    scenario_tabs[[1+j]] <- list()
    for(i in 1:2){
      scenario_tabs[[1+j]][[i]] <- scenario_tabs[[1]][[i]]
      scenario_tabs[[1+j]][[i]]$cas_distance[scenario_tabs[[1+j]][[i]]$casualty_mode=='cyclist'] <- 
        scenario_tabs[[1]][[i]]$cas_distance[scenario_tabs[[1+j]][[i]]$casualty_mode=='cyclist']*2
      scenario_tabs[[1+j]][[i]]$strike_distance[scenario_tabs[[1+j]][[i]]$strike_mode=='cyclist'] <- 
        scenario_tabs[[1]][[i]]$strike_distance[scenario_tabs[[1+j]][[i]]$strike_mode=='cyclist']*2
      scenario_tabs[[1+j]][[i]]$total_cas_distance[scenario_tabs[[1+j]][[i]]$casualty_mode=='cyclist'] <- 
        scenario_tabs[[1]][[i]]$total_cas_distance[scenario_tabs[[1+j]][[i]]$casualty_mode=='cyclist']*2
      scenario_tabs[[1+j]][[i]]$total_strike_distance[scenario_tabs[[1+j]][[i]]$strike_mode=='cyclist'] <- 
        scenario_tabs[[1]][[i]]$total_strike_distance[scenario_tabs[[1+j]][[i]]$strike_mode=='cyclist']*2
    }
  }
  scenario_tabs
}

## function to fit model
test_model <- function(formula,data,model){
  if(model=='poisson'){
    form <- test_poisson_model(formula,data)
  }else if(model=='NB'){
    form <- test_NB_model(formula,data)
  }
  form
}

## function to fit negative binomial model
test_NB_model <- function(formula,data,indices){
  out <- tryCatch(
    {
      suppressWarnings(fit <- glm.nb(as.formula(formula),data=data,init.theta=50,control=glm.control(maxit=25)))
      #print(sum(coef(summary(fit))[,2]))
      c(sum(coef(summary(fit))[,2]),fit$aic)
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
      suppressWarnings(fit <- glm(as.formula(formula),data=data,family=poisson(),control=glm.control(maxit=100)))
      #print(sum(coef(summary(fit))[,2]))
      c(sum(coef(summary(fit))[,2]),fit$aic)
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
  ages <- as.numeric(levels(tab$casualty_age))
  # if length of ages is not a multiple of number of ages per group, pre-group final ages
  last_age_to_remove <- length(ages)%%ages_per_group
  if(last_age_to_remove>0){
    ages_to_remove <- ages[(length(ages)-last_age_to_remove+1):length(ages)]
    last_age <- ages[length(ages)-last_age_to_remove]
    tab$casualty_age[tab$casualty_age%in%ages_to_remove] <- last_age
    distance$age[distance$age%in%ages_to_remove] <- last_age
  }
  
  age_indices <- seq(0,ages_per_group-1)
  ages_to_keep <- tab$casualty_age%in%seq(min(ages),max(ages),by=ages_per_group)
  # add up casualty distances
  tab$cas_distance[ages_to_keep] <- 
    apply(tab[ages_to_keep,1:4],1,
      function(x)sum(distance[distance$age%in%ages[age_indices+which(ages==as.numeric(x[3]))]&tolower(distance$gender)==tolower(x[4]),sapply(names(distance),function(y)strsplit(y,'[. ]+')[[1]][1])==x[1]]))
  ##TODO add up striker distances, for cases where striker age is a covariate
  # add up counts
  tab$count[ages_to_keep] <- 
    apply(tab[ages_to_keep,1:4],1,
      function(x)sum(tab$count[tab$casualty_age%in%ages[age_indices+which(ages==as.numeric(x[3]))]&tab$casualty_gender==x[4]&tab$casualty_mode==x[1]&tab$strike_mode==x[2]]))
  tab <- tab[ages_to_keep,]
  return(tab)
}

## generate predictions
pred_generation <- function(fit,tab1,covariate,subgroup,over,overs){
  ##TODO include log_lambda_se here
  tab <- tab1[tab1[[covariate]]==subgroup,]
  not.na <- !is.na(coef(fit))
  Xp <- model.matrix(formula(fit$terms),data=tab1)[tab1[[covariate]]==subgroup,not.na]
  n <- 1000
  samples <- matrix(0,nrow=n,ncol=length(overs))
  for(i in 1:length(overs)){
    Xp_temp <- Xp[tab[[over]]==overs[i],]
    tab_temp <- tab[tab[[over]]==overs[i],]
    offsets <-  log(tab_temp$cas_distance) + log(tab_temp$strike_distance) + (tab_temp$casualty_sin-1)*log(tab_temp$total_cas_distance) + (tab_temp$strike_sin-1)*log(tab_temp$total_strike_distance)
    meanvec <- drop(Xp_temp %*% coef(fit)[not.na])
    covmat <- Xp_temp %*% vcov(fit) %*% t(Xp_temp) # diag(sqrt((model.matrix(fit)) %*% vcov(fit) %*% t(model.matrix(fit))))
    if(length(meanvec)>0) {
      samples[,i] <- rowSums(exp(rmvnorm(n,meanvec,covmat) + t(replicate(n,offsets))))
    }else{
      cat(paste0('Zero distance travelled for ',over,' group ',overs[i],'\n'))
    }
  }
  count <- sapply(overs,function(x)sum(tab1$count[tab1[[over]]==x]))
  list(samples=samples,count=count)
}

prepPlots <- function(object_store,input){
  scenario_tabs <- object_store$scenario_tabs
  lq <- object_store$lq
  uq <- object_store$uq
  covariate <- paste(strsplit(input$group,' ')[[1]],collapse='_')
  subgroup <- input$subgroup
  over <- paste(strsplit(input$over,' ')[[1]],collapse='_')
  # how to handle the two data sets. Either: 1 prediction, summed over; 2 predictions, concatenated; or 2 predictions, appended.
  ##TODO generalise to arbitrary covariates.
  rounds <- c(1,2)
  if(covariate=='strike_mode') rounds <- as.numeric(subgroup%in%unique(scenario_tabs[[1]][[2]][[covariate]]))+1
  samples <- list()
  fits <- list(object_store$fit_whw,object_store$fit_nov)
  quantiles <- c(0.5,lq,uq)
  if(object_store$sinuncertainty==F) quantiles <- 0.5
  ##TODO count number of scenarios
  all_samples <- list()
  for(k in 1:length(quantiles)){
    for(i in rounds){
      overs <- unique(scenario_tabs[[1]][[i]][[over]])
      samples[[i]] <- list()
      for(j in 1:(1+object_store$nScenarios)) {
        fit <- fits[[i]][[k]]; tab1 <- scenario_tabs[[j]][[i]]
        tab1$casualty_sin <- qnorm(quantiles[k],tab1$casualty_sin,tab1$casualty_sin_uncertainty)
        tab1$strike_sin <- qnorm(quantiles[k],tab1$strike_sin,tab1$casualty_sin_uncertainty)
        sam <- pred_generation(fit,tab1,covariate,subgroup,over,overs)
        samples[[i]][[j]] <- sam[[1]]
      }
    }
    ##TODO check whether these make sense when we have additional covariates e.g. strike age
    ##TODO there will be additional constraints, e.g. if covariate=strike age and we want to plot over strike mode, we do not need to concatenate or sum, as we won't use tab_nov
    all_samples[[k]] <- list()
    for(j in 1:(object_store$nScenarios+1)){
      if(covariate=='strike_mode'){ # if covariate=strike mode, we are looking at one option from strike mode, which determines which data set we need
        all_samples[[k]][[j]] <- samples[[rounds]][[j]]
        names <- unique(scenario_tabs[[1]][[rounds]][[over]])
      }else if(over=='strike_mode'){ # if we are plotting over strike mode, we need to calculate two models and concatenate results
        all_samples[[k]][[j]] <- cbind(samples[[1]][[j]],samples[[2]][[j]])
        names <- c(unique(as.character(scenario_tabs[[1]][[1]][[over]])),unique(as.character(scenario_tabs[[1]][[2]][[over]])))
      }else{ # otherwise, we are adding up over strike modes
        all_samples[[k]][[j]] <- samples[[1]][[j]]+samples[[2]][[j]]
        names <- unique(scenario_tabs[[1]][[1]][[over]])
      }
    }
  }
  gamma_mat <- matrix(1,nrow=nrow(all_samples[[1]][[1]]),ncol=ncol(all_samples[[1]][[1]]))
  if(input$rr){
    gamma_vec <- r(object_store$rrdist)(dim(all_samples[[1]][[1]])[1])
    gamma_mat <- replicate(dim(all_samples[[1]][[1]])[2],gamma_vec)
  }
  expected <- t(sapply(all_samples[[1]],function(x)apply(x/gamma_mat,2,median)))
  lower <- t(sapply(all_samples[[1]],function(x)apply(x/gamma_mat,2,quantile,lq)))
  upper <- t(sapply(all_samples[[1]],function(x)apply(x/gamma_mat,2,quantile,uq)))
  if(length(quantiles)>1){
    for(j in 1:(object_store$nScenarios+1)){
      full_set <- rbind(all_samples[[2]][[j]]/gamma_mat,all_samples[[3]][[j]]/gamma_mat)
      lower[j,] <- apply(full_set,2,quantile,lq/2)
      upper[j,] <- apply(full_set,2,quantile,1-(1-uq)/2)
    }
  }
  par(mar=c(7,5,3,1)); 
  plotBars(expected=expected,main=input$subgroup,upper=upper,lower=lower,names=names,SE=input$SE)
}

## function to plot bars
plotBars <- function(expected,main,upper,lower,names,SE){
  maxy <- 1.2*max(expected,upper,lower)
  bar <- barplot(expected,beside=T,las=2,cex.lab=1.5,cex.axis=1.5,main=main,
    ylim=c(0,maxy),col=c('navyblue','darkorange2'),cex.names=1.25,names=names); 
  mtext(2,line=3.5,text='Number of injuries',cex=1.5)
  legend(x=bar[1],y=maxy,legend=c('Baseline','Scenario 1'),fill=c('navyblue','darkorange2'),bty='n',cex=1.25)
  if(SE==T)
    suppressWarnings(arrows(x0=bar,y0=upper,y1=lower,angle=90,code=3,length=0.1))
}

## rewrite predict.lm to take tol parameter to pass to qr.solve
predict.lm <- function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
  interval = c("none", "confidence", "prediction"), level = 0.95, 
  type = c("response", "terms"), terms = NULL, na.action = na.pass, 
  pred.var = res.var/weights, weights = 1, tol = 1e-7, ...) {
  tt <- terms(object)
  if (!inherits(object, "lm")) 
    warning("calling predict.lm(<fake-lm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
    mmDone <- TRUE
    offset <- object$offset
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action, 
      xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses"))) 
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset"))) 
      for (i in off.num) offset <- offset + eval(attr(tt, 
        "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset)) 
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  n <- length(object$residuals)
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p) 
    stats:::qr.lm(object)$pivot[p1]
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) 
    warning("prediction from a rank-deficient fit may be misleading")
  beta <- object$coefficients
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  if (!is.null(offset)) 
    predictor <- predictor + offset
  interval <- match.arg(interval)
  if (interval == "prediction") {
    if (missing(newdata)) 
      warning("predictions on current data refer to _future_ responses\n")
    if (missing(newdata) && missing(weights)) {
      w <- weights.default(object)
      if (!is.null(w)) {
        weights <- w
        warning("assuming prediction variance inversely proportional to weights used for fitting\n")
      }
    }
    if (!missing(newdata) && missing(weights) && !is.null(object$weights) && 
        missing(pred.var)) 
      warning("Assuming constant prediction variance even though model fit is weighted\n")
    if (inherits(weights, "formula")) {
      if (length(weights) != 2L) 
        stop("'weights' as formula should be one-sided")
      d <- if (missing(newdata) || is.null(newdata)) 
        model.frame(object)
      else newdata
      weights <- eval(weights[[2L]], d, environment(weights))
    }
  }
  type <- match.arg(type)
  if (se.fit || interval != "none") {
    w <- object$weights
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
    }
    else scale^2
    if (type != "terms") {
      if (p > 0) {
        XRinv <- if (missing(newdata) && is.null(w)) 
          qr.Q(stats:::qr.lm(object))[, p1, drop = FALSE]
        else X[, piv] %*% qr.solve(qr.R(stats:::qr.lm(object))[p1, 
          p1], tol = tol)
        ip <- drop(XRinv^2 %*% rep(res.var, p))
      }
      else ip <- rep(0, n)
    }
  }
  if (type == "terms") {
    if (!mmDone) {
      mm <- model.matrix(object)
      mmDone <- TRUE
    }
    aa <- attr(mm, "assign")
    ll <- attr(tt, "term.labels")
    hasintercept <- attr(tt, "intercept") > 0L
    if (hasintercept) 
      ll <- c("(Intercept)", ll)
    aaa <- factor(aa, labels = ll)
    asgn <- split(order(aa), aaa)
    if (hasintercept) {
      asgn$"(Intercept)" <- NULL
      avx <- colMeans(mm)
      termsconst <- sum(avx[piv] * beta[piv])
    }
    nterms <- length(asgn)
    if (nterms > 0) {
      predictor <- matrix(ncol = nterms, nrow = NROW(X))
      dimnames(predictor) <- list(rownames(X), names(asgn))
      if (se.fit || interval != "none") {
        ip <- matrix(ncol = nterms, nrow = NROW(X))
        dimnames(ip) <- list(rownames(X), names(asgn))
        Rinv <- qr.solve(qr.R(stats:::qr.lm(object))[p1, p1], tol = tol)
      }
      if (hasintercept) 
        X <- sweep(X, 2L, avx, check.margin = FALSE)
      unpiv <- rep.int(0L, NCOL(X))
      unpiv[piv] <- p1
      for (i in seq.int(1L, nterms, length.out = nterms)) {
        iipiv <- asgn[[i]]
        ii <- unpiv[iipiv]
        iipiv[ii == 0L] <- 0L
        predictor[, i] <- if (any(iipiv > 0L)) 
          X[, iipiv, drop = FALSE] %*% beta[iipiv]
        else 0
        if (se.fit || interval != "none") 
          ip[, i] <- if (any(iipiv > 0L)) 
            as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii, 
              , drop = FALSE])^2 %*% rep.int(res.var, 
                p)
        else 0
      }
      if (!is.null(terms)) {
        predictor <- predictor[, terms, drop = FALSE]
        if (se.fit) 
          ip <- ip[, terms, drop = FALSE]
      }
    }
    else {
      predictor <- ip <- matrix(0, n, 0L)
    }
    attr(predictor, "constant") <- if (hasintercept) 
      termsconst
    else 0
  }
  if (interval != "none") {
    tfrac <- qt((1 - level)/2, df)
    hwid <- tfrac * switch(interval, confidence = sqrt(ip), 
      prediction = sqrt(ip + pred.var))
    if (type != "terms") {
      predictor <- cbind(predictor, predictor + hwid %o% 
          c(1, -1))
      colnames(predictor) <- c("fit", "lwr", "upr")
    }
    else {
      if (!is.null(terms)) 
        hwid <- hwid[, terms, drop = FALSE]
      lwr <- predictor + hwid
      upr <- predictor - hwid
    }
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
    if (type == "terms" && !is.null(terms) && !se.fit) 
      se <- se[, terms, drop = FALSE]
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
    if (se.fit) 
      se <- napredict(na.act, se)
  }
  if (type == "terms" && interval != "none") {
    if (missing(newdata) && !is.null(na.act)) {
      lwr <- napredict(na.act, lwr)
      upr <- napredict(na.act, upr)
    }
    list(fit = predictor, se.fit = se, lwr = lwr, upr = upr, 
      df = df, residual.scale = sqrt(res.var))
  }
  else if (se.fit) 
    list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
  else predictor
}

predict.glm <- function (object, newdata = NULL, type = c("link", "response",
  "terms"), se.fit = FALSE, dispersion = NULL, terms = NULL,
  na.action = na.pass, tol=1e-7, ...){
  type <- match.arg(type)
  na.act <- object$na.action
  object$na.action <- NULL
  if (!se.fit) {
    if (missing(newdata)) {
      pred <- switch(type, link = object$linear.predictors,
        response = object$fitted.values, terms = predict.lm(object,
          se.fit = se.fit, scale = 1, type = "terms",
          terms = terms))
      if (!is.null(na.act))
        pred <- napredict(na.act, pred)
    }
    else {
      pred <- predict.lm(object, newdata, se.fit, scale = 1,
        type = ifelse(type == "link", "response", type),
        terms = terms, na.action = na.action,tol=tol)
      switch(type, response = {
        pred <- family(object)$linkinv(pred)
      }, link = , terms = )
    }
  }
  else {
    if (inherits(object, "survreg"))
      dispersion <- 1
    if (is.null(dispersion) || dispersion == 0)
      dispersion <- summary(object, dispersion = dispersion)$dispersion
    residual.scale <- as.vector(sqrt(dispersion))
    pred <- predict.lm(object, newdata, se.fit, scale = residual.scale,
      type = ifelse(type == "link", "response", type),
      terms = terms, na.action = na.action,tol=tol)
    fit <- pred$fit
    se.fit <- pred$se.fit
    switch(type, response = {
      se.fit <- se.fit * abs(family(object)$mu.eta(fit))
      fit <- family(object)$linkinv(fit)
    }, link = , terms = )
    if (missing(newdata) && !is.null(na.act)) {
      fit <- napredict(na.act, fit)
      se.fit <- napredict(na.act, se.fit)
    }
    pred <- list(fit = fit, se.fit = se.fit, residual.scale = residual.scale)
  }
  pred
}

fast_vcov <- function (lmObject, Xp, diag = TRUE) {
  ## compute Qt
  QR <- lmObject$qr   ## qr object of fitted model
  piv <- QR$pivot    ## pivoting index
  r <- QR$rank    ## model rank / numeric rank
  if (is.unsorted(piv)) {
    ## pivoting has been done
    Qt <- forwardsolve(t(QR$qr), t(Xp[, piv]), r)
  } else {
    ## no pivoting is done
    Qt <- forwardsolve(t(QR$qr), t(Xp), r)
  }
  ## residual variance
  sigma2 <- sum(residuals(lmObject)^2) / df.residual(lmObject)
  ## return
  if (diag) {
    ## return point-wise prediction variance
    ## no need to compute full hat matrix
    return(colSums(Qt ^ 2) * sigma2)
  } else {
    ## return full variance-covariance matrix of predicted values
    return(crossprod(Qt) * sigma2)
  }
}

#shinyApp(ui, server)


