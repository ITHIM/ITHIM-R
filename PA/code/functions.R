# Function to clear columns with labelled class
# taken from: https://stackoverflow.com/a/24070958
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


create.lookups <- function(df, cols){
  lt <- data.frame("name" = as.character(), "id" = as.integer(), "val" = as.character())
  for (i in 1:length(cols)){
    val <- as.character(unique(as_factor(df[[cols[i]]], labels = "values")))
    id <- unique(df[[cols[i]]])
    m <- matrix(c(rep(cols[i], length(id)), id, val), nrow = length(id), ncol = 3)
    lt <- rbind(lt, as.data.frame(m, stringsAsFactors = F))
  }
  colnames(lt) <- c("names", "id", "val")
  lt$id <- as.numeric(lt$id)
  lt
}


mmet2RR <- function(m, cn){
    for (i in 1:length(cn)){
      lcn <- as.character(cn[i])
      m[[lcn]]  <- apply(data.frame(m[[lcn]]), 1, function(x) mmet2RRVal(x[1]))
    }
    m
  }


mmet2RRVal <-function(val) {
  if ((!is.null( val) && !is.na(val))){
    # Cap values at 35 mmet
    mmet2RR_mat[which.min(abs(ifelse(mmet2RR_mat$dose < 35, mmet2RR_mat$dose, 35)  - val)), 2]
  }
  else
    0
}


PAF <- function(pop, attr, cn){
  # pop = rr
  # attr = c('female', 'agecat')
  # cn = c('total_mmet', 'total_mmet_sc')
  
  unique_gender <- unique(pop[[attr[1]]])
  unique_age_group <- unique(pop[[attr[2]]])
  combinations <- length(unique_age_group)*length(unique_gender)
  
  m = matrix(nrow=combinations,ncol=(2 + length(cn)))
  
  colnames(m) <- append(c("age band", "gender"), cn)
  mi <- 1
  
  for (i in 1:length(unique_age_group)){
    
    for (j in 1:length(unique_gender)){
      
      reduced_pop <- filter(pop, UQ(as.name(attr[1])) == unique_gender[j] & UQ(as.name(attr[2])) == unique_age_group[i])
      # cat(attr[1], " - ", unique_gender[j], " - ", attr[2], " - ", unique_age_group[i], " - ", nrow(reduced_pop), "\n")
      
      total_pop <- nrow (reduced_pop)
      active_percent <- nrow (reduced_pop) / total_pop * 100
      non_active_percent <- 100 - active_percent
      size <- nrow (reduced_pop)
      sumPRR <- 0
      sumPRR <- (active_percent * sum (reduced_pop[[cn[1]]]) / nrow(reduced_pop))
      m[mi, 1] = unique_age_group[i]
      m[mi, 2] = unique_gender [j]
      for (k in 1:length(cn)){
        sumPRRi <- 0
        sumPRRi <- (active_percent * sum (reduced_pop[[cn[k]]]) / nrow(reduced_pop))
        PRA <- (sumPRR - sumPRRi) / sumPRR
        m[mi, 2 + k] = round(PRA, digits = 6)
      }
      mi <- mi + 1
    }
  }
  m
}



combine_health_and_pif <- function(pop, hc, hm){
    
    # combine_health_and_pif(pif, gbd_data, "yll")
    # hc <- gbd_data
    # hm <- "YLLs (Years of Life Lost)"
    m <- pif
    n <- pif
    cn <- c("total_mmet", "total_mmet_sc")
    
    for (i in 2:length(cn)){
      for (j in 1:nrow(m)){
        # i <- 2
        new_row <- j
        sub <- filter(hc, sex == m$gender[new_row] & age ==  m$age.band[new_row] & measure == hm & metric == "Number")
        if (length(sub) > 0){
          if (length(sub$val) > 0){
            val <- filter(m, gender == m$gender[new_row] & age.band ==  m$age.band[new_row]) %>% select(cn[i]) %>% as.double()
            
            baseline_val <- filter(m, gender == m$gender[new_row] & age.band ==  m$age.band[new_row]) %>% select(cn[1]) %>% as.double()
            
            n[n$gender == n$gender[new_row] & n$age.band ==  n$age.band[new_row], ][[cn[i]]] <- 
              round((val * as.numeric(sub$val)) / baseline_val, 5)
            
            m[m$gender == m$gender[new_row] & m$age.band == m$age.band[new_row], ][[cn[i]]] <- 
              val * as.numeric(sub$val)
          }else{
            n[n$gender == n$gender[new_row] & n$age.band ==  n$age.band[new_row], ][[cn[i]]] <- 0
            
            m[m$gender == m$gender[new_row] & m$age.band == m$age.band[new_row], ][[cn[i]]] <- 0
          }
        }else{
          n[n$gender == n$gender[new_row] & n$age.band ==  n$age.band[new_row], ][[cn[i]]] <- 0
          
          m[m$gender == m$gender[new_row] & m$age.band == m$age.band[new_row], ][[cn[i]]] <- 0
        }
      }
    }
    list(m, n)
  }
