##code to convert victim counts of mexico city injury data to long-form injury dataset with striking vehicles using Bogota imputed dataset
##reading victim counts of mexico city using GBD-reported and police-reported data
vic<- read.csv('data/local//mexico_city/injuries_nov.csv')

##reading bogota's imputed injury data using which we will assign striking vehicles
bogota<- read.csv('inst/extdata/local/bogota/injuries_bogota.csv')

##checking to see if the victim mode names are the same as in bogota dataset
unique(vic$mode)
unique(bogota$cas_mode)

###creating long form data for BA injury counts
x<- "NA"
for (i in 1: nrow(vic))
{
  x<-c(x, rep(vic$mode[i],vic$deaths[i]))

}
x<-x[-1]

##creating a dataframe for vicitm and stirking type
whw<-as.data.frame(x)
whw$strike_type<- "NA"
names(whw)[1]<-"cas_type"

##following lines uses the rows in bogota injuyry data for a specific victim type and randomly select a strike vehicle type
##and create 5 different iterations of striking vehicle type to retain variability
##this one excludes "other" as a victim category
for (i  in 1: nrow(whw))
{
  if(whw$cas_type[i]!="other")
  {   
  whw$strike_type[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type2[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type3[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type4[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type5[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  }
  
}
##for "other" victim category we allocated striking vehicle for any of the victim types from bogota dataset
for (i  in 1: nrow(whw))
{
  if(whw$cas_type[i]=="other")
  {
    whw$strike_type[i]<- as.character(sample(bogota$strike_mode,1))
    whw$strike_type2[i]<- as.character(sample(bogota$strike_mode,1))
    whw$strike_type3[i]<- as.character(sample(bogota$strike_mode,1))
    whw$strike_type4[i]<- as.character(sample(bogota$strike_mode,1))
    whw$strike_type5[i]<- as.character(sample(bogota$strike_mode,1))
    
  }

}
write.csv(whw, 'inst/extdata/local/mexico_city/mexico_city_injuries.csv')
