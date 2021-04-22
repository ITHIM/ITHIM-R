##code to convert victim counts of Buenos Aires injury data to long-form injury dataset with striking vehicles using Bogota imputed dataset
##reading victim counts of buenos aires
##two columns of data in this file represent data for 24 municipalities and BA autonomous city -respectively
vic<- read.csv('data/local/buenos_aires/buenos_aires_injuries_NOV.csv')
##selecting total
vic<-vic[,-c(2,3)]
##reading bogota's imputed injury data using which we will assign striking vehicles
bogota<- read.csv('inst/extdata/local/bogota/bogota_injuries.csv')

##checking to see if the victim mode names are the same as in bogota dataset
unique(vic$mode)
unique(bogota$cas_mode)
##buenos aires has one 'other' category (n=1) that is not there in bogota, assigning it as pedestrian
vic$total[which(vic$mode=="pedestrian")]<- vic$total[which(vic$mode=="pedestrian")]+vic$total[which(vic$mode=="other")]
vic<- vic[which(vic$mode != "other"),]

###creating long form data for BA injury counts
x<- "NA"
for (i in 1: nrow(vic))
{
  x<-c(x, rep(vic$mode[i],vic$total[i]))

}
x<-x[-1]

##creating a dataframe for vicitm and stirking type
whw<-as.data.frame(x)
whw$strike_type<- "NA"
names(whw)[1]<-"cas_type"

##following lines uses the rows in bogota injuyry data for a specific victim type and randomly select a strike vehicle type
##and create 5 different iterations of striking vehicle type to retain variability
for (i  in 1: nrow(whw))
{
    
  whw$strike_type[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type2[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type3[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type4[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  whw$strike_type5[i]<- as.character(sample(bogota[which(bogota$cas_mode==whw$cas_type[i]), "strike_mode"],1))
  
}

write.csv(whw, 'inst/extdata/local/buenos_aires/buenoes_aires_injuries.csv')
