##Accra
##reading file with injury data
accra<- readRDS('data/local/accra/injuries_long_accra.RDS')
##saving the original file as backup
saveRDS(accra,'data/local/accra/injuries_long_accra_original.RDS')
accra$strike_mode[which(accra$strike_mode =="?")]<- "unk"
##saving the modified file
saveRDS(accra,'data/local/accra/injuries_long_accra.RDS')