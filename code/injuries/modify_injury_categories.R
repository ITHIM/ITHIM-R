##Sao Paulo
##reading the original file
injuries_long_sao_paulo <- readRDS("data/local/sao_paulo/injuries_long_sao_paulo.Rds")
##saving the original file as backup
saveRDS(injuries_long_sao_paulo, file = "data/local/sao_paulo/injuries_long_sao_paulo_original.rds")
##renaming ? as unk
injuries_long_sao_paulo$strike_mode[which(injuries_long_sao_paulo$strike_mode =="?")]<- "unk"
saveRDS(injuries_long_sao_paulo, file = "data/local/sao_paulo/injuries_long_sao_paulo.rds")


##Accra
##reading file with injury data
accra<- readRDS('data/local/accra/injuries_long_accra.RDS')
##saving the original file as backup
saveRDS(accra,'data/local/accra/injuries_long_accra_original.RDS')
accra$strike_mode[which(accra$strike_mode =="?")]<- "unk"
##saving the modified file
saveRDS(accra,'data/local/accra/injuries_long_accra.RDS')



##Delhi

whw_file <- 'delhi_road_deaths_2010_2013_average.csv'
path <- file.path(find.package('ithimr',lib.loc=.libPaths()),'extdata/local/delhi//')
whw <- read.csv(paste0(path,whw_file))
write.csv(whw,paste0(path,'delhi_road_deaths_2010_2013_average_original.csv'))
whw$Unknown<- whw$Unknown+whw$unspecified
whw$unspecified<-0
write.csv(whw,paste0(path,'delhi_road_deaths_2010_2013_average.csv'))

##Bangalore
##Not needed
whw_file <- 'bangalore_who_hit_who_matrix2011.csv'
path <- file.path(find.package('ithimr',lib.loc=.libPaths()),'extdata/local/bangalore//')
whw <- read.csv(paste0(path,whw_file))
write.csv(whw,'results/injury_matrices/bangalore_whw.csv')
