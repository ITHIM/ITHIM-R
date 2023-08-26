# Script to knit the various .Rmd files and re-name the corresponding output .html files
# to contain the output version of the corresponding ithim run

# if output_version is not already defined then you need to define it before calling the various .Rmd files



# output_version <- 

# create summary tables for AP and PA
rmarkdown::render('summary_tables_PA_AP.Rmd', params = list(output_version = output_version))

# rename output .html files
file.rename('summary_tables_PA_AP.html', paste0('summary_tables_PA_AP_',output_version,'.html'))



# create summary tables
rmarkdown::render('summary_tables.Rmd', params = list(output_version = output_version))

# rename output .html files
file.rename('summary_tables.html', paste0('summary_tables_',output_version,'.html'))



# create injury summary tables
rmarkdown::render('injury_tables.Rmd', params = list(output_version = output_version))

# rename output .html files
file.rename('injury_tables.html', paste0('injury_tables_',output_version,'.html'))
