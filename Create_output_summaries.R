# Script to knit the various .Rmd files and re-name the corresponding output .html files
# to contain the output version of the corresponding ithim run

# if output_version is not already defined then you need to define it before calling the various .Rmd files



# output_version <- "c28f173d_test_run"

if (!exists("output_version")){
  ## Get the current repo sha
  gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path("repo_sha"))
  # Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
  if (.Platform$OS.type == "windows"){
    shell(paste(append("git", gitArgs), collapse = " "), wait = T)
  } else {
    system2("git", gitArgs, wait = T)
  }
  
  repo_sha <-  as.character(readLines(file.path("repo_sha")))
  output_version <- paste0(repo_sha, "_test_run")
} 


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

