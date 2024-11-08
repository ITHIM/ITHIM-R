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

#output_version <- '5054de5a_orig_nomidpt'

# create summary tables for AP and PA
rmarkdown::render('summary_tables_PA_AP.Rmd', params = list(output_version = output_version))

# create summary tables
rmarkdown::render('summary_tables.Rmd', params = list(output_version = output_version))

# create injury summary tables
rmarkdown::render('injury_tables.Rmd', params = list(output_version = output_version))



