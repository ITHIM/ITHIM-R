---
output:
  html_document: default
  pdf_document: default
---
# ITHIM Global - Accra Model

This repository contains all the files required to run the analysis reported in the paper "Health impacts of changes in travel patterns in Greater Accra Metropolitan Area, Ghana".

This study uses a new version of the Integrated Transport and Health Impact Model (ITHIM), named ITHIM-Global (https://github.com/ITHIM/ithim-r).

This work was conducted under the umbrella of the Urban Health Initiative pilot project in Accra, Ghana, a partnership between the World Health Organization, Accra Metropolitan Assembly, Ghana Health Service, Ghana Environmental Protection Agency, World Health Organization’s international implementing partners, UN-Habitat and the Local Governments for Sustainability.

## How the code works

First, download (https://github.com/ITHIM/ITHIM-R/archive/accra_model.zip) and extract the repository in our local computer. The only file you need to open and execute to run the model and obtain the results is `master_script.R`. The code builds the ITHIM-R package, install and call all the packages required to run the model, run the model, and generates tables and illustrations summarinsing the results.

There are three main blocks to the model: setting up, running the model, and assessing uncertainty. The first block, `run_ithim_setup`, sets the variables for computing the ITHIM. This is done in the second block, `run_ithim`, which computes the ITHIM output by calling each module in turn and summarising their outputs in terms of the health burden. Finally, `ithim_uncertainty` is a wrapper for sampling from the ITHIM output in a loop.

After the results are calculated, the remaining of the code summarises the results and generates the illustrations and tables for the paper.