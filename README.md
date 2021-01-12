---
output:
  html_document: default
  pdf_document: default
---
# ITHIM-Global - Accra model

This repository contains all the files required to run the analysis reported in the paper "Health impacts of changes in travel patterns in Greater Accra Metropolitan Area, Ghana".

This study uses a new version of the Integrated Transport and Health Impact Model (ITHIM), known as ITHIM-Global (https://github.com/ITHIM/ithim-r), as of July 2020.

## How to run the model

1) Download and install R (https://www.r-project.org/).

2) Download and install RTools (https://cran.r-project.org/bin/windows/Rtools/history.html). If you are using R version 4.0 and up, follow the instructions in https://cran.r-project.org/bin/windows/Rtools/.

3) Download (https://github.com/ITHIM/ITHIM-R/archive/accra_model.zip) and extract the repository in your local computer.

4) Open and execute the `master_script.R` file. This code installs and calls all the packages required to run the model, executes the model, and generates tables and illustrations summarising the results.
OBS: If you just want to test the code, change the NSAMPLES argument (line 33) in the `run_ithim_setup` function (starting in line 30) from 1024 to 5.

## How the code works

The model has three main blocks. The first block, `run_ithim_setup`, sets the model's parameters. The second block, `run_ithim`, computes the model's outputs by calling each module in turn and summarising their outputs in terms of the health burden. The third block, `ithim_uncertainty`, is a wrapper for sampling from the model's output in a loop.

After the outputs are calculated, the remaining code summarises the results and generates the illustrations and tables shown in the paper.

## Disclaimers and acknowledgements

This work was conducted under the umbrella of the Urban Health Initiative pilot project, a partnership between the World Health Organization, Accra Metropolitan Assembly, Ghana Health Service, Ghana Environmental Protection Agency, World Health Organization’s international implementing partners, UN-Habitat and the Local Governments for Sustainability, all members of the Climate and Clean Air Coalition.

We would like to acknowledge the contribution and inputs of all stakeholders involved in the activities of the Urban Health Initiative pilot project in Accra, Ghana.

The work was undertaken at the Centre for Diet and Activity Research (CEDAR), a UKCRC Public Health Research Centre of Excellence. Funding from the British Heart Foundation, Cancer Research UK, Economic and Social Research Council, Medical Research Council (MRC), the National Institute for Health Research, and the Wellcome Trust, under the auspices of the UK Clinical Research Collaboration, is gratefully acknowledged (MR/K023187/1 and MR/K023187/1). Additionally, this work has been supported by the Climate and Clean Air Coalition through the grant provided for the Urban Health and SLCP Reduction project in Accra and by the Government of Norway through its financial contribution to advance WHO’s work on air pollution and urban health.

TIGTHAT project was funded by MRC Global Challenges (MR/P024408/1).

The authors alone are responsible for the views expressed in this article and they do not necessarily represent the views, decisions, or policies of the institutions with which they are affiliated.