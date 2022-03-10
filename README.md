# PCB Fluxes IHSC

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

----------------------
General Information
----------------------

Deposit Title: PCB fugacity ratios from IHSC

Contributor information:

Andres Martinez, PhD
University of Iowa - Department of Civil & Environmental Engineering
Iowa Superfund Research Program (ISRP)
andres-martinez@uiowa.edu
ORCID: 0000-0002-0572-1494

This README file was generated on March 07 2022 by Andres Martinez.

This work was supported by the National Institutes of Environmental Health Sciences (NIEHS) grant #P42ES013661.  The funding sponsor did not have any role in study design; in collection, analysis, and/or interpretation of data; in creation of the dataset; and/or in the decision to submit this data for publication or deposit it in a repository.


This README file describes a revised version of R script for calculating the fugacity ratios in this paper: Martinez, A., A. M. Awad, N. J. Herkert and K. C. Hornbuckle (2019). Determination of PCB fluxes from Indiana Harbor and Ship Canal using dual-deployed air and water passive samplers. Environmental Pollution Volume 244, January 2019, Pages 469-476 https://www.sciencedirect.com/science/article/pii/S0269749118331269

The data can be downloaded from PANGAEA, https://doi.pangaea.de/10.1594/PANGAEA.894908, using the R package pangaear (https://cran.microsoft.com/snapshot/2022-01-01/web/packages/pangaear/pangaear.pdf).

The scripts are developed for individual deployment times, so in the scripts the deployment time (1 to 9) needs to be changed for both air and water concentrations, as well as for the meteorological parameters.

--------
FILE OVERVIEW
--------

File Name: FugacityCalculations
File Size: 11 kb
File Format: .R
File description: Contains the raw code for the calculation of individual PCB fugacity ratios in "R". It also contains the codes to directly download the data from Pangaea.

--------
PREREQUISITES & DEPENDENCIES
--------

This section of the ReadMe file lists the necessary software and packages required to run the PCB reactive transport model in "R" and to view documentation provided in the "R Markdown" file, as well as in the various output files (e.g., .html, .pdf, and .docx).

For more information on "R Markdown" documentation please see: https://rmarkdown.rstudio.com/index.html

Software:
- Any web browser (e.g., Google Chrome, Microsoft Edge, Mozilla Firefox, etc.)
- R-studio for easily viewing, editing, and executing "R" code as a regular "R script" file and as an "R markdown" file: https://www.rstudio.com/products/rstudio/download/

--------
SOFTWARE INSTALLATION
--------

This section of the ReadMe file provides short instructions on how to download and install "R Studio".  "R Studio" is an open source (no product license required) integrated development environment (IDE) for "R" and completely free to use.  To install "R Studio" follow the instructions below:

1. Visit the following web address: https://www.rstudio.com/products/rstudio/download/
2. Click the "download" button beneath RStudio Desktop
3. Click the button beneath "Download RStudio Desktop".  This will download the correct installation file based on the operating system detected.
4. Run the installation file and follow on-screen instructions. 


