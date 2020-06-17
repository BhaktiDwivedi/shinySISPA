shinySISPA: Sample Integrated Set Profile Analysis with Shiny
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Authors
~~~~~~~
Bhakti Dwivedi & Jeanne Kowalski


Maintainer
~~~~~~~~~~
Bhakti Dwivedi


Organization
~~~~~~~~~~~~
Biostatistics & Bioinformatics Shared Resource
Winship Cancer Institute
Emory University
Atlanta, GA 30322


Purpose
~~~~~~~
shinySISPA is a web-based tool intended for the researchers who are interested in defining samples with similar, a priori specified molecular profile. shinySISPA is based on the SISPA method published in our previous paper (Kowalski et al., 2016; PMID: 26826710). The tool is developed using shiny, a web-application framework for R. 


Availability
~~~~~~~~~~~~
SISPA Bioconductor R package is available at https://www.bioconductor.org/packages/release/bioc/html/SISPA.html and shinySISPA application is available from the GitHub, https://github.com/BhaktiDwivedi/shinySISPA.


Current Version
~~~~~~~~~~~~~~~
shinySISPA version 1.0


Copyright
~~~~~~~~~
Copyright (c) 2017-1018, Bhakti Dwivedi


Bug Reports/Questions
~~~~~~~~~~~~~~~~~~~~~
Please contact Bhakti Dwivedi, bhakti.dwivedi@emory.edu for questions, comments, or feature requests.


Running shinySISPA on a local computer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1)	Download and install R or RStudio (version 3.3.2. or later) from https://cran.r-project.organd 
2)	Open R and install the below required packages: 
	> install.packages(c("shiny", "GSVA", "genefilter", "changepoint", "data.table", “ggplot2”, "plyr")). 
3)	If any of these packages are not available for your R version, please download them via Bioconductor
	if (!requireNamespace("BiocManager", quietly = TRUE))
    		install.packages("BiocManager")
		BiocManager::install("package_name")
4)	Users can run shinySISPA locally using the source code available from the GitHub: https://github.com/BhaktiDwivedi/shinySISPA, by typing the below commands in R console:
    > library(shiny)
    > runApp("shinySISPA")
5)	Users can also download and run the app from GitHub directly using:
    > shiny::runGitHub('shinySISPA', 'BhaktiDwivedi')
  
Please see shinySISPA_manual.pdf for more details.  


Compatibility
~~~~~~~~~~~~~
Compatible with Firefox or Chrome browsers.


Funding
~~~~~~~
This work is funded by the Georgia Research Alliance Scientist Award (Jeanne Kowalski); Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI [Award number P30CA138292, in part]. The content is solely the responsibility of the authors and does not necessarily represent the official views of the NIH.


Citing the SISPA method:
~~~~~~~~~~~~~~~~~~~~~~~~
Kowalski J, Dwivedi B, Newman S, Switchenko JM, Pauly R, Gutman DA, Arora J, Gandhi K, Ainslie K, Doho G, Qin Z, Moreno CS, Rossi MR, Vertino PM, Lonial S, Bernal-Mizrachi L, Boise LH. Gene integrated set profile analysis: a context-based approach for inferring biological endpoints. Nucleic Acids Res. 2016 Apr 20;44(7):e69. doi: 10.1093/nar/gkv1503. Epub 2016 Jan 29. PubMed PMID: 26826710; PubMed Central PMCID: PMC4838358.
