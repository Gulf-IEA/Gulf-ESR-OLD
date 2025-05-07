This repository houses data, code, documentation, and report files for the Gulf of America Ecosystem Status Report created by the NOAA Fisheries Southeast Fisheries Science Center. We are currently working on the 2025 ESR.

This report is being built as a quarto book using the NMFS Open Science NOAA quarto book [template](https://nmfs-opensci.github.io/NOAA-quarto-book/).

The methods documentation for how this report was created can be found [here](https://html-preview.github.io/?url=https://github.com/Gulf-IEA/Caribbean-ESR-2/blob/main/METHODS_DOC.html).

The list of indicators included in the 2025 ESR will be added to a technical documentation (forthcoming).

There are several folders in this repository.

1.  indicator_data --\> houses all the non-confidential data needed to create each indicator. Confidential data are stored elsewhere. Scripts with confidential data (mainly fisheries-dependent indicators) will not run without access to the data files.

2.  indicator_processing --\> houses all the code (R scripts) for creating each indicator. The 1-CalculateAllIndicators.R file runs all of the R scripts for all indicators. This should be run anytime the report needs to be updated. This folder also contains the 2-PlotAllIndicators.R file, which creates all of the time series plots for the ESR and saves them to the indicator_plots folder. This should be run after running 1-CalculateAllIndicators.R whenever the report needs to be updated. R scripts 3, 4, and 5 in this file are used for indicator synthesis. The script extra_ConvertRDatatoCSV is a function that will convert all the data in the indicator_objects folder to CSV files in case they are needed.

3.  indicator_objects --\> houses all of the .RData outputs from the R scripts used to create each indicator. These are objects all in a standardized format with each indicator variable and a time column as well as label information. There is a subfolder in this folder called objects_as_csvs. This is the output of the extra_ConvertRDatatoCSV function. 

4.  indicator_plots --\> houses all the output plots from the PlotAllIndicators.R script as well as any additional plots that are created separately to be used in the report.

5.  content --\> houses the quarto files needed to create the ESR report.

6.  additional folders and files are associated with building the quarto book.

------------------------------------------------------------------------

### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### License

This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the CC0 1.0 Universal public domain dedication.
