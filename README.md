# bst270-final-project

The aim of the following project is to recreate one table and one figure from the FiveThirtyEight article:

["Inside The Political Donation History Of Wealthy Sports Owners"](https://fivethirtyeight.com/features/inside-the-political-donation-history-of-wealthy-sports-owners/)



# Project Structure

The current project contains two main folders. The first, `scripts`, contains the following files of interest: (1) `analysis.Rmd`, the R Markdown file used to generate all analyses, (2) `analysis.html`, the saved html output from the R Markdown file, and (3) `renv.lock`, a document generated by the `renv` package in R to store session information, including versions of packages used in the analysis and their dependencies. The remaining files in this folder, including the subfolder `renv` are created and used by the `renv` package to recreate the environment used in the analysis. The analysis can be fully recreated by simply loading and knitting the `analysis.Rmd` file.

The second folder, `data`, contains the link to download the dataset, though note that the dataset used in the analysis is automatically downloaded from a url as part of the main analysis script.

