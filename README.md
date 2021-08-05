# Performing statistical analysis for complex survey data

## Motivation

This repo is intended to illustrate the analysis of complex survey data in R. My motivations comes to the several challenges i faced in the past with this kind of data:

- Lack of documentation
- Information fragmented in forums (dhsforum, stackstats)
- Hard to find good datasets

Two stage sampling in R is a little bit tricky at the beginning, so this guide tries to speed up the learning curve I faced in the past. You will find a complete pipeline (ETL, statistical analysis, disease mapping) + some advance topics, everything in one place :).

## Complex surveys

The DHS data is based on a two-stage sampling procedure through a set of defined locations (primary sample units or PSU per region/province). Those locations are statistically weighted to control for sample biases (specific strate, urban|rural for our case). Complex surveys are not only limited to DHS, several other health institutions around the world also use this procedure due to its cost optimization (i.e. Behavioral Risk Factor Surveillance System [BRFSS] from CDC). So all the code here can be easily adapted for other surveys ;). 

## Materials & Data
- R and R Markdown (\*.Rmd)
- Dataset for all countries is available upon request at [DHS](https://dhsprogram.com/data/available-datasets.cfm). For illustration purposes, the complete pipeline (ETL, analysis and Bivariate Kriging) is done over the 2015's Zimbabwe dataset.

## Context

Growing travel connectivity and economic development have dramatically increased the magnitude of human mobility in Africa. In public health, vulnerable population groups such as mobile individuals are at an elevated risk of sexually transmitted diseases, including HIV. The population-based Demographic Health Survey data of five Southern African countries with different HIV epidemic intensities (Angola, Malawi, South Africa, Zambia, and Zimbabwe) were used to investigate the association between HIV serostatus and population mobility adjusting for socio-demographic, sexual behavior and spatial covariates. 

## Statistical analysis

In summary, I performed a Variance Inflated Factor (VIF) to account for multicollinearity. All variables less than five (VIF<5.0) were included in the final adjusted model. Logistic regression models were fitted to assess the association between mobility and the selected covariates. DHS two-stage cluster sampling procedure was considered to correctly estimate sampling errors through all fitted models. Since there is an expectation that the two variables might be related in space (percentage of mobile individuals and HIV prevalence), continuous surface maps of mobile population and HIV prevalence were generated. Each map was normalized and classified with an equal interval scheme. Finally, a bivariate palette was designed to visually depict all possible combinations of the proportion of mobile individuals and HIV prevalence in all countries.

![](img/3.2.png| width=50%)

## File index

1. [Load and ETL](https://github.com/maurosc3ner/twostagesampling_playground/blob/main/unit1/01-load_datasets.md)
2. [Statistical analysis](https://github.com/maurosc3ner/twostagesampling_playground/blob/main/unit2/02-analysis.md)
3. [Kriging & bivariate maps in R](https://github.com/maurosc3ner/twostagesampling_playground/blob/main/unit3/03-kriging-bivariate.md)

Advance topics: 

4. [Creating new PSU-Strata for combining multiple surveys]()
5. [Post-stratification]()

## References

- Esteban Correa-Agudelo, Hae-Young Kim, Godfrey N. Musuka, Zindoga Mukandavire, Adam Akullian, Diego F. Cuadros. Associated health and social determinants of mobile populations across HIV epidemic gradients in Southern Africa. Journal of Migration and Health, Volume 3, 2021, 100038, ISSN 2666-6235, https://doi.org/10.1016/j.jmh.2021.100038.
- Thomas Lumley. Complex Surveys: A Guide to Analysis Using R. ISBN: 978-0-470-28430-8. 
