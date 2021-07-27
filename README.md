# dhs_hiv_twostagesampling
## Performing statistical analysis for HIV data from DHS survey data

This repo is intended for a brief summary of all statistical analyses required for the manuscript *Associated health and social determinants of mobile populations across
HIV epidemic gradients in Southern Africa* (See references)

## Context

Growing travel connectivity and economic development have dramatically increased the magnitude of human mobility in Africa. In public health, vulnerable population groups such as mobile individuals are at an elevated risk of sexually transmitted diseases, including HIV. The population-based Demographic Health Survey data of five Southern African countries with different HIV epidemic intensities (Angola, Malawi, South Africa, Zambia, and Zimbabwe) were used to investigate the association between HIV serostatus and population mobility adjusting for socio-demographic, sexual behavior and spatial covariates. 

## Materials & Data
R Markdown
Dataset for all countries is available upon request at [DHS](https://dhsprogram.com/data/available-datasets.cfm). For illustration purposes, the complete pipeline (ETL, analysis and Bivariate Kriging) is done over the 2015's Zimbabwe dataset.

## Study area

The study area focuses on five geographically contiguous countries with differential national HIV prevalence in Southern Africa. In 2019, the prevalence of HIV was 2.0% in Angola, 9.2% in Malawi, 20.0% in South Africa, 11.3% in Zambia, and 12.7% in Zimbabwe. The DHS survey design is a two-stage sampling procedure through a set of defined locations (primary sample units or PSU) statistically weighted to control for sample biases (ICF International 2012).     

## Statistical analysis

In summary, I performed a Variance Inflated Factor (VIF) to account for multicollinearity. All variables less than five (VIF<5.0) were included in the final adjusted model. Logistic regression models were fitted to assess the association between mobility and the selected covariates. DHS two-stage cluster sampling procedure was considered to correctly estimate sampling errors through all fitted models. Since there is an expectation that the two variables might be related in space (percentage of mobile individuals and HIV prevalence), continuous surface maps of mobile population and HIV prevalence were generated. Each map was normalized and classified with an equal interval scheme. Finally, a bivariate palette was designed to visually depict all possible combinations of the proportion of mobile individuals and HIV prevalence in all countries.

## References

- Esteban Correa-Agudelo, Hae-Young Kim, Godfrey N. Musuka, Zindoga Mukandavire, Adam Akullian, Diego F. Cuadros. Associated health and social determinants of mobile populations across HIV epidemic gradients in Southern Africa. Journal of Migration and Health, Volume 3, 2021, 100038, ISSN 2666-6235, https://doi.org/10.1016/j.jmh.2021.100038.
- Thomas Lumley. Complex Surveys: A Guide to Analysis Using R. ISBN: 978-0-470-28430-8. 
