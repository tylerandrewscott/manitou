# The forest ranger (and the legislator)

This repository subdirectory is for a paper written as part of NSF project #1829239. The paper is published in the Journal of Public Administration Research & Theory, [DOI and link to be filled in].The project broadly concerns what and how environmental science is used in environmental impact assessment by the U.S. Forest Service. In this specific paper, we consider a slightly different question: How agency decision-making related to environmental impact assessment is associated with local communities’ political representation.

This subdirectory contains all of the data and code materials necessary to replicate the project. For questions, please raise an issue and tag us here on Github. The project materials are

## Paper Abstract

Research on political control over government bureaucracy has primarily focused on direct exercises of power such as appointments, funding, agency design, and procedural rules. In this analysis we extend this literature to consider politicians who leverage their institutional standing to influence the decisions of local field officials over whom they have no explicit authority. Using the case of the U.S. Forest Service (USFS), we investigate whether field-level decisions are associated with the political preferences of individual congressional representatives. Our sample encompasses 7,681 resource extraction actions initiated and analyzed by 107 USFS field offices between 2005 and 2018. Using hierarchical Bayesian regression, we show that under periods of economic growth and stability, field offices situated in the districts of congressional representatives who oppose environmental regulation initiate more extractive actions (timber harvest, oil and gas drilling, grazing) and conduct less rigorous environmental reviews than field offices in the districts of representatives who favor environmental regulation. By extending existing theories about interactions between politicians and bureaucrats to consider informal means of influence, this work speaks to: (1) the role of local political interests in shaping agency-wide policy outcomes; and (2) the importance of considering informal and implicit means of influence that operate in concert with explicit control mechanisms to shape bureaucratic behavior.


## Getting Started

### Dependencies

* The project requires the R statistical programming language, version 4.0.2 (2020-06-22). The project has not been tested on Windows, but it was developed jointly in OSX (11.4) and Linux. Some of the R packages required might require loading underlying dependencies (e.g., if you use the rgdal package, the GDAL and PROJ libraries must be installed).

### Installing

* The policypolitics subdirectory is designed to run as a self-contained folder. Data inputs are either stored in the subdirectory or read directly from online locations. Thus, the project is designed to replicate from any folder without modification.

### Subdirectory items

The subdirectory has 5 folders:
- code: scripts for replicating the project
- model objects: .rds files that store model run objects that can be read directly into R. These objects are lists of INLA files.
- raw_curated_inputs: these files are the raw ingredients for the project, with the exception of files that are read directly in from online locations (e.g., the USFS’ GIS library). In some case, the files have been curated/cleaned already (e.g, naturalresource_gdp_by_county_2001-2018.csv) or  or had developed by the authors (countyVoteShare_3-2020_imputed.rds). If you make use of the data inputs, please be sure to cite the relevant source (either this project or those who developed the data that we draw from and cite).


### Replicating the project

THE /POLICYPOLITICS/CODE/MASTER_SCRIPT.R FILE SOURCES ALL THE CODE OUTLINED BELOW

1. Run the scripts in /code/prep_work/
	1.1 generate_admin_unit_shapefile.R curates a master R sf object of national forests used in the sample by taking the USFS admin units shape files, filtering cases like the Savannah River Site, fixing any bad polygons, and saving an .rds file.
	1.2 compute_nf_cd_spatial_overlaps.R measures overlaps between national forests and congressional districts. These proportions are used for aggregating/weighting covariates.
	1.3 generate_national_forest_data.R aggregates all the predictor data inputs to generate a flat file with one row for each national forest - year combination.


2. Run the scripts in /code/analysis/
	2.1 combine_and_model.R merges NF covariates with PALS project data, fits a series of multiple likelihood model specifications in R-inla.
	—— scripts below rely on models fit and saved using combine_and_model.R —
	2.2 evaluate_results.R produces basic coefficient tables and plots
	2.3 fit_linear_combinations.R computes linear combinations of interacting variables to test core hypotheses
	2.4 make_example_plots.R produces figures 1 and 2 in the paper

3. Run the scripts in /code/extra_checks/
	3.1 priors_sensitivity.R fits the model in 2.1 using different Bayesian priors
	3.2 joint_gof_comparison.R compares models with separate, joint, joint + shared, and shared-only random intercept terms across the two likelihood components of the multiple likelihood model.
	3.3 combine_and_model_fe.R fits fixed intercepts instead of random intercepts for state/USFS region/National Forest/year
	3.4 evaluate_results_FE.R produces basic tables and figures showing results of 3.3
	3.5 fit_linear_combinations.R fits interactions for fixed effect models
	3.6 compare_fe_and_re.R makes a table and a figure that compares focal result estimates for models with random and fixed intercepts


## Help

Please feel free to ping us if any links appear broken. Note that in some case, e.g., reading in wildfire burn scar GIS data from the USFS online repository, the call periodically throws an error for reasons that appear due to the idiosyncrasies of their online portal—this is often solved simply by waiting a minute and trying again.


## Authors

Contributors names and contact info

Cory Struthers, cory.struthers@uga.edu, @corystruthers
Tyler A. Scott, tascott@ucdavis.edu, @tylerscottphd
Forrest Fleischman, ffleisch@umn.edu, @ForrestFleisch1
Gwen Arnold, gbarnold@ucdavis.edu, @policyandpuppies


## License

This project is licensed under the MIT License - see the LICENSE file for details

## Acknowledgments

Inspiration, code snippets, etc.
* [R-INLA](https://www.r-inla.org/)
* [Gomez-Rubio](https://becarioprecario.bitbucket.io/inla-gitbook/)
* [USFS data](https://conservancy.umn.edu/handle/11299/211669)
