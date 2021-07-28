### compile data and preliminary stuff ###
source('policypolitics/code/prep_work/generate_admin_unit_shapefile.R')
source('policypolitics/code/prep_work/compute_nf_cd_spatial_overlaps.R')
source('policypolitics/code/prep_work/generate_national_forest_data.R')


### run primary models and results ####
source('policypolitics/code/analysis/make_example_plots.R')
source('policypolitics/code/analysis/combine_and_model.R')
source('policypolitics/code/analysis/evaluate_results.R')
source('policypolitics/code/analysis/fit_linear_combinations.R')

### run specification and sensitivity checks ###
source('policypolitics/code/extra_checks/priors_sensitivity.R')
source('policypolitics/code/extra_checks/joint_gof_comparison.R')
source('policypolitics/code/extra_checks/combine_and_model_fe.R')
source('policypolitics/code/extra_checks/evaluate_results_FE.R')
source('policypolitics/code/extra_checks/fit_linear_combinations_FE.R')
source('policypolitics/code/extra_checks/compare_fe_and_re.R')
