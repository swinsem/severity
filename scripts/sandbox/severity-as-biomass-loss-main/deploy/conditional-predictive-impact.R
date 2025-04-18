set.seed(20240724)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard = sf::st_read("data/ARD_07192024.gpkg") |>
  dplyr::filter(!is.na(pcnt_ba_mo))

ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10)

# # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
learner_sev_biomass <- mlr3::lrn(
  "regr.ranger",
  mtry = 21,
  num.trees = 1000,
  sample.fraction = (1 - 1/exp(1)),
  replace = FALSE,
  min.node.size = 10,
  num.threads = 11,
  keep.inbag = TRUE
)

analysis_data = ard_for_task$splits[[1]] |>
  rsample::analysis() |>
  sf::st_drop_geometry()

assessment_data = ard_for_task$splits[[1]] |>
  rsample::assessment() |>
  sf::st_drop_geometry()

task_sev_biomass =
  mlr3::as_task_regr(
    analysis_data[, c(target, features)],
    target = target,
    id = target
  )

out = cpi::cpi(
  task = task_sev_biomass,
  learner = learner_sev_biomass,
  measure = "regr.mse",
  test_data = assessment_data,
  test = "t"
)

out = 
  out  |>  
  dplyr::mutate(
    id = folds_repeat$id[idx], 
    iter = folds_repeat$iter[idx]
  )
# When we use the matthews correlation coefficient metric for our model skill
# It's just a single value from the whole observed/predicted set, rather than 
# a value for each observation, like with the other metrics. So the statistical 
# tests implemented in {cpi} are useless here. But because there isn't an 
# option to just not do a statistical test, we'll use the fisher test with 
# only 1 replication to speed things up.
out = cpi::cpi(
  task = task_sev_biomass,
  learner = learner_sev_biomass,
  measure = "regr.mse",
  test_data = assessment_data,
  test = "t"
)

### go straight for the spatial sampling
learner_sev_biomass <- mlr3::lrn(
  "regr.ranger", 
  mtry = 21,
  num.trees = 1000,
  sample.fraction = (1 - 1/exp(1)),
  replace = FALSE,
  min.node.size = 10,
  num.threads = 11,
  keep.inbag = TRUE
)

tictoc::tic()
out = cpi::cpi(
  task = task_sev_biomass,
  learner = learner_sev_biomass, 
  test_data = assessment_data,
  measure = "regr.mse",
  test = "fisher",
  B = 1
)
tictoc::toc()
out |> dplyr::filter(ci.lo > 0)

# ard_for_task = ard |>
#   dplyr::mutate(
#     x = sf::st_coordinates(ard)[, 1],
#     y = sf::st_coordinates(ard)[, 2]
#   ) |> 
#   dplyr::select(tidyselect::all_of(c(target, features, "x", "y"))) |>
#   sf::st_drop_geometry()
# 
# task_sev_biomass = 
#   mlr3spatiotempcv::as_task_regr_st(
#     x = ard_for_task,
#     target = target,
#     id = target,
#     coordinate_names = c("x", "y"),
#     crs = sf::st_crs(4326),
#     coords_as_features = FALSE
#   )

ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10) |> 
  purrr::pmap(.f = function(id, splits) {
    
    spatial_fold <- id
    assessment_data = 
      splits |>  
      rsample::assessment() |>  
      sf::st_drop_geometry()
    
    return(cbind(assessment_data, spatial_fold))
  }) |> 
  data.table::rbindlist() |> 
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |> 
  tibble::as_tibble()

rf_formula = as.formula(
  glue::glue(
    "{target} ~ {paste(features, collapse = ' + ')}"
  )
)

task_sev_biomass = 
  mlr3::as_task_regr(
    x = ard_for_task[, c(target, features, "spatial_fold")],
    target = target, 
    id = target
  )

task_sev_biomass$set_col_roles(cols = "spatial_fold", roles = "group")

resampler_sev_biomass = mlr3::rsmp("custom_cv")
resampler_sev_biomass$instantiate(
  task = task_sev_biomass, 
  col = "spatial_fold"
)

resampler_sev_biomass$initialize()
resampler_sev_biomass

task_sev_biomass$data(cols = "spatial_fold")[[1L]]

tictoc::tic()
out = cpi::cpi(
  task = task_sev_biomass,
  learner = learner_sev_biomass, 
  resampling = resampler_sev_biomass,
  measure = "regr.mse",
  test = "t",
)
tictoc::toc()
out |> dplyr::filter(ci.lo > 0)

any(task_sev_biomass$feature_types$type == "factor")
cpi::cpi

task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling:
custom_cv = rsmp("custom_cv")
f = factor(c(rep(letters[1:3], each = 3), NA))
custom_cv$instantiate(task, f = f)
custom_cv$iters # 3 folds

out = cpi::cpi(
  task = task,
  learner = learner_sev_biomass, 
  resampling = custom_cv,
  measure = "regr.mse",
  test = "t",
)














library(mlr3verse)
data(diplodia)
as.data.table(mlr_tasks)
mlr3::mlr_tasks
mlr3::mlr_tasks$get("diplodia")
task = tsk("diplodia")

# create a task
task = mlr3::tsk("diplodia")
task


mlr3::rsmp("ResamplingCustomCV")





















|>  
  purrr::pmap(
    .f = function(id, splits) {
      spatial_fold <- id
      assessment_data <- 
        splits |>  
        rsample::assessment() |>  
        sf::st_drop_geometry()
      
      return(cbind(assessment_data, spatial_fold))
    }
  ) |> 
  data.table::rbindlist()

features = ard |> 
  dplyr::select(-c(PlotID, YrFireName, Dataset, Unit, ID, FireYear, Start_Day, End_Day, pcnt_ba_mo, spatial_fold)) |> 
  colnames()

target = "pcnt_ba_mo"

mtry_vec = seq(
  from = floor(sqrt(length(features))) - 3,
  to = floor(length(features) / 1.75),
  by = 2
)

tune_df =
  expand.grid(
    mtry = mtry_vec, 
    num.trees = c(1000), 
    sample.fraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
    min.node.size = c(1, 5, 10, 25, 50, 60),
    iter = 1:10
  )

spatial_cv_tune <- function(i, target, features, folds, tune.df, num.threads) {
  
  rf_formula = as.formula(
    glue::glue(
      "{target} ~ {paste(features, collapse = ' + ')}"
    )
  )
  
  mtry <- tune_df$mtry[i]
  num.trees <- tune_df$num.trees[i]
  sample.fraction <- tune_df$sample.fraction[i]
  min.node.size <- tune_df$min.node.size[i]
  iter <- tune_df$iter[i]
  
  results <- vector(mode = "list", length = nrow(folds))
  
  for(k in 1:nrow(folds)) {
    analysis_data <- sf::st_drop_geometry(rsample::analysis(folds$splits[[k]]))
    assessment_data <- sf::st_drop_geometry(rsample::assessment(folds$splits[[k]]))
    
    # fit the model to the analysis set
    # to avoid biased random forests that favor variables with lots of unique values
    fitted_model = ranger::ranger(
      formula = rf_formula,
      data = analysis_data[, c(target, features)],
      num.trees = num.trees,
      mtry = mtry,
      classification = FALSE,
      sample.fraction = sample.fraction,
      replace = FALSE,
      min.node.size = min.node.size,
      num.threads = num.threads)
    
    results[[k]] <- 
      tibble::tibble(
        id = folds$id[k],
        o = assessment_data$ewe,
        p = predict(fitted_model, data = assessment_data, type = "response")$predictions[, "1"],
        assessment_ewe_n = nrow(assessment_data),
        assessment_ewe_1 = length(which(assessment_data$ewe == 1)),
        assessment_ewe_0 = assessment_ewe_n - assessment_ewe_1,
        mtry = mtry,
        num.trees = num.trees,
        sample.fraction = sample.fraction,
        min.node.size = min.node.size,
        class.wgts = class.wgts,
        iter = iter
      )
  }
  
  results_out <- data.table::rbindlist(results)
  
  return(results_out)
}


fold_ids = unique(ard$spatial_fold)
ard = as.data.frame(ard)

make_split = function(spatial_fold) {
  
  analysis_data = ard[ard$spatial_fold != spatial_fold, ]
  assessment_data = ard[ard$spatial_fold == spatial_fold, ]
  
  split = rsample::make_splits(x = analysis_data, 
                               assessment = assessment_data)
  
  return(split)
}

spatial_fold = "Fold01"

ard = data.table::as.data.table(ard)

rsample::make_splits(
  x = list(
    analysis = which(ard$spatial_fold != spatial_fold),
    assessment = which(ard$spatial_fold == spatial_fold)
  ),
  data = ard
)

splits = 
  purrr::map(fold_ids, .f = rsample::make_splits) 

?rsample::group_initial_split()

test = rsample::group_initial_split(data = ard, group = spatial_fold)

rsample::testing(test)
folds <- rsample::manual_rset(splits = splits, ids = fold_ids)
?rsample::manual_rset
