
# Define function to calculate LOCO
loco = function(task, learner, resampling, loss) {
  features = task$feature_names
  
  res = numeric(length(features))
  names(res) = features
  
  original_model = resample(task = task, learner = learner, resampling = resampling)
  original_loss = original_model$aggregate(loss)
  
  for (i in 1:length(features)) {
    features2 = features[-i]
    task2 = task$clone()$select(features2)
    new_model = resample(task = task2, learner = learner, resampling = resampling)
    new_loss = new_model$aggregate(loss)
    res[i] = new_loss - original_loss
  }
  return(res)
}

# Define function to calculate LOCI
loci = function(task, learner, resampling, loss) {
  features = task$feature_names
  
  res = numeric(length(features))
  names(res) = features
  
  featureless_learner = lrn("classif.featureless", predict_type = "prob")
  featureless_model = resample(task, featureless_learner, resampling)
  featureless_loss = featureless_model$aggregate(loss)
  
  for (i in 1:length(features)) {
    features2 = features[i]
    task2 = task$clone()$select(features2)
    new_model = resample(task2, learner, resampling)
    new_loss = new_model$aggregate(loss)
    res[i] = featureless_loss - new_loss
  }
  return(res)
}