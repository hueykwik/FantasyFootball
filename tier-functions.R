read_score_data = function(path, week) {
  sdf = read_csv(path)
  sdf$Week = week
  sdf$actualPoints[sdf$actualPoints == "null"] = 0
  sdf$actualPoints = as.numeric(sdf$actualPoints)
  return(sdf)
}

read_ecr_data = function(path) {
  ecr_df = read_tsv(path) 
  names(ecr_df) = make.names(names(ecr_df))
  ecr_df$Team[ecr_df$Team == "LA"] = "STL"  # FantasyPros changed STL to LA
  return(ecr_df)
}

randomPartition = function(k, len) {
  indexes = sample(seq(len-1), k-1)
  return(sort(indexes))
}

expand = function(tiers, indexes, len) {
  vec = vector("list", len)
  k = length(indexes)
  start = 1
  for (i in seq(k)) {
    idx = indexes[i]
    vec[start:idx] = tiers[i]
    start = idx + 1 
  }
  vec[start:len] = tiers[i+1]
  return(unlist(vec))
}

bestPartition = function(k, points, tries) {
  # Usage example: (also suggest setting seed for reproducibility)
  # test_df = ecr_scores %>% select(playername, points, actualPoints) %>% arrange(desc(points))
  # bestPartition(7, test_df$actualPoints, test_df$points)
  
  max_acc = -1
  best_indexes = c()
  len = length(points)
  for (i in seq(tries)) {
    indexes = randomPartition(k, len)
    tier = expand(1:k, indexes, len)
    acc = computeAccuracy(points, tier)
    if (acc > max_acc) {
      max_acc = acc
      best_indexes = indexes
    }
  }
  return (best_indexes)
}