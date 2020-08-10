library(dplyr)
library(magrittr)
library(tibble)

# Input vectors: 
# model scores (between 0 and 1)
# case counts 1
# case counts 0

dftest = tribble(
  ~scores, ~counts1, ~counts0,
  0.1, 5, 0,
  0.1, 0, 45,
  0.5, 100, 100,
  0.7, 20, 5,
  0.9, 40, 5
)

compute_confusion_matrix_counts = function(scores, counts1, counts0) {
  
  # aggregate rows with same scores. 
  # -> no need to combine rows separately
  df = data.frame(scores, counts1, counts0) %>% 
    group_by(scores) %>% 
    summarise(counts1 = sum(counts1),
              counts0 = sum(counts0)) %>% 
    arrange(desc(scores))
  
  N0 = sum(counts0)
  N1 = sum(counts1)
  N = N0 + N1
  
  # Compute true 0's and 1s IF we predict 1 FOR all data points >= score of
  # current row.
  rowwise_confusion = df %<>%
    mutate(cum_count1 = cumsum(counts1),
           cum_count0 = cumsum(counts0),
           cum_cases = cum_count1 + cum_count0,
           
           true_positives = cum_count1,
           true_negatives = N0 - cum_count0,
           false_positives = cum_count0,
           false_negatives = N1 - cum_count1, 
           
           precision = true_positives / (true_positives + false_positives), 
           recall = true_positives / (true_positives + false_negatives))

  
  return(rowwise_confusion)
  
}
compute_confusion_matrix_counts(dftest$scores, dftest$counts1, dftest$counts0)


compute_confusion_matrix_raw = function(scores, weights, class) {
  # This ASSMUES class to be a vector of 0's and 1s. 
  df = data.frame(scores = scores, weights = weights, class = class)
  formatted = bind_rows(
    df %>% filter(class == 0) %>% mutate(counts0 = weights, counts1 = 0),
    df %>% filter(class == 1) %>% mutate(counts1 = weights, counts0 = 0)
  )
  return(compute_confusion_matrix_counts(scores = formatted$scores,
                                         counts1 = formatted$counts1,
                                         counts0 = formatted$counts0))
}


dftest2 = tribble(
  ~scores, ~weights, ~class,
  0.1, 5, 1,
  0.1, 45, 0,
  0.5, 100, 1,
  0.5, 100, 0,
  0.7, 5, 0,
  0.7, 20, 1,
  0.9, 5, 0,
  0.9, 40, 1
)

compute_confusion_matrix_raw(dftest2$scores, dftest2$weights, dftest2$class)
