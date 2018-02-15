

# each row is a trial, comprised of a subset of word (object) indices 
# sampled from the entire vocabulary
hiCDord = read.table('freq369-3x3hiCD.txt', sep='\t', header=F)
loCDord = read.table('freq369-3x3loCD.txt', sep='\t', header=F)

# define a function that accepts an array of trials (e.g., hiCDord)
# and returns a word x object co-occurrence matrix
coocMatrix <- function(ord) {
  N = max(unlist(ord))
  M = matrix(0, nrow=N, ncol=N)
  for(t in 1:nrow(ord)) {
    tr = unlist(ord[t,])
    M[tr,tr] = M[tr,tr] + 1
  }
  return(M)
}

# define a test function that accepts a 'memory' matrix
# --containing word-object hypotheses or associations--
# and a decision parameter, and returns choice probabilities
# of each object, given each word, according to softmax:
# https://en.wikipedia.org/wiki/Softmax_function 
# (decision parameter = RL 'temperature')
softmax <- function(M, temp) {
  expM = exp(M/temp)
  return( diag(expM) / rowSums(expM) )
}


# define a function that accepts an array of trials and parameter
# values, and returns a memory matrix with the learned representation
model <- function(ord, parms) {
  # nw = number of words
  # no = number of objects
  M = matrix(0, nrow=nw, ncol=nw)
  # learning: i.e., not just co-occurrence counting,
  # but a process that corresponds to what you think 
  # people might be doing as they go through the trials
  # (guess-and-test hypothesis generation? biased association?)
  return(M) # this matrix will then be passed through softmax to extract pr(correct)
}

# define a function that accepts a vocabulary (1:M), a 
# distribution over the likelihood of sampling each word (object)
# and a desired number and size of trials, and returns a trial order
# (e.g., to accomplish simulations like those in Blythe et al., (2016))


# graph the mean performance for different softmax parameter values (e.g., .1 to 10)
require(ggplot2)
plot_performance_by_temperature <- function(ord) {
  temp = seq(.1,10,.5)
  item_perf = matrix(0, nrow=length(temp), ncol=18)
  for(i in 1:length(temp)) {
    item_perf[i,] = softmax(coocMatrix(ord), temp[i])
  }
  dat = data.frame(cbind(temp, perf = rowMeans(item_perf)))
  ggplot(dat, aes(temp, perf)) + geom_point(color="red", alpha=.5) + geom_line(alpha=.5)
}

### Evaluating model fit ###
# try implementing each of the following three methods (SSE, crossEntropy, 
# and negative log likelihood) and get a sense of their values for varying discrepancies of p and q
# human response probabilities for each correct of the 18 correct pairs:
load("human_accuracy_variedCD.RData") # hiCDacc and loCDacc

# implement sum of squared error measure of model fit (p) to observed data (q)
SSE <- function(p,q) {
  return(sse)
}

# implement cross entropy measure
crossEntropy <- function(observed_probs,model_probs) {
  return(xent)
}

# implement negative log-likelihood measure, assuming each test 
# problem is binomial (since I didn't give you the full response matrix)
negloglik <- function(obs, mod) {
  return(nll)
}

# implement a function (BIC) calculating the Bayesian Information Criterion
# https://en.wikipedia.org/wiki/Bayesian_information_criterion



### Fitting a model ### 

# given a trial-ordering, model parameters, and a set of human Pr(correct),  
# return your favorite goodness-of-fit value.
evaluateModel <- function(parms, order, hum) {
  return(fitval)
}

require(DEoptim)
?DEoptim