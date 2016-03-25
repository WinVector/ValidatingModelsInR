


#
# Functions for creating the data
#


mkCoefs = function(ngood) {
  if(ngood < 1) return(c())
  goodnames = paste('g', seq_len(ngood), sep='_')
  coefs = rnorm(seq_len(ngood))
  names(coefs) = goodnames
  coefs
}

# build a data frame with pure noise columns
# and columns weakly correlated with y
mkData <- function(nrows, coefs, nnoise) {
  noiseMagnitude = 1

  mkcol = function(x){rnorm(nrows)}

  weighted_column_sum = function(df, coefs) {
    cm = matrix(data=coefs, ncol=1)
    as.numeric(as.matrix(df[,names(coefs)]) %*% cm)
  }

  d = data.frame(y = noiseMagnitude*rnorm(nrows))
  ngood = length(coefs)
  goodvals = NULL
  noisevals = NULL

  if(ngood > 0) {
    goodvals = data.frame(lapply(coefs,mkcol))
    colnames(goodvals) = names(coefs)
    d$y = d$y + weighted_column_sum(goodvals, coefs)
    d = cbind(d, goodvals)
  }
  if(nnoise > 0) {
    noisenames = paste('n', seq_len(nnoise), sep='_')
    noisevals = data.frame(lapply(noisenames, mkcol))
    colnames(noisevals) = noisenames
    d = cbind(d, noisevals)
  }

  d$y = ifelse(d$y > 0, 'pos', 'neg')
  d
}

# ================================================================

#
# Functions for calculating the deviance and significance of a model:
#
get_deviance = function(y, pred) {
  -2*sum(y*log(pred) + (1-y)*log(1-pred))
}

#
# The theoretical significance of a glm model (with respect to the deviance metric)
# is given between the model's deviance on training and the data's "null deviance"
# (the deviance of the training data's grand mean). The difference between the
# two is distributed as a chi-square distribution with P - 1 degrees of freedom,
# where P is the number of parameters of the model (P counts the DC term, so P is
# the number of variables).
#
glm_significance = function(glm_mod) {
  delta_deviance = glm_mod$null.deviance - glm_mod$deviance
  dof = length(glm_mod$coefficients)-1
  sig = pchisq(delta_deviance, dof, lower.tail=FALSE)
}

# ================================================================

#
# Display auc and return deviance for a set of predictions
#
performance_eval = function(predScore, truth, posclass,
                            title='',
                            verbose=TRUE) {
  negclass = setdiff(unique(truth), posclass) # should be only 1
  data = data.frame(y=truth, predScore=predScore)

  deviance = get_deviance(data$y==posclass, data$predScore)

  if(verbose) {
    if(length(unique(data$pred))>1) {
      print(ROCPlot(data,'predScore','y',title=paste(title,'ROC plot')))
    }
  }
  output = data.frame(deviance=deviance, label=title)
  output
}


#
# Example code for running a permutation test.
#

# return a vector of deviances on the permuted data
permutation_test = function(dataf, nperm) {
  nrows = dim(dataf)[1]
  y = dataf$y
  X = dataf[, setdiff(colnames(dataf), "y")]
  varnames = colnames(X)
  fmla = paste("y=='pos' ~", paste(varnames, collapse=" + "))

  doperm = function(i) {
    # random order of rows
    ord = sample.int(nrows, size=nrows, replace=FALSE)
    model = glm(fmla, data=cbind(y=y[ord], X),
                family=binomial(link="logit"))
    predscore=predict(model, newdata=X, type='response')
    get_deviance(y[ord]=="pos", predscore)
  }
  vapply(seq_len(nperm), doperm, numeric(1))


}


# a crude empirical estimate of the fraction of
# time that a nulldist score is less than
# our score of interest
left_tail = function(score, nullscores) {
  num = sum(nullscores <= score)
  denom = length(nullscores)
  num/denom
}

# ================================================================

#
# Show an example of a permutation test on a full model
#
run_example = function(ngood, nnoise, datasize, nperm, title='') {
  coefs = mkCoefs(ngood)
  dTrain = mkData(datasize, coefs, nnoise)
  varnames = setdiff(colnames(dTrain), "y")
  fmla = paste("y=='pos' ~", paste(varnames, collapse=" + "))

  trainmean = mean(dTrain$y=='pos')
  nulldeviance = get_deviance(dTrain$y=='pos', trainmean)

  if(ngood > 0) {
    print("True coefficients of signal variables")
    print(coefs)
  }
  print(paste(title, ": training prevalence = ", trainmean,
              "null deviance of mean =", nulldeviance))
  #
  # train a model and look at training performance
  #
  model = glm(fmla, data=dTrain, family=binomial(link="logit"))

  trainpred = predict(model, newdata=dTrain, type="response")
  trainperf = performance_eval(trainpred,
                               dTrain$y, "pos", title=paste(title, "Training"))

  #
  # compare to a hold out set
  #
  dTest = mkData(datasize, coefs, nnoise)
  testpred = predict(model, newdata=dTest, type="response")
  testperf = performance_eval(testpred,
                              dTest$y, "pos", title=paste(title, "Test"), verbose=FALSE)


  print(paste("Compare training and test performance estimates,", title))
  perfests = do.call(rbind, list(trainperf, testperf))
  print(perfests)

  #
  # do a permutation test on the training data
  #
  nullperf = permutation_test(dTrain, nperm)

  print("Training performance compared to permutation test results")
  print(summary(nullperf))

  print(paste("Left tail area, deviance", left_tail(trainperf$deviance, nullperf)))

  nullframe = data.frame(deviance=nullperf)
  ggplot(nullframe, aes(x=deviance)) + geom_density() +
    geom_vline(xintercept=trainperf$deviance, color="red") +
    ggtitle(paste(title, ": deviance"))

}

# ================================================================

#
# Variable selection
#


# returns a vector of the permutation tests scores
# for deviance
permtest_col = function(col, dataf, nperm) {
  nrows = dim(dataf)[1]
  y = dataf$y
  X = data.frame(x = dataf[[col]])
  fmla = "(y=='pos') ~ x"

  # permute the y-value, fit a model, and get the deviance
  doperm = function(i) {
    # random order of rows
    ord = sample.int(nrows, size=nrows, replace=FALSE)
    mod = glm(fmla,  data=cbind(y=y[ord], X),
              family=binomial(link="logit"))
    # this works because we are predicting on the training data
    predscore = predict(mod,type="response")
    get_deviance(y[ord]=='pos', predscore)
  }

  # run nperm iterations of doperm() and collect the results
  vapply(seq_len(nperm), doperm, numeric(1))
}


#
# Get the scores for the columns of a variable, both
# by permutation test and by chi-squared estimate
#
score_columns = function(dataf, nperm) {
  ysymbol = "(y=='pos')"
  varnames = setdiff(colnames(dataf), "y")

  ylogical = dataf$y=='pos'
  ymean = mean(ylogical)
  null_deviance = get_deviance(ylogical, ymean)

  # get the one-variable model scores for every column
  getscore = function(var) {
    fmla = paste(ysymbol, var, sep="~")
    mod = glm(fmla, data=dataf[, c("y", var)],
              family=binomial(link="logit"))
    predscore = predict(mod, type="response")
    get_deviance(dataf$y=='pos', predscore)
  }

  # vector of scores
  modelscores = vapply(varnames, getscore, numeric(1))
  names(modelscores) = varnames

  # return the frame of permutation test scores
  # one for every column of the data frame
  ptests = data.frame(lapply(varnames,
                             function(v) {permtest_col(v, dataf,nperm)}))

  colnames(ptests) = varnames

  # now get the tail scores
  tailareas = vapply(varnames,
                     function(v) {left_tail(modelscores[v],
                                            ptests[[v]])},
                     numeric(1))

  # get the theoretical significance of a one variable model
  # with deviance = dev
  get_significance = function(dev) {
    delta_deviance = null_deviance - dev
    dof = 1
    sig = pchisq(delta_deviance, dof, lower.tail=FALSE)
  }

  # now get the significances of each one variable model
  varsigs = vapply(varnames,
                   function(v) {get_significance(modelscores[v])},
                   numeric(1))

  data.frame(var=varnames, ptail=tailareas, pchi = varsigs)
}

#
# Plot the scores of each variable
#
scoreplot = function(frm, pcol, threshold, sort=1) {
  n = dim(frm)[1]
  frm$var = reorder(frm$var, frm[[pcol]]*sort, FUN=sum)
  frm$goodvar = frm[[pcol]] < threshold

  # round the score to 4 decimal places, just to make the plots a bit more legible
  pvals = frm[[pcol]]
  frm[[pcol]] = round(pvals*10000)/10000

  breaks = c(0.001, 0.01, 0.05, 0.1, 0.5)

  ggplot(frm, aes_string(x='var', y=pcol, ymin=0, ymax=pcol, color='goodvar')) +
    geom_pointrange() +
    geom_hline(yintercept=threshold, color="red", linetype=2) +
    scale_color_manual(values=c("TRUE"="darkgreen", "FALSE"="darkgray")) +
    scale_y_log10(breaks=breaks, labels=breaks) +
    theme(legend.position="none")
}

#
# run an example of variable selection
#
run_vs_example = function(ngood, nnoise, datasize, nperm,
                          threshold, title='', advisory=FALSE) {
  coefs = mkCoefs(ngood)
  dTrain = mkData(datasize, coefs, nnoise)
  varnames = setdiff(colnames(dTrain), "y")

  scores = score_columns(dTrain, nperm)
  if(ngood+nnoise > 25) sort=-1 else sort=1

  # plot the variables selected

  sp = scoreplot(scores, "ptail", threshold, sort)
  if(ngood+nnoise > 25) sp = sp + coord_flip()
  sp = sp + ggtitle(paste(title, ": score by permutation"))
  print(sp)

  sp = scoreplot(scores, "pchi", threshold, sort)
  if(ngood+nnoise > 25) sp = sp + coord_flip()
  sp = sp + ggtitle(paste(title, ": score by chi-squared"))
  print(sp)


  if(ngood > 0) {
    print("Coefficients of true signal variables:")
    print(coefs)
  }
  goodvars = scores$var[scores$ptail < threshold]
  print("Selected variables (permutation):"); print(as.character(goodvars))
  print("Selected variables (chi-squared):"); print(as.character(scores$var[scores$pchi < threshold]))

  if(advisory) {
    cutoffs = c(0.01, 0.025, 0.05)
    varcounts = vapply(cutoffs, function(thresh) {sum(scores < thresh)}, numeric(1))
    print("Number of selected variables for different thresholds")
    print(data.frame(threshold=cutoffs, count=varcounts))
  }

  model_full = glm(paste("(y=='pos') ~", paste(varnames, collapse=" + ")),
                   dTrain, family=binomial(link="logit"))
  dTest = mkData(datasize, coefs, nnoise)
  dTest$fullpred = predict(model_full, newdata=dTest, type="response")

  if(length(goodvars) > 0) {
    model_reduced = glm(paste("(y=='pos') ~", paste(goodvars, collapse=" + ")),
                        dTrain, family=binomial(link="logit"))
    dTest$redpred = predict(model_reduced, newdata=dTest, type="response")
  } else {
    dTest$redpred = mean(dTrain$y == 'pos') # yes, dTrain$y
    title = paste(title, "reduced model = null model")
  }


  # compare the results of the full and reduced models on holdout data
  print(" ========= Compare full and reduced models (holdout performance) ==========")
  print(rbind(performance_eval(dTest$fullpred,dTest$y, 'pos', title=paste(title, 'full model')),
              performance_eval(dTest$redpred,dTest$y, 'pos', title=paste(title, 'reduced model'))))
}

