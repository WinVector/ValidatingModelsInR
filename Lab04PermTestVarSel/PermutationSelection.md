For simplicity, we will do this all with glm, and mostly with deviance, but in theory this method will determine model significance for any modeling method and any metric. However, for variable selection, you might as well use a simple and easy to run modeling method like glm or lm, regardless of what your final modeling algorithm will be. In fact, permutation tests with lm can be made very efficient, because you are keeping the same design matrix and only changing the y vector.

``` r
source("functions.R")
# install.packages("devtools")
# devtools::install_github("WinVector/WVPlots",build_vignettes=TRUE)
library("WVPlots")

set.seed(12959437)
```

Some Examples of Estimating Model Significance
----------------------------------------------

### A case with signal

``` r
# clean data
run_example(ngood=10, nnoise=3,
            datasize=1000, nperm=500, 'Data with signal')
```

    ## [1] "True coefficients of signal variables"
    ##        g_1        g_2        g_3        g_4        g_5        g_6 
    ##  2.2137823  0.7404727  1.5618447 -0.1593522 -0.2512571  0.3290297 
    ##        g_7        g_8        g_9       g_10 
    ##  0.5927275  0.5463776 -1.0104262 -0.7135154 
    ## [1] "Data with signal : training prevalence =  0.488 null deviance of mean = 1385.71830581115"

![](PermutationSelection_files/figure-markdown_github/cleandata-1.png)

    ## [1] "Compare training and test performance estimates, Data with signal"
    ##   deviance                     label
    ## 1 399.0531 Data with signal Training
    ## 2 457.1625     Data with signal Test
    ## [1] "Training performance compared to permutation test results"
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1349    1369    1373    1373    1377    1383 
    ## [1] "Left tail area, deviance 0"

![](PermutationSelection_files/figure-markdown_github/cleandata-2.png)

### A case with no signal

``` r
run_example(ngood=0, nnoise=10,
            datasize=1000, nperm=500, 'Data with no signal')
```

    ## [1] "Data with no signal : training prevalence =  0.478 null deviance of mean = 1384.35773595297"

![](PermutationSelection_files/figure-markdown_github/nosignal-1.png)

    ## [1] "Compare training and test performance estimates, Data with no signal"
    ##   deviance                        label
    ## 1 1378.063 Data with no signal Training
    ## 2 1392.584     Data with no signal Test
    ## [1] "Training performance compared to permutation test results"
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1357    1372    1375    1374    1377    1383 
    ## [1] "Left tail area, deviance 0.824"

![](PermutationSelection_files/figure-markdown_github/nosignal-2.png)

### Bad Bayes: very wide data with no signal

``` r
run_example(ngood=0, nnoise=300, 
            datasize=1000, nperm=200, 'Bad Bayes situation')
```

    ## [1] "Bad Bayes situation : training prevalence =  0.518 null deviance of mean = 1384.99808103867"

![](PermutationSelection_files/figure-markdown_github/badbayes-1.png)

    ## [1] "Compare training and test performance estimates, Bad Bayes situation"
    ##   deviance                        label
    ## 1 1023.542 Bad Bayes situation Training
    ## 2 2155.707     Bad Bayes situation Test
    ## [1] "Training performance compared to permutation test results"
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   914.6  1003.0  1028.0  1025.0  1044.0  1095.0 
    ## [1] "Left tail area, deviance 0.465"

![](PermutationSelection_files/figure-markdown_github/badbayes-2.png)

Using model significance (chi-squared test) to select variables.
----------------------------------------------------------------

### Data with signal

``` r
run_vs_example(ngood=10, nnoise=20,
               datasize=1000, nperm=200,
               threshold=0.05, 'Data with signal, threshold p=0.05')
```

![](PermutationSelection_files/figure-markdown_github/varSelclean-1.png)![](PermutationSelection_files/figure-markdown_github/varSelclean-2.png)

    ## [1] "Coefficients of true signal variables:"
    ##         g_1         g_2         g_3         g_4         g_5         g_6 
    ## -1.99475022 -0.28044429 -0.43687434 -2.03531353  0.03543782 -0.81308889 
    ##         g_7         g_8         g_9        g_10 
    ## -0.28042344 -0.45561740  1.65888855  0.05908969 
    ## [1] "Selected variables (permutation):"
    ## [1] "g_1" "g_2" "g_3" "g_4" "g_6" "g_7" "g_8" "g_9"
    ## [1] "Selected variables (chi-squared):"
    ## [1] "g_1"  "g_2"  "g_3"  "g_4"  "g_6"  "g_7"  "g_8"  "g_9"  "n_12"
    ## [1] " ========= Compare full and reduced models (holdout performance) =========="

![](PermutationSelection_files/figure-markdown_github/varSelclean-3.png)![](PermutationSelection_files/figure-markdown_github/varSelclean-4.png)

    ##   deviance                                            label
    ## 1 435.0796    Data with signal, threshold p=0.05 full model
    ## 2 402.7902 Data with signal, threshold p=0.05 reduced model

### Wider data with some signal

``` r
run_vs_example(ngood=5, nnoise=100,
               datasize=1000, nperm=200,
               threshold=0.05, 'Wide data with some signal, threshold p=0.05')
```

![](PermutationSelection_files/figure-markdown_github/varSelnoisySig-1.png)

    ## [1] "Coefficients of true signal variables:"
    ##        g_1        g_2        g_3        g_4        g_5 
    ## -0.9821458 -0.2626748 -1.7964945 -2.2394453 -0.5700087 
    ## [1] "Selected variables (permutation):"
    ##  [1] "g_1"   "g_3"   "g_4"   "g_5"   "n_32"  "n_50"  "n_83"  "n_96" 
    ##  [9] "n_99"  "n_100"
    ## [1] "Selected variables (chi-squared):"
    ## [1] "g_1"   "g_3"   "g_4"   "g_5"   "n_24"  "n_32"  "n_50"  "n_96"  "n_100"

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

![](PermutationSelection_files/figure-markdown_github/varSelnoisySig-2.png)

    ## [1] " ========= Compare full and reduced models (holdout performance) =========="

![](PermutationSelection_files/figure-markdown_github/varSelnoisySig-3.png)![](PermutationSelection_files/figure-markdown_github/varSelnoisySig-4.png)

    ##   deviance                                                      label
    ## 1 978.8872    Wide data with some signal, threshold p=0.05 full model
    ## 2 513.8200 Wide data with some signal, threshold p=0.05 reduced model

### Data with no signal

You would expect a threshold of 0.05 (or 1/20) to let about one out of every twenty noise variables to slip by. This gives you a rule of thumb for picking the threshold.

``` r
# scoring columns, data with no signal
nnoise = 100
p=0.05
# the number of bad variables to expect
print(nnoise*p)
```

    ## [1] 5

``` r
run_vs_example(ngood=0, nnoise=nnoise,
               datasize=1000, nperm=200,
               threshold=p, 'Data with no signal, threshold p=0.05')
```

![](PermutationSelection_files/figure-markdown_github/varSelnosig-1.png)![](PermutationSelection_files/figure-markdown_github/varSelnosig-2.png)

    ## [1] "Selected variables (permutation):"
    ## [1] "n_39" "n_58" "n_75" "n_94"
    ## [1] "Selected variables (chi-squared):"
    ## [1] "n_39" "n_58" "n_75" "n_94"
    ## [1] " ========= Compare full and reduced models (holdout performance) =========="

![](PermutationSelection_files/figure-markdown_github/varSelnosig-3.png)![](PermutationSelection_files/figure-markdown_github/varSelnosig-4.png)

    ##   deviance                                               label
    ## 1 1507.437    Data with no signal, threshold p=0.05 full model
    ## 2 1421.753 Data with no signal, threshold p=0.05 reduced model

### Bad Bayes

``` r
nnoise=300
p=0.01
# the number of bad variables to expect
print(nnoise*p)
```

    ## [1] 3

``` r
run_vs_example(ngood=0, nnoise=nnoise, 
            datasize=1000, nperm=200, 
            threshold=p, 'Bad Bayes situation, threshold p=0.01')
```

![](PermutationSelection_files/figure-markdown_github/varSelnosig2-1.png)![](PermutationSelection_files/figure-markdown_github/varSelnosig2-2.png)

    ## [1] "Selected variables (permutation):"
    ## [1] "n_54"  "n_65"  "n_99"  "n_199"
    ## [1] "Selected variables (chi-squared):"
    ## [1] "n_54" "n_65"
    ## [1] " ========= Compare full and reduced models (holdout performance) =========="

![](PermutationSelection_files/figure-markdown_github/varSelnosig2-3.png)![](PermutationSelection_files/figure-markdown_github/varSelnosig2-4.png)

    ##   deviance                                               label
    ## 1 2257.270    Bad Bayes situation, threshold p=0.01 full model
    ## 2 1429.987 Bad Bayes situation, threshold p=0.01 reduced model
