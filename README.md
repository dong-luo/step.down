
<!-- README.md is generated from README.Rmd. Please edit that file -->

# step.down

<!-- badges: start -->
<!-- badges: end -->

The goal of step.down is to …

## Installation

You can install the released version of step.down from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gargoyle1919/step.down")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(step.down)
## basic example code
set.seed(0)
x <- rnorm(1000, 1, 1)
y <- rnorm(1000)
target_scores <- apply(cbind(x, y), 1, max)
decoy_scores <- rnorm(1000)
result <- step_down(target_scores, decoy_scores, 0.2, 0.1)
result$indices_discoveries
#>   [1]  406  950  908  115  458  665  436  531  610  306  799  801  948  627  576
#>  [16]  673  577  422    6  201  879  977  145  170  535  747  429  248  134   63
#>  [31]  569   46  140  311  982  629  202  220  895  259  480  453  157  519  402
#>  [46]  323  498  927  267   65  312  245  340   45  878  112  802    7  849  380
#>  [61]  946  188  431  533  800 1000  297  934   28  940  857  293  958  280  858
#>  [76]  939  836  274   34  693  724  721  825  477  432  501  373  515  926  585
#>  [91]  246  268  671  608   20  500  582  964  979  907  395  222  510  623  414
#> [106]  322  805  200  471  319  213  683  210  810  137   43  148  583  273  446
#> [121]  733  182   85   86  111  310  479  730  909  943  893  199  824  615  426
#> [136]  160  685  366  542  175  906  902  867  804  335  584  703  275  887  823
#> [151]  279    8  711  637  718   14  593  710  407  759  947  660   40   88  328
#> [166]  634  305   92  886  238  156  876  530   67  707  214  309  754   31  524
#> [181]  777  473   21  919   84  546  740  294  957  474  981  169  287  761  394
#> [196]  339  189  935  555  994  833  945  873  345  626  697  466  389   18  589
#> [211]  447  359  915  424  240  864  497  952  795  233  482  739  493  381  301
#> [226]   78   64  651  954  440  568  588  662  989  153  470
result$cutoff_score
#>           
#> 0.9064424
```
