
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semdistflow

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of semdistflow is to …

## Installation

You can install the development version of semdistflow from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Reilly-ConceptsCognitionLab/semdistflow")
```

## Example of Cleaning Function

This is a basic example which shows you how the cleanme function works:

``` r
library(semdistflow)
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.8
#> ✓ tidyr   1.2.0     ✓ stringr 1.4.0
#> ✓ readr   2.1.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

doc_id <- "fox"
doc_text <- "The quick brown fox jumps over the lazy dog."
fox_text <-as.data.frame(cbind(doc_id,doc_text))
fox_text
#>   doc_id                                     doc_text
#> 1    fox The quick brown fox jumps over the lazy dog.
```

``` r
fox_clean <- cleanme(fox_text)
#> Performing cleaning retaining pronouns
fox_clean
#> # A tibble: 1 × 3
#> # Groups:   doc_id [1]
#>   doc_id doc_text                                     doc_clean                 
#>   <chr>  <chr>                                        <chr>                     
#> 1 fox    The quick brown fox jumps over the lazy dog. " quick brown fox jumps l…
```

## Example of Semantic Distance Function

This is a basic example which shows you how the cleanme function works:

``` r
fox_token <-fox_clean %>%
  group_by(doc_id, doc_text) %>%
  tidytext::unnest_tokens(word, doc_clean, drop=F)

fox_token$lemma<- textstem::lemmatize_words(fox_token$word)


fox_token
#> # A tibble: 6 × 5
#> # Groups:   doc_id, doc_text [1]
#>   doc_id doc_text                                     doc_clean      word  lemma
#>   <chr>  <chr>                                        <chr>          <chr> <chr>
#> 1 fox    The quick brown fox jumps over the lazy dog. " quick brown… quick quick
#> 2 fox    The quick brown fox jumps over the lazy dog. " quick brown… brown brown
#> 3 fox    The quick brown fox jumps over the lazy dog. " quick brown… fox   fox  
#> 4 fox    The quick brown fox jumps over the lazy dog. " quick brown… jumps jump 
#> 5 fox    The quick brown fox jumps over the lazy dog. " quick brown… lazy  lazy 
#> 6 fox    The quick brown fox jumps over the lazy dog. " quick brown… dog   dog
```

``` r
fox_dist <-  bigram_cos_sim(targetdf = fox_token, lookupdb = semdist15, colname1 = lemma, colname2 = word)
#> Isolating join columns
#> Adding missing grouping variables: `doc_text`
#> Joining data + print
#> Calculating pairwise cosine similarities
#> Writing output dataframe

fox_dist
#> # A tibble: 6 × 7
#>   doc_id doc_text                  doc_clean word  lemma_pair1 lemma cosine.dist
#>   <chr>  <chr>                     <chr>     <chr> <chr>       <chr>       <dbl>
#> 1 fox    The quick brown fox jump… " quick … quick <NA>        quick     NA     
#> 2 fox    The quick brown fox jump… " quick … brown quick       brown     -0.171 
#> 3 fox    The quick brown fox jump… " quick … fox   brown       fox        0.481 
#> 4 fox    The quick brown fox jump… " quick … jumps fox         jump      -0.0473
#> 5 fox    The quick brown fox jump… " quick … lazy  jump        lazy       0.303 
#> 6 fox    The quick brown fox jump… " quick … dog   lazy        dog       -0.293
```

``` r
ggplot(fox_dist, aes(x=as.numeric(row.names(fox_dist)), y=cosine.dist)) +  geom_line(color="#02401BD9", size= 1) + theme_classic() + xlab(NULL) + ylab(NULL)
#> Warning: Removed 1 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-plot-1.png" width="100%" />
