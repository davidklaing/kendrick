# kendrick

David Laing, May 2017

This repository contains some analysis and visualization of Kendrick Lamar's music, using data from Spotify's and Genius's APIs. The script I used to query the data, which is adapted from RCharlie's post [fitteR happieR](http://rcharlie.com/2017-02-16-fitteR-happieR/), can be found [here](https://github.com/laingdk/kendrick/blob/master/src/scrape_kendrick.R). If you'd like to download the data yourself, you can find it [here](https://github.com/laingdk/kendrick/blob/master/data/scraped_kendrick_data.csv). My knitted markdown file is [here](https://github.com/laingdk/kendrick/blob/master/results/kendrick.md).

## Dependencies

```
> sessionInfo()

R version 3.3.1 (2016-06-21)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X 10.12.4 (Sierra)

locale:
[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] wordcloud_2.5      topicmodels_0.2-6  tm_0.6-2           NLP_0.1-9          dplyr_0.5.0       
 [6] purrr_0.2.2        readr_1.0.0        tidyr_0.6.1        tibble_1.2         tidyverse_1.0.0   
[11] tidytext_0.1.2     stringr_1.1.0      scales_0.4.1       rvest_0.3.2        xml2_1.0.0        
[16] reshape2_1.4.2     RColorBrewer_1.1-2 png_0.1-7          lubridate_1.6.0    httr_1.2.1        
[21] gridExtra_2.2.1    ggthemes_3.4.0     ggrepel_0.6.5      ggplot2_2.2.1      magick_0.4        

loaded via a namespace (and not attached):
 [1] splines_3.3.1      modelr_0.1.0       assertthat_0.1     stats4_3.3.1       coin_1.1-3        
 [6] yaml_2.1.14        slam_0.1-40        backports_1.0.4    lattice_0.20-33    digest_0.6.12     
[11] minqa_1.2.4        sandwich_2.3-4     colorspace_1.3-1   htmltools_0.3.5    Matrix_1.2-6      
[16] plyr_1.8.4         psych_1.6.9        broom_0.4.1        sjPlot_2.1.2       haven_1.0.0       
[21] mvtnorm_1.0-5      stringdist_0.9.4.2 lme4_1.1-12        effects_3.1-2      TH.data_1.0-7     
[26] nnet_7.3-12        lazyeval_0.2.0     mnormt_1.5-4       survival_2.39-4    magrittr_1.5      
[31] evaluate_0.10      tokenizers_0.1.4   janeaustenr_0.1.4  nlme_3.1-128       SnowballC_0.5.1   
[36] MASS_7.3-45        foreign_0.8-66     rsconnect_0.7      tools_3.3.1        multcomp_1.4-6    
[41] munsell_0.4.3      nloptr_1.0.4       labeling_0.3       rmarkdown_1.2.9000 codetools_0.2-14  
[46] gtable_0.2.0       sjstats_0.7.0      DBI_0.5-1          sjmisc_2.1.0       R6_2.1.3          
[51] zoo_1.7-13         knitr_1.15.1       rprojroot_1.1      modeltools_0.2-21  stringi_1.1.2     
[56] parallel_3.3.1     Rcpp_0.12.9.2      lmtest_0.9-34
```