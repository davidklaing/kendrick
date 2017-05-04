Exploring Kendrick Lamar's discography through data science
================

A few months

``` r
# Load it up.
kendrick <- read.csv("../data/kendrick_data.csv")

# Fix the factor levels for the albums.
kendrick$album_name <- factor(kendrick$album_name, levels = c("Overly Dedicated", "Section.80", "good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.", "DAMN."))

# Remove Overly Dedicated (because it's technically a mixtape, not a studio album).
kendrick <- kendrick %>% filter(album_name != "Overly Dedicated")

# Get word counts for each album. This might be useful later.
album_word_counts <- kendrick %>% group_by(album_name) %>% summarise(word_count = sum(song_word_count))

# Fix the factor levels.
kendrick$track_name <- factor(kendrick$track_name, levels = as.character(kendrick$track_name))
```

``` r
# Plot the annotations per word.
annotation_plot <- ggplot(kendrick) +
        geom_col(aes(x = track_name,
                     y = annotations/song_word_count,
                     fill = album_name), 
                 alpha = 0.8,
                 show.legend = FALSE) +
        facet_grid(~album_name, scales = "free", space = "free") +
        theme_tufte(base_family = 'GillSans') +
        theme(axis.text.x=element_text(size=5, angle=90,hjust=0.95,vjust=0.2),
              axis.text.y=element_text(size = 5, angle=90),
              axis.title.x=element_text(size = 9, angle=180),
              axis.title.y=element_text(size = 9, angle = 90),
              strip.text = element_text(size = 7, angle = 90, vjust=0)) +
        scale_fill_manual(values = c("purple", "darkblue", "darkgrey", "darkgreen", "red")) +
        ylab("Annotations Per Word across Kendrick Lamar's Discography") +
        xlab("")

# Save the plot.
ggsave("../data/annotation_plot.png", width = 6, height = 5)

# Read it back in.
annotation_plot <- image_read('../data/annotation_plot.png')

# Rotate it.
annotation_plot <- image_rotate(annotation_plot, 90)

# Save it again.
image_write(annotation_plot, path = "../data/annotation_plot.png", format = "png")
```

![](../data/annotation_plot.png)

Sentiment Analysis
------------------

``` r
# Change the text from factor to character.
kendrick$lyrics <- as.character(kendrick$lyrics)

# Get one word per row.
tidy_kendrick <- kendrick %>% unnest_tokens(word, lyrics)

# Remove the stop words.
cleaned_kendrick <- tidy_kendrick %>%
        anti_join(stop_words)

# Check out the most common words in the corpus.
cleaned_kendrick %>%
        count(word, sort = TRUE) %>% 
        head()
```

    ## # A tibble: 6 × 2
    ##    word     n
    ##   <chr> <int>
    ## 1 nigga   264
    ## 2  love   178
    ## 3  fuck   159
    ## 4  shit   155
    ## 5 bitch   144
    ## 6  feel   131

``` r
# Get the sentiment of the Bing lexicon.
bing <- get_sentiments("bing")

# Get the sentiment across the tracks.
kendrick_sentiment <- cleaned_kendrick %>%
        inner_join(bing) %>%
        count(track_name, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = (positive - negative)/(positive + negative))

# Join.
kendrick <- inner_join(kendrick, kendrick_sentiment)

# Get a smarter measure of sentiment.
kendrick <- kendrick %>% mutate(smart_sentiment = (sentiment + (valence*2)-1)/2)
```

``` r
# See how the sentiment changes across the post order.
sentiment_plot <- ggplot(kendrick, aes(x = track_name, y = smart_sentiment, color = smart_sentiment)) +
        geom_hline(aes(yintercept=1, color=1), linetype="dashed", show.legend = FALSE) +
        geom_hline(aes(yintercept=0.5, color=0.5), linetype="dashed", show.legend = FALSE) +
        geom_hline(aes(yintercept=0, color=0), linetype="dashed", show.legend = FALSE) +
        geom_hline(aes(yintercept=-0.5, color=-0.5), linetype="dashed", show.legend = FALSE) +
        geom_hline(aes(yintercept=-1, color=-1), linetype="dashed", show.legend = FALSE) +
        geom_point(aes(x = track_name, y = smart_sentiment), size=0.1, show.legend = FALSE) +
        geom_smooth(aes(x = as.numeric(track_number), color=..y..), size = 1.5, show.legend = FALSE, se = FALSE, span = 0.3) +
        geom_smooth(aes(x = as.numeric(track_number)), color="black", size = 0.3, show.legend = FALSE, alpha = 0.9, se = FALSE, span = 0.3) +
        facet_grid(~album_name, scales = "free", space = "free") +
        scale_color_distiller(type = "div", palette = "RdYlGn", direction = 1, values = c(0,0.5,1)) +
        theme_tufte(base_family = 'GillSans') +
        theme(axis.text.x=element_text(size=5, angle=90,hjust=0.95,vjust=0.2),
              axis.text.y=element_text(size = 5, angle=90, hjust=0.5),
              axis.title.x=element_text(size = 9, angle=180),
              axis.title.y=element_text(size = 9, angle = 90),
              strip.text = element_text(size = 7, angle = 90, vjust=0)) +
        #labs(title = "Sentiment in Kendrick Lamar's Music and Lyrics") +
        xlab("") +
        ylab("Sentiment in Kendrick Lamar's Music and Lyrics") +
        scale_y_continuous(limits = c(-1,1),
                           labels = c("very negative", 
                                      "negative", 
                                      "neutral", 
                                      "positive", 
                                      "very positive"))

# Save the plot.
ggsave("../data/sentiment_plot.png", width = 6, height = 5)

# Read it back in.
sentiment_plot <- image_read('../data/sentiment_plot.png')

# Rotate it.
sentiment_plot <- image_rotate(sentiment_plot, 90)

# Save it again.
image_write(sentiment_plot, path = "../data/sentiment_plot.png", format = "png")
```

![](../data/sentiment_plot.png)

Topic Modelling
---------------

``` r
# Get the word counts for each track.
word_counts <- tidy_kendrick %>%
        select(album_name, track_number, track_name, word) %>% 
        anti_join(stop_words) %>%
        count(album_name, track_number, track_name, word, sort = TRUE) %>% 
        ungroup() %>% 
        left_join(album_word_counts)

# Take a look.
head(word_counts)
```

    ## # A tibble: 6 × 6
    ##               album_name track_number track_name  word     n word_count
    ##                   <fctr>        <int>     <fctr> <chr> <int>      <int>
    ## 1                  DAMN.            8    HUMBLE.   hol    64       8531
    ## 2 good kid, m.A.A.d city           11       Real  real    50      11644
    ## 3 good kid, m.A.A.d city           11       Real  love    48      11644
    ## 4                  DAMN.            5      FEEL.  feel    44       8531
    ## 5                  DAMN.            3       YAH.   yah    43       8531
    ## 6             Section.80            2    Hol' Up  hold    41       9181

``` r
# Get the tf-idf
album_words <- word_counts %>%
        bind_tf_idf(word, album_name, n)

# Look at the words with the highest tf-idf.
album_words %>%
        select(-word_count) %>%
        arrange(desc(tf_idf)) %>% 
        head()
```

    ## # A tibble: 6 × 8
    ##             album_name track_number                track_name     word
    ##                 <fctr>        <int>                    <fctr>    <chr>
    ## 1                DAMN.            3                      YAH.      yah
    ## 2 untitled unmastered.            7 untitled 07 | 2014 - 2016 levitate
    ## 3 untitled unmastered.            6 untitled 06 | 06.30.2014.  explain
    ## 4           Section.80            7         Ronald Reagan Era     woop
    ## 5  To Pimp A Butterfly           16                Mortal Man      fan
    ## 6 untitled unmastered.            7 untitled 07 | 2014 - 2016      bam
    ## # ... with 4 more variables: n <int>, tf <dbl>, idf <dbl>, tf_idf <dbl>

``` r
# Look at the words with the highest tf-idf within good kid, m.A.A.d city.
album_words %>%
        filter(album_name == "good kid, m.A.A.d city") %>%
        select(-word_count) %>%
        arrange(desc(tf_idf)) %>% 
        head()
```

    ## # A tibble: 6 × 8
    ##               album_name track_number
    ##                   <fctr>        <int>
    ## 1 good kid, m.A.A.d city           10
    ## 2 good kid, m.A.A.d city            9
    ## 3 good kid, m.A.A.d city            6
    ## 4 good kid, m.A.A.d city            9
    ## 5 good kid, m.A.A.d city            4
    ## 6 good kid, m.A.A.d city            5
    ## # ... with 6 more variables: track_name <fctr>, word <chr>, n <int>,
    ## #   tf <dbl>, idf <dbl>, tf_idf <dbl>

``` r
plot_albums <- album_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word))))

plot_albums %>% 
        group_by(album_name) %>%
        top_n(5) %>%
        ungroup %>%
        ggplot(aes(word, tf_idf, fill = album_name)) +
        geom_col(show.legend = FALSE, alpha = 0.8) +
        labs(x = NULL,
             y = "Term Frequency-Inverse Document Frequency",
             title = "Most representative words across Kendrick Lamar's Discography") +
        facet_wrap(~album_name, nrow = 1, scales = "free") +
        scale_fill_manual(values = c("purple", "darkblue", "darkgrey", "darkgreen", "red")) +
        theme_tufte(base_family = "GillSans") +
        theme(axis.text.x = element_text(angle = 90)) +
        coord_flip()
```

![](kendrick_files/figure-markdown_github/tfidf-1.png)

Term Frequency-Inverse Document Frequency
-----------------------------------------

``` r
font <- 1

# Word cloud for Section.80
pal1 <- brewer.pal(6, "Purples")
pal1 <- pal1[-(1:4)]
png("../results/album1.png", width=6, height=3, units="in", res=300)
layout(matrix(c(2, 1), ncol=2), widths =c(3, 3))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Section.80", font = font)
plot_albums[plot_albums$album_name == "Section.80",] %>%
        with(wordcloud(word,
                       tf_idf,
                       max.words = 75,
                       random.order = F,
                       scale = c(3,.5),
                       rot.per = 0.05,
                       color = pal1))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
?text
# Word cloud for good kid
pal2 <- brewer.pal(7, "Blues")
pal2 <- pal2[-(1:2)]
png("../results/album2.png", width=6, height=3, units="in", res=300)
layout(matrix(c(2, 1), ncol=2), widths =c(3, 3))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "good kid, m.A.A.d city", font = font)
plot_albums[plot_albums$album_name == "good kid, m.A.A.d city",] %>%
        with(wordcloud(word,
                       tf_idf,
                       max.words = 75,
                       random.order = F,
                       scale = c(3,.5),
                       rot.per = 0.05,
                       color = pal2))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Word cloud for TPAB
pal3 <- brewer.pal(7, "Greys")
pal3 <- pal3[-(1:2)]
png("../results/album3.png", width=6, height=3, units="in", res=300)
layout(matrix(c(2, 1), ncol=2), widths=c(3, 3))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "To Pimp A Butterfly", font = font)
plot_albums[plot_albums$album_name == "To Pimp A Butterfly",] %>%
        with(wordcloud(word,
                       tf_idf,
                       max.words = 75,
                       random.order = F,
                       scale = c(3,.5),
                       rot.per = 0.05,
                       color = pal3))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Word cloud for untitled unmastered.
pal4 <- brewer.pal(7, "Greens")
pal4 <- pal4[-(1:2)]
png("../results/album4.png", width=6, height=3, units="in", res=300)
layout(matrix(c(2, 1), ncol=2), widths=c(3, 3))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "untitled unmastered.", font = font)
plot_albums[plot_albums$album_name == "untitled unmastered.",] %>%
        with(wordcloud(word,
                       tf_idf,
                       max.words = 75,
                       random.order = F,
                       scale = c(3,.5),
                       rot.per = 0.05,
                       color = pal4))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Word cloud for DAMN.
pal5 <- brewer.pal(7, "Reds")
pal5 <- pal5[-(1:2)]
png("../results/album5.png", width=6, height=3, units="in", res=300)
layout(matrix(c(2, 1), ncol=2), widths=c(3, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "DAMN.", font = font)
plot_albums[plot_albums$album_name == "DAMN.",] %>%
        with(wordcloud(word,
                       tf_idf,
                       max.words = 75,
                       random.order = F,
                       scale = c(3,.5),
                       rot.per = 0.05,
                       color = pal5))
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Combine all the word clouds onto a single row.
rl <- lapply(sprintf("../results/album%i.png", 1:5), readPNG)
gl <- lapply(rl, rasterGrob, interpolate=TRUE, width = unit(2,"in"), height=unit(1,"in"))
g <- arrangeGrob(grobs=gl, ncol = 1, padding = unit(0.1, "line"),
                 top=textGrob("Representative Words Across Kendrick Lamar's Discography",
                               gp=gpar(fontsize=5,font=font)))
ggsave(file="../results/album_top_words.png", g, height = unit(5, "in"), width = unit(2.5,"in"))
```

![](../results/album_top_words.png)

More danceable songs have fewer annotations per word
----------------------------------------------------

``` r
# Danceable songs are more likely to have a higher number of pageviews.
ggplot(kendrick) +
        geom_point(aes(x = danceability, y = pageviews), alpha = 0.5) +
        geom_smooth(aes(x = danceability, y = pageviews), method = "lm") +
        labs(title="Danceability vs pageviews") +
        xlab("Danceability") +
        ylab("Pageviews") +
        scale_y_continuous(labels = comma)
```

![](kendrick_files/figure-markdown_github/testing-1.png)

``` r
summary(lm(pageviews ~ danceability, kendrick))
```

    ## 
    ## Call:
    ## lm(formula = pageviews ~ danceability, data = kendrick)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1467729  -668564  -347870   149006  4146400 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   -320546     626259  -0.512   0.6106  
    ## danceability  2353219    1013904   2.321   0.0235 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1117000 on 63 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.07877,    Adjusted R-squared:  0.06415 
    ## F-statistic: 5.387 on 1 and 63 DF,  p-value: 0.02354

``` r
# ...but danceable songs also have fewer annotations per word.
ggplot(kendrick) +
        geom_point(aes(x = danceability, y = (annotations/song_word_count)), alpha = 0.5) +
        geom_smooth(aes(x = danceability, y = (annotations/song_word_count)), method = "lm") +
        labs(title="Danceability vs annotations per word") +
        xlab("Danceability") +
        ylab("Annotations per word")
```

![](kendrick_files/figure-markdown_github/testing-2.png)

``` r
summary(lm(I(annotations/song_word_count) ~ danceability, kendrick))
```

    ## 
    ## Call:
    ## lm(formula = I(annotations/song_word_count) ~ danceability, data = kendrick)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.039237 -0.010693 -0.001782  0.011479  0.046241 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.08318    0.01027   8.101 2.14e-11 ***
    ## danceability -0.05377    0.01657  -3.245  0.00187 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01839 on 64 degrees of freedom
    ## Multiple R-squared:  0.1413, Adjusted R-squared:  0.1278 
    ## F-statistic: 10.53 on 1 and 64 DF,  p-value: 0.001871

References:

<http://rcharlie.com/2017-02-16-fitteR-happieR/> <http://tidytextmining.com/tfidf.html#the-bind_tf_idf-function> <https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html> <https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html>

``` r
sessionInfo()
```

    ## R version 3.3.1 (2016-06-21)
    ## Platform: x86_64-apple-darwin13.4.0 (64-bit)
    ## Running under: OS X 10.12.4 (Sierra)
    ## 
    ## locale:
    ## [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] wordcloud_2.5      topicmodels_0.2-6  tm_0.6-2          
    ##  [4] NLP_0.1-9          dplyr_0.5.0        purrr_0.2.2       
    ##  [7] readr_1.0.0        tidyr_0.6.1        tibble_1.2        
    ## [10] tidyverse_1.0.0    tidytext_0.1.2     stringr_1.1.0     
    ## [13] scales_0.4.1       rvest_0.3.2        xml2_1.0.0        
    ## [16] reshape2_1.4.2     RColorBrewer_1.1-2 png_0.1-7         
    ## [19] magick_0.4         lubridate_1.6.0    httr_1.2.1        
    ## [22] gridExtra_2.2.1    ggthemes_3.4.0     ggrepel_0.6.5     
    ## [25] ggplot2_2.2.1     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] modeltools_0.2-21  slam_0.1-40        lattice_0.20-33   
    ##  [4] colorspace_1.3-1   htmltools_0.3.5    SnowballC_0.5.1   
    ##  [7] stats4_3.3.1       yaml_2.1.14        foreign_0.8-66    
    ## [10] DBI_0.5-1          plyr_1.8.4         munsell_0.4.3     
    ## [13] gtable_0.2.0       psych_1.6.9        evaluate_0.10     
    ## [16] labeling_0.3       knitr_1.15.1       parallel_3.3.1    
    ## [19] broom_0.4.1        tokenizers_0.1.4   Rcpp_0.12.9.2     
    ## [22] backports_1.0.4    mnormt_1.5-4       digest_0.6.12     
    ## [25] stringi_1.1.2      rprojroot_1.1      tools_3.3.1       
    ## [28] magrittr_1.5       lazyeval_0.2.0     janeaustenr_0.1.4 
    ## [31] Matrix_1.2-6       assertthat_0.1     rmarkdown_1.2.9000
    ## [34] R6_2.1.3           nlme_3.1-128
