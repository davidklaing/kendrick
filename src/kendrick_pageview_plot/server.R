library(ggthemes)
library(plotly)
library(RColorBrewer)
library(scales)
library(shiny)
library(tidytext)
library(tidyverse)

# Read in the data.
kendrick <- read.csv("kendrick_data.csv")

# Fix the factor levels for the albums.
kendrick$album_name <- factor(kendrick$album_name, levels = c("Overly Dedicated", "Section.80", "good kid, m.A.A.d city", "To Pimp A Butterfly", "untitled unmastered.", "DAMN."))

# Remove Overly Dedicated (because it's technically a mixtape, not a studio album).
kendrick <- kendrick %>% filter(album_name != "Overly Dedicated")

# Fix the factor levels for the tracks.
kendrick$track_name <- factor(kendrick$track_name, levels = as.character(kendrick$track_name))

# Change the text from factor to character.
kendrick$lyrics <- as.character(kendrick$lyrics)

# Get one word per row.
tidy_kendrick <- kendrick %>% unnest_tokens(word, lyrics)

# Remove stop words. (These are words like "the" and "a", which only carry syntactic meaning.)
cleaned_kendrick <- tidy_kendrick %>%
        anti_join(stop_words)

# Get the sentiment of words in the Bing lexicon.
bing <- get_sentiments("bing")

# Get the sentiment across the tracks.
kendrick_sentiment <- cleaned_kendrick %>%
        inner_join(bing) %>%
        count(track_name, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = (positive - negative)/(positive + negative))

# Join the sentiment with the original dataset.
kendrick <- inner_join(kendrick, kendrick_sentiment)

# Transform the valence to the same scale as the sentiment.
kendrick <- kendrick %>% mutate(valence = ((valence*2)-1))

# Get a smarter measure of sentiment
kendrick <- kendrick %>% mutate(smart_sentiment = (sentiment + valence)/2)

# Get a measure of the difference between lyric sentiment and song valence.
# This tells us which songs sound positive but are filled with especially negative
# lyrics, or vice versa.
kendrick <- kendrick %>% mutate(sent_val_dif = abs(valence - sentiment))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        # Get whether the plot is to be on a log scale or normal scale.
        pageview_scale <- reactive({
                if (input$log_or_not == FALSE) {
                        return(kendrick$pageviews)
                } else {
                        return(log(kendrick$pageviews))
                }
        })
        
        y_ax_title <- reactive({
                if (input$log_or_not == FALSE) {
                        return("Pageviews on genius.com")
                } else {
                        return("Log of pageviews on genius.com")
                }
        })
        
        plot_title <- reactive({
                if (input$log_or_not == FALSE) {
                        return("Pageviews of Kendrick Lamar's songs on genius.com,<br>as predicted by musical/lyrical consistency")
                } else {
                        return("Log of pageviews of Kendrick Lamar's songs on genius.com,<br>as predicted by musical/lyrical consistency")
                }
        })
        
        observe({
                
                g <- ggplot(kendrick, aes(x = sent_val_dif, y = pageview_scale(), text = paste0(track_name, "<br>", album_name))) +
                        geom_point(aes(color = album_name), size = 2, alpha = 0.7) +
                        theme_tufte(base_family = 'GillSans') +
                        theme(legend.position = "none") +
                        scale_y_continuous(labels = comma) +
                        scale_color_manual(values = c("purple", "darkblue", "black", "darkgreen", "red")) +
                        labs(title=plot_title()) +
                        ylab(y_ax_title()) +
                        xlab("Absolute difference between musical sentiment and lyrical sentiment")
                
                output$pageview_plot <- renderPlotly({
                        ggplotly(g, tooltip = "text")
                })
                
        })
  
})
