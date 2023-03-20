library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

library(tidygraph)
library(tidyverse)
library(tibble)
library(igraph)
library(GGally)
library(corrgram)
library(bubbles)
library(stringr)

quotes <- read_csv("quotes.csv")
quotes_tags <- separate_rows(quotes,4,sep = ';')

# TAGS
tags <- unlist(strsplit(quotes$tags, ";"))
tags_freq <- table(tags)

#frequency
tags_freq_df <- quotes %>%
  separate_rows(tags, sep = ";") %>%
  group_by(tags) %>%
  summarise(freq = n()) %>%
  ungroup()

ggplot(tags_freq_df[order(tags_freq_df$freq, decreasing = TRUE), ][1:25,], aes(x = freq, y = tags)) +
  geom_point(size = 3, color = "blue") +
  geom_segment(aes(x = 0, y = tags, xend = freq, yend = tags),
               color = "gray70", size = 0.5) +
  theme_classic() +
  labs(title = "Occurence Map of Goodreads Quote Tags",
       x = "Frequency of Tag", y = "Tag")

# bubbles
top_tags <- names(sort(tags_freq, decreasing = TRUE)[1:15])
top_freq <- as.numeric(tags_freq[top_tags])

bubbles(top_freq, 
        label = top_tags,
        textColor = "tomato",color = hcl.colors(15,palette = "viridis", alpha=NULL)[sample(15)]
          )

# network
tag_pair <- crossprod(table(quotes_tags[3:4]))
diag(tag_pair) <- 0

top_100_tags <- names(sort(tags_freq, decreasing = TRUE)[1:20])

sub_matrix <- tag_pair[top_100_tags, top_100_tags]

ggnet2(sub_matrix, size = 12, label = TRUE, color = "black", label.color = "tomato")

# correlogram
ggcorr(sub_matrix, method = c("everything", "pearson"))

#heatmap
heatmap(sub_matrix, Colv = NA, Rowv = NA, scale="column",col = rainbow(256))

# AUTHORS
normalize_author <- function(author) {
  # Remove any extra commas and spaces
  author <- gsub(",\\s*", "", author)
  # Convert to lowercase
  author <- tolower(author)
  # Return the normalized author name
  return(author)
}

authors <- sapply(quotes$author, normalize_author)
quotes_authors <- quotes
quotes_authors$author <- authors
author_freq <- table(authors)

author_freq_df <- quotes_authors %>%
  separate_rows(author, sep = ";") %>%
  group_by(author) %>%
  summarise(freq = n()) %>%
  ungroup()

#author frequency
ggplot(author_freq_df[order(author_freq_df$freq, decreasing = TRUE), ][1:25,], aes(x = freq, y = author)) +
  geom_point(size = 3, color = "blue") +
  geom_segment(aes(x = 0, y = author, xend = freq, yend = author),
               color = "gray70", size = 0.5) +
  theme_classic() +
  labs(title = "Occurence Map of Goodreads Quote Authors",
       x = "Frequency of Author", y = "Author")

#author bubbles
top_authors <- names(sort(author_freq, decreasing = TRUE)[1:25])
top_author_freq <- as.numeric(author_freq[top_authors])

bubbles(top_author_freq, 
        label = top_authors,
        textColor = "tomato",color = hcl.colors(25,palette = "viridis", alpha=NULL)[sample(25)]
)



