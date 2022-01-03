
#rm(list = ls())

# Libraries ---------------------------------------------------------------
library(visNetwork)
library(geomnet) #devtools::install_github("sctyner/geomnet") #devtools::install_version('geomnet', '0.3.1', repos="https://cran.rstudio.com/")
library(igraph)
library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(plotly)
library(ggplot2)
library(wordcloud2) 
library(ggExtra)

# Resources
#https://www.kaylinpavlik.com/classifying-songs-genres/
#https://www.reddit.com/r/spotify/comments/50b2p9/every_noise_at_once_a_genre_map_with/
#https://www.reddit.com/r/dataisbeautiful/comments/h7uoia/oc_network_of_artist_genres_in_spotify/
#https://www.rcharlie.com/spotifyr/ 
#http://developer.spotify.com/community/showcase/music-popcorn/

# Data Preparation --------------------------------------------------------
# Follow the instructions here: https://www.rcharlie.com/spotifyr/ 

id <- 'a41f9a0aea274995a97901daeef8fea5'
secret <- '1b9b37c00c814902a7b22980ab2da1c7'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

# More permissions needed. This requires having http://localhost:1410/ added to the list of redirect URL's through https://developer.spotify.com/dashboard/applications/ 
my_id <- '12162125686'

# Get my saved songs 
tracks <- data.frame()
artists_genres <- data.frame()
track_features <- data.frame()

i_max = 100
limit = 25

for(i in 1:i_max){
  print(paste0("Running ", i , " out of ", i_max))

  offset = limit * i - limit 
  
  tracks_tmp <- get_my_saved_tracks(limit = limit, offset = offset) %>%
    unnest(track.artists) %>%
    unnest(track.album.release_date) %>%
    unique() %>%
    filter(!is.null(name))
  
  artists_genres_tmp <- get_artists(unique(tracks_tmp$id)) 
  # artists_genres_tmp <- get_artists(unique(tracks_tmp$id)) 
  
  track_features_tmp <- get_track_audio_features(tracks_tmp$track.id)
  
  tracks <- rbind(tracks, tracks_tmp)
  artists_genres <- rbind(artists_genres, artists_genres_tmp)
  track_features <- rbind(track_features, track_features_tmp)
}


names(artists_genres) <- paste("genre", names(artists_genres), sep="_")
names(track_features) <- paste("features", names(track_features), sep="_")

artists_tracks_merge <- merge(tracks, artists_genres, by.x = "id", by.y = "genre_id" ) %>%
  mutate(date_raw = track.album.release_date) %>%
  #select(date_raw) %>%
  mutate(date_date = as.Date(date_raw, format="%Y-%m-%d")) %>%
  mutate(date_year = ifelse(is.na(date_date), date_raw, year(date_date))) %>%
  mutate(date_year = as.numeric(date_year)) %>%
  mutate(qty = 1) %>%
  group_by(name, date_year) %>%
  mutate(artist_qty = sum(qty)) %>%
  ungroup() %>%
  arrange(date_year)

track_features_artists_tracks_merge <- merge(artists_tracks_merge, track_features, by.x = "track.id", by.y = "features_id")

#### Visualization over time ####
g <- ggplot(artists_tracks_merge, aes(x=date_year, y=qty, fill = paste0(name))) + 
  geom_bar(position="stack", stat = "identity", width=0.9) +
  scale_fill_manual("legend", values = rep_len( c("skyblue"), nrow(artists_tracks_merge))) +
  theme_bw() + 
  theme(legend.position="none")
p <- ggplotly(g)
p



### Visualization by location ####





#### Visualization of circle bar chart - top 25 ####
#Data wrangling for genres circular bar chart
data <- artists_tracks_merge %>%
  select(genre_genres, genre_name, qty) %>%
  unnest(genre_genres) %>%
  group_by(genre_genres) %>%
  summarize(value = sum(qty)) %>%
  ungroup() %>%
  arrange(desc(value)) %>% 
  #slice(1:25) %>%
  arrange(value) %>%
  mutate(id = row_number()) %>%
  mutate(individual = genre_genres)

# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

#### Visualization of genre wordcloud ####
# wordcloud2(data %>% rename("word" = "genre_genres", "freq" = "value") %>% select(word, freq), size=0.5, color='random-light', backgroundColor="black") #color=rep_len( c("green","blue"), nrow(demoFreq) )
wordcloud2(data %>% rename("word" = "genre_genres", "freq" = "value") %>% select(word, freq), size=0.5, color='skyblue', backgroundColor="white") #color=rep_len( c("green","blue"), nrow(demoFreq) )

#### Visualization by features ####
#Too much data to display when looking at all liked songs 
data2 <- track_features_artists_tracks_merge %>%
  unnest(genre_genres)

p <- ggplot(data2, aes(x=features_danceability, y=features_acousticness, fill=name, color=genre_genres)) + #, size=cyl
  geom_point() +
  scale_fill_manual("legend", values = rep_len( c("skyblue"), nrow(artists_tracks_merge))) +
  theme_bw() + 
  theme(legend.position="none")
ggplotly(p)




















# #Get my saved songs
# my_tracks <- get_my_saved_tracks(limit = 40)
# my_tracks_wide <- unnest(my_tracks, track.artists) %>%
#   unnest(track.album.release_date)
# 
# #Get genres
# artists_genres <- get_artists(unique(my_tracks_wide$id)) 
# names(artists_genres) <- paste("genre", names(artists_genres), sep="_")
# 
# artists_tracks_merge <- merge(my_tracks_wide, artists_genres, by.x = "id", by.y = "genre_id" ) %>%
#   mutate(date_raw = track.album.release_date) %>%
#   #select(date_raw) %>%
#   mutate(date_date = as.Date(date_raw, format="%Y-%m-%d")) %>%
#   mutate(date_year = ifelse(is.na(date_date), date_raw, year(date_date))) %>%
#   mutate(date_year = as.numeric(date_year)) %>%
#   mutate(qty = 1) %>%
#   group_by(name, date_year) %>%
#   mutate(artist_qty = sum(qty)) %>%
#   ungroup() %>%
#   arrange(date_year)
# 
# #Get track audio features 
# track_features <- get_track_audio_features(artists_tracks_merge$track.id)
# names(track_features) <- paste("features", names(track_features), sep="_")
# 
# track_features_artists_tracks_merge <- merge(artists_tracks_merge, track_features, by.x = "track.id", by.y = "features_id")

#### Visualization over time ####
g <- ggplot(artists_tracks_merge, aes(x=date_year, y=qty, fill = paste0(name))) + 
  geom_bar(position="stack", stat = "identity", width=0.9) +
  scale_fill_manual("legend", values = rep_len( c("skyblue"), nrow(artists_tracks_merge))) +
  theme_bw() + 
  theme(legend.position="none")
p <- ggplotly(g)
p

#### Visualization of circle bar chart ####
#Data wrangling for genres circular bar chart
data <- artists_tracks_merge %>%
  select(genre_genres, genre_name, qty) %>%
  unnest(genre_genres) %>%
  group_by(genre_genres) %>%
  summarize(value = sum(qty)) %>%
  ungroup() %>%
  arrange(value) %>%
  mutate(id = row_number()) %>%
  mutate(individual = genre_genres)

# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Start the plot
p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p

#### Visualization of wordcloud ####
# wordcloud2(data %>% rename("word" = "genre_genres", "freq" = "value") %>% select(word, freq), size=0.5, color='random-light', backgroundColor="black") #color=rep_len( c("green","blue"), nrow(demoFreq) )
wordcloud2(data %>% rename("word" = "genre_genres", "freq" = "value") %>% select(word, freq), size=0.5, color='skyblue', backgroundColor="white") #color=rep_len( c("green","blue"), nrow(demoFreq) )


#### Visualization by features ####
data2 <- track_features_artists_tracks_merge %>%
  unnest(genre_genres)

p <- ggplot(data2, aes(x=features_danceability, y=features_acousticness, fill=name, color=genre_genres)) + #, size=cyl
  geom_point() +
  scale_fill_manual("legend", values = rep_len( c("skyblue"), nrow(artists_tracks_merge))) +
  theme_bw() + 
  theme(legend.position="none")
ggplotly(p)


# Another data driven pull
my_plists <- get_user_playlists(my_id, limit = 50) %>%
  arrange(desc(tracks.total))

tracks <- get_playlist_tracks(my_plists$id[1])
features <- get_track_audio_features(tracks$track.id)


#danceability vs acousticness


#energy vs valence 













# Legacy


colnames(artists_genres) <- paste0("genre_",artists_genres)



artists_tracks_merge <- merge(my_tracks_wide, artists_genres, by = "id")




#Test: Get details for an artist
beatles <- get_artist_audio_features('the beatles')

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

beatles_genre <- get_artist(beatles$artist_id[1])

print(beatles_genre$genres)

#Test: Get genre artists
get_genre_artists <- get_genre_artists('wonky') 

#Test 2: more permissions needed. This requires having http://localhost:1410/ added to the list of redirect URL's through https://developer.spotify.com/dashboard/applications/ 
my_id <- '12162125686'
my_plists <- get_user_playlists(my_id)
my_plists <- get_my_playlists()

plist1 <- get_playlist(my_plists$id[3])

plist1 <- get_playlist_tracks(my_plists$id[3])

#Get my saved songs
my_tracks <- get_my_saved_tracks()
my_tracks_wide <- unnest(my_tracks, track.artists)

#Test: Get genre info for multiple artists
# artist_ids <- as.data.frame(do.call(rbind, my_tracks$track.artists))
# artists_genres <- get_artists(artist_ids$id)

artists_genres <- get_artists(unique(my_tracks_wide$id)) 
  
colnames(artists_genres) <- paste0("genre_",artists_genres)

arists_tracks_merge <- merge(my_tracks_wide, artists_genres, by.x = "id", by.y = "genre_id" )

#Plot distribution of my saved songs as circle chart. Todo: Trim to top genres only
ggplot(my_tracks, aes(track.album.release_date, fill = added_at)) +
  stat_bin(breaks = seq(4, 8, .5), closed = "left") +
  stat_bin(breaks = seq(4, 8, .5), closed = "left", geom = "text", mapping = aes(track.album.release_date, label = ..count..), inherit.aes = FALSE, vjust = -.5) +
  theme_light()

ggplotly()

#Plot disribution of genres using a wordcloud 














#Load dataset
data(lesmis)

#Nodes
nodes <- as.data.frame(lesmis[2])
colnames(nodes) <- c("id", "label")

#id has to be the same like from and to columns in edges
nodes$id <- nodes$label

#Edges
edges <- as.data.frame(lesmis[1])
colnames(edges) <- c("from", "to", "width")

#Create graph for Louvain
graph <- graph_from_data_frame(edges, directed = FALSE)

#Louvain Comunity Detection
cluster <- cluster_louvain(graph)

cluster_df <- data.frame(as.list(membership(cluster)))
cluster_df <- as.data.frame(t(cluster_df))
cluster_df$label <- rownames(cluster_df)

#Create group column
nodes <- merge(nodes, cluster_df, by = "label", all.x = TRUE)
colnames(nodes)[3] <- "group"

#Fast visualization
visNetwork(nodes, edges)

#Join visualization with other functions
visNetwork(nodes, edges, width = "100%") %>%
  visIgraphLayout() %>%
  visNodes(
    shape = "dot",
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 10)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = "group") %>% 
  visLayout(randomSeed = 11)

#Save rdata files
saveRDS(nodes, "nodes.rds")
saveRDS(edges, "edges.rds")







