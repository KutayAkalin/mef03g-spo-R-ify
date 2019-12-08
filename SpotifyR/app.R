#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(httr)
library(spotifyr)
library(shiny)
library(dplyr)
library(tidyverse)
library(dplyr)
library(readr)
library(reprex)
library(fmsb)
library(tidyr)
library(scales)
library(radarchart)
library(RCurl)

url<-getURL("https://raw.githubusercontent.com/pjournal/mef03g-spo-R-ify/master/SpotifyR/top_50_charts.txt")
top_50_charts <- read.delim(text=url)

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Playlist Features by Country"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    textInput("playlist_id", "Enter User 1 Playlist ID", value = "7mJKc32vPRWxI8dg8awSus"),
    textInput("playlist_id2", "Enter User 2 Playlist ID", value = "0mu9Wiek5PuCxcRy0mrrgB"),
    checkboxGroupInput("country_id","Select Playlists",choices = c("User 1", "User 2",as.character(top_50_charts$country)), selected = c("Turkey","Turkey70s","Turkey2000s"))),
  # Show a plot of the generated distribution
  mainPanel(
    chartJSRadarOutput("radarPlot")
  )
)


# Define server logic required to draw
server <- function(input, output) {
  
  output$radarPlot <- renderChartJSRadar({
    input_playlist_id<-input$playlist_id
    input_playlist_id_2 <- input$playlist_id2
    #Spotify login
    Sys.setenv(SPOTIFY_CLIENT_ID = 'f5adea41ba0c4184a3d15e9960b4a0c2')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b5362e9b6a44ea6a774911ae7700334')
    access_token <- get_spotify_access_token()
    input_audio_features <- get_playlist_audio_features("spotifycharts", input_playlist_id)
    input_audio_features_2 <- get_playlist_audio_features("spotifycharts", input_playlist_id_2)
    #First User's Playlist
    mean_values_user<-input_audio_features %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo, track.duration_ms)%>%
      mutate(country="User 1",danceability=mean(danceability,na.rm=T),energy=mean(energy,na.rm=T),loudness=mean(loudness,na.rm=T),
             speechiness=mean(speechiness,na.rm=T),acousticness=mean(acousticness,na.rm=T),instrumentalness=mean(instrumentalness,na.rm=T),
             liveness=mean(liveness,na.rm=T),valence=mean(valence,na.rm=T),tempo=mean(tempo,na.rm=T), track.duration_ms = mean(track.duration_ms, na.rm=T))
    mean_values_user<-mean_values_user[1,]
    #Second User's Playlist
    mean_values_user_2<-input_audio_features_2 %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo, track.duration_ms)%>%
      mutate(country="User 2",danceability=mean(danceability,na.rm=T),energy=mean(energy,na.rm=T),loudness=mean(loudness,na.rm=T),
             speechiness=mean(speechiness,na.rm=T),acousticness=mean(acousticness,na.rm=T),instrumentalness=mean(instrumentalness,na.rm=T),
             liveness=mean(liveness,na.rm=T),valence=mean(valence,na.rm=T),tempo=mean(tempo,na.rm=T), track.duration_ms = mean(track.duration_ms, na.rm=T))
    mean_values_user_2<-mean_values_user_2[1,]
    df_combined<-full_join(top_50_charts,mean_values_user)
    df_combined <- full_join(df_combined, mean_values_user_2)
    df_combined[c("loudness", "tempo", "track.duration_ms")] <- lapply(df_combined[c("loudness", "tempo", "track.duration_ms")], 
                                                                       function(x){(x-min(x)) /(max(x) - min(x))})
    
    #Radar Chart
    Features_df<- df_combined[df_combined$country %in% input$country_id,]
    #Normalizing Data
    radarDF <- gather(Features_df, key=Attribute, value=Score, -country) %>%
      spread(key=country, value=Score)
    
    chartJSRadar(scores = radarDF,
                 scaleStartValue = 0, 
                 maxScale =1, 
                 showToolTipLabel = TRUE)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)