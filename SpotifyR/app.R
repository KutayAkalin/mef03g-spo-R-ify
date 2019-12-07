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


top_50_charts <- read.delim("~/top_50_charts.txt")


# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Playlist Features by Country"),
    
    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        textInput("playlist_id", "Enter playlist id", value = "7mJKc32vPRWxI8dg8awSus"
        ),
        checkboxGroupInput("country_id","Select Countries",choices = c("User",country_mean_df$country), selected = c("Turkey","UnitedStates","Japan"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
        chartJSRadarOutput("radarPlot")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$radarPlot <- renderChartJSRadar({
        input_playlist_id<-input$playlist_id
        #Spotify login
        Sys.setenv(SPOTIFY_CLIENT_ID = 'f5adea41ba0c4184a3d15e9960b4a0c2')
        Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b5362e9b6a44ea6a774911ae7700334')
        access_token <- get_spotify_access_token()
        input_audio_features <- get_playlist_audio_features("spotifycharts", input_playlist_id)
        #User Playlist
        mean_values_user<-input_audio_features %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo,track.duration_ms)%>%
            mutate(country="User",danceability=mean(danceability,na.rm=T),energy=mean(energy,na.rm=T),loudness=mean(loudness,na.rm=T),speechiness=mean(speechiness,na.rm=T),acousticness=mean(acousticness,na.rm=T),instrumentalness=mean(instrumentalness,na.rm=T),liveness=mean(liveness,na.rm=T),valence=mean(valence,na.rm=T),tempo=mean(tempo,na.rm=T),track.duration_ms=mean(track.duration_ms,na.rm=T))
        mean_values_user<-mean_values_user[1,]
        df_combined<-full_join(top_50_charts,mean_values_user)
        df_names<-df_combined[1]
        
        
        #Radar Chart
        Features_df<- df_combined[df_combined$country %in% input$country_id,]
        #Normalizing Data
        df_normalized <- cbind(Features_df[1], 
                               apply(Features_df[-1],2,
                                     function(x){(x-min(x)) / diff(range(x))}))
        radarDF <- gather(df_normalized, key=Attribute, value=Score, -country) %>%
            spread(key=country, value=Score)
        
        chartJSRadar(scores = radarDF,
                     scaleStartValue = 0, 
                     maxScale =1, 
                     showToolTipLabel = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
