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
playlistURI <-read_csv("~/playlistURI.csv")
playlistURI<-playlistURI%>%separate("Playlist;URI",c("country_name","country_id",sep = ";"))%>%select("country_name","country_id")
country_playlist_id<-as.list(playlistURI$country_id)
country_name<-as.list(playlistURI$country_name)

#Spotify login
Sys.setenv(SPOTIFY_CLIENT_ID = 'f5adea41ba0c4184a3d15e9960b4a0c2')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b5362e9b6a44ea6a774911ae7700334')
access_token <- get_spotify_access_token()

#Template for extracting country data
top_50_df<-data.frame()
for (i in 1:52) {
    playlist_id = country_playlist_id[i]
    a <- get_playlist_audio_features("spotifycharts",playlist_id)
    a<-a %>% mutate(country=paste0(country_name[i]))%>%select(country,danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo,track.duration_ms,track.album.release_date)
    
    top_50_df <- rbind(top_50_df, a)
}

#Country Mean Values
test<-top_50_df%>%group_by(country)%>%mutate(danceability=mean(danceability,na.rm=T),energy=mean(energy,na.rm=T),loudness=mean(loudness,na.rm=T),speechiness=mean(speechiness,na.rm=T),acousticness=mean(acousticness,na.rm=T),instrumentalness=mean(instrumentalness,na.rm=T),liveness=mean(liveness,na.rm=T),liveness=mean(liveness,na.rm=T),valence=mean(valence,na.rm=T),tempo=mean(tempo,na.rm=T),track.duration_ms=mean(track.duration_ms,na.rm=T))%>%select(-12)
country_mean_df<-test[!duplicated(test$country), ]


# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Playlist Features by Country"),
    
    # Sidebar with a slider input for number of bins 
    sidebarPanel(
            textInput("playlist_id", "Enter playlist id", value = "7mJKc32vPRWxI8dg8awSus"
            ),
            checkboxGroupInput("country_id","Select Countries",choices = c("User",country_mean_df$country), selected = c("User","Turkey","UnitedStates","Japan"))
        ),
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("radarPlot")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$radarPlot <- renderPlot({
        input_playlist_id<-input$playlist_id
        #Spotify login
        Sys.setenv(SPOTIFY_CLIENT_ID = 'f5adea41ba0c4184a3d15e9960b4a0c2')
        Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b5362e9b6a44ea6a774911ae7700334')
        access_token <- get_spotify_access_token()
        input_audio_features <- get_playlist_audio_features("spotifycharts", input_playlist_id)
        #User Playlist
        mean_values_user<-input_audio_features %>% select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo,track.duration_ms,track.album.release_date)%>%
            mutate(country="User",danceability=mean(danceability,na.rm=T),energy=mean(energy,na.rm=T),loudness=mean(loudness,na.rm=T),speechiness=mean(speechiness,na.rm=T),acousticness=mean(acousticness,na.rm=T),instrumentalness=mean(instrumentalness,na.rm=T),liveness=mean(liveness,na.rm=T),valence=mean(valence,na.rm=T),tempo=mean(tempo,na.rm=T),track.duration_ms=mean(track.duration_ms,na.rm=T))
        mean_values_user<-test[!duplicated(mean_values_user$country), ]
        df<-full_join(country_mean_df,mean_values_user)
        df_input<-df%>%filter(df$country== input$country_id)
        #Normalizing Data
        range02 <- function(x) {
            (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))
        }
        normalized_df<-as.data.frame(apply(df_input[-1],2,function(x){(x-min(x)) / diff(range(x))}))
        
        #Adding max and min!
        dfradar<-rbind(rep(1, nrow(normalized_df)) , rep(0, nrow(normalized_df)) , normalized_df)
        radarchart(dfradar, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
                   title="Spotify Graph")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


