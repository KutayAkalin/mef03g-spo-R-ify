library(spotifyr)
library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(treemap)

Sys.setlocale('LC_ALL','C')

ui <- fluidPage(
  titlePanel("Musical Horoscpoe"),
  textInput("userName", "Please Enter Your Username", value = "spotifycharts"),
  textInput("userPL", "Please Enter Your Playlist Code", value = "7mJKc32vPRWxI8dg8awSus"),
  tabsetPanel(
    tabPanel(title = " Playlist Key Attributes", plotOutput("userPlot", width = 900, height = 700)),
    tabPanel(title = " Playlist Key Characteristics", plotOutput("userTree", width = 1000, height = 700)),
    tabPanel(title = " Personality Type", h3("What is your personality type?"), br(),  
p("The key signatures of music are kind of like the signs of the zodiac."),  
"If you take all the music in a given key, you’ll discover certain characteristics — 
characteristics that can define your personality.", hr(), tableOutput("userPersona"), hr(), br(),
print(em("The information above is for display purposes only and does not depend on any scientific, 
astrological or musical know-how.")))
  )
)



server <- function(input, output) {
  Sys.setenv(SPOTIFY_CLIENT_ID = 'f5adea41ba0c4184a3d15e9960b4a0c2')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b5362e9b6a44ea6a774911ae7700334')
  access_token <- get_spotify_access_token()
  
  output$userPlot <- renderPlot({
    uSer <- get_playlist_audio_features(input$userName, input$userPL) %>%
      select(danceability, energy, speechiness, instrumentalness, liveness, valence, acousticness) %>%
      sapply(FUN = mean) %>% as.data.frame()
    
    ggplot(uSer, aes(x = rownames(uSer), y = ., fill = rownames(uSer))) + 
      geom_col() + 
      coord_polar() + 
      labs(x = paste(toString(input$userName), "Playlist Attributes"), title = "Playlist Average Attributes") + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(axis.title.x = element_text(size = 14, face = "bold")) +
      theme(legend.title = element_blank()) +
      theme(plot.background = element_rect(fill = "grey"))
  })
  output$userTree <- renderPlot({
    get_playlist_audio_features(input$userName, input$userPL) %>% group_by(key_name) %>%
      summarise(count = n()) %>%
      treemap(index = "key_name", vSize = "count", type = "index", 
              palette = "Set1", inflate.labels = TRUE, 
              title = paste(toString(input$userName), "Playlist Key Characteristics"), 
              fontsize.title = 20) 
  })
  output$userPersona <- renderTable ({
    personaKey <- get_playlist_audio_features(input$userName, input$userPL) %>% group_by(key_name) %>%
      summarise(count = n()) %>% filter(count == max(count))
    if (personaKey$key_name == "A") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/A.csv?raw=true")
    } else if (personaKey$key_name == "A#") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Amj.csv?raw=true")
    } else if (personaKey$key_name == "B") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/B.csv?raw=true")
    } else if (personaKey$key_name == "C") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/C.csv?raw=true")
    } else if (personaKey$key_name == "C#") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Cmj.csv?raw=true")
    } else if (personaKey$key_name == "D") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/D.csv?raw=true")
    } else if (personaKey$key_name == "D#") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Dmj.csv?raw=true")
    } else if (personaKey$key_name == "E") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/E.csv?raw=true")
    } else if (personaKey$key_name == "F") {
      personaInfo <- read.csv2("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/F.csv?raw=true")
    } else if (personaKey$key_name == "F#") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Fmj.csv?raw=true")
    } else if (personaKey$key_name == "G") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/G.csv?raw=true")
    } else if (personaKey$key_name == "G#") {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Gmj.csv?raw=true")
    } else {
      personaInfo <- read.csv("https://github.com/pjournal/mef03g-spo-R-ify/blob/master/SpotifyR/Amj.csv?raw=true")
    }
    as.data.frame(personaInfo)

  })

}


shinyApp(ui, server)
