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






write.table(country_mean_df, "~/top_50_charts.txt", sep="\t")

