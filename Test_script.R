## pętla do zaciągania top tracków dla wszystkich userów
base_url <- "http://ws.audioscrobbler.com/2.0/"
api_key <- Sys.getenv("API_KEY")
users <- list("tonykk22","mireeeg","mikers_","tatel0107")

top_tracks_df <- data.frame()
user_df <- data.frame()

for (x in users) {
  api_method <- "method=user.getTopTracks"
  api_url <- paste0(base_url, "?", api_method, "&user=", URLencode(x), "&api_key=",   api_key, "&format=json")
  response <- GET(api_url)
  status_code <- response$status_code
  content <- content(response, "text", encoding = "UTF-8")
  user_df <- flatten(as.data.frame(fromJSON(content)))
  top_tracks_df <- rbind(top_tracks_df, user_df)
}

