library(httr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(plotly)

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

top_albums_df <- data.frame()

for (x in users) {
  api_method <- "method=user.getTopAlbums"
  api_url <- paste0(base_url, "?", api_method, "&user=", URLencode(x), "&api_key=",   api_key, "&format=json")
  response <- GET(api_url)
  status_code <- response$status_code
  content <- content(response, "text", encoding = "UTF-8")
  user_df <- flatten(as.data.frame(fromJSON(content)))
  top_albums_df <- rbind(top_albums_df, user_df)
}

## pętla do zaciągania top Artystów dla wszystkich userów

top_artists_df <- data.frame()

for (x in users) {
  api_method <- "method=user.getTopArtists"
  api_url <- paste0(base_url, "?", api_method, "&user=", URLencode(x), "&api_key=",   api_key, "&format=json")
  response <- GET(api_url)
  status_code <- response$status_code
  content <- content(response, "text", encoding = "UTF-8")
  user_df <- flatten(as.data.frame(fromJSON(content)))
  top_artists_df <- rbind(top_artists_df, user_df)
}

#removing placeholder df
rm(user_df)

user_albums <- data.frame(
  Album = top_albums_df$topalbums.album.name,
  Artist = top_albums_df$topalbums.album.artist.name,
  Playcount = as.numeric(top_albums_df$topalbums.album.playcount),
  User = top_albums_df$topalbums..attr.user
)

user_tracks <- data.frame(
  Tracks = head(top_tracks_df$toptracks.track.name,5),
  Playcount = top_tracks_df$toptracks.track.playcount,
  user = top_tracks_df$toptracks..attr.user
)


ui <- dashboardPage(
  dashboardHeader(title = "Last.fm Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                # Panels showing top tracks for each user
                lapply(users, function(user) {
                  column(width = 3,
                         box(title = user, width = NULL,
                             tableOutput(substitute(user_tracks[user_tracks$user == user,], list(user = as.name(user)))))
                  )
                })
              ),
              fluidRow(
                # Bubble chart
                box(title = "Top Albums", width = 12,
                    selectInput("user_selection", "Select User:",
                                choices = unique(user_albums$User),
                                selected = unique(user_albums$User)[1]),
                    plotlyOutput("bubble_chart")
                )
              )
      )
    )
  )
)


# Server
server <- function(input, output) {
  # Render bubble chart
  output$bubble_chart <- renderPlotly({
    user <- user_albums[user_albums$User == input$user_selection, ]
    
    # Create the bubble chart
    p <- plot_ly(user, x = ~Artist, y = ~Album, 
                 size = ~Playcount, color = ~Artist, colors = "Set3",
                 text = ~paste("Album: ", Album, "<br>",
                               "Artist: ", Artist, "<br>",
                               "Playcount: ", Playcount),
                 hoverinfo = "text",
                 type = "scatter",
                 mode = "markers",
                 marker = list(sizemode = "diameter"))
    
    # Set the chart layout
    p <- p %>% layout(
      xaxis = list(title = "Artist", tickangle = -45),
      yaxis = list(title = "Album"),
      showlegend = FALSE
    )
    
    # Return the bubble chart
    p
  })
}

# Run the app
shinyApp(ui, server)