library(httr)
library(jsonlite)
library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)

## pętla do zaciągania top tracków dla wszystkich userów
base_url <- "http://ws.audioscrobbler.com/2.0/"
api_key <- Sys.getenv("API_KEY")
users <- list("tonykk22","Mireeeg","mikers_","Tatel0107")

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

print("Top tracks were loaded")

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

print("Top albums were loaded")

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

print("Top artists were loaded")

# top tracki w ostatnim tygodniu
top_tracks_lw_df <-data.frame()

for (x in users) {
  api_method <- "method=user.getWeeklyTrackChart"
  api_url <- paste0(base_url, "?", api_method, "&user=", URLencode(x), "&api_key=",   api_key, "&format=json")
  response <- GET(api_url)
  status_code <- response$status_code
  content <- content(response, "text", encoding = "UTF-8")
  user_df <- flatten(as.data.frame(fromJSON(content)))
  top_tracks_lw_df <- rbind(top_tracks_lw_df, user_df)
}

print("Last week's tracks were loaded")

print("Preparing data")

top_tracks_lw_df 


last_week_sums <- top_tracks_lw_df %>% 
  group_by(weeklytrackchart..attr.user) %>%
  summarize(total_playcount = sum(as.numeric(weeklytrackchart.track.playcount)))

#removing placeholder df
rm(user_df)

user_albums <- data.frame(
  Album = top_albums_df$topalbums.album.name,
  Artist = top_albums_df$topalbums.album.artist.name,
  Playcount = as.numeric(top_albums_df$topalbums.album.playcount),
  user = top_albums_df$topalbums..attr.user
)

user_tracks <- data.frame(
  Tracks = top_tracks_lw_df$weeklytrackchart.track.name,
  Playcount = top_tracks_lw_df$weeklytrackchart.track.playcount,
  user = top_tracks_lw_df$weeklytrackchart..attr.user
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
                         box(title = paste(user, ' - top tracks last week'), width = NULL,
                             tableOutput(paste0("topTracksTable_", user))
                         )
                  )
                })
              ),
              fluidRow(
                # Bubble chart
                column(width = 12,
                       box(title = "Top Albums", width = NULL,
                           selectInput("user_selection", "Select user:",
                                       choices = unique(user_albums$user),
                                       selected = unique(user_albums$user)[1]),
                           plotlyOutput("bubble_chart")
                       )
                ),
                fluidRow(
                  # Bar chart
                  column(width = 4,
                         box(title = "Last Week Playcount", width = NULL,
                             plotlyOutput("bar_chart")
                       )
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  # Render top tracks tables
  lapply(users, function(user) {
    output[[paste0("topTracksTable_", user)]] <- renderTable({
      subset_user_tracks <- head(user_tracks[user_tracks$user == user, 1:2], 5)
      subset_user_tracks
    })
  })
  
  # Render bar chart
  output$bar_chart <- renderPlotly({
    # Create the bar chart
    p <- plot_ly(last_week_sums, x = ~weeklytrackchart..attr.user, y = ~total_playcount,
                 type = "bar", marker = list(color = "#6e83bb"),
                 text = ~total_playcount)
    
    
    p <- p %>% layout(annotations = list(
      x = last_week_sums$weeklytrackchart..attr.user,
      y = last_week_sums$total_playcount,
      showarrow = FALSE,
      font = list(size = 12),
      textposition = "outside"
    ))
    
    # Return the bar chart
    p
  })
  
  # Render bubble chart
  output$bubble_chart <- renderPlotly({
    user <- user_albums[user_albums$user == input$user_selection, ]
    
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

