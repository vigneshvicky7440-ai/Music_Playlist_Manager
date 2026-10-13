library(shiny)

# Define User Interface
ui <- fluidPage(
  titlePanel("🎵 Enhanced Music Playlist Manager Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("Playlist Controls"),
      
      # CSV Upload Section
      fileInput("file1", "Upload Custom CSV File", accept = c(".csv")),
      helpText("Note: CSV must contain 'Title', 'Artist', and 'Genre' columns."),
      hr(),
      
      # Add New Song Form
      h4("➕ Add a New Song"),
      textInput("new_title", "Song Title:", value = "Perfect"),
      textInput("new_artist", "Artist:", value = "Ed Sheeran"),
      selectInput("new_genre", "Genre:", 
                  choices = c("Pop", "Rock", "Hip-Hop", "Jazz", "Classical", "R&B", "Electronic", "Other"), 
                  selected = "Pop"),
      actionButton("add_btn", "Add Song to Playlist", class = "btn-primary"),
      hr(),
      
      # Filter Control
      selectInput("filter_genre", "Filter Playlist by Genre:", choices = c("All"), selected = "All"),
      hr(),
      helpText("Built with R Shiny | Single-File Architecture")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Playlist Dashboard", 
                 br(),
                 uiOutput("summaryMetrics"),
                 br(),
                 h4("Current Songs"),
                 tableOutput("playlistTable")
        ),
        tabPanel("Playlist Structure (List View)", 
                 br(),
                 h4("List Structure & Elements"),
                 verbatimTextOutput("listStructureOutput")
        ),
        tabPanel("Quick Lookups", 
                 br(),
                 h4("Track Element Lookups"),
                 verbatimTextOutput("lookupOutput")
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive list storing the current playlist state
  playlistData <- reactiveVal({
    # Built-in fallback sample playlist
    list(
      Titles = c("Shape of You", "Blinding Lights", "Bohemian Rhapsody"),
      Artists = c("Ed Sheeran", "The Weeknd", "Queen"),
      Genres = c("Pop", "Pop", "Rock")
    )
  })
  
  # Handle CSV File Upload
  observeEvent(input$file1, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
    if (all(c("Title", "Artist", "Genre") %in% names(df))) {
      playlistData(list(
        Titles = df$Title,
        Artists = df$Artist,
        Genres = df$Genre
      ))
    } else {
      showNotification("CSV must contain 'Title', 'Artist', and 'Genre' columns.", type = "error")
    }
  })
  
  # Handle Adding New Songs Dynamic Addition
  observeEvent(input$add_btn, {
    req(input$new_title, input$new_artist, input$new_genre)
    
    current <- playlistData()
    updated <- list(
      Titles = c(current$Titles, input$new_title),
      Artists = c(current$Artists, input$new_artist),
      Genres = c(current$Genres, input$new_genre)
    )
    
    playlistData(updated)
    
    # Update UI Genre Filter Choices dynamically
    updateSelectInput(session, "filter_genre",
                      choices = c("All", unique(updated$Genres)),
                      selected = input$filter_genre)
    
    # Clear input fields
    updateTextInput(session, "new_title", value = "")
    updateTextInput(session, "new_artist", value = "")
    
    showNotification("Song added successfully!", type = "message")
  })
  
  # Keep UI genre selection updated when playlist changes
  observe({
    pl <- playlistData()
    updateSelectInput(session, "filter_genre",
                      choices = c("All", unique(pl$Genres)),
                      selected = input$filter_genre)
  })
  
  # Helper to get active tabular data with filtering applied
  filteredData <- reactive({
    pl <- playlistData()
    df <- data.frame(
      ID = seq_along(pl$Titles),
      Title = pl$Titles,
      Artist = pl$Artists,
      Genre = pl$Genres,
      stringsAsFactors = FALSE
    )
    
    if (input$filter_genre != "All") {
      df <- df[df$Genre == input$filter_genre, ]
    }
    
    df
  })
  
  # Render Metric Cards
  output$summaryMetrics <- renderUI({
    pl <- playlistData()
    total_songs <- length(pl$Titles)
    total_artists <- length(unique(pl$Artists))
    total_genres <- length(unique(pl$Genres))
    
    tagList(
      fluidRow(
        column(4, wellPanel(h4("Total Songs"), h3(total_songs))),
        column(4, wellPanel(h4("Unique Artists"), h3(total_artists))),
        column(4, wellPanel(h4("Unique Genres"), h3(total_genres)))
      )
    )
  })
  
  # Render Structured Data Table
  output$playlistTable <- renderTable({
    filteredData()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Render Raw R List Output (Preserves Original List Functionality)
  output$listStructureOutput <- renderPrint({
    playlistData()
  })
  
  # Render Quick Lookups (Targeted element access)
  output$lookupOutput <- renderText({
    pl <- playlistData()
    second_song <- if (length(pl$Titles) >= 2) pl$Titles[2] else "N/A"
    third_artist <- if (length(pl$Artists) >= 3) pl$Artists[3] else "N/A"
    
    paste0(
      "🔍 Element Index Extractions:\n\n",
      "• Second Song Title: ", second_song, "\n",
      "• Third Artist:      ", third_artist, "\n",
      "• Total Songs Count: ", length(pl$Titles)
    )
  })
}

# Run the Application
shinyApp(ui = ui, server = server)