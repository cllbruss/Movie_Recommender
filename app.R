library(shiny)
library(arules)

load("MOVIERECSpring2022.RData")

POPULARITY <- POPULARITY[POPULARITY$percentSeen * 100 >= 0.1, ]
ALL_MOVIE_TITLES <- sort(unique(POPULARITY$title))

ui <- fluidPage(
  titlePanel("Hidden Gem Movie Recommender"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_movies",
        label = "Select your favorite movies!",
        choices = ALL_MOVIE_TITLES,
        multiple = TRUE,
      ),
      
      numericInput(
        inputId = "number_of_recs", 
        label = "How Many Recommendations?", 
        value = 10),
      
      numericInput(
        inputId = "max_popularity", 
        label = "Maximum % of users who have rated the movie?", 
        value = 5),
      
      sliderInput(
        inputId = "min_confidence", 
        label = "Minimum Level of Confidence", 
        min = 0, 
        max = 1, 
        value = .25),
      
      radioButtons(
        inputId = "selected_sort", 
        label = "Sort By",
        choices = list("Confidence" = 1, "Popularity" = 2, "Rating" = 3, "Title" = 4), 
        selected = 1),
      
      submitButton(
        text = "Recommend Hidden Gems!",
        icon = icon("film")
      )
      
    ),
    mainPanel(
      tableOutput("movie_rec_table")
    )
  )
)

server <- function(input, output, session) {
  
  output$movie_rec_table <- renderTable({
    RECS <- NULL
    if (isTruthy(input$selected_movies)) {
      # Rule out too popular movies early on
      too_popular <- POPULARITY$title[which(100 * POPULARITY$percentSeen > input$max_popularity)]
      
      # Keep popular movies that the user input
      too_popular <- setdiff(too_popular, input$selected_movies)
      
      min_support <- 4
      max_time <- 0
      
      RULES <- apriori(
        TRANS,
        parameter = list(
          supp = min_support / length(TRANS),
          conf = input$min_confidence,
          minlen = 2,
          maxtime = max_time
        ),
        appearance = list(
          none = too_popular,
          lhs = input$selected_movies,
          default = "rhs"
        ),
        control = list(
          verbose = FALSE
        )
      )
      
      if (length(RULES) > 0) {
        RULES <- RULES[!is.redundant(RULES)]
        RULES <- RULES[is.significant(RULES, TRANS)]
        
        RULESDF <- DATAFRAME(RULES, itemSep = " + ", setStart = "", setEnd = "")
        names(RULESDF)[1:2] <- c("BasedOn", "title")
        
        # Remove recs that the user gave as input
        RULESDF <- RULESDF[!(RULESDF$title %in% input$selected_movies), ]
        if (nrow(RULESDF) > 0) {
          RECS <- aggregate(confidence ~ title, data = RULESDF, FUN = max)
          
          RECS <- merge(RECS, POPULARITY, by = "title")
          
          RECS$item_id <- NULL
          RECS$countSeen <- NULL
          RECS$Year <- NULL
          names(RECS) <- c("Movie", "Confidence", "PercentSeen", "imdbRating")
          
          
          # Order the recommendations by confidence
          RECS <- RECS[order(RECS$Confidence, decreasing = TRUE), ]
          RECS <- head(RECS, input$number_of_recs)
          
          RECS$Movie <- as.character(RECS$Movie)
          
          # Take out confusing row names
          row.names(RECS) <- NULL
          
          RECS$Confidence <- round(RECS$Confidence * 100, 2)
          RECS$PercentSeen <- round(RECS$PercentSeen * 100, 2)
          
          if(input$selected_sort == 1) {RECS <- RECS[order(RECS$Confidence, decreasing = TRUE), ]}
          if(input$selected_sort == 2) {RECS <- RECS[order(RECS$PercentSeen, decreasing = TRUE), ]} 
          if(input$selected_sort == 3) {RECS <- RECS[order(RECS$imdbRating, decreasing = TRUE), ]}
          if(input$selected_sort == 4) {RECS <- RECS[order(RECS$Movie, decreasing = FALSE), ]}
        }
      }
    }
    
    if (is.null(RECS)) {
      RECS <- data.frame(
        Error = "No recommendations with these parameters.  Add more movies, decrease confidence, or increase popularity!"
      )
    }
    
    RECS
    
  })
    
 
}

shinyApp(ui, server)