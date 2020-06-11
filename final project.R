library(shiny)
library(dplyr)
library(tidyverse)
library(httr)
library(jsonlite)
library(styler)
library(leaflet)
library(shinyWidgets)
####table generation do not change code below 
readRenviron(".Renviron")
queryCity <- function(location_city) {
  r <- GET(
    "https://api.yelp.com/v3/businesses/search",
    add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN"))),
    query = list(
      location = location_city,
      term ="Boba"
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  thisTable <- fromJSON(json, flatten = TRUE)$businesses %>% select(id, name, location.city, rating, review_count, name, price, url, image_url, coordinates.latitude, coordinates.longitude) 
  return(thisTable)
}

CollgeTownList <- c("Berkeley","Los Angeles", "Irvine", "Merced", "Riverside", "San Diego", "San Francisco", "Santa Barbara", "Santa Cruz")
newCollgeTownList <- c("Davis", "Berkeley","Los Angeles", "Irvine", "Merced", "Riverside", "San Diego", "San Francisco", "Santa Barbara", "Santa Cruz")
Totaltable <- queryCity("Davis")
for (i in CollgeTownList){
  tableGot <- queryCity(i)
  Totaltable <- Totaltable %>% rbind(tableGot)
}
#####do not change code above


#build the app
ui <- fluidPage(
  titlePanel("Which Boba Shop To Go"),
  setBackgroundImage(
    src = "https://techcrunch.com/wp-content/uploads/2017/04/gettyimages-487304467.jpg"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("cityname", "Where I go to college", choices = newCollgeTownList),
      uiOutput("option1control"),
      uiOutput("option2control")
    ),
    mainPanel(
      h1("Overview of boba shops in 10 different UC college towns"),
      plotOutput("dotplot"),
      h1("Details of my college town"),
      plotOutput("histo"),
      h1("Boba Shop Winner- which one should I go"),
      h4("Please select option 1 and option 2 boba shops"),
      #textOutput("summary"),
      span(h3(textOutput("summary")), style="color:grey"),
      h1("Where is my boba shop located"),
      leafletOutput("location"),
      h1("Meme Time!"),
      h4("Enter information in option 1 and option 2 boba shops and then you can see this meme!"),
      htmlOutput("picture")
      #h1("Click here to see a meme!"),
      #actionButton("button", "Click Me!")
    )
  )
)

server <- function(input, output) {
  output$option1control <- renderUI({
    currdata <-  Totaltable %>%  filter(location.city == input$cityname)
    currdata <- currdata$name
    selectInput("opt1", "Option 1 Boba Shop", c("-", currdata %>% unique() %>% sort()))
  })
  output$option2control <- renderUI({
    req(input$opt1)
    currdata <- Totaltable %>%  filter(location.city == input$cityname & name != input$opt1)
    currdata <- currdata$name
    selectInput("opt2", "Option 2 Boba Shop", c("-", currdata %>% unique() %>% sort()))
  })
  
  ####part1
  output$dotplot <- renderPlot({
    Totaltable %>%
      filter(location.city %in% newCollgeTownList) %>% 
      ggplot(aes(x = location.city, y = review_count)) + 
      geom_point(size = 2, alpha = 0.5) +  
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      labs(title = "Popularity of Boba Shops in Ten UC College Towns and City Nearby", x = "College Town", y = "Review Counts") 
  })
  ####part1
  
  ####part2
  output$histo <- renderPlot({
    req(input$cityname)
    if (!is.null(input$cityname)) {
     result <- Totaltable %>% filter(location.city == input$cityname) %>% select(review_count, name)
    }
    req(result)
    if (!is.null(result) && nrow(result) > 0) {
      result %>%
        ggplot(aes(x = name, y = review_count)) +
        geom_point() +
        labs(title = paste("Distribution of Across College Town", input$cityname), x = "Boba Shop Name", Y = "Review Counts") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust=1)) + #Changes x-axis tick labels 90 degrees
        coord_flip()
    }
  })
  ####part2
  
  ####part 3
  output$summary <- renderText({
    req(input$cityname)
    req(input$opt1)
    req(input$opt2)
    if (!is.null(input$opt1) && !is.null(input$opt2) && input$opt1 != "-" && input$opt2 != "-") {
      boba1review <-  Totaltable%>% filter(name == input$opt1)
      boba2review <- Totaltable %>% filter(name == input$opt2)
      if (boba1review$review_count > boba2review$review_count && boba1review$rating > boba2review$rating) {
        toReturn <- boba1review$name
      }else {
        toReturn <- boba2review$name
      }
    }
  })
  ###part3
  
  ####part4 demostrate where the selected boba shop is in the map 
  output$location <- renderLeaflet({
    req(input$cityname)
    req(input$opt1)
    req(input$opt2)
    if (!is.null(input$opt1) && !is.null(input$opt2) && input$opt1 != "-" && input$opt2 != "-") {
      boba1review <-  Totaltable%>% filter(name == input$opt1)
      boba2review <- Totaltable %>% filter(name == input$opt2)
      if (boba1review$review_count > boba2review$review_count) {
        boba1review %>% leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>%
          addMarkers(~coordinates.longitude, ~coordinates.latitude)
      }else {
        boba2review %>% leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>%
          addMarkers(~coordinates.longitude, ~coordinates.latitude)
      }
    }
  })
  ####part4
  
  ####last part A meme
    src1 = "https://image.shutterstock.com/image-vector/top-secret-stamp-260nw-208531021.jpg"
    ##secret image
    src2 = "https://i.redd.it/73c1qbeab2221.jpg"
    ##meme image 
    output$picture<-renderText({
      if (!is.null(input$opt1) && !is.null(input$opt2) && input$opt1 != "-" && input$opt2 != "-") {
        src = src2}
      else{src=src1}
          c('<img src="',src,'">')})
  ####
}
#lauch the app
shinyApp(ui, server)
