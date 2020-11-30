# Shiny App for Nutritional Advantage Capstone Group 2020
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library (shiny)
library (ggplot2)
library (maps)
library (tidyr)
library (shinythemes)

setwd ("/Users/sadiefermelia/Documents/Classes_2020/Capstone_MICR_4321/R_Analyses/shiny_app")


#read in data:
#extracting wyoming county data from map data
counties.wy <- map_data("county", region="wyoming")

#read in covid data downloaded from https://datahub.io/core/covid-19
covid.us <- read.csv("us_simplified_csv.csv") 
#subset wyoming counties and get rid of extras
covid.wy <- covid.us[covid.us$Province.State=="Wyoming",]
covid.wy <- covid.wy[covid.wy$Admin2!="Unassigned" & covid.wy$Admin2!="Out of WY",]
#subset most recent dates
covid.wy.recent <- covid.wy [covid.wy$Date==max(covid.wy$Date),]
#change the county column so it matches the map data
colnames (covid.wy.recent)[2] <- "subregion"
covid.wy.recent$subregion <- tolower(covid.wy.recent$subregion)

#read in food data
food <- read.csv ("Food_data.csv")
#adjusting some names
#food$County <- gsub(" County", "", food$County)
#food$State <- gsub(" ", "", food$State)
#convert to wide form
#food <- spread (food, Variable_Code, Value)
#subset wyoming counties and get rid of extras
food.wy <- food[food$State=="WY" & food$County != "Total",]

#reading in data for label coordinates that I also added population data to because why not
point.data <- read.csv ("County_labels.csv")

#combining food, point, and covid data
combined.data <- cbind (point.data, covid.wy.recent, food.wy)
combined.data <- combined.data[,-(1:3)]
combined.data$Fatality_rate <- combined.data$Deaths/combined.data$Confirmed
#length (colnames (combined.data)) == length (unique (colnames (combined.data)))
#good, all unique


#merge covid and county data
#counties.plus.data <- left_join(counties.wy, covid.wy.recent, by="subregion")


data <- data.frame(subregion=combined.data$subregion, County=combined.data$County,
                   Long=combined.data$long, Lat=combined.data$lat)



# Define UI for application
ui <- fluidPage(navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,"Nutritional Advantage", id="nav",
            tabPanel("Mercedes' Project",

                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(width=5,
                                 selectInput("covid_var", #this tells us what to refer to this variable as later on
                                             label = "Select COVID-19 Variable",
                                             choices = c ("Confirmed Cases", "Deaths", 
                                                          "Cases per 100,000", "Deaths per 100,000",
                                                          "Fatality Rate")), #could also do these per population and/or add testing numbers
                                 selectInput("food_var", #this tells us what to refer to this variable as later on
                                             label = "Select Food Access Variable",
                                             choices = c("% Low access to stores", "SNAP Participation")), #add whatever else you are interested in
                                 selectInput("race_var",
                                             label = "Select Race",
                                             choices =c ("White", "Black", "Native American")), #can add more here too
                                 plotlyOutput("plot", width="100%", height = "300px"),
                                 em ("you could add a description of your plot here")
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(width=7,
                              plotlyOutput("map"),
                              em ("you could add a description of the map here")
                    )
                )
             ),
        tabPanel("Other Tab",
                 em ("This could be a place where another group member shows their work")
                 ),
        tabPanel("About",
                 em ("This could be a place where you write about your project. For Example:"),
                 h3 ("Introduction"),
                 p ("(paragraph text)"),
                 h3 ("Methods"),
                 p ("(paragraph text)"),
                 h3 ("etc...")
                 )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$map <- renderPlotly({
        data$covid <- switch (input$covid_var,
                              "Confirmed Cases" = combined.data$Confirmed,
                              "Deaths" = combined.data$Deaths, 
                              "Cases per 100,000"= combined.data$Confirmed/combined.data$Population*100000,
                              "Deaths per 100,000"= combined.data$Deaths/combined.data$Population*100000,
                              "Fatality Rate"= combined.data$Deaths/combined.data$Confirmed)
        data$food <- switch (input$food_var,
                             "% Low access to stores" = combined.data$PCT_LACCESS_POP15,
                             "SNAP Participation" = combined.data$SNAPS17)
        data$race <- switch (input$race_var,
                             "White" = combined.data$PCT_NHWHITE10,
                             "Black" = combined.data$PCT_NHBLACK10,
                             "Native American" = combined.data$PCT_NHNA10)
        
        counties.plus.data <- left_join(counties.wy, data, by="subregion")
        
        map <- ggplot() +
            #add counties as polygons and color by confirmed cases
            #color=black gives black borders
            geom_polygon(data=counties.plus.data, aes(x=long, y = lat, group = group, 
                                                      fill=covid, text=paste (input$covid_var, ":", covid)), 
                                                      color="black") +
            #Add points in each county scaled by population
            geom_point (data=data, aes(x=Long, y = Lat + 0.12, size=food, color=race,
                                       text= paste ("</br>", input$covid_var, ":", covid, 
                                                    "</br>", input$food_var, ":", food, 
                                                    "</br>%", input$race_var, ";", race))) +
            #adding labels
            geom_text(data=data, aes(x=Long, y = Lat - 0.06, label=County, 
                                     text=paste (County, "County")), size=2.5) +
            #Changing the color scheme to obnoxiously bright colors
            scale_fill_gradient (low="#FFEBEE", high="#FF0000", name=input$covid_var) +
            #changing legend labels
            scale_color_continuous(name=paste ("%", input$race_var)) +
            #getting rid of axis/labels
            theme_void() +
            #creating a title
            ggtitle("Map Title") +
            #centering the title
            theme(plot.title = element_text(hjust = 0.5))
        ggplotly (map, tooltip = "text")
        
    })
    
    output$plot <- renderPlotly({
        data$covid <- switch (input$covid_var,
                              "Confirmed Cases" = combined.data$Confirmed,
                              "Deaths" = combined.data$Deaths, 
                              "Cases per 100,000"= combined.data$Confirmed/combined.data$Population*100000,
                              "Deaths per 100,000"= combined.data$Deaths/combined.data$Population*100000,
                              "Fatality Rate"= combined.data$Deaths/combined.data$Confirmed)
        data$food <- switch (input$food_var,
                             "% Low access to stores" = combined.data$PCT_LACCESS_POP15,
                             "SNAP Participation" = combined.data$SNAPS17)
        data$race <- switch (input$race_var,
                             "White" = combined.data$PCT_NHWHITE10,
                             "Black" = combined.data$PCT_NHBLACK10,
                             "Native American" = combined.data$PCT_NHNA10)
        thing <- lm (covid~food, data=data)
        
        plot <- ggplot (data, aes (x=food, y=covid, color=race, 
                                   text= paste ("</br>", County, "County",
                                                "</br>", input$covid_var, ":", covid, 
                                                "</br> ", input$food_var, ":", food, 
                                                "</br> %", input$race_var, ";", race))) +
                    geom_point() +
                    labs (x=input$food_var, y=input$covid_var) +
                    scale_color_continuous(name=paste ("%", input$race_var)) +
                    theme_classic() +
                    if (summary (thing)$coefficients[2,4]<0.05) geom_smooth (method="lm", se=FALSE)
        ggplotly (plot, tooltip="text")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)