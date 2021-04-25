#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(scales)
library(leaflet.providers)
library(mapview)
library(tigris)
library(DT)
library(ggplot2)


e <- read.csv("energy-usage-2010.csv")
names(e)[names(e) == "TOTAL.KWH"] <- "Electricity"
names(e)[names(e) == "TOTAL.THERMS"] <- "Gas"
names(e)[names(e) == "AVERAGE.BUILDING.AGE"] <- "Building_Age"
names(e)[names(e) == "AVERAGE.STORIES"] <- "Building_Height"
names(e)[names(e) == "BUILDING.TYPE"] <- "Building_Type"
names(e)[names(e) == "TOTAL.POPULATION"] <- "Population"
cook <- blocks(state = "IL", county = "Cook", year = 2010)
cook_tract <- tracts(state = "IL", county = "Cook", year = 2010 )
print("Have got the goods")


names(e)[names(e) == "KWH.JANUARY.2010"] <- "JANUARY_Electricity"
names(e)[names(e) == "KWH.FEBRUARY.2010"] <- "FEBRUARY_Electricity"
names(e)[names(e) == "KWH.MARCH.2010"] <- "MARCH_Electricity"
names(e)[names(e) == "KWH.APRIL.2010"] <- "APRIL_Electricity"
names(e)[names(e) == "KWH.MAY.2010"] <- "MAY_Electricity"
names(e)[names(e) == "KWH.JUNE.2010"] <- "JUNE_Electricity"
names(e)[names(e) == "KWH.JULY.2010"] <- "JULY_Electricity"
names(e)[names(e) == "KWH.AUGUST.2010"] <- "AUGUST_Electricity"
names(e)[names(e) == "KWH.SEPTEMBER.2010"] <- "SEPTEMBER_Electricity"
names(e)[names(e) == "KWH.OCTOBER.2010"] <- "OCTOBER_Electricity"
names(e)[names(e) == "KWH.NOVEMBER.2010"] <- "NOVEMBER_Electricity"
names(e)[names(e) == "KWH.DECEMBER.2010"] <- "DECEMBER_Electricity"

names(e)[names(e) == "THERM.JANUARY.2010"] <- "JANUARY_Gas"
names(e)[names(e) == "THERM.FEBRUARY.2010"] <- "FEBRUARY_Gas"
names(e)[names(e) == "THERM.MARCH.2010"] <- "MARCH_Gas"
names(e)[names(e) == "THERM.APRIL.2010"] <- "APRIL_Gas"
names(e)[names(e) == "THERM.MAY.2010"] <- "MAY_Gas"
names(e)[names(e) == "THERM.JUNE.2010"] <- "JUNE_Gas"
names(e)[names(e) == "THERM.JULY.2010"] <- "JULY_Gas"
names(e)[names(e) == "THERM.AUGUST.2010"] <- "AUGUST_Gas"
names(e)[names(e) == "THERM.SEPTEMBER.2010"] <- "SEPTEMBER_Gas"
names(e)[names(e) == "THERM.OCTOBER.2010"] <- "OCTOBER_Gas"
names(e)[names(e) == "THERM.NOVEMBER.2010"] <- "NOVEMBER_Gas"
names(e)[names(e) == "THERM.DECEMBER.2010"] <- "DECEMBER_Gas"
#View(e)

chicago <- subset(cook, cook$GEOID10 %in% e$CENSUS.BLOCK)

#View(cook)
#View(cook_tract)
b <- "Electricity"
months <- c("ALL","JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")


near_west <- subset(e, e$COMMUNITY.AREA.NAME == "Near West Side")
#View(near_west)
print("hello read in and got them blocks")

n_w_map <- subset(cook, cook$GEOID10 %in% near_west$CENSUS.BLOCK)
#View(n_w_map)
names(near_west)[names(near_west) == "CENSUS.BLOCK"] <- "GEOID10"
m_t <- merge(n_w_map, near_west, by = "GEOID10")
nw_table <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
community_areas <- c("Chicago (Tract)",unique(e$COMMUNITY.AREA.NAME))
#View(m_t)
#print(str(m_t))
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
        title = "Energy Exploration"
    ),
    dashboardSidebar(
        radioButtons("radio_building","Building Types",choices = c("ALL","Commercial","Industrial","Residential"), selected = c("ALL"), inline = TRUE)
    ),
    dashboardBody(
        tabsetPanel(
            type = "tabs",
            tabPanel("Part 1 Map",
                     leafletOutput("p1_near_west"),
                     radioButtons("radio_type","Types to Explore",choices = c("Electricity","Gas","Building_Age","Building_Type","Building_Height","Population"), selected = c("Electricity"), inline = TRUE),
                     radioButtons("radio_month","Months",choices = months, selected = c("ALL"), inline = TRUE)
                     
                     
                     
                     ),
            tabPanel("Part 1 Tables and Graphs",
                     dataTableOutput("p1_table"),
                     splitLayout(plotOutput("p1_graph_e"),
                                 plotOutput("p1_graph_g"))
                     ),
            tabPanel("Part 2",
                     h4("Select Either Blocks or Tract"),
                     radioButtons("b_or_c", "Select",choices = c("Blocks","Tract"), inline = TRUE),
                     fluidRow(column(6, 
                                     leafletOutput("p2_map1"),
                                     selectInput("p2_ca1", "Select Community Area for map 1", choices = community_areas, selected = "Near West Side"),
                                     radioButtons("radio_type_map1","Types to Explore",choices = c("Electricity","Gas","Building_Age","Building_Type","Building_Height","Population"), selected = c("Electricity"), inline = TRUE),
                                     radioButtons("radio_month_map1","Months",choices = months, selected = c("ALL"), inline = TRUE)
                                     
                                     ), 
                              column(6,
                                     leafletOutput("p2_map2"),
                                     selectInput("p2_ca2", "Select Community Area for map 2", choices = community_areas, selected = "Loop"),
                                     radioButtons("radio_type_map2","Types to Explore",choices = c("Electricity","Gas","Building_Age","Building_Type","Building_Height","Population"), selected = c("Electricity"), inline = TRUE),
                                     radioButtons("radio_month_map2","Months",choices = months, selected = c("ALL"), inline = TRUE)
                                     )
                              )
                     
                     ),
                    
            tabPanel("Part 3")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    p1_type <- reactive({
        print("Aite fam we got to here")
        print(input$radio_type)
        print(input$radio_month)
        if(input$radio_type == "Electricity" && input$radio_month != "ALL" ){
            b <- paste(input$radio_month,"_Electricity", sep = "")
            print(b)
            return(b)
        }
        if(input$radio_type == "Gas" && input$radio_month != "ALL" ){
            b <- paste(input$radio_month,"_Gas", sep = "")
            print(b)
            return(b)
        }
        
        b <- input$radio_type
        print(b)
        b
    })
    
    p1_building <- reactive({
        if(input$radio_building != "ALL"){
            m <- subset(m_t, m_t$Building_Type == input$radio_building)
        }
        else{
            return(m_t)
        }
    })
    
    output$p1_near_west <- renderLeaflet({
        b <- p1_type()
        Near_West <- p1_building()
        #View(Near_West)
        m <- mapview(Near_West, zcol = b)
        m@map
    })
    
    output$p1_table <- DT::renderDataTable({
        n_w <- p1_building()
        nn <- as.data.frame(n_w)
    #View(nw_table)
        elec_num <- as.list(mapply(sum, nn[21:32], na.rm = TRUE))
        gas_num <- as.list(mapply(sum, nn[36:47], na.rm = TRUE))
        nw_table <- do.call(rbind.data.frame, Map('c', elec_num, gas_num))
        #row.names(nw_table) <- months
        #View(nw_table)
        colnames(nw_table)[1] <- "Electricity"
        colnames(nw_table)[2] <- "Gas"
        #nw_table$month <- months
        datatable( nw_table, rownames = FALSE )
        
    })
    
    output$p1_graph_e <- renderPlot({
        n_w <- p1_building()
        nn <- as.data.frame(n_w)
        #View(nw_table)
        elec_num <- as.list(mapply(sum, nn[21:32], na.rm = TRUE))
        gas_num <- as.list(mapply(sum, nn[36:47], na.rm = TRUE))
        nw_table <- do.call(rbind.data.frame, Map('c', elec_num, gas_num))
        #row.names(nw_table) <- months
        #View(nw_table)
        colnames(nw_table)[1] <- "Electricity"
        colnames(nw_table)[2] <- "Gas"
        nw_table$month <- month.name
        #nw_table$month <- factor(nw_table$month, levels = months)
        ggplot(nw_table, aes(y=Electricity, x=month)) + 
            geom_bar( stat="identity") + scale_y_continuous(name="Electricity", labels = scales::comma)
    })
    
    output$p1_graph_g <- renderPlot({
        n_w <- p1_building()
        nn <- as.data.frame(n_w)
        View(nn)
        elec_num <- as.list(mapply(sum, nn[21:32], na.rm = TRUE))
        gas_num <- as.list(mapply(sum, nn[36:47], na.rm = TRUE))
        nw_table <- do.call(rbind.data.frame, Map('c', elec_num, gas_num))
        #row.names(nw_table) <- month.name
        #View(nw_table)
        colnames(nw_table)[1] <- "Electricity"
        colnames(nw_table)[2] <- "Gas"
        nw_table$month <- month.name
        #nw_table$month <- factor(nw_table$month, levels = months)
        ggplot(nw_table, aes(y=Gas, x=month)) + 
            geom_bar( stat="identity")+ scale_y_continuous(name="Gas", labels = scales::comma)
    })
    
    p2_map1_reactive <- reactive({
        if(input$p2_ca1 != "Chicago (Tract)"){
            c_a <- subset(e, e$COMMUNITY.AREA.NAME == input$p2_ca1)
            n_w_map <- subset(cook, cook$GEOID10 %in% c_a$CENSUS.BLOCK)
            names(c_a)[names(c_a) == "CENSUS.BLOCK"] <- "GEOID10"
            p2m_t <- merge(n_w_map, c_a, by = "GEOID10")
            return(p2m_t)
        }
        else{
            tract <- subset(cook_tract, cook_tract$GEOID10 %in% e$CENSUS.BLOCK)
            #View(tract)
            names(e)[names(e) == "CENSUS.BLOCK"] <- "GEOID10"
            p2m_t <- merge(tract, e, by = "GEOID10")
            return(p2m_t)
        }
    })
    
    p2_map2_reactive <- reactive({
        c_a <- subset(e, e$COMMUNITY.AREA.NAME == input$p2_ca2)
        n_w_map <- subset(cook, cook$GEOID10 %in% c_a$CENSUS.BLOCK)
        names(c_a)[names(c_a) == "CENSUS.BLOCK"] <- "GEOID10"
        p2m_t <- merge(n_w_map, c_a, by = "GEOID10")
        p2m_t
    })
    
    
    
    
    p2_type_map1 <- reactive({
        
        if(input$radio_type_map1 == "Electricity" && input$radio_month_map1 != "ALL" ){
            b <- paste(input$radio_month_map1,"_Electricity", sep = "")
            print(b)
            return(b)
        }
        if(input$radio_type_map1 == "Gas" && input$radio_month_map1 != "ALL" ){
            b <- paste(input$radio_month_map1,"_Gas", sep = "")
            print(b)
            return(b)
        }
        
        b <- input$radio_type_map1
        b
    })
    
    p2_building_map1 <- reactive({
        if(input$radio_building != "ALL"){
            m <- subset(m_t, m_t$Building_Type == input$radio_building)
        }
        else{
            return(m_t)
        }
    })
    p2_type_map2 <- reactive({
        
        if(input$radio_type_map2 == "Electricity" && input$radio_month_map2 != "ALL" ){
            b <- paste(input$radio_month_map2,"_Electricity", sep = "")
            print(b)
            return(b)
        }
        if(input$radio_type_map2 == "Gas" && input$radio_month_map2 != "ALL" ){
            b <- paste(input$radio_month_map2,"_Gas", sep = "")
            print(b)
            return(b)
        }
        
        b <- input$radio_type_map2
        b
    })
    
    p2_building_map2 <- reactive({
        if(input$radio_building != "ALL"){
            m <- subset(m_t, m_t$Building_Type == input$radio_building)
        }
        else{
            return(m_t)
        }
    })
    
    
    output$p2_map1 <- renderLeaflet({
        b <- p2_type_map1()
        info <- p2_map1_reactive()
        mapview(info, zcol = b)@map
    })
    
    
    
    output$p2_map2 <- renderLeaflet({
        b <- p2_type_map2()
        info <- p2_map2_reactive()
        mapview(info, zcol = b)@map
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
