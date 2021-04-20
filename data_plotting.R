################################################################################
# Authos:       James Brueckheimer
# Project:      MB2 - Wetland-detection
# Date:         20. Apr. 2021 
# Description:  This Code serves to plot the data calculated in 
#               data_manipulation.R. Aim of it is to show flood frequency for a
#               predefined area in Bangladesh south-east to Vermara Bazar.
#
#               It plots a simply Time Series Chart using
#               ggplot2, a simple RasterPlot, showing a Map with flood frequency
#               And a small shiny App, where the user can select a specific
#               RasterStack layer by selecting a day over a slide panel. So if 
#               feel like it... you can run this one
#
#               !!!RUN THIS ONE!!!


#Used packages (Install if you havent alreaddy)
# install.packages("raster");
# install.packages("ggplot2");
# install.packages("RColorBrewer");
# install.packages("shiny");
# install.packages("shinydashboard");
# install.packages("leaflet");


library(raster)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(leaflet)


#loading data
df_wetland <- read.csv2("./Data/MB2_Imagery/df_wetland.csv")
View(df_wetland)
df_wetland$Date <- as.Date(df_wetland$Date)
wetland_stack <- stack("./Data/MB2_Imagery/2010-2020_indices/2010-2020_fitted_indices/2010-2020_fitted_mask_wetland.tif")
View(wetland_stack)


#plot timeseries of flooded area over date
ggplot(df_wetland,aes(x = Date, y = Area_covered, group=1)) +
  geom_area(colour = "blue",fill="blue", size=1)+
  labs(title="Flooded Area TimeSeries with fitted values", x="Date", 
       y="Area flooded in percentage") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 13)) +
  ylim(0 , 40) +
  scale_x_date(name = "Date")

#Plot map of flood frequency during the investigation period by summing pixel
#values (0 or 1) and deviding it by the total amount of pixel (equals 
#length(dates)) times 100
area_sum <- (sum(wetland_stack)/length(df_wetland$Date)) * 100
plot(area_sum,
     breaks = c(0,20,40,60,80,100),
     col = brewer.pal(6, "Blues"),
     main = "Flood frequency between 2010 and 2020")



#creating small shiny App to display a specific wetland_stack Raster for a 
#specific date given.
ui <- fluidPage(
  titlePanel("Monthly flooded area"),
  
  sidebarLayout(
    
    #creating a sidebarPanel as a slider, to select specific day of the
    #investigation period
    sidebarPanel(
      
      sliderInput(inputId = "bins", 
                  label ="Month of Year", 
                  min =1, 
                  max = length(df_wetland$Date), 
                  value = 1)
    ),
    
    #mainPanel showing the selected Raster
    mainPanel(
      
      leafletOutput(outputId = "Raster")
    )
    
  )
)

#define Server attributes and its output
server <- function(input, output){
  
  output$Raster <- renderLeaflet({
    #get wetland_stack layer matching selected date
    r <- wetland_stack[[input$bins]]
    #setting all values below 1 to NA (for transperency)
    r[r<1] <- NA
    #define leaflet
    leaflet() %>% 
      #add basemap as underlying foundation
      addTiles() %>%
      #add selected Raster
      addRasterImage(r, colors = colorBin("blue", 
                     values(r), na.color = "transparent") )
  })
  
}

#Start shiny App
shinyApp(ui, server)

