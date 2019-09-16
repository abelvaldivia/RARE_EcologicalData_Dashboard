## RARE Ecological Data Dashboard ##

#Load required libraries

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(sf)
library(ggvis)
library(grid)
library(gridExtra)
library(lme4)
library(car)
library(data.world)
library(tools)
library(Hmisc)
library(tidyr)
library(scales)


### Load and clean data from dataworld ####
  ## Fish Surveys
  fish.surveys <- read.csv("https://download.data.world/s/e6kfpzmhsezfa55f33cadtrwd5a5xn", 
                     header= TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")

  fish.surveys$locationstatus <- recode_factor(fish.surveys$locationstatus, "ma" = "Outside \nReserve",
                                               "reserve"  = "Inside \nReserve")
  levels(fish.surveys$ma_name)
  
## Remove test sites
  fish.surveys <- droplevels(subset(fish.surveys, locationname != "Test"))

  
### Remove fish density over 10000 ind per ha
  
  #fish.surveys <- subset (fish.surveys, density_ind_ha <10000)
  

### Recode size range for Indonesia
   fish.surveys$sizeclass <- recode_factor(factor(fish.surveys$sizeclass), "<10" = "0-10")
   fish.surveys$sizeclass <- recode_factor(factor(fish.surveys$sizeclass),
                                           "0-5" = "0-5", "0-10"="0-10", "6-10"= "6-10",
                                            "10-20" = "11-20", "20-30"="21-30", "30-40" = "31-40",
                                            "40-50" = "41-50", "50-60" = "51-60")
   levels(fish.surveys$sizeclass)
  
  ## FISH BIOMASS
  ##Agregate Fish biomass data transect
  fish.biomass <- aggregate(biomass_kg_ha ~  country_full + community + level1_name + level2_name + ma_name+
                                      reefzone + reefslope + locationname + 
                                      locationstatus  + transectnumber,
                                    data = fish.surveys, FUN = sum)
  
  ## Fish biomass per survey location
  fish.biomass.mean <- aggregate(biomass_kg_ha ~  country_full + community + level1_name + level2_name + ma_name +
                                       reefzone + reefslope + locationname+locationstatus,
                                     data = fish.biomass, FUN = mean)
 
  ### FISH SIZE STRUCTURE 
  
  ### Agregate Fish data transect for density
  ## by species
  fish.density <- aggregate(density_ind_ha ~  country_full + community + level1_name + level2_name + ma_name+
                                      reefzone + reefslope + locationname + locationstatus  + length +
                                      waterdepth + transectnumber + species + sizeclass + lmax,
                                    data = fish.surveys, FUN = sum)
  
  
  ##Sort species species with the most data 
  Common_species <- data.frame(Frequency=sort(summary(factor(fish.surveys$species)), decreasing = TRUE),
                               stringsAsFactors = TRUE)
  ## Add row names as column
  Common_species <- tibble::rownames_to_column(Common_species, "species")
  
  ## Extract the 20 most common species to plot size class structures
  Top_common_species <- Common_species[c(2:30),]
  
  ##Create file for plotting size structure
  fish.size <- droplevels(subset(fish.density, species %in% Top_common_species$species))
  
  
  ### FISH DIVERSITY 
  ### Determine unique number of species per location name
  numberofspecies <- function(x) { length(unique (x))}
  
  fish.diversity <- aggregate(species ~  country_full + community + level1_name + level2_name + ma_name+
                              locationname + locationstatus ,
                            data = fish.surveys, FUN = numberofspecies)


## DEFINE UI ###

fluidPage(theme = shinytheme("lumen"), titlePanel(strong(h2("Ecological Monitoring Data", 
                                                  style='color:#005BBB;padding-left: 15px'))), 
                 
                #sidebarLayout ( 
      fluidRow(
        column(2,
               wellPanel(
                 ## custumize tooltip text
                 tags$head(tags$style(HTML(' .tooltip .tooltiptext {
                              visibility: hidden;
                              width: 120px;
                              background-color: red;
                              color: #000000;
                              text-align: center;
                              border-radius: 6px;
                              padding: 5px 0;
                              position: absolute;
                              z-index: 1;
                              bottom: 100%;
                              left: 50%;
                              margin-left: -60px;
                              }'))),
                 #### Input Data ####
                 #Select the type of analysis to run
                 radioButtons(inputId = "Analysis", label = strong("Type of Analysis"), 
                                       choices = c("Reserve Performance", "Site Trends"),
                                       selected = "Reserve Performance"),
                  ## help text
                    bsTooltip(id="Analysis", title ="Select the type of analysis you would like to perform",
                          placement ="right", trigger = "hover", options = list(container = "body", font=2)),
                 #Select the type of plot to use
                 radioButtons(inputId = "plot_type", strong("Plot type"),
                                      choices = c("Bar plots", "Mean ± 95% CI"), 
                                        selected = "Bar plots", inline = FALSE),
                 ## help text
                    bsTooltip(id="plot_type", title ="Select the type of plot you would like to see",
                           placement ="right", trigger = "hover", options = list(container = "body")),
                 
                 #Select the type of y axis (free or fixed)
                 radioButtons(inputId = "y_axis", strong ("Y axis (free or fixed)"),
                                      choices = c("Free"='free_y', "Fixed"='fixed'), 
                              selected = "free_y", inline=TRUE),
                 ## help text
                   bsTooltip(id="y_axis", title ="Change the scale of the y axis to compare between panels",
                           placement ="right", trigger = "hover", options = list(container = "body")),
                 #Select the scale of the axis (normal or log)
                # radioButtons(inputId = "y_log", strong ("Y axis scale"),
                  #            choices = c("Normal"= 'scale_y_continuous()',"Log"= 'scale_y_log10()'), 
                 #             selected = 'scale_y_continuous()', inline=TRUE),
                 #Select the type of plot to use
                 radioButtons(inputId = 'grouping_level', strong("Analysis Level"),
                              choices = c("Country" = 'country_full', "Subnational Government" = 'level1_name',
                                          "Local Government"= "level2_name", "Managed Access"='ma_name'),
                              selected = "ma_name"),
                ## help text
                      bsTooltip(id="grouping_level", title ="Select a grouping level to calculate means and errors",
                             placement ="right", trigger = "hover", options = list(container = "body")),
                 
                               #Select the Year range of the analyses to determine fish biomass
                 #uiOutput(outputId = "Year"),   
                 
                #Select the Country to determine fish biomass
                 selectInput(inputId = "Country", label = strong("Country"),
                                choices = sort(c("Belize", "Honduras", "Mexico", "Brazil", 
                                                 "Mozambique", "Indonesia", "Philippines")),
                                                        selected = "Indonesia", multiple =TRUE, selectize = FALSE),
                     bsTooltip(id="Country", title ="Select all countries using Ctrl+A or Comm+A",
                            placement ="right", trigger = "hover", options = list(container = "body")),
                
                    #Select the Region/Province range of the analyses to determine fish biomass
                    uiOutput("Subnational_Government"),  
                          bsTooltip(id="Subnational_Government", title ="Select all subnational governments using Ctrl+A or Comm+A",
                                placement ="right", trigger = "hover", options = list(container = "body")),
                
                      #Select the District/Adminstration range of the analyses to determine fish biomass
                      uiOutput(outputId = 'Local_Government'),   
                            bsTooltip(id="Local_Government", title ="Select all local governments using Ctrl+A or Comm+A",
                                  placement ="right", trigger = "hover", options = list(container = "body")),
                
                        #Select the Municipality/Subdistrict range of the analyses to determine fish biomass
                        uiOutput(outputId = "Managed_Access"),  
                            bsTooltip(id="Managed_Access", title ="Select all managed access areas using Ctrl+A or Comm+A",
                                  placement ="right", trigger = "hover", options = list(container = "body")),
                            
                
                         #Select the Municipality/Subdistrict range of the analyses to determine fish biomass
                          #uiOutput(outputId = 'Community_Village'), 
                
                            #Select the Reef Zone to determine fish biomass
                            #uiOutput(outputId = "Reef.Zone"),
                
                            #Note about page reloading
                            strong(textOutput(outputId = "Note")),
                            textOutput(outputId ="Notetext")
                
                          )
                     ),
                  
        column(width=8,
                   mainPanel(width =14,
                      tabsetPanel(type = "tabs",
                                  
                       # Fish biomass tab
                      tabPanel("Fish Biomass",
                          textOutput(outputId = "Maptitle"),
                              # Download data button
                          
                          downloadLink("downloadData0", "Download this data", style="float:right"), 
                          
                           plotOutput(outputId = "plot_fish.biomass", height = "800px", width = "800px",
                                      hover = hoverOpts(id = "plot_hover")), # hover option
                           
                          # Map
                          imageOutput(outputId = "map", height = "20px", width = "400px"),
                         # textOutput(outputId = "summary_table"),
                          tableOutput(outputId = "summary_fish.biomass")),
                    
                      
                      # Fish density tab
                        tabPanel("Fish Density", verbatimTextOutput("fishdensity"),
                           downloadLink("downloadData1", "Download this data", style="float:right"),
                                 plotOutput(outputId = "plot_fish.density", height = "800px", width = "800px",
                                            hover = hoverOpts(id = "plot_hover"))),
                      # Fish size structure tab 
                        tabPanel("Fish Size Structure", verbatimTextOutput("fishsize"),
                           downloadLink("downloadData2", "Download this data", style="float:right"),
                                 plotOutput(outputId = "plot_fish.size", height = "800px", width = "800px",
                                            hover = hoverOpts(id = "plot_hover"))),
                      # Fish diversity tab
                        tabPanel("Fish Diversity", verbatimTextOutput("fishdiversity"),
                          downloadLink("downloadData3", "Download this data", style="float:right"),
                          plotOutput(outputId = "plot_fish.deversity", height = "800px", width = "800px",
                                     hover = hoverOpts(id = "plot_hover"))),
                       
                      # Reef Habitat cover tab
                        tabPanel("Habitat Cover", verbatimTextOutput("habitatcover"),
                          downloadLink("downloadData4", "Download this data", style="float:right"),
                                 textOutput(outputId = "habitatcovertitle")),
                      
                      # Reef Habitat diversity tab
                        tabPanel("Habitat Diversity", verbatimTextOutput("habitatdiversity"),
                          downloadLink("downloadData5", "Download this data", style="float:right"),
                                 textOutput(outputId = "habitatdiversitytitle")),
                      
                      # Create Report Tab
                        tabPanel(strong("Create Report",  style='color:#005BBB'), verbatimTextOutput("create_report"),
                              textOutput(outputId = "create_reporttitle"))
                      
                      )
                    )
                  ),
        
        column(width = 2,
               wellPanel(br(), strong(textOutput(outputId ="hover_text")), br(),
                         #strong(textOutput(outputId = "Legend")),br(),
                         htmlOutput("x_value"), br(),
                         #textOutput(outputId ="Legendtext"),
                         verbatimTextOutput("hover_info")
                         )
                        
                  )
                )
              )
            
             
          
             
       


