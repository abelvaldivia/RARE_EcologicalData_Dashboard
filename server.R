## SHINY SERVER CODE


### Load and clean data from dataworld ####
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



#### ~ Create Shiny Server ~ ####
  
  shinyServer(function(input, output, session) {
  
  ### Create nested selection based on inputs
    
  #output$Density_plots <- renderUI({
    
  #  if (tabPanel == "Fish size structure") {
  
 #  radioButtons(inputId = "plot_type", strong("Plot type"),
  #               choices = c("Bar plots", "Density plots"), 
  #               selected = "Bar plots") 
  #  }
  #})
  
  ## Region/Province
  
  output$Subnational_Government <- renderUI({
    selectInput(inputId = "Subnational_Government", label= strong("Subnational Government"), 
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                       country_full %in% input$Country))$level1_name))),
                selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                      country_full %in% input$Country))$level1_name))),
                multiple = TRUE, selectize = FALSE) #, options = list(`actions-box` = TRUE))
        })

  ## District/Administration
  output$Local_Government <- renderUI({
    selectInput(inputId = 'Local_Government', label= strong("Local Government"), #selected = "No Fishing Zone",
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country_full %in% input$Country & 
                                   level1_name %in% input$Subnational_Government))$level2_name))),
                selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country_full %in% input$Country & 
                                   level1_name %in% input$Subnational_Government))$level2_name))),
                multiple = TRUE, selectize = FALSE) #, options = list(`actions-box` = TRUE))
         })
  
  ## Municipality/Subdistrict
  output$Managed_Access <- renderUI({
    selectInput(inputId = 'Managed_Access', label = strong("Proposed Managed Access"), #selected = "No Fishing Zone",
                choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                              country_full %in% input$Country & 
                                 level1_name %in% input$Subnational_Government &
                                  level2_name %in% input$Local_Government))$ma_name))),
                selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
                                country_full %in% input$Country & 
                                   level1_name %in% input$Subnational_Government &
                                    level2_name %in% input$Local_Government))$ma_name))),
                          multiple = TRUE, selectize = FALSE) #, options = list(`actions-box` = TRUE))
          })
  
  ## Community/Municipality
  #output$Community_Village <- renderUI({
   # selectInput(inputId = 'Community_Village', label= strong("Community/Village"), #selected = "No Fishing Zone",
    #            choices = as.vector(sort(unique(droplevels(subset(fish.surveys, 
     #                         country_full %in% input$Country & 
      #                            level1_name %in% input$Subnational_Government &
       #                            level2_name %in% input$Local_Government & 
        #                             ma_name %in% input$Municipality_Subdistrict))$level4_name))),
         #       selected = as.vector(sort(unique(droplevels(subset(fish.surveys, 
          #                      country_full %in% input$Country & 
           #                         level1_name %in% input$Subnational_Government &
            #                          level2_name %in% input$Local_Government & 
             #                           ma_name %in% input$Municipality_Subdistrict))$level4_name))),
              #              multiple = TRUE, selectize = FALSE) #, options = list(`actions-box` = TRUE))
          # })
  

  
  # Management Zone
  #output$Zone <- renderUI({
   # selectInput("Zone", strong("Management Zone:"), selected = "No Fishing Zone",
    #            choices = sort(unique(mar1[mar1$Country == input$Country, "Zone"])))
     #     })
  
  #MPA Name
  #output$MPA.Name <- renderUI({
   # selectInput("MPA.Name", strong("MPA Name:"), 
    #            selectize = FALSE, multiple = TRUE, selected = "Glover's Reef",
     #           choices = sort(unique(droplevels(subset(mar1, 
      #               (Country %in% input$Country & Zone %in% input$Zone)))$MPA.Name)))
        #})
  
  #Reef Zone
  #output$Reef.Zone <- renderUI({
   #selectInput("Reef.Zone", strong("Reef Zone:"), selectize = FALSE, multiple = TRUE,
    #           choices = unique(mar1[mar1$MPA.Name == input$MPA.Name, "Reef.Zone"]),
     #          selected = "Fore reef") 
    #})
  
  #Site Name
  #output$Site.Code <- renderUI({
  #selectInput("Site.Code", strong("Site Name:"), 
 #            choices = sort(unique(mar1[mar1$Reef.Zone == input$Reef.Zone, "Site.Code"]))) })
  
  
  #Select Year range to be plotted with sliders
   #output$Year <- renderUI({
    # sliderInput(inputId = "Year", strong("Year:"), round = T, step=1,
     #         min = 2005, max = 2019, ticks = T,
      #      value = range(c(2005:2019), na.rm = T),
       #    dragRange = F)
        #   })

  #Create new dataframe with selection inputs and outputs for plotting
  
  #selectedData <- reactive({
    
    #marCMPA <- droplevels(subset(mar1, Year %in% input$Year & Country %in% input$Country & 
     #                             Subnational_Government %in% input$Subnational_Government & 
      #                              Local_Government %in% input$Local_Government &
       #                               Municipality_Subdistrict %in% input$Municipality_Subdistrict &
        #                                Community_Village %in% input$Community_Village))
                                  
      #droplevels(subset(marCMPA, Year == max(Year) | Year == min(Year)))
  #})
  
  
  #Selected Fish biomass data
   selectedData_fish.biomass <- reactive ({ 
            droplevels(subset(fish.biomass, 
                          #Year %in% input$Year & 
                            country_full %in% input$Country & 
                              level1_name %in% input$Subnational_Government & 
                                level2_name %in% input$Local_Government &
                                   ma_name %in% input$Managed_Access))
   })
   
   
   #Selected Fish species data
   selectedData_fish.size <- reactive ({ 
         droplevels(subset(fish.size,
                       #Year %in% input$Year & 
                       country_full %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                              ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish density data
   selectedData_fish.density <- reactive ({ 
          droplevels(subset(fish.density,
                       #Year %in% input$Year & 
                       country_full %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access))
   })
   
   #Selected Fish diversity data
   selectedData_fish.diversity <- reactive ({ 
        droplevels(subset(fish.diversity,
                       #Year %in% input$Year & 
                       country_full %in% input$Country & 
                         level1_name %in% input$Subnational_Government & 
                            level2_name %in% input$Local_Government &
                                ma_name %in% input$Managed_Access))
   })
   
  
 #### Reserve performace ####
   ### Fish biomass
   
   
 output$plot_fish.biomass<- renderPlot({
     req(nrow(selectedData_fish.biomass())> 0)
     
     #if (input$Analysis == "Reserve Performace") {
     
  if (input$plot_type == "Mean ± 95% CI") {
  
 plot_fish.biomass.mci <- ggplot(selectedData_fish.biomass(), aes(locationstatus, biomass_kg_ha), na.rm = T) +
     theme_bw() + 
     facet_wrap(c(input$grouping_level), ncol=3, scale = input$y_axis)+
          geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
       stat_summary(aes(col=locationstatus), na.rm=TRUE,
                  fun.data = "mean_cl_normal", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
           axis.title.y = element_text(size=14),
           axis.text  = element_text(size=14),
           strip.text = element_text(size=14),
           legend.position = "none",
           legend.title = element_blank())+
     #scale_y_continuous(breaks = seq(0,1000, 100))+
      ggtitle("\nFISH BIOMASS ")+
        scale_colour_manual (values = c("#F58233", "#00AFD8"))+
        #input$y_log +
       #scale_x_log10()
       xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
    
    plot_fish.biomass.mci 
    
    }
       
   
    else if (input$plot_type == "Bar plots") {
      
      #Function for summary tables
      summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                            conf.interval=.95, .drop=TRUE) {
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
          if (na.rm) sum(!is.na(x))
          else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           SD   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
        )
        
        # Rename the "mean" column    
        datac <- plyr::rename(datac, c("mean" = measurevar))
        
        datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval: 
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$CI <- datac$SE * ciMult
        
        return(datac)
      }
      
      
      summary_data <- summarySE(data=selectedData_fish.biomass(), measurevar = "biomass_kg_ha",
                                groupvars= c("country_full", input$grouping_level, "locationstatus"))
                                  
      
      plot_fish.biomass.bar <- ggplot(summary_data, aes(locationstatus, biomass_kg_ha), na.rm=TRUE) +
        theme_bw() + 
        facet_wrap(c(input$grouping_level), ncol=3, scale = input$y_axis)+
          geom_bar(aes(fill = locationstatus), position=position_dodge(), stat = "identity",
                   width = .5, colour="black", size=.3)+
          geom_errorbar(aes(ymin=biomass_kg_ha-CI, ymax = biomass_kg_ha + CI),
           #stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
                     size = .5, position=position_dodge(width=.2),
                     na.rm=TRUE, width=.2) +
          #geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
              axis.title.y = element_text(size=14),
              axis.text  = element_text(size=14),
              strip.text = element_text(size=14),
              legend.position = "none",
              legend.title = element_blank())+
        #scale_y_continuous(breaks = seq(0,1000, 100))+
        scale_fill_manual (values = c("#F58233", "#00AFD8"))+
        ggtitle("\nFISH BIOMASS ")+
        #scale_y_log10()+
        xlab ("") + ylab ("Fish biomass (kg/ha)") #+ coord_flip(clip="on")
      
      plot_fish.biomass.bar
      
            }
     })
   
   
   #Function for summary tables
   summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE) {
     # New version of length which can handle NA's: if na.rm==T, don't count them
     length2 <- function (x, na.rm=FALSE) {
       if (na.rm) sum(!is.na(x))
       else       length(x)
     }
     
     # This does the summary. For each group's data frame, return a vector with
     # N, mean, and sd
     datac <- plyr::ddply(data, groupvars, .drop=.drop,
                    .fun = function(xx, col) {
                      c(N    = length2(xx[[col]], na.rm=na.rm),
                        mean = mean   (xx[[col]], na.rm=na.rm),
                        SD   = sd     (xx[[col]], na.rm=na.rm)
                      )
                    },
                    measurevar
     )
     
     # Rename the "mean" column    
     datac <- plyr::rename(datac, c("mean" = measurevar))
     
     datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
     
     # Confidence interval multiplier for standard error
     # Calculate t-statistic for confidence interval: 
     # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
     ciMult <- qt(conf.interval/2 + .5, datac$N-1)
     datac$CI <- datac$SE * ciMult
     
     return(datac)
   }
   
   #Summary table for fish biomass
   summary_fish_biomass <- reactive({summarySE(selectedData_fish.biomass(), measurevar = "biomass_kg_ha",
                                            groupvars= c("country_full", input$grouping_level,
                                                         "locationstatus"))})
    
   output$summary_fish_biomas <- renderTable ({ summary_fish_biomass() })
    
   output$downloadData0 <- downloadHandler(
      filename = function() {
          paste0("summary_fish_biomass_",input$Country, "_",input$grouping_level, ".csv", sep="")
              },  
          content = function(file) {
              write.csv(summary_fish_biomass(), file, row.names = FALSE)
          }
      )
     

   #### FISH DENSITY ####
   
   output$plot_fish.density<- renderPlot({
     req(nrow(selectedData_fish.density())> 0)
     
     #if (input$Analysis == "Reserve Performace") {
     
     if (input$plot_type == "Mean ± 95% CI") {
       
       plot_fish.density.mci <- ggplot(selectedData_fish.density(), aes(locationstatus, density_ind_ha), na.rm = T) +
         theme_bw() + 
         facet_wrap(c(input$grouping_level), ncol=3, scale = "free_y")+
         stat_summary(aes(col=locationstatus), na.rm=TRUE,
                      fun.data = "mean_cl_normal", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         #geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
               axis.title.y = element_text(size=14),
               axis.text  = element_text(size=14),
               strip.text = element_text(size=14),
               legend.position = "none",
               legend.title = element_blank())+
         #scale_y_continuous(breaks = seq(0,1000, 100))+
         ggtitle("\nFISH DENSITY ")+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         #scale_y_log10()+
         xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
       
       plot_fish.density.mci
       
     }
     
     else if (input$plot_type == "Bar plots") {
       
       #Function for summary tables
       summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                             conf.interval=.95, .drop=TRUE) {
         # New version of length which can handle NA's: if na.rm==T, don't count them
         length2 <- function (x, na.rm=FALSE) {
           if (na.rm) sum(!is.na(x))
           else       length(x)
         }
         # This does the summary. For each group's data frame, return a vector with
         # N, mean, sd, and sum
         datac <- plyr::ddply(data, groupvars, .drop=.drop,
                        .fun = function(xx, col) {
                          c(N    = length2(xx[[col]], na.rm=na.rm),
                            mean = mean   (xx[[col]], na.rm=na.rm),
                            SD   = sd     (xx[[col]], na.rm=na.rm)
                            )
                        },
                        measurevar
         )
         
         # Rename the "mean" column    
         datac <- plyr::rename(datac, c("mean" = measurevar))
         
         datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
         
         # Confidence interval multiplier for standard error
         # Calculate t-statistic for confidence interval: 
         # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
         ciMult <- qt(conf.interval/2 + .5, datac$N-1)
         datac$CI <- datac$SE * ciMult
         
         return(datac)
         
       }
       
  
       # Summary table
       
       summary_data <- summarySE(selectedData_fish.density(), measurevar = "density_ind_ha",
                                 groupvars= c("country_full", input$grouping_level,
                                              "locationstatus"))
       head(summary_data)
       
       
       plot_fish.density.bar <- ggplot(summary_data, aes(locationstatus, density_ind_ha), na.rm=TRUE) +
         theme_bw() + 
         facet_wrap(c(input$grouping_level), ncol=3, scale = "free_y")+
         geom_bar(aes(fill = locationstatus), position=position_dodge(), stat = "identity",
                  width = 0.5, colour="black", size=.3)+
         geom_errorbar(aes(ymin=density_ind_ha-CI, ymax = density_ind_ha + CI),
                       size = .5, position=position_dodge(width=.2),
                       na.rm=TRUE, width = .2) +
         #geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
               axis.title.y = element_text(size=14),
               axis.text  = element_text(size=14),
               strip.text = element_text(size=14),
               legend.position = "none",
               legend.title = element_blank())+
         #scale_y_continuous(breaks = seq(0,1000, 100))+
         scale_fill_manual (values = c("#F58233", "#00AFD8"))+
         ggtitle("\nFISH DENSITY ")+
         #scale_y_log10()+
         xlab ("") + ylab ("Fish density (individuals / ha)") #+ coord_flip(clip="on")
       
       plot_fish.density.bar
       
     }
   })
   
   #Summary table for fish density
   summary_fish_density <- reactive({ summarySE(selectedData_fish.density(), measurevar = "density_ind_ha",
                                                groupvars= c("country_full", input$grouping_level,
                                                             "locationstatus")) })
   
   output$summary_fish_density <- renderTable ({ summary_fish_density() })
   
   output$downloadData1 <- downloadHandler(
     filename = function() {
       paste("summary_fish_density_",input$Country, "_",input$grouping_level, ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_fish_density(), file, row.names = FALSE)
     }
   )
   
   
   #### FISH SIZE  ####
   #Check species abundances 
   
   output$plot_fish.size<- renderPlot({
     
     req(nrow(selectedData_fish.size())> 0)
     
     if (input$plot_type == "Bar plots") {
       
      plot_fish.size <- ggplot(selectedData_fish.size(), aes(sizeclass), na.rm=T)+ theme_bw()+
         facet_wrap (~species, ncol=3, scale= "free_y")+
         geom_bar(aes(fill=locationstatus), position = position_dodge(preserve = "single"),
                  width = .6, colour="black", size=.3) +
         #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
         xlab("Size class (cm)")+ ylab("Counts")+
         ggtitle("\nSIZE STRUCTURE FOR MOST COMMON SPECIES")+
         theme(legend.position = "bottom",
               legend.title = element_blank(),
               legend.text = element_text(size=14),
               panel.grid = element_blank(),
               strip.text = element_text(face = "italic", size=14),
               plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
               axis.title.y = element_text(size=14),axis.title.x = element_text(size=14),
               axis.text  = element_text(size=14))+
        scale_fill_manual (values = c("#F58233", "#00AFD8"))
 
      plot_fish.size
      
     }
     
     else if (input$plot_type == "Mean ± 95% CI") {
       
   ## Fish Size Structure density
     
     plot_fish.size <- ggplot(selectedData_fish.size(), aes(length), na.rm=T)+ theme_bw()+
       facet_wrap (~species, ncol=3, scale= "free_y")+
       geom_density(aes(fill=locationstatus), colour="black", size=.3, alpha = 0.5) +
        geom_vline( aes (xintercept = lmax), col = 2, lty= 3)+
         geom_text (aes(label="L-max", x=lmax+2, y = 0, hjust=-0.3), angle = 90, parse = TRUE, col = "grey50")+
       #scale_y_continuous(breaks=pretty_breaks(u5.bias = .5)) +
       scale_x_continuous(limits = c(0,50)) +
       xlab("Length (cm)")+ ylab("Density")+
       ggtitle("\nLENGTH FOR MOST ABUNDANT FISH SPECIES")+
       theme(legend.position = "bottom",
             legend.title = element_blank(),
             legend.text = element_text(size=14),
             panel.grid = element_blank(),
             strip.text = element_text(face = "italic", size=14),
             plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
             axis.title.y = element_text(size=14),axis.title.x = element_text(size=14),
             axis.text  = element_text(size=14))+
       scale_fill_manual (values = c("#F58233", "#00AFD8"))
     
     plot_fish.size
     
     }
   
   })

  ### FISH DIVERSITY
    
   output$plot_fish.deversity <- renderPlot({
     
     req(nrow(selectedData_fish.diversity())> 0)
     
     #if (input$Analysis == "Reserve Performace") {
     
     if (input$plot_type == "Mean ± 95% CI") {
       
       plot_fish.diversity <- ggplot(selectedData_fish.diversity(), aes(locationstatus, species), na.rm = T) +
         theme_bw() + 
         facet_wrap(c(input$grouping_level), ncol=3)+
         stat_summary(aes(col=locationstatus), na.rm=TRUE,
                      fun.data = "mean_cl_normal", geom = "pointrange", size = .7, position=position_dodge(width=.5)) +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
               axis.title.y = element_text(size=14),
               axis.text  = element_text(size=14),
               strip.text = element_text(size=14),
               legend.position = "none",
               legend.title = element_blank())+
         scale_y_continuous(breaks = pretty_breaks(n=5))+
         ggtitle("\nFISH DIVERSITY ")+
         scale_colour_manual (values = c("#F58233", "#00AFD8"))+
         #scale_y_log10()+
         xlab ("") + ylab ("Number of fish species") #+ coord_flip(clip="on")
       
       plot_fish.diversity
       
     }
     
     else if (input$plot_type == "Bar plots") {
       
       #Function for summary tables
       summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                             conf.interval=.95, .drop=TRUE) {
         # New version of length which can handle NA's: if na.rm==T, don't count them
         length2 <- function (x, na.rm=FALSE) {
           if (na.rm) sum(!is.na(x))
           else       length(x)
         }
         # This does the summary. For each group's data frame, return a vector with
         # N, mean, sd, and sum
         datac <- plyr::ddply(data, groupvars, .drop=.drop,
                        .fun = function(xx, col) {
                          c(N    = length2(xx[[col]], na.rm=na.rm),
                            mean = mean   (xx[[col]], na.rm=na.rm),
                            SD   = sd     (xx[[col]], na.rm=na.rm)
                          )
                        },
                        measurevar
         )
         
         # Rename the "mean" column    
         datac <- plyr::rename(datac, c("mean" = measurevar))
         
         datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
         
         # Confidence interval multiplier for standard error
         # Calculate t-statistic for confidence interval: 
         # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
         ciMult <- qt(conf.interval/2 + .5, datac$N-1)
         datac$CI <- datac$SE * ciMult
         
         return(datac)
       }
         
         summary_data1 <- summarySE(selectedData_fish.diversity(), measurevar = "species",
                                    groupvars= c("country_full", input$grouping_level,
                                                 "locationstatus"))
        
         plot_fish.diversity.bar <- ggplot(summary_data1, aes(locationstatus, species), na.rm=TRUE) +
           theme_bw() + 
           facet_wrap(c(input$grouping_level), ncol=3)+
           geom_bar(aes(fill = locationstatus), position=position_dodge(), stat = "identity",
                    width = 0.5, colour="black", size=.3)+
           geom_errorbar(aes(ymin=species-CI, ymax = species + CI),
                         size = .5, position=position_dodge(width=.2),
                         na.rm=TRUE, width = .2) +
           #geom_jitter(alpha=0.1, width = 0.05, height = 0, size =2) + 
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=14),
                 axis.text  = element_text(size=14),
                 strip.text = element_text(size=14),
                 legend.position = "none",
                 legend.title = element_blank())+
           #scale_y_continuous(breaks = seq(0,1000, 100))+
           scale_fill_manual (values = c("#F58233", "#00AFD8"))+
           ggtitle("\nFISH DIVERSITY ")+
           #scale_y_log10()+
           xlab ("") + ylab ("Number of fish species") #+ coord_flip(clip="on")
         
         plot_fish.diversity.bar
       }
   })
   
   
   #Summary table for fish density
   summary_fish_diversity <- reactive({ summarySE(selectedData_fish.diversity(), measurevar = "species",
                                                  groupvars= c("country_full", input$grouping_level,
                                                               "locationstatus")) })
   
   output$summary_fish_diversity <- renderTable ({ summary_fish_diversity() })
   
   output$downloadData3 <- downloadHandler(
     filename = function() {
       paste("summary_fish_diversity_",input$Country, "_",input$grouping_level, ".csv", sep="")
     },  
     content = function(file) {
       write.csv(summary_fish_diversity(), file, row.names = FALSE)
     }
   )
  
   
  ## Sites trends ####
   ## Fish biomass
   ### Create a boxplot  ####
   output$plot1 <- renderPlot({
     
     par(mfrow = c(2,3), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
     
     # IF BOXPLOT IS  CHOSEN ###
     if (input$Analysis == "Site trends") {
       
       if (input$plot_type == "Bar plots") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         boxplot(TF/10~Year, selectedData(), main = "TOTAL FISH", ylab=NULL, outline=T, lty=1,
                 col=c("lightblue"), las=1)
         points(factor(selectedData()$Year), (selectedData()$TF/10), col = "darkgrey")
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         
         boxplot(HHRI/10~Year, selectedData(), main = "HERBIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) 
         points(factor(selectedData()$Year), jitter(selectedData()$HHRI/10), col = "darkgrey")
         
         boxplot(I/10~Year, selectedData(), main = "INVERTIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) 
         points(factor(selectedData()$Year), jitter(selectedData()$I/10), col = "darkgrey")
         
         boxplot(P/10~Year, selectedData(), main = "PISCIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$P/10), col = "darkgrey")
         
         boxplot(CSHRI/10~Year, selectedData(), main = "COMMERCIAL", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$CSHRI/10), col = "darkgrey")
         
         boxplot(PARR/10~Year, selectedData(), main = "PARROTFISH", ylab="biomass (kg/ha)", outline=F,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$PARR/10), col = "darkgrey")
         
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         boxplot(SEAB/10~Year, selectedData(), main = "GROUPERS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$SEAB/10), col = "darkgrey")
         
         boxplot(SNAP/10~Year, selectedData(), main = "SNAPPERS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) # names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$SNAP/10), col = "darkgrey")
         
         boxplot(JACK/10~Year, selectedData(), main = "JACKS", ylab=NULL, outline=T,
                 lty=1, col=c("lightblue"), las=1) #, names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$JACK/10), col = "darkgrey")
         
         boxplot(GRUN/10~Year, selectedData(), main = "GRUNTS", ylab=NULL, outline=T, 
                 lty=1, col=c("lightblue"), las=1) #names = c("current", "predicted"))
         points(factor(selectedData()$Year), jitter(selectedData()$GRUN/10), col = "darkgrey")
         
       } 
       
       
       ## IF MEAN and SE is selected ##
       else if (input$plot_type == "Mean ± 95% CI") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         p1 <- ggplot(selectedData(), aes(factor(Year), selectedData()$TF/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p1a <- ggplot(selectedData_IDN(), aes(factor(Year), log(selectedData_IDN()$biomass_kg_ha+1))) + 
           #facet_wrap(~inside_or_outside_NTZ)+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         geom_boxplot()+ theme_bw() +
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           geom_jitter(width=0.05,height=0.05, alpha = 0.5)
         
         p2 <- ggplot(selectedData(), aes(factor(Year), selectedData()$HHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("HERBIVOROUS") + xlab("") + ylab(NULL)
         
         p3 <- ggplot(selectedData(), aes(factor(Year), selectedData()$I/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("INVERTIVOROUS") + xlab("") + ylab(NULL)
         
         p4 <- ggplot(selectedData(), aes(factor(Year), selectedData()$P/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("PISCIVOROUS") + xlab("") + ylab(NULL)
         
         p5 <- ggplot(selectedData(), aes(factor(Year), selectedData()$CSHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("COMMERCIAL") + xlab("") + ylab(NULL)
         
         p6 <- ggplot(selectedData(), aes(factor(Year), selectedData()$PARR/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12)) +
           ggtitle("PARROTFISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p7 <- ggplot(selectedData(), aes(factor(Year), selectedData()$SEAB/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GROUPERS") + xlab("") + ylab(NULL)
         
         p8 <- ggplot(selectedData(), aes(factor(Year), selectedData()$SNAP/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("SNAPPERS") + xlab("") + ylab(NULL)
         
         p9 <- ggplot(selectedData(), aes(factor(Year), selectedData()$JACK/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("JACKS") + xlab("") + ylab(NULL)
         
         p10 <- ggplot(selectedData(), aes(factor(Year), selectedData()$GRU/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           stat_summary(fun.data = "mean_se", geom = "pointrange", 
                        size = .5, position=position_dodge(width=.5)) +
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GRUNTS") + xlab("") + ylab(NULL)
         
         grid.arrange(p1, p1a, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2, ncol=5)
         
       }
       
       else if (input$plot_type == "Trend lines") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         p1 <- ggplot(selectedData(), aes((Year), selectedData()$TF/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12))+
           ggtitle("TOTAL FISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p2 <- ggplot(selectedData(), aes((Year), selectedData()$HHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("HERBIVOROUS") + xlab("") + ylab(NULL)
         
         p3 <- ggplot(selectedData(), aes( (Year), selectedData()$I/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("INVERTIVOROUS") + xlab("") + ylab(NULL)
         
         p4 <- ggplot(selectedData(), aes( (Year), selectedData()$P/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("PISCIVOROUS") + xlab("") + ylab(NULL)
         
         p5 <- ggplot(selectedData(), aes( (Year), selectedData()$CSHRI/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("COMMERCIAL") + xlab("") + ylab(NULL)
         
         p6 <- ggplot(selectedData(), aes( (Year), selectedData()$PARR/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.title.y = element_text(size=12),
                 axis.text  = element_text(size=12)) +
           ggtitle("PARROTFISH") + xlab("") + ylab("biomass (kg/ha)")
         
         p7 <- ggplot(selectedData(), aes( (Year), selectedData()$SEAB/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GROUPERS") + xlab("") + ylab(NULL)
         
         p8 <- ggplot(selectedData(), aes( (Year), selectedData()$SNAP/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
                 axis.text  = element_text(size=12)) +
           ggtitle("SNAPPERS") + xlab("") + ylab(NULL)
         
         p9 <- ggplot(selectedData(), aes( (Year), selectedData()$JACK/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("JACKS") + xlab("") + ylab(NULL)
         
         p10 <- ggplot(selectedData(), aes( (Year), selectedData()$GRU/10) ) + theme_bw()+
           #geom_jitter(shape=1, width = .1, alpha=.2) +
           geom_smooth()+
           theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust=0.5, face = 'bold', size =14),
                 axis.text  = element_text(size=12)) +
           ggtitle("GRUNTS") + xlab("") + ylab(NULL)
         
         grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2, ncol=5)
         
       }
       
     }
     
     else if (input$Analysis == "Site Trends") {
       
       if (input$plot_type == "Bar plots") {
         
         par(mfrow = c(2,5), mar=c(3,2,2,1), oma=c(1,2,1,1), cex=1)
         
         boxplot(TF/10~Zone, mar1, main = "TOTAL FISH", ylab=NULL, outline=T, lty=1,
                 col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "GU", "Reserve"))
         points(factor(selectedData()$Zone), (selectedData()$TF/10), col = "darkgrey")
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         
         boxplot(HHRI/10~Zone, selectedData(), main = "HERBIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$HHRI/10), col = "darkgrey")
         
         boxplot(I/10~Zone, selectedData(), main = "INVERTIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$I/10), col = "darkgrey")
         
         boxplot(P/10~Zone, selectedData(), main = "PISCIVOROUS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$P/10), col = "darkgrey")
         
         boxplot(CSHRI/10~Zone, selectedData(), main = "COMMERCIAL", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$CSHRI/10), col = "darkgrey")
         
         boxplot(PARR/10~Zone, selectedData(), main = "PARROTFISH", ylab="biomass (kg/ha)", outline=F,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$PARR/10), col = "darkgrey")
         
         mtext("biomass (kg/ha)", side = 2, line = 3, cex= 1.2)
         boxplot(SEAB/10~Zone, selectedData(), main = "GROUPERS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), 
                 las=1, names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$SEAB/10), col = "darkgrey")
         
         boxplot(SNAP/10~Zone, selectedData(), main = "SNAPPERS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$SNAP/10), col = "darkgrey")
         
         boxplot(JACK/10~Zone, selectedData(), main = "JACKS", ylab=NULL, outline=T,
                 lty=1, col=c("darkorange","white", "lightblue"), las=1,
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$JACK/10), col = "darkgrey")
         
         boxplot(GRUN/10~Zone, selectedData(), main = "GRUNTS", ylab=NULL, outline=T, 
                 lty=1, col=c("darkorange","white", "lightblue"), las=1, 
                 names=c("Fished", "General Use", "Reserve"))
         points(factor(selectedData()$Zone), jitter(selectedData()$GRUN/10), col = "darkgrey")
         
       }
     }
     
   }) 
   
   
  ## If all MPAs are selected, compare by MPAs
  
  #boxplot(TF/10 ~ MPA.Name, selectedData1())
  
  #ggplot(selectedData(), aes(input$Site.Code, input$Fishgroup)) + #facet_wrap(~Zone, ncol=4) + 
  # geom_boxplot(aes(group=input$Site.Code), notch=FALSE, outlier.shape=NA) +
  #  #geom_jitter(aes(col=Habitat), alpha=0.4) +
  #  theme_bw() + ylab ("Fish Biomass (kg/ha)") + 
  #  scale_y_continuous(limits = c(0, 4000), breaks = seq(0,4000, 500)) #+
  #  #scale_y_log10(breaks = c(10,25,50,100,250,500,1000,2000,3000, 5000))+
  #  #geom_boxplot(aes(Year, exp(TF.pred.nohuman), group=Year), fill="lightblue", outlier.shape = NA)
  
  
  ### Plot fish biomass by MPAs and countries ####
  
  #par(mar= c(0,15,1,1), oma = c(3,7,1,1))
  #nf <- layout(matrix(c(1,2,3,4),ncol=1), widths=10, heights=c(3,4.5,0.7,2), TRUE);
  #layout.show(nf)
  #maryear <-droplevels(subset(mar, Year > 2005))
  #for (i in c("Mexico", "Belize", "Guatemala", "Honduras")) {
  #boxplot(TF/10 ~ MPA.Name, droplevels(subset(mar, Country == i)),
  #         log="x", las= 1, horizontal=T, ylim= c(8,5000), lty =1, xaxt= "n")
  
  # axis(1, at=c(10,30,100,300, 1000,2500,5000), labels=NA)
  #  mtext(i, side =3, adj=0.95, line=-1.5)
  #}
  #mtext("fish biomass (kg/ha)", side=1, adj=0.5, line=3, cex=0.9)
  #axis(1, at=c(10,30,100,300, 1000,2500,5000), labels=c(10,30,100,300, 1000,2500,5000))
  
  
  #output$Maptitle <- renderText({c("Country:",input$Country," | ", "MPA:", input$MPA.Name, " | ",
  #                                 "Management:", input$Zone, " | ", "Reef Zone:", input$Reef.Zone) 
   #                                })
   
  #output$Legend <- renderText("Legend")
  #output$Legendtext <-renderText("Average ± 95% Confidence Intervals")
  output$Note <- renderText("PLEASE NOTE:") 
  output$Notetext <- renderText("This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page")
  output$habitatcovertitle <- renderText (c("Loading habitat cover..."))
  output$habitatdiversitytitle <- renderText (c("Loading habitat diversity..."))
  output$create_reporttitle <- renderText(c("A report will be automacally created here after selecting 'Create Report' in one of the main tab panels"))
  output$hover_text <- renderText(c("Hover over the graph to show specific values"))
  
  
  ### Hover to show mean and CI for each X value 
  
  output$x_value <- renderText({
    if (is.null(input$plot_hover$x)) return("")
    else {
      lvls <- c("Outside Reserve", "Inside Reserve")
      name <- lvls[round(input$plot_hover$x)]
      HTML("You've selected <code>", name, "</code>",
           "<br>Here is the biomass values that ",
           "match that category:")
    }
  })
  
  output$hover_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return(" ")
      paste0(round(e$y, 1), "\n")
    }
    
    # xy_mean_str <- function(e) {
    #  if(is.null(e)) return("")
    # paste0(round(e$ymin, 1), round(e$ymax, 1))
    #}
    paste0("Biomass:", xy_str(input$plot_hover), "kg/ha")#,
    #"Biomass Mean:", xy_mean_str(input$plot_hover), "kg/ha")
    
  })
  
  ## Add popover with each Metric Info
  addPopover(session, id="plot_fish.biomass", title="Fish Biomass (kg/ha)", placement = "bottom",
             content = paste0("<p> This is where fish biomass is calculated ",
             " by selecting different options from the left column </p>"), trigger = 'hover')

  
  #Read shapefile with MPA boundaries
  
  #MPAbound <- st_read("/Users/abelvaldivia/Dropbox/Collaborations/RARE/MAR Data/MAR Dispersal model/No takes/no-takes-MAR.shp")
  
  ## Show Map of the MPAs and sites selected
  #output$map <- renderPlot({
   # qmplot(Lon, Lat,  data = selectedData(), maptype = "satellite", source = "google", 
  #         extent = "panel", zoom ="auto", scale = "auto") + geom_point(col="yellow")
    #geom_sf(aes(MPAbound)) + coord_sf()
 # })
  
})


