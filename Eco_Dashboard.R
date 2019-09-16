
# Create Shiny object
 shinyApp(ui = ui, server = server)
 

#library(rsconnect)
 #Set WD Directory for MAC
  #setwd("/Users/abelvaldivia/Dropbox/Collaborations/RARE/Fish Forever M&E/ME Framework and Metrics/Eco_DataPortal")
#Set WD for DELL 
  setwd("C:/Users/avaldivia/Dropbox/Collaborations/RARE/Fish Forever M&E/ME Framework and Metrics/Eco_DataPortal")
 
   dir()
    library(rsconnect)
      rsconnect::setAccountInfo(name='abelvaldivia', token='5B3896627F68B3FD51DC17C5D5FB77D4', secret='/eFEcQKMN6aHhaNNxAjdF+3p9xT6G+4fRkY5lGkd')
        
       runApp()

        ### Publish to server
      deployApp()
     




      
  
      
 