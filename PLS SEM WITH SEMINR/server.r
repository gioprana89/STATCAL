



server <- function(input, output) {
  
  

  
  
  output$plssem_seminr <- renderUI({
    
    
    
    source("module//pls_sem_seminr.R")
    callModule(module = pls_sem_seminr_server, id = "pls_sem_seminr")
    pls_sem_seminr_ui(id = "pls_sem_seminr")
    
      
  
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server