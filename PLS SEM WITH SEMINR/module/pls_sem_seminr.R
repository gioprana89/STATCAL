


########################################
########UI (User Interface)#############
########################################

pls_sem_seminr_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    #HTML('<center><img src="ugi.png" width="800px"></center>'),

    #h3("UPLOAD DATA & PEMILIHAN VARIABEL",
      # style="color:red; text-align:center"),
    
   # br(),
   # br(),
   # br(),

   
    
    
   # h2("Upload Data", style="
   # font-family: 'cursive';
   # color: #ad1d28;
   # text-align:center
   # "),
    
    
    
    
    
    sidebarLayout(
      
     
      sidebarPanel(
        
        h4("Upload Data Anda", style="
         font-family: 'cursive';
         color: blue;
         text-align:center
         "),
        
        
        
        fileInput(ns("ambil_file_data"), "Choose .txt/.csv/.xlsx File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xlsx")
                  
        ),
        
        
        
        radioButtons(ns("pemisah_variabel"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 xlsx = "xlsx"),
                     selected = ",", inline = TRUE)
        
        
      ),
      
      
      
      
      mainPanel(
        
        
        uiOutput(ns("data_anda")),
        
        br(),
        
        DT::DTOutput(ns("dataku")),
        
        br(),
        br(),
        
        
        
        uiOutput(ns("pilih_variabel")),
        
        
        
        br(),
        
        uiOutput(ns("pemilihan_variabel_checkboxGroupInput")),
        
        br(),
        
        DT::DTOutput(ns("data_yang_dianalisis")),
        
        
        
        
        
      ) #Akhir main panel
      
      
      
    ),
    
    
    
    
    
    fluidRow(
      
      column(4,
             
             
             textAreaInput(ns("awal_indikator"), 
                           "Dari", 
                           value = "1\n1\n1\n1", 
                           height = 250, width = 250),
             
             br()
             
      ),
      
      
      column(4,
             
             
             
             textAreaInput(ns("akhir_indikator"), 
                           "Ke", 
                           value = "3\n4\n3\n4", 
                           height = 250, width = 250),
             
             
             br()
             
      ),
      
      
      column(4,
             
             
             
             
             textAreaInput(ns("akhir_indikator"), 
                           "Ke", 
                           value = "MOT\nLK\nDK\nKK", 
                           height = 250, width = 250),
             
             
             
             
             br()
             
      )
      
      
    ), #Akhir fluidrow
    
    
    verbatimTextOutput(ns("hasil1")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    br()
    
  ) #akhir dari fluidpage
  
  
} #Akhir dari UI

















































########################################
################Server##################
########################################



pls_sem_seminr_server <- function(input, output, session) {
  

  
  kirim_data <- reactive({
    
    
    ambil_file_data <- input$ambil_file_data
    
    if(is.null(ambil_file_data))
      return(NULL)
    
    
    
    ##Keren juga ya, output$data_anda bisa diletak di reactive
    output$data_anda <- renderUI({
      
      
      
      h2("Data Anda", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    ")
      
      
    })
    
    
    ##############
    
    output$pilih_variabel <- renderUI({
      
      
      
      h2("Pilih Variabel", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    ")
      
      
    })
    
    
    
    ################
    
    
    
    
    output$pemilihan_variabel_checkboxGroupInput <- renderUI({
      
      
      
      checkboxGroupInput(session$ns("terpilih_checkboxGroupInput"), label="Pilih Variabel yang Ingin Dianalisis:", choices = c( kirim_nama()), selected=c())
      
      
      
      
    })
    
    
    ###############
    
    
    pemisah_variabel = input$pemisah_variabel
    
    
    
    #######Memproses Jika Data yang Diinput .xlsx
    
    if(pemisah_variabel == 'xlsx')
    {
      
      inFile <- input$ambil_file_data
      
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      data <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      return(data)
      
    }
    
    
    #Memproses Jika Data yang Diupload .txt atau .csv
    if(pemisah_variabel != 'xlsx')
    {
      
      data <- read.csv(ambil_file_data$datapath, sep = input$pemisah_variabel)
      
      
      return(data)
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  #########Mencetak Data yang Telah Diupload
  
  output$dataku <- DT::renderDT({
    
    
    
    
    
    kirim_data()
    
  })
  
  
  
  
  
  
  kirim_nama <- function()
  {
    
    dat <- kirim_data()
    
    nama <- colnames(dat)
    
    return(nama)
    
  }
  
  
  
  
  
  ###########
  ###########
  
  
  output$data_yang_dianalisis <- DT::renderDT({
    
    
    
    dat <- kirim_data()
    
    variabel_terpilih <- input$terpilih_checkboxGroupInput
    
    dat <- dat[c(variabel_terpilih)]
    
    print(dat)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############Racikan
  
  
  output$hasil1 <- renderPrint({ 
    
    
    cat(sprintf("Halo"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  


} #akhir dari server











