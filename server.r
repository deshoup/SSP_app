#Packages to be installed and loaded############################
library(shiny)
# library(profvis)Used for profiling to see errors and find slow steps in code
library(dplyr) 
library(tidyr)
library(tidyselect)
library(FSA)
library(nnet)#multinomial regression for ALK's
library(nlstools) #von Bert curv fitting 
library(tibble)
library(rlang) #is_empty() which catches NULL values, but also other forms of null like conditions not caught by is.null()
library(data.table) #used for fread, which is faster way to load .csv files
library(fst)#for loading fst saved files
library(ggplot2)
library(cowplot)
library(ggh4x) #provides better minor tick mark option for figures
library(grid) # used for convertWidth() and stringWidth() when calculating best location for spp name on figures
library(viridis) #used to extract color codes on ALK plot and adjust age text appropriately
library(shadowtext) #used to make species name label on ALK plot
# library(periscope)#trying this to make downloadable figures

#.csv's to read in#############################################
gearinfo <- read.fst("gearinfo.fst", as.data.table = TRUE)
lakeinfo <- read.fst("lakeinfo.fst", as.data.table = TRUE)  
  ###for debuging new age data...wanted to add data that clearly was not real so need lake name for this
    # addLake <- data.frame(lake.Name_Code="NOT A REAL DATA - NOTALAKE", Lake.Code="NOTALAKE",
    #                       Lake.Name="NOT REAL DATA",	Lake.System=NA,
    #                       ODWC.Region=NA,	Lake.Size=NA,	CTHFP=NA)
    # lakeinfo <- rbind(lakeinfo,addLake)

speciesinfo <- read.fst("speciesinfo.fst", as.data.table = TRUE)
gabel <- read.fst("gabelhousenames.fst", as.data.table = TRUE)
wsnames <- read.fst("WSnames.fst", as.data.table = TRUE)
ageData_file <- read.fst("compiledagedata.fst", as.data.table = TRUE)
stockingData <- read.fst("stockingData.fst", as.data.table = TRUE)
SortPSD <- data.table(PSDname=c("","substock", "stock","quality", "preferred", "memorable", "trophy"),
                      sortOrder=c(1,2,3,4,5,6,7), key="sortOrder")
SortPSDabrv <- data.table(PSDabrev=c("PSD","PSD-P","PSD-M","PSD-T","PSD S-Q","PSD Q-P","PSD P-M","PSD M-T"),
                          sortOrder=c(1,2,3,4,5,6,7,8), key="sortOrder")
    #above creates table with psd size classes and a sort order variable to use for sorting PSD by size rather 
    #than alphabetical.  Note first size class is blank so it can be used to sort the "total" category first (i.e.,
    #when reporting CPUE by size class, this allows total CPUE to be first, then substock CPUE, stock, etc.)

par(bg = "white") #fixes problem where figures that are downloaded have grey background. This makes them white


function(input, output, session) {
  #bs_themer() # turns on theming engine of bslib
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Modal box to select main database years to use##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 # callModule(profvis_server, "profiler") #used for debugging only
  
 pick_Database <- modalDialog(
    title = "What data would you like to use in the application today?",
    checkboxGroupInput("selectedData", "Select from the options below:",
                       choiceNames = list("load your own file (upload file below)",
                                          "Use data from last 5 years",
                                          "Use data from last 10 years",
                                          "Use data from all years in the database"),
                       choiceValues = list("loadedSampleData", "Main_database_5yr",
                                           "Main_database_10yr", "Main_database_name"),
                      ),
    hr(),
    fileInput("loadedSampleData", "Upload your own Data:"),
    size="s",
    easyClose = FALSE,
    multiple = FALSE,
    footer = actionButton("loadData", "Use data selected above")
  )
  
showModal(pick_Database) #activate above modal
  
  #second modal in case user fails to load data, but marks user-uploaded data as data source
  NoDataLoaded <- modalDialog(
    title = "You did not select a file to upload...please select it below",
    size="s",
    easyClose = TRUE,
    footer = actionButton("dismiss", "Click to go back to load data")
  )
  
  #respond to dismiss button of above modal in case where user failed to load file
  observeEvent(input$dismiss, {
    removeModal()
    showModal(pick_Database)
    updateCheckboxGroupInput(session, "selectedData", selected = "loadedSampleData")
  })

  #if "Change the data being used" button hit, re-show modal and reset selected selectize values
  #using selected = NULL did not work, but found setting character(0) does
    observeEvent(input$change,{
        updateSelectizeInput(session, "selectlake", choices = LakeAvail(),
                   selected = character(0), options = list(placeholder = "click/type here")
                   )
        updateSelectizeInput(session, "selectyear", choices = YearAvail(),
                   selected = character(0), options = list(placeholder = "click/type here")
                   )
        updateSelectizeInput(session, "selectmonth", choices = MonthAvail(),
                   selected = character(0), options = list(placeholder = "click/type here")
                   )
        updateSelectizeInput(session, "selectgear", choices = GearAvail(),
                   selected = character(0), options = list(placeholder = "click/type here")
                   )
     
      reset ("selectlakename")
      reset ("selectyearname")
      reset ("selectmonthname")
      reset ("selectgearname")
      reset ("selectspecies")
      reset("loadedSampleData")
      output$dataBeingUsed <- renderText({
                  dataBeingUsed <- as.character("")
                })
      showModal(pick_Database)
    })

    #create mainData$df_value as container to hold data that user will select with modal dialog
    mainData <- reactiveValues(df_data = NULL)
    allAgeData <- reactiveVal(ageData_file)

    #if user uploads a file, automatically check the "load your own data" box. 
    observeEvent(input$loadedSampleData, {
      if(!is_empty(input$loadedSampleData)){ 
        updateCheckboxGroupInput(session, "selectedData", selected = "loadedSampleData")
      }
    })

  #populate mainData$df_data and remove modal dialog once value selected and button is pressed
    observeEvent(input$loadData, {
      withProgress(message = "Please wait while database loads", min=0,max=10,value=1,{
        if(is.null(input$selectedData)){#if nothing selected, load last 5 years
          updateCheckboxGroupInput(session, "selectedData", selected = "Main_database_5yr")
          mainDataRead <- read.fst("Main_database_5yr.fst", as.data.table = TRUE)
          output$dataBeingUsed <- renderText({
                  return(paste("<span style=\"color:black\">Using data from past 5 years</span>"))
                })
          mainData$df_data <- mainDataRead
          removeModal()
          
        #deal with user-uploaded data####  
       }else if(input$selectedData =="loadedSampleData"){
         if(is.null(input$loadedSampleData)){
           removeModal()
           showModal(NoDataLoaded) 
         }else{
         mainDataRead <- fread(input$loadedSampleData$datapath)
            mainDataRead[mainDataRead == "."] <- NA
            #deal with Verified.?? columns if they exist
            if("Verified.TL.Wr" %in% colnames(mainDataRead)){
              mainDataRead <- mainDataRead %>% select(-Verified.TL.Wr)
            } #need to keep above as had app deployed a while with this combined
            if("Verified.TL" %in% colnames(mainDataRead)){
              mainDataRead <- mainDataRead %>% select(-Verified.TL)
            }
            if("Verified.Wr" %in% colnames(mainDataRead)){
              mainDataRead <- mainDataRead %>% select(-Verified.Wr)
            }
            mainDataRead <- mutate(mainDataRead, TL_mm = as.numeric(as.character(TL_mm)),
                              Wt_g = as.numeric(as.character(Wt_g))) %>%
                merge(select(lakeinfo, Lake.Code, Lake.Name), by="Lake.Code", all.x = TRUE) %>%
                merge(select(gearinfo, Gear.Code, Gear.Name), by="Gear.Code", all.x = TRUE) %>%
                unite(Lake.Name, Lake.Code, col="lake.Name_Code", sep = " - ", remove = F) %>%
                unite(Gear.Name, Gear.Code, col="gear.Name_Code", sep = " - Code ", remove = F) %>%
                merge(select(speciesinfo, Species.Code, Species.Name, species.Code_Name), by="Species.Code", all.x=TRUE) %>% 
                relocate(any_of(c("Lake.Code", "SampleID", "Station", "Month", "Day", "Year", "Time", "Pool.Elevation", 
                      "Surface.Temp", "Secchi", "Conductivity", "Gear.Code", "Gear.Name", "Gear.Length", "Habitat", 
                      "Effort", "Species.Code", "Species.Name", "Number.of.individuals", "TL_mm", "Wt_g", "Age", "Sex", 
                      "Genetic.ID", "Latitude", "Longitude", "Comments", "Lake.Name", "lake.Name_Code", 
                      "gear.Name_Code", "species.Code_Name"))) %>% 
                as.data.table() %>% 
                setkey(Lake.Code, Year, Gear.Code, Month)
           mainData$df_data <- mainDataRead
           setkey(mainData$df_data, Lake.Code, Year, Gear.Code, Month)
           
           #Extract age data from uploaded SSP data to append to ageData_file (now reactive in allAgeData())
           if("Age" %in% colnames(mainDataRead)){
             if(length(mainDataRead %>% filter(!is.na(Age))) > 0){
                extractedAgeData <- mainDataRead %>% 
                  rename_with(~ setNames(c("Gear", "TLmm"), c("Gear.Code", "TL_mm"))[.x],
                              .cols = any_of(c("Gear", "TLmm"))) %>% 
                  filter(!is.na(Age)) %>%
                  mutate(Gear.Code = as.numeric(Gear.Code), SampleID = paste(Lake.Code, Year, Month, Gear.Code, sep = ""),
                         TL_mm = as.numeric(TL_mm), Age=as.numeric(Age) )%>%
                  select(any_of(c("Lake.Code", "SampleID", "Month", "Day", "Year", "Gear.Code", "Species.Code", "Number.of.individuals",
                                  "TL_mm", "Age", "Sex", "Genetic.ID", "Lake.Name", "Gear.Name", "Species.Name", "lake.Name_Code",
                                  "gear.Name_Code", "species.Code_Name"))) %>%
                  #add optional columns if they are missing
                  mutate(Sex = if ("Sex" %in% names(.)) Sex else NA_character_,
                         Genetic.ID = if ("Genetic.ID" %in% names(.)) Genetic.ID else NA_character_) %>%
                  relocate(Lake.Code, SampleID, Month, Day, Year, Gear.Code, Species.Code, Number.of.individuals, TL_mm, Age, Sex,
                           Genetic.ID, Lake.Name, Gear.Name, Species.Name, lake.Name_Code, gear.Name_Code, species.Code_Name)
               appendedAgeData <- rbind(allAgeData(), extractedAgeData)
               allAgeData(appendedAgeData)
             }
           }
           removeModal()
         }
       }else{
          #if a check box was used, load associated data file accordingly
          #Load selected data from SSP database####
          if(input$selectedData == "Main_database_5yr"){
                 mainDataRead <- read.fst("Main_database_5yr.fst", as.data.table = TRUE)
             output$dataBeingUsed <- renderText({
                  return(paste("<span style=\"color:black\">Using data from past 5 years</span>"))
             })
           }else if(input$selectedData == "Main_database_10yr"){
             mainDataRead <- read.fst("Main_database_10yr.fst", as.data.table = TRUE)
             output$dataBeingUsed <- renderText({
                  return(paste("<span style=\"color:black\">Using data from past 10 years</span>"))
                })
           }else if(input$selectedData == "Main_database_name"){
             mainDataRead <- read.fst("Main_database_allYr.fst", as.data.table = TRUE)
             output$dataBeingUsed <- renderText({
                  return(paste("<span style=\"color:black\">Using full database</span>"))
                })
            }
        mainData$df_data <- mainDataRead
          removeModal()
       }
      })
    })
    
    #for debugging purposes
    output$troubleshootTable <- DT::renderDataTable({
      req(mainData$tempData)
      withProgress(message = "Creating table of selected data", min=0,max=10,value=1,{
        incProgress(10)
        expr=allAgeData()
      })
    })
    
     
    yr_range <- reactive({   #calculates text for specifying which database range is in use
        req(input$selectedData)
        if(input$selectedData == "Main_database_name"){
          as.character("Entire database is being used")
        }else if(input$selectedData =="loadedSampleData"){
          as.character(input$loadedSampleData$name)
        }else{
          req(mainData$df_data$Year)
          minYr <- min(mainData$df_data$Year)
          maxYr <- max(mainData$df_data$Year)
          yr_range <- paste(minYr, maxYr, sep = "-")
          as.character(yr_range)
        }
      })
   
    output$currentDataID <- renderText({
        paste("Currently loaded data: ", yr_range())
    })
   
    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Select Sample tab code##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #bs_themer() #adds bslib theme selecter
    
    #hide red "Loading Data" when .csv's are loaded
      hide(id = "loading-content", anim = TRUE, animType = "fade")
    #Adds ability to upload files greater than 5 MB (max now 100 MB)
      options(shiny.maxRequestSize=100*1024^2)
      
    #Initial functions needed to help self filter choices in lake, year, gear, month selectize boxes
        #Additional reactive statements...these first 2 reactive statements can be reused, so they will be more efficient
            availLake <- reactive({
              df <- mainData$df_data
                if(!is.null(input$selectlake)){
                  if(input$toggleCodeName == TRUE){
                      df <- df[df$Lake.Code %chin% c(input$selectlake),]
                  }else{
                    df <- df[df$lake.Name_Code %chin% c(input$selectlake),]
                  }
                }
              return(df)
            })
            
            availLakeYr <- reactive({
              req(availLake())
              df <- availLake()
                  if(!is.null(input$selectyear)){
                    df <- df[df$Year %in% c(input$selectyear),]
                  }
              return(df)
            })
        
        #code to translate selected values between codes and name-code as toggleCodeName box is checked/unchecked
        prevSelBoxes <- reactiveValues(
              selectlake = NULL,
              selectyear = NULL,
              selectmonth = NULL,
              selectgear = NULL
        )
        observeEvent(input$toggleCodeName, {
          prevSelBoxes$selectlake <- if(input$toggleCodeName == TRUE){
            #return code from name.code
            selectlakeDT <- data.table(as.character(input$selectlake))
            colnames(selectlakeDT) <- "lake.Name_Code"
            lakemerge <- merge(selectlakeDT, lakeinfo, by="lake.Name_Code", all.x=TRUE)
            lake.code <- sort.default(as.character(lakemerge$Lake.Code))
            return(lake.code)
          }else{
            #return name.code from code
            selectlakeDT <- data.table(as.character(input$selectlake))
            colnames(selectlakeDT) <- "Lake.Code"
            lakemerge <- merge(selectlakeDT, lakeinfo, by="Lake.Code", all.x=TRUE)
            lake.name.code <- sort.default(as.character(lakemerge$lake.Name_Code))
            return(lake.name.code)
          }
          prevSelBoxes$selectyear <- input$selectyear
          prevSelBoxes$selectmonth <- input$selectmonth
          prevSelBoxes$selectgear <- input$selectgear
          
          output$selectLakeBox <- renderUI({
            selectizeInput("selectlake", "Lake Code(s):", choices = LakeAvail(), multiple = T,
                           selected = if(length(prevSelBoxes$selectlake) ==1){
                             prevSelBoxes$selectlake
                           }else{c(prevSelBoxes$selectlake)},#need to make input a list if multiple items are to be selected sometimes
                           options = list(placeholder = "click/type here")
            )
          })
          
          updateSelectizeInput(session, "selectyear", choices = YearAvail(),
                               selected = character(0), options = list(placeholder = "click/type here")
          )
          updateSelectizeInput(session, "selectmonth", choices = MonthAvail(),
                               selected = character(0), options = list(placeholder = "click/type here")
          )
          updateSelectizeInput(session, "selectgear", choices = GearAvail(),
                               selected = character(0), options = list(placeholder = "click/type here")
          )
        })
            
            #using above 2 statements that can be reused, need function to create choices for each box
            LakeAvail <- reactive({
              req(mainData$df_data)
                dfLake <- mainData$df_data
                if(!is.null(input$selectyear)){
                    dfLake <- dfLake[dfLake$Year %in% c(input$selectyear),]
                }
                if(!is.null(input$selectgear)){
                  if(input$toggleCodeName == TRUE){
                    dfLake <- dfLake[dfLake$Gear.Code %in% c(input$selectgear),]
                  }else{
                    dfLake <- dfLake[dfLake$gear.Name_Code %chin% c(input$selectgear),]
                  }
                }
                if(!is.null(input$selectmonth)){
                  dfLake <- dfLake[dfLake$Month %in% c(input$selectmonth),]
                }
              if(input$toggleCodeName == TRUE){
                filterLake <- sort.default(unique(dfLake$Lake.Code))
              }else{
                filterLake <- sort.default(unique(dfLake$lake.Name_Code))
              }
              return(filterLake)
            })
            
            YearAvail <- reactive({
              req(availLake())
                dfYr <- availLake()
                if(!is.null(input$selectgear)){
                  if(input$toggleCodeName == TRUE){
                    dfYr <- dfYr[dfYr$Gear.Code %in% c(input$selectgear),]
                  }else{
                    dfYr <- dfYr[dfYr$gear.Name_Code %chin% c(input$selectgear),]
                  }
                }
                if(!is.null(input$selectmonth)){
                  dfYr <- dfYr[dfYr$Month %in% c(input$selectmonth),]
                }
                filterYear <- sort.default(unique(dfYr$Year))
                return(filterYear)
            })
            
            GearAvail <- reactive({
              req(availLakeYr())
                dfGear <- availLakeYr()
                if(!is.null(input$selectmonth)){
                  dfGear <- dfGear[dfGear$Month %in% c(input$selectmonth),]
                }
                  if(input$toggleCodeName == TRUE){
                    filterGear <- sort.default(unique(dfGear$Gear.Code))
                  }else{
                    filterGear <- sort.default(unique(dfGear$gear.Name_Code))
                    
                  }
                # }
                return(filterGear)
            })
            
           MonthAvail <- reactive({
                req(availLakeYr())
                dfMonth <- availLakeYr()
                if(!is.null(input$selectgear)){
                  if(input$toggleCodeName == TRUE){
                    dfMonth <- dfMonth[dfMonth$Gear.Code %in% c(input$selectgear),]
                  }else{
                    dfMonth <- dfMonth[dfMonth$gear.Name_Code %chin% c(input$selectgear),]
                  }
                }
                filterMonth <- sort.default(unique(dfMonth$Month))
                return(filterMonth)
            }) 
            
    #Server-side processing of selectize boxes
      output$selectLakeBox <- renderUI({
          selectizeInput("selectlake", "Lake Code(s):", choices = LakeAvail(), multiple = T,
                         selected = if(length(LakeAvail()) ==1){
                           LakeAvail()
                         }else{c(input$selectlake)},#need to make input a list if multiple items are to be selected sometimes
                           options = list(placeholder = "click/type here")
                         )
      })
      
      output$selectYearBox <- renderUI({ 
      selectizeInput("selectyear", "Year(s):", choices = YearAvail(), multiple = T, 
                      selected = if(length(YearAvail()) ==1){
                       YearAvail()
                     }else{c(input$selectyear)}, 
                           options = list(placeholder = "click/type here")
                           )
      })
      
      output$selectMonthBox <- renderUI({
        selectizeInput("selectmonth", "Month(s):", choices = MonthAvail(), multiple = T,
                         c(input$selectmonth), #no need to auto-select if only 1 month...proves more anoying than helpful
                       options = list(placeholder = "click/type here")
        )
      })
      
        output$selectGearBox <- renderUI({
          selectizeInput("selectgear", "Gear Code(s):", choices = GearAvail(), multiple = T,
                         selected = if(length(GearAvail()) ==1){
                             GearAvail()
                           }else{c(input$selectgear)}, 
                         options = list(placeholder = "click/type here")
          )
        })
        
      #clear all search criteria if clearBoxes button pressed
        observeEvent(input$clearBoxes,{
          updateSelectizeInput(session, "selectlake", choices = LakeAvail(),
                           selected = character(0), options = list(placeholder = "click/type here")
                           )
          updateSelectizeInput(session, "selectyear", choices = YearAvail(),
                           selected = character(0), options = list(placeholder = "click/type here")
          )
          updateSelectizeInput(session, "selectmonth", choices = MonthAvail(),
                           selected = character(0), options = list(placeholder = "click/type here")
          )
          updateSelectizeInput(session, "selectgear", choices = GearAvail(),
                           selected = character(0), options = list(placeholder = "click/type here")
          )
        })
      

    #Create selected data table for catch analysis using input from user in the selectizeInput boxes 
    #(no species selected) 
      #filter data from the selectizeInput values
      selData <- reactive({ 
        req(mainData$df_data)
        withProgress(message = "Filtering data", min=0,max=10,value=1,{
          incProgress(10)
          selData <- mainData$df_data
          if(!is.null(input$selectlake)){
            if(input$toggleCodeName == TRUE){
              selData <- selData[selData$Lake.Code %chin% c(input$selectlake),]
            }else{
              selData <- selData[selData$lake.Name_Code %chin% c(input$selectlake),]
            }
          }
          if(!is.null(input$selectyear)){
            selData <- selData[selData$Year %in% c(input$selectyear),]
          }
          if(!is.null(input$selectmonth)){
            selData <- selData[selData$Month %in% c(input$selectmonth),]
          }
          if(!is.null(input$selectgear)){
            if(input$toggleCodeName == TRUE){
              selData <- selData[selData$Gear.Code %in% c(input$selectgear),]
            }else{
              selData <- selData[selData$gear.Name_Code %in% c(input$selectgear),]
            }
          }
          selData <- as.data.table(selData)
          return(selData)
        })
      })
      
  ##create final selected data table and download button####
    #output selected data table
        output$selectedDataTable <- DT::renderDataTable({
          req(selData())
                  withProgress(message = "Creating table of selected data", min=0,max=10,value=1,{
                    incProgress(10)
                    expr=selData() %>% select(-lake.Name_Code, -gear.Name_Code, -species.Code_Name)
                  })
          })
    
    # Downloadable csv of selected dataset
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(input$selectlake, input$selectyear, input$selectgear, "sampledata.csv", sep = ".")
          },
          content = function(file) {
            write.csv(selData(), file, row.names = FALSE)
          }
        )
  
  ##Functions to grab codes from names and vice versa using values chosen in selection boxes for codes/names####
  ##used to create text printed on first tab indicating what data have been selected
        lakename <- eventReactive(input$selectlake, {
        if(input$toggleCodeName == TRUE){
          #code for selecting by lake codes
          selectlakeDT <- data.table(as.character(input$selectlake))
          colnames(selectlakeDT) <- "Lake.Code"
          lakemerge <- merge(selectlakeDT, lakeinfo, by="Lake.Code", all.x=TRUE)
          lake.name <- as.character(lakemerge$Lake.Name)
          return(lake.name)
        }else{
          #code for name.code
          selectlakeDT <- data.table(as.character(input$selectlake))
          colnames(selectlakeDT) <- "lake.Name_Code"
          lakemerge <- merge(selectlakeDT, lakeinfo, by="lake.Name_Code", all.x=TRUE)
          lake.name <- as.character(lakemerge$Lake.Name)
          return(lake.name)
        }
      })
        
      yearname <- reactive({
        if(is_empty(input$selectyear)){
          character(0)
        }else{
        yearname <- input$selectyear
        }
      })
      
      monthname <- reactive({
        if(is.null(input$selectmonth)){
          character(0)
        }else{
        monthname <- input$selectmonth
        }
      })
      
      gearname <- eventReactive(input$selectgear, {
        if(input$toggleCodeName == TRUE){
          #code for select by codes
          selectgearDT <- data.table(as.numeric(input$selectgear))
          colnames(selectgearDT) <- "Gear.Code"
          gearmerge <- merge(selectgearDT, gearinfo, by="Gear.Code", all.x=TRUE)
          gear.name <- as.character(gearmerge$Gear.Name)
          return(gear.name)
        }else{
          #code for name.code
          selectgearDT <- data.table(as.character(input$selectgear))
          colnames(selectgearDT) <- "gear.Name_Code"
          gearmerge <- merge(selectgearDT, gearinfo, by="gear.Name_Code", all.x=TRUE)
          gear.name <- as.character(gearmerge$Gear.Name)
          return(gear.name)
        }
      })
      
  ##Summarize selected data in center  box####
    #Provide reference for full lake and gear names to right of selectize inputs (Data Selection Tab)
    output$lakename <- renderText({lakename()})
    output$year <- renderText({yearname()})
    output$month <- renderText({monthname()})
    output$gearname <- renderText({gearname()})
    

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Select Types of Analyses tab ##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ##Create/update selectize boxes for species selection that populate with options available from selected sample####
  
    #create list of species.codes from selData()
    callspecies <- reactive({
        withProgress(message = "Filtering for available species codes", min=0,max=10,value=1, {
          if(input$toggleSppCodeName==TRUE){
                callspeciesAgeData <- sort.default(unique(selData()$Species.Name[selData()$Species.Name != 
                                                                                   "No fish in sample"]))
              }else{
                callspeciesAgeData <- sort.default(unique(selData()$species.Code_Name[selData()$species.Code_Name !=
                                                                                          "98 - No fish in sample"]))
              }
          return(callspeciesAgeData)
        })
    })
    
    callspeciesToSelect <- reactive(if(length(callspecies()) == 1){
                                        callspecies()
                                        }else{
                                          character(0)
                                    })
              
    speciesname <- reactive({
          req(input$selectspecies)
          if(input$toggleSppCodeName==FALSE){
            species <- data.table(input$selectspecies)
            colnames(species) <- "species.Code_Name"
            species <- merge(species, speciesinfo, by="species.Code_Name", all.x=T)
            speciesname <- sort(species$Species.Name)
          }else{
            speciesname <- sort(input$selectspecies)
          }
        })
                                   
    #Top selectize box (under "Single Species Analyses Selection" heading)
        #SSP data Species code selection box using server-side choices processing to match available spp
          observe({
               withProgress(message = "Loading Species Options Data", min=0,max=10,value=1, {
                  updateSelectizeInput(session, "selectspecies", choices = callspecies(),
                                       selected = callspeciesToSelect(),
                                       server = TRUE)
              })
          })
      
    
    #Selectize boxes in middle section (under "Select Age Dataset" heading)
    
          #find spp codes available in age data
          callspeciesAgeData <- reactive({
            withProgress(message = "Filtering for available species codes", min=0,max=10,value=1, {
              if(input$toggleSppCodeName==TRUE){
                callspeciesAgeData <- sort.default(unique(agedata()$Species.Name))
              }else{
                callspeciesAgeData <- sort.default(unique(agedata()$species.Code_Name))
              }
            })
          })
          
        #Age data - species selection box choices and default value set to species selected for SSP data at top of page
          observe({
              req(callspeciesAgeData())
              withProgress(message = "Loading Data",min=0,max=10,value=1, {
                updateSelectizeInput(session, "selagespp", choices = callspeciesAgeData(),
                    selected = input$selectspecies, server = TRUE)
              })
          })
        
        #Lake selection for Age data - lake selection box built here (server.r) so only values matching selected 
        #sample are in drop down list
            observe({
              withProgress(message = "Loading Age Data",min=0,max=10,value=1, {
                  tempAgeSpp <- agedata()
                  #filter for matching spp
                  if(!is.null(input$selagespp)){
                    if(input$toggleSppCodeName==TRUE){
                      tempAgeSpp <- tempAgeSpp[tempAgeSpp$Species.Name %in% c(input$selagespp),]
                    }else{
                      tempAgeSpp <- tempAgeSpp[tempAgeSpp$species.Code_Name %in% c(input$selagespp),]
                    }
                    #create possible lakes given selected spp
                    if(input$toggleCodeName == TRUE){
                      callagelake <- sort.default(unique(tempAgeSpp$Lake.Code))
                      updateSelectizeInput(session, "selagelake", choices = as.character(callagelake),
                        selected = input$selectlake, server = TRUE)
                    }else{
                      callagelake <- sort.default(unique(tempAgeSpp$lake.Name_Code))
                      updateSelectizeInput(session, "selagelake", choices = as.character(callagelake),
                                           selected = input$selectlake, server = TRUE)
                    }
                  }
              })
            })
        
        #Age data - years selection box built here (server.r) so only values matching selected sample are in drop down list
           observe({
             req(selageData2())
              withProgress(message = "Pairing Age Dataset",min=0,max=10,value=1, {
                updateSelectizeInput(session, "selageyears", choices = sort.default(unique(selageData()$Year)),
                  selected = input$selectyear, server = TRUE)
              })
           })
          
    ##Make checks on selData and selected age data to see if matched, provide text output about match status####
    #Reminder to select species above in orange
      output$nospp <- renderText({
        if(is.null(input$selagespp)){
            nospp <- as.character("Select Species Above")
        }
      })
      
    #Display text showing no paired data in orange
      output$nodata <- renderText({
        if(is.null(input$selagelake) || is.null(input$selageyears)){
            nodata <- as.character("Use boxes below to select Age Data")
        }
      })

    #display text showing matched age dataset in green
    output$yesmatch <- renderText({
      req(selDataspp(), input$selagelake, input$selageyears, input$selagespp, input$selectspecies)
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp) &&
         length(input$selagelake) != 0 && length(input$selageyears) != 0  && length(input$selagespp) != 0 &&
         length(input$selectspecies) !=0){
           
             if(input$toggleCodeName == TRUE){
                  if(input$selectspecies == input$selagespp &&
                     all(selDataspp()$Lake.Code == c(input$selagelake)) &&
                     all(selDataspp()$Year == c(input$selageyears))
                     ){
                      yesmatch <- as.character("Matched Age Dataset")
                  }
             }else{
                 if(all(input$selectspecies == input$selagespp) &&
                    all(selDataspp()$lake.Name_Code == c(input$selagelake)) &&
                    all(selDataspp()$Year == c(input$selageyears))
                    ){
                   yesmatch <- as.character("Matched Age Dataset")
                 }
             }
        }
    })
      
    #display text showing not a matched age dataset in red
    output$nomatch <- renderText({
      req(selDataspp(), input$selagelake, input$selageyears, input$selagespp, input$selectspecies)
        if(!is_empty(input$selagelake) && !is_empty(input$selageyears) && !is_empty(input$selagespp) &&
           !is_empty(input$selectspecies) && !is_empty(selDataspp())){
          if(input$toggleCodeName == TRUE){
            if(any(input$selectspecies != input$selagespp) ||
               any(selDataspp()$Lake.Code != input$selagelake) ||
               any(selDataspp()$Year != input$selageyears))
              {
                nomatch <- as.character("Not a Matched Age Dataset")
                return(nomatch)
            }
          }else{
            if(any(input$selectspecies != input$selagespp) ||
               any(selDataspp()$lake.Name_Code != input$selagelake) ||
               any(selDataspp()$Year != input$selageyears)){
              nomatch <- as.character("Not a Matched Age Dataset")
              return(nomatch)
            }
          }
      } 
    })

    #display text letting user know that app is using uploaded age data
    output$useUploadedage <- renderText({
      if(input$loadageCheck == TRUE){
        useUploadedage <- as.character("Using Uploaded Age Data")
      }
    })
    
  
  ##Create selected data table for reference age data#####
  
    #Create table from selData/SSP data (i.e., catch data rather than age data) for single species analysis 
      selDataspp <- reactive({
        req(selData(), input$selectspecies)
          if(input$toggleSppCodeName==TRUE){
            selDataspp <- selData()[selData()$Species.Name == input$selectspecies,]
            selDataspp <- selDataspp[rep(seq_len(nrow(selDataspp)), selDataspp$Number.of.individuals),]
              #Above line repeats each row by number of times in Number of individuals column
          }else{
            selDataspp <- selData()[selData()$species.Code_Name == input$selectspecies,]
            selDataspp <- selDataspp[rep(seq_len(nrow(selDataspp)), selDataspp$Number.of.individuals),]
              #Above line repeats each row by number of times in Number of individuals column
          }
      })
      
    # Create downloadable csv of selected selData/SSP species dataset
        output$downloadsppData <- downloadHandler(
          filename = function() {
            paste(input$selectlake,input$selectyear,input$selectgear,input$selectspecies,"sppdata.csv", sep = ".")
          },
          content = function(file) {
            write.csv(selDataspp(), file, row.names = FALSE)
          }
        )

     #Create agedata either from user-supplied data (based on check box) or built-in data (if box not checked)
        observeEvent(input$loadedageData, {
          updateCheckboxInput(session, "loadageCheck", value = TRUE)
        })
        agedata <- reactive(if(input$loadageCheck == TRUE){
          uploadAgeData <- input$loadedageData
              if(is.null(uploadAgeData)){
                return(NULL)
              }
              agedata <- fread(uploadAgeData$datapath)
              agedata <- agedata %>% 
                rename_with(~ case_when(.x == "Gear" ~ "Gear.Code",
                                        .x == "TLmm" ~ "TL_mm",
                                        TRUE ~ .x),
                            .cols = any_of(c("Gear", "TLmm"))
                )%>% 
                merge(select(lakeinfo, Lake.Code, Lake.Name), by = "Lake.Code", all.x=T) %>%
                merge(select(gearinfo, Gear.Code, Gear.Name), by = "Gear.Code", all.x=T) %>%
                merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x=T) %>%
                unite(Lake.Name, Lake.Code, col="lake.Name_Code", sep = " - ", remove = F) %>%
                unite(Gear.Name, Gear.Code, col="gear.Name_Code", sep = " - Code ", remove = F) %>%
                unite(Species.Code, Species.Name, col="species.Code_Name", sep = " - ", remove = F) %>%
                relocate(lake.Name_Code, gear.Name_Code, species.Code_Name, .after = last_col()) %>%
                as.data.table()
              agedata <- setkey(agedata, Lake.Code, Year, Gear.Code, Species.Code)
        }else{
          agedata <- allAgeData()
        })
        
    ##Create final selected age data
      #first filter by species selected by user
        selageData <- reactive({
          req(agedata())
          selageData <- agedata()
          if(!is.null(input$selagespp)){
            if(input$toggleSppCodeName==TRUE){
              selageData <- selageData[selageData$Species.Name %chin% c(input$selagespp),]
            }else{
              selageData <- selageData[selageData$species.Code_Name %chin% c(input$selagespp),]
            }
          }
          return(selageData)
        })
        
        #Next, filter by lake selected by user
          selageData2 <- reactive({
            req(selageData())
              selageData2 <- selageData()
              if(!is.null(input$selagelake)){
                if(input$toggleCodeName == TRUE){
                  selageData2 <- selageData2[selageData2$Lake.Code %chin% c(input$selagelake),]
                  return(selageData2)
                }else{
                  selageData2 <- selageData2[selageData2$lake.Name_Code %chin% c(input$selagelake),]
                  return(selageData2)
                }
              }
          })
  
      #Finally, filter by year selected by user
        selageDatafinal <- reactive({
          req(selageData2())
            selageDatafinal <- selageData2()
            if(!is.null(input$selageyears)){
              selageDatafinal <- selageDatafinal[selageDatafinal$Year %in% c(input$selageyears),]
              if(length(selageDatafinal != 0)){
                  if(nrow(selageDatafinal)>1){
                    selageDatafinal <- mutate(selageDatafinal, TL_mm = as.numeric(as.character(TL_mm)),
                                            Age = as.numeric(as.character(Age)))
                  }
              }
              selageDatafinal <- select(selageDatafinal, -lake.Name_Code, -gear.Name_Code, -species.Code_Name)
              return(selageDatafinal)
            }
        })
  
  ##Selected sample summary and download info#### 
   
    #output selected age data
    output$selectedageData <- DT::renderDataTable({
      withProgress(message = "Pairing Age Dataset", min=0, max=10,value = 1,{
        incProgress(10)
        expr=selageDatafinal()
      })
    })
    
    #Age dataset sample size display
      output$agecount <- renderText({
        if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
          agecount <- length(selageDatafinal()$Age)
          agecount <- paste(as.character("N = "), agecount, sep = "")
        }
      })
    
   #Download csv of selected age dataset
    output$downagedata <- downloadHandler(
      filename = function() {
        paste(input$selectlake, input$selectyear, input$selectgear,input$selectspecies, 
              "agedata.csv", sep = ".")
      },
      content = function(file) {
        write.csv(selageDatafinal(), file, row.names = FALSE)
      }
    )
    
    #get month information about selected sampling data
    sampleMonths <- reactiveValues()
    observe({
      req(selDataspp())
      sampleMonths$allMonths <- as.numeric(unique(selDataspp()$Month))
      sampleMonths$minMonth <- min(sampleMonths$allMonths)
      sampleMonths$maxMonth <- max(sampleMonths$allMonths)
      sampleMonths$midMonth <- mean(sampleMonths$allMonths)
    })
    
    #Selected sample summary###
      output$lakename3 <- renderText({lakename()})
      output$year3 <- renderText({yearname()})
      output$monthRange <- renderText({
        req(sampleMonths)#this is a set of reactiveValues and does not use sampleMonths() as a result
        if(length(sampleMonths$allMonths) > 1){
          text <- paste(month.abb[min(sampleMonths$allMonths)], "-", month.abb[max(sampleMonths$allMonths)])
        } else if(length(sampleMonths$allMonths) == 1){
          text <- month.abb[sampleMonths$allMonths]
        } else {
          text <- NULL
        }
        return(text)
      })
      output$month3 <- renderText({monthname()})
      output$gearname3 <- renderText({gearname()})
      output$speciesname3 <- renderText({speciesname()})
    
    #Selected age dataset summary###
      output$agespp <- renderText({
        req(input$selagespp)
        if(input$toggleSppCodeName ==FALSE){
          speciesco <- data.table(input$selagespp)
          colnames(speciesco) <- "species.Code_Name"
          fullsname <- merge(speciesco, speciesinfo, by="species.Code_Name", all.x=T)
          speciesname1 <- fullsname$Species.Name
        }else{
          speciesname1 <- input$selagespp
        }
      })
      output$agelake <- renderText({
        req(input$selagelake)
        lakeco <- data.table(input$selagelake)
        if(input$toggleCodeName == TRUE){
          colnames(lakeco) <- "Lake.Code"
          fullname <- merge(lakeco, lakeinfo, by="Lake.Code", all.x=T)
          lakename1 <- fullname$Lake.Name
        }else{
          colnames(lakeco) <- "lake.Name_Code"
          fullname <- merge(lakeco, lakeinfo, by="lake.Name_Code", all.x=T)
          lakename1 <- fullname$Lake.Name
        }
      })  
      output$ageyear <- renderText(input$selageyears)
    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Catch Analysis Tab#####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ##Preliminary sample summary info on left
    #Data Selection Summary ###
      output$lakename2 <- renderText({lakename()})
      output$year2 <- renderText({yearname()})
      output$month2 <- renderText({monthname()})
      output$gearname2 <- renderText({gearname()})
      
    ##Calculate Abiotic means, mins, maxs###   
      output$abioticsum <- renderTable(digits=1, spacing="xs", width=10, rownames=TRUE,{
        req(selData())
        if(input$abiotic == TRUE){
          #We should be able to just aggregate by mean and take mins/maxs from it because they only take one measurement/sample
          if(sum(!is.na(selData()$Pool.Elevation)>0)){
            aggpool <- aggregate(Pool.Elevation ~ SampleID, data=selData(), mean, na.action=NULL)
              poolmean <- round(mean(aggpool$Pool.Elevation, na.rm=TRUE), digits = 1)
              poolmin <- min(aggpool$Pool.Elevation, na.rm=TRUE)
              poolmax <- max(aggpool$Pool.Elevation, na.rm=TRUE)
            }else{
              poolmean <- as.character("NA")
              poolmin <- as.character("NA")
              poolmax <- as.character("NA")
            }
          if(sum(!is.na(selData()$Surface.Temp)>0)){
            aggtemp <- aggregate(Surface.Temp ~ SampleID, data=selData(), mean, na.action=NULL)
              tempmean <- round(mean(aggtemp$Surface.Temp, na.rm=TRUE), digits = 1)
              tempmin <- min(aggtemp$Surface.Temp, na.rm=TRUE)
              tempmax <- max(aggtemp$Surface.Temp, na.rm=TRUE)
            }else{
              tempmean <- as.character("NA")
              tempmin <- as.character("NA")
              tempmax <- as.character("NA")
            }
          if(sum(!is.na(selData()$Secchi)>0)){
            aggsecchi <- aggregate(Secchi ~ SampleID, data=selData(), mean, na.action=NULL)
              secchimean <- round(mean(aggsecchi$Secchi, na.rm=TRUE), digits = 1)
              secchimin <- min(aggsecchi$Secchi, na.rm=TRUE)
              secchimax <- max(aggsecchi$Secchi, na.rm=TRUE)
            }else{
              secchimean <- as.character("NA")
              secchimin <- as.character("NA")
              secchimax <- as.character("NA")
            }
          if(sum(!is.na(selData()$Conductivity)>0)){
            aggcond <- aggregate(Conductivity ~ SampleID, data=selData(), mean, na.action=NULL)
              condmean <- round(mean(aggcond$Conductivity, na.rm=TRUE), digits = 1)
              condmin <- min(aggcond$Conductivity, na.rm=TRUE)
              condmax <- max(aggcond$Conductivity, na.rm=TRUE)
            }else{
              condmean <- as.character("NA")
              condmin <- as.character("NA")
              condmax <- as.character("NA")
            }
    #Make abiotic table
          abiotictable <- matrix(c(poolmean,poolmin,poolmax, tempmean,tempmin,tempmax,
                                   secchimean,secchimin,secchimax, condmean,condmin,condmax),
                                 ncol=3, nrow=4, byrow=TRUE,
            dimnames = list(c("Pool Elev (ft)","Surface Temp (C)","Secchi (in)","Conductivity"),
                            c("Mean","Min","Max")))
          abiotictable
        }
      })
      
    ###Species Sample Size Table###
      output$totsamplesize <- renderTable({
        req(selData())
        if(input$samplesize == TRUE){
          numspp2 <- aggregate(Number.of.individuals ~ Species.Name, selData(), sum)
          sppchar <- as.character(numspp2$Species.Name)
          numsppmatrix <- matrix(c(sppchar, numspp2$Number.of.individuals),
                                 ncol=2,
                                 dimnames = list(c(),c("Species", "# of fish")))
        }
      })
      
    ###Total Effort Table###
      output$toteffort <- renderTable(digits=1, spacing="xs", rownames=FALSE, {
        req(selData())
        if(input$totaleffort == TRUE){
          if(selData()$Gear.Code[[1]] <= 40){ #netting and seining (use effort)
            if(selData()$Gear.Code[[1]] != 10){ #any net but seine
              aggeffort <- aggregate(Effort ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
              Ef_name <- "Total effort (net-hr)"
            }else{ #seines
              aggeffort <- aggregate(Effort ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
              aggeffort$Effort <- aggeffort$Effort/10.764
              Ef_name <- "Total effort (m²)"
              }
            
          }else{ #electrofishing (use gear length)
            aggeffort <- aggregate(Gear.Length ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
            Ef_name <- "Total effort (min)"
          }  
          colnames(aggeffort)[3] <- "Effort"
          effortbygear <- aggregate(Effort ~ Gear.Code, data=aggeffort, sum, na.action=NULL)
          nstations <- aggregate(SampleID ~ Gear.Code, data=aggeffort, function(x) length(unique(x)))
          effort_table <- merge(effortbygear, nstations, by = "Gear.Code")
          effort_table <- merge(effort_table, gearinfo, by = "Gear.Code", all.x = TRUE)
          
          # Now arrange columns and rename
          final_eff_table <- effort_table[, c("Gear.Code", "Gear.Name", "Effort", "SampleID")]
          colnames(final_eff_table) <- c("Gear Code", " Gear Name", Ef_name, "# Stations")
          return(final_eff_table)
        }
      })
      
    ##Total CPUE Table##################################################
      cpuetable <- reactive({
        req(selData())
        if(input$cpue == TRUE){
          #Aggregate into total catch by species per sample (also return gear code and effort) - transform SampleID to character
          aggselData <- as.data.table(aggregate(Number.of.individuals ~ SampleID*Species.Name+Gear.Code,  #note SampleID has Station in it
                                  data=selData(), sum))
          aggselDatach <- aggselData %>% mutate_if(is.factor, as.character)
          #Add zeroes for samples that didn't catch a particular species - FSA
          selDatazero <- as.data.table(addZeroCatch(as.data.frame(aggselDatach), "SampleID", "Species.Name", #must be data.fram as addZeroCatch won't work on data
                                                    "Number.of.individuals") %>% 
            filter(Species.Name != "No fish in sample"))
          
        #Pull out effort from selData and join to dataset
          #Calculate CPUE based on gear code
          if(aggselData$Gear.Code[[1]]  >= 41 & aggselData$Gear.Code[[1]] != 68){ #electrofishing; get value from 1st row of Gear.Code (col 3), and see if >=41
            lengthbysample <- as.data.table(aggregate(Gear.Length ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, lengthbysample, by="SampleID", all.x=TRUE)
            samplecpue <- (mutate(allcpue, CPUE = (Number.of.individuals*60)/Gear.Length))
            cpue_eff_name <- "Mean (fish/hr)"
          } 
          if(aggselData$Gear.Code[[1]] <= 40 & aggselData[1,3] != 10){ #netting; get value from 1st row of Gear.Code (col 3), and see if >=41
            effortbysample <- as.data.table(aggregate(Effort ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, effortbysample, by="SampleID", all.x=TRUE)
            samplecpue <- (mutate(allcpue, CPUE = (Number.of.individuals*24)/Effort))
            cpue_eff_name <- "Mean (fish/24hr set)"
          }   
          if(aggselData$Gear.Code[[1]] == 10){ #seining; get value from 1st row of Gear.Code (col 3), and see if >=41
            effortbysample <- as.data.table(aggregate(Effort ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, effortbysample, by="SampleID", all.x=TRUE)
            samplecpue <- as.data.table(mutate(allcpue, CPUE = (Number.of.individuals*1076)/Effort))
            cpue_eff_name <- "Mean (fish/100 m²)" #"Mean (fish/100 m^2)"
          }
          if(aggselData$Gear.Code[[1]] == 68){ #Bow fishing...need to count...no divide by effort
            effortbysample <- as.data.table(aggregate(Effort ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, effortbysample, by="SampleID", all.x=TRUE)
            samplecpue <- as.data.table(mutate(allcpue, CPUE = (Number.of.individuals)))
            cpue_eff_name <- "# fish"
          }
          
          #aggregate by species and create dataframes for mean, sd, and count 
          cpuedfs <- list(
            meancpue = aggregate(CPUE ~ Species.Name, data=samplecpue, mean, na.action = na.omit),
            sdcpue = aggregate(CPUE ~ Species.Name, data=samplecpue, sd, na.action = na.omit),
            countcpue = aggregate(CPUE ~ Species.Name, data=samplecpue, length, na.action = na.omit)
          )
          
          #join all previous dataframes together - rename columns
          cpuejoinall <- list(cpuedfs$meancpue, cpuedfs$sdcpue[2],cpuedfs$countcpue[2])  %>% as.data.table()
            #the above list itemizes both columns from 1st item in list, then just 2nd item for other 2 lists
            #and merges them into data.table.  Much faster than the join_all command (and prevents needing plyr)
          colnames(cpuejoinall)[2:4] <- c("Mean", "SD", "Count") 
          
          #calculate and add standard error, confidence intervals, CV's 
          cpueerror <- mutate(cpuejoinall, SE = (SD/sqrt(Count)))
          cpuetable <- mutate(cpueerror, LCI = Mean - (1.96*SE),
                              UCI = Mean + (1.96*SE),
                              RSE = (SE/Mean)*100,
                              CV = (SD/Mean)*100)
          cpuetable <- mutate(cpuetable, N25 = (CV/12.5)^2, N40 = (CV/20)^2)
          cpuetable <- mutate(cpuetable, N25 = as.integer(round(N25,0)),
                              N40 = as.integer(round(N40,0)))
          cpuetable <- select(cpuetable,c(1,2,4,8,5,6,7,10,11))
          setnames(cpuetable, c(1:9), c("Species",cpue_eff_name,"# of Samples","RSE","SE","L 95% CI",
                                   "U 95% CI","N RSE = 12.5 (25% range)","N RSE = 20 (40% range)"))
          cpuetable <- setorder(cpuetable, Species)
        }
      })
    #Render total cpue table
      output$totcpue <- renderTable(digits = 2, spacing = "xs", {
        if(input$cpue == TRUE){
          cpuetable()
        }
      })
   ##PSD groupings and related output##### 
    #Render species name for PSD grouping reference
      output$speciesref <- renderText({speciesname()})
      
    #Make reference table for PSD groupings (mm) 
    output$sizerefmm <- renderTable(digits=0, rownames=TRUE, spacing="xs", na = " ",{
      if(input$cpuesize == TRUE){
        if(any(selData()$Species.Code %in% gabel$Species.Code)){ #test if at least 1 species has PSD size classes in gabel dataframe
          sp <- data.table(unique(selData()$Species.Code))
          colnames(sp) <- "Species.Code"
          sp <- merge(sp,gabel, by="Species.Code", all.x=T)
          sp <- merge(sp, speciesinfo, by="Species.Code", all.x=T)
          sp <- mutate(sp, Gabelhouse.Name = as.character(Gabelhouse.Name))
          sp <- sp[complete.cases(sp),]
          
          match.fun(psdVal)
          buildtablemm <- data.frame(lapply(sp$Gabelhouse.Name, psdVal))
          colnames(buildtablemm) <- sp$Species.Name
          buildtablemm <- data.frame(t(buildtablemm))
          buildtablemm <- buildtablemm[order(row.names(buildtablemm)),]
        }
      }
    })
    
    #Make reference table for PSD groupings (in) 
    output$sizerefin <- renderTable(digits=1, rownames=TRUE, spacing="xs", na = " ",{
      if(input$cpuesize == TRUE){
        if(any(selData()$Species.Code %in% gabel$Species.Code)){ #test if at least 1 species has PSD size classes in gabel dataframe
          sp <- data.table(unique(selData()$Species.Code))
          colnames(sp) <- "Species.Code"
          sp <- merge(sp,gabel,by="Species.Code", all.x=T)
          sp <- merge(sp, speciesinfo, by="Species.Code", all.x = T)
          sp <- mutate(sp, Gabelhouse.Name = as.character(Gabelhouse.Name))
          sp <- sp[complete.cases(sp),]
          
          match.fun(psdVal)
          buildtablemm <- data.frame(lapply(sp$Gabelhouse.Name, psdVal))
          colnames(buildtablemm) <- sp$Species.Name
          
          buildtablein <- as.data.frame(buildtablemm)
          buildtablein <- mutate_all(buildtablein, ~(./25.4))
          buildtablein <- round(buildtablein,1)
          rownames(buildtablein) <- c("substock","stock","quality","preferred",
                                      "memorable","trophy")
          buildtablein <- data.frame(t(buildtablein))
          buildtablein <- buildtablein[order(row.names(buildtablein)),]
        }
      }
    })
            
    ##CPUE by Length class or PSD Size Category Calculations#############################
    
      ##Preliminary objects needed for changing between PSD/size class and inch vs mm
      
          #code to completely hide things if unchecked on "Select Analysis tab"
          observeEvent (input$cpuesize,{
            req(!is.null(input$cpuesize))
            if(input$cpuesize == T){
              show("CPUEbyPSD_length")
              hide("noCPUEbyLength_downld")
              show("CPUEbyLength_downld")
            }else{
              hide("CPUEbyPSD_length")
              show("noCPUEbyLength_downld")
              hide("CPUEbyLength_downld")
            }
          })
    
          #Code for button changing CPUE by size between PSD and length classes (mm vs inches on separate control)
          CPUEsizeToggle <- reactiveVal("PSD")
          observeEvent(input$CPUEbySize_PSD_inch, {
            # print(paste0("toggle pressed, input$CPUEMinBin= ",input$CPUEMinBin))
            if (CPUEsizeToggle() == "Length_class") {
              CPUEsizeToggle("PSD")
              updateActionButton(session, "CPUEbySize_PSD_inch", label = "Change table to CPUE by inch class")
              hide("CPUEbyLength")
            } else { #CPUE by length class
              CPUEsizeToggle("Length_class")
              updateActionButton(session, "CPUEbySize_PSD_inch", label = "Change table to CPUE by PSD size class")
              show("CPUEbyLength")
              #need to do this to ensure CPUElengthbin observer triggers and makes starting bin value
                if(is.na(input$CPUEMinBin)){ #test so this does not run if toggling back and forth...only needed once after initialization
                  cpueValues$updateStatus <- "invalid" #set flag to prevent any further updates until CPUEMinBin is created
                  updateNumericInput(session, "CPUElengthbin", value = cpueValues$lengthBin+0.02)#just need something different so there is a change
                  updateNumericInput(session, "CPUElengthbin", value = round(cpueValues$lengthBin,1)) #now set it back to right value
                }
            }
          })   
  
          # Reactive values to store the current state of length bin and min bin
          cpueValues <- reactiveValues(
            lengthBin = 2,  # Default value in inches
            minBin = NULL,  # To be calculated later
            units = "inch",  # Default unit
            initialized = FALSE,  # Flag to track initialization
            updateStatus= "valid", #Flag to prevent exicution during updates
            onlyConvertLengthBin = FALSE,
            onlyConvertMinBin = FALSE
          )
          
          #if data selected changes, reset the CPUE by length class Bin size and min Bin
          observeEvent(selData(),{
            if(cpueValues$initialized == T){
              cpueValues$updateStatus <- "invalid" #set flag to prevent any further updates until this run completes
              updateNumericInput(session, "CPUElengthbin", value = NULL)
              updateNumericInput(session, "CPUEMinBin", value = NULL)
              InchSelData <- selData()
              if(input$InchCPUE == T){#now set it back to right value...this will trigger calc of CPUEMinBin
                updateNumericInput(session, "CPUElengthbin", value = 2)
                cpueValues$minBin <- 2
                #reset CPUEMinBin
                  InchSelData$TL_inch <- InchSelData$TL_mm / 25.4
                  InchSelData$TL_inch <- InchSelData$TL_mm / 25.4
                  max_length <- max(InchSelData$TL_inch, na.rm = TRUE)
                  bin_breaks <- seq(0, (max_length + cpueValues$lengthBin), by = round(cpueValues$lengthBin,1))
                  bin_indices <- findInterval(InchSelData$TL_inch, bin_breaks)
                  valid_bins <- na.omit(bin_breaks[unique(bin_indices)])
                  min_bin <- if (length(valid_bins) > 0) min(valid_bins) else 0
                  updateNumericInput(session, "CPUEMinBin", paste0("Starting Bin (", cpueValues$units, ")"),
                                   value = round(cpueValues$minBin,1))
              }else{ #if mm were the units
                updateNumericInput(session, "CPUElengthbin", value = 2  *25.4) 
                cpueValues$minBin <- 2 * 25.4
                #reset CPUEMinBin
                  max_length <- max(InchSelData$TL_mm, na.rm = TRUE)
                  bin_breaks <- seq(0, (max_length + cpueValues$lengthBin), by = round(cpueValues$lengthBin,0))
                  bin_indices <- findInterval(InchSelData$TL_mm, bin_breaks)
                  valid_bins <- na.omit(bin_breaks[unique(bin_indices)])
                  min_bin <- if (length(valid_bins) > 0) min(valid_bins) else 0
                  updateNumericInput(session, "CPUEMinBin", paste0("Starting Bin (", cpueValues$units, ")"),
                                   value = min_bin)
              }
              cpueValues$updateStatus <- "valid"
            }
          })
          
          # Observe changes in InchCPUE checkbox and update values accordingly
          observeEvent(input$InchCPUE, {
            req(input$CPUElengthbin) #cpueValues$initialized, , input$CPUEMinBin
            if (cpueValues$updateStatus == "invalid") return() 
            cpueValues$updateStatus <- "invalid" #set flag to prevent any further updates until this run completes
                cpueValues$onlyConvertLengthBin <- TRUE
                cpueValues$onlyConvertMinBin <- TRUE
            isolate({# Prevent circular updates by isolating changes
              if (input$InchCPUE == T && cpueValues$units != "inch") {
                cpueValues$units <- "inch"
                updateCheckboxInput(session, "InchCPUE", "Uncheck to give length bin in mm")
                cpueValues$lengthBin <- input$CPUElengthbin / 25.4
                updateNumericInput(session, "CPUElengthbin",
                  label = paste0("Bin width (", cpueValues$units, ")"),
                  value = round(cpueValues$lengthBin,1),
                  min = ifelse(cpueValues$units == "inch", 0.1, 2.54),
                  max = ifelse(cpueValues$units == "inch", 20, 508))
                cpueValues$minBin <- input$CPUEMinBin / 25.4
                updateNumericInput(session, "CPUEMinBin", paste0("Starting Bin (", cpueValues$units, ")"),
                                   value = round(cpueValues$minBin,1))
              } else if(input$InchCPUE == F && cpueValues$units != "mm") {
                cpueValues$units <- "mm"
                updateCheckboxInput(session, "InchCPUE", "Check to give lengh bin in inches")
                cpueValues$lengthBin <- input$CPUElengthbin * 25.4
                updateNumericInput(session, "CPUElengthbin",
                  label = paste0("Bin width (", cpueValues$units, ")"),
                  value = round(cpueValues$lengthBin,0),
                  min = ifelse(cpueValues$units == "inch", 0.1, 2.54),
                  max = ifelse(cpueValues$units == "inch", 20, 508))
                cpueValues$minBin <- input$CPUEMinBin * 25.4
                updateNumericInput(session, "CPUEMinBin", paste0("Starting Bin (", cpueValues$units, ")"),
                                   value = round(cpueValues$minBin,0))
              }else{
                cpueValues$updateStatus <- "valid"
              }
            })
          }, ignoreInit = TRUE)

          # Observe changes in CPUElengthbin input
          observeEvent(input$CPUElengthbin, {#works, but only if I have code in CPUEbySize_PSD_inch observer above that forces this to run upon initialization
            req(input$CPUElengthbin>0, selData())
            if(cpueValues$onlyConvertLengthBin==TRUE){
              cpueValues$onlyConvertLengthBin <- FALSE
            }else{
              cpueValues$updateStatus <- "invalid" #set flag to prevent any further updates until this run completes
              cpueValues$lengthBin <- input$CPUElengthbin
                InchSelData <- selData()
                if(cpueValues$units == "inch"){
                  InchSelData$TL_inch <- InchSelData$TL_mm / 25.4
                  max_length <- max(InchSelData$TL_inch, na.rm = TRUE)
                  bin_breaks <- seq(0, (max_length + cpueValues$lengthBin), by = round(cpueValues$lengthBin,1))
                  bin_indices <- findInterval(InchSelData$TL_inch, bin_breaks)
                }else{ #units are mm
                  max_length <- max(InchSelData$TL_mm, na.rm = TRUE)
                  bin_breaks <- seq(0, (max_length + cpueValues$lengthBin), by = round(cpueValues$lengthBin,0))
                  bin_indices <- findInterval(InchSelData$TL_mm, bin_breaks)
                }
                valid_bins <- na.omit(bin_breaks[unique(bin_indices)])
                min_bin <- if (length(valid_bins) > 0) min(valid_bins) else 0
                updateNumericInput(session, "CPUEMinBin", value = min_bin+0.001) #need to make sure the value for CPUEMinBin actually changes to trigger downstream observer
                updateNumericInput(session, "CPUEMinBin", paste0("Starting Bin (", cpueValues$units, ")"),
                                   value = min_bin)
            }
          })

          #Observe changes in CPUEMinBin input
          observeEvent(input$CPUEMinBin, {
            req(input$CPUEMinBin)
            if(cpueValues$onlyConvertMinBin==TRUE){
              cpueValues$onlyConvertMinBin <- FALSE
              cpueValues$updateStatus <- "valid" #release flag to allow updating
            }else{
              cpueValues$minBin <- input$CPUEMinBin
              cpueValues$initialized <- TRUE #release flag to allow updating on first initialization
              cpueValues$updateStatus <- "valid" #release flag to allow updating
            }
          })
          
      ###Calculate CPUE by length or PSD size class for table
      cpuesizetable <- reactive({
        if(input$cpuesize == TRUE){
        #read in data...I'm doing this to avoid running selData() 2x (once for noTL and again to make cpuesizetable)
        selData <- selData()
        effort_unit_name <- if(selData()$Gear.Code[[1]]>=40){
            "Mean (fish/hr)"
          }else if(selData()$Gear.Code[[1]] == 10){
            "Mean (fish / 100 m²)"
          }else{
            "Mean (Fish / 24hr set)"
          }
        if(CPUEsizeToggle() == "PSD"){
            if(any(selData()$Species.Code %in% gabel$Species.Code)){
              #Find spp for which no TL data were taken
                noTL <- selData %>% group_by(Species.Code) %>% summarise(non_na_count = sum(!is.na(TL_mm)), 
                  .groups = "drop") %>% subset(non_na_count == 0) 
              #Add Spp names using Gabelhouse.Name spelling, make sure everything is character data type (no factors),
                cpuesizetable <-merge(selData, gabel, by = "Species.Code", all.x=T) %>% 
                  mutate_if(is.factor, as.character) %>% 
                #add PSD size classes to each row based on fish lengths/spp
                  mutate(psdval = psdAdd(TL_mm ~ Gabelhouse.Name, verbose = FALSE)) %>%  
                #sum up # fish by spp and psd size class
                  group_by(SampleID, Gear.Code, Effort, Gear.Length, Species.Code, psdval) %>%
                    summarise(numbCaught = sum(Number.of.individuals), .groups = "drop") %>%
                #force 0 into numbCaught variable when spp or size class was not caught at a given sample station (best to convert into a tibble first, then back into dataframe)
                  as_tibble() %>%
                    complete(nesting(SampleID, Gear.Code, Effort, Gear.Length), Species.Code, psdval, fill = 
                               list(numbCaught=0)) %>% as.data.table()  %>%
                #Create proper effort based on gear type as "Effort 2" so can multiply # caught by this to get CPUE
                  mutate(Effort2 = case_when(Gear.Code >= 41 & Gear.Code != 68 ~ 60/Gear.Length,  #EF gears (Number.of.individuals*60)/Gear.Length where gear length is in min. = #/hr
                                 Gear.Code <= 40 & Gear.Code != 10 ~ 24/Effort,  #net gears (Number.of.individuals*24)/Effort where effort is number of net nights= #/24h
                                 Gear.Code == 10 ~ 1076/Effort)) %>%  #Seine (Number.of.individuals*1076)/Effort...3.28084ft in a m, so 3.28084^2 * 100 = 1067, which is a conversion from ft^2 to 100 m^2 
                #Calculate CPUE by SampleID
                  group_by(SampleID, Effort2, Species.Code, psdval) %>% 
                  summarise(CPUEsite = numbCaught*Effort2, .groups = "drop") %>%
                #Remove species code 98 (no species caught) or data with no PSD size class or TL
                  merge(gabel, by = "Species.Code", all.x=T) %>% subset(!is.na(Gabelhouse.Name) & !is.na(psdval) &
                     !Species.Code %in% noTL$Species.Code) %>% select(-Gabelhouse.Name) %>%
                #Add Species.Name and drop Species.Code
                  merge(speciesinfo, by = "Species.Code", all.x=T) %>% select(-Species.Code) %>% relocate(Species.Name) %>%
                #average catch rates across sites (still within psdval groupings) and create RSE/95%CI/etc.
                  group_by(Species.Name, psdval) %>% summarise(Mean = mean(CPUEsite), SD = sd(CPUEsite), Count = n(), 
                     SE=if(SD==0){NA}else{ (SD/sqrt(Count))},  #make SE missing if SD was zero...prevents 95% CI that is 0-0 (and not correct)
                     "L 95% CI" = Mean - (1.96*SE), "U 95% CI" = Mean + (1.96*SE), RSE = (SE/Mean)*100, CV = (SD/Mean)*100,
                     "N RSE = 12.5 (25% range)" = as.integer(round(((CV/12.5)^2),0)), "N RSE = 20 (40% range)" =
                     as.integer(round(((CV/20)^2),0)), .groups = "drop") %>%
                #organize/format for output table (drop unneeded columns and rename things)
                  mutate(psdval2 = case_when(psdval == "substock" ~ "< Stock", psdval == "stock" ~"S-Q", psdval == "quality" ~
                                              "Q-P", psdval == "preferred" ~ "P-M", psdval == "memorable" ~ "M-T", 
                                             psdval == "trophy" ~ "Trophy+")) %>% 
                  select(-SD, -Count, -CV, -psdval) %>% relocate(RSE, .after=Mean) %>% relocate(psdval2, .after=Species.Name) %>% 
                  rename(Species = Species.Name, "Size Category" = psdval2)%>%
                  rename_with(~ effort_unit_name, .cols = "Mean") #need rename_with to use variable for new name
            }
          #CPUE by inch class
          }else if(CPUEsizeToggle() == "Length_class"){ 
            req(cpueValues$updateStatus == "valid", cpueValues$minBin, cpueValues$lengthBin)
            selData <- selData()[!is.na(selData$TL_mm),] #omit fish with no TL data...they will not be needed in CPUE by size class
              if(cpueValues$units == "inch"){
                selData$TL_Inch <- (selData$TL_mm/25.4)
                #Note bin_breaks needs to use cpueValues rather than input$ box values or this will not update correctly              
                bin_breaks <- seq(round(cpueValues$minBin,0), (max(selData$TL_Inch) +  round(cpueValues$lengthBin,1)), 
                                  by = round(cpueValues$lengthBin,1))
                bin_labels <- paste0(bin_breaks[-length(bin_breaks)], "-", bin_breaks[-1] - 0.1)
                selData$Lclass <- cut(selData$TL_Inch, breaks = bin_breaks, labels = bin_labels,
                                      include.lowest = TRUE, right = FALSE)
              }else{
                #Note bin_breaks needs to use cpueValues rather than input$ box values or this will not update correctly 
                bin_breaks <- seq(round(cpueValues$minBin,0), max(selData$TL_mm) + round(cpueValues$lengthBin,0), 
                                          by = round(cpueValues$lengthBin,0))
                bin_labels <- paste0(bin_breaks[-length(bin_breaks)], "-", bin_breaks[-1] - 1)
                selData$Lclass <- cut(selData$TL_mm, breaks = bin_breaks, labels = bin_labels,
                                      include.lowest = TRUE, right = FALSE)
              }
              cpuesizetable <- selData %>% group_by(SampleID, Gear.Code, Effort, Gear.Length, Species.Code, Lclass) %>% #note SampleID has Station number so this is grouped by site
                summarise(numbCaught = sum(Number.of.individuals), .groups = "drop") %>%
                #force 0 into numbCaught variable when spp or size class was not caught at a given sample station (best to convert into a tibble first, then back into dataframe)
                as_tibble() %>%
                complete(nesting(SampleID, Gear.Code, Effort, Gear.Length), Species.Code, Lclass, fill = 
                           list(numbCaught=0)) %>% as.data.table()  %>%
                #Create proper effort based on gear type as "Effort 2" so can multiply # caught by this to get CPUE
                mutate(Effort2 = case_when(Gear.Code >= 41 & Gear.Code != 68 ~ 60/Gear.Length,  #EF gears (Number.of.individuals*60)/Gear.Length where gear length is in min. = #/hr
                                           Gear.Code <= 40 & Gear.Code != 10 ~ 24/Effort,  #net gears (Number.of.individuals*24)/Effort where effort is number of net nights= #/24h
                                           Gear.Code == 10 ~ 1076/Effort),#Seine (Number.of.individuals*1076)/Effort...3.28084ft in a m, so 3.28084^2 * 100 = 1067, which is a conversion from ft^2 to 100 m^2 
                       CPUEsite = numbCaught*Effort2) %>% 
                #Remove species code 98 (no species caught) then replace species.Code with Species.Name
                subset(Species.Code != 98) %>% merge(speciesinfo, by = "Species.Code", all.x=T) %>% 
                select(-Species.Code) %>% relocate(Species.Name) %>%
                #average catch rates across sites (still within psdval groupings) and create RSE/95%CI/etc.
                group_by(Species.Name, Lclass) %>% summarise(Mean = mean(CPUEsite), SD = sd(CPUEsite), Count = n(), 
                   SE=if(SD==0){NA}else{ (SD/sqrt(Count))},  #make SE missing if SD was zero...prevents 95% CI that is 0-0 (and not correct)
                   "L 95% CI" = Mean - (1.96*SE), "U 95% CI" = Mean + (1.96*SE), RSE = (SE/Mean)*100, CV = (SD/Mean)*100,
                   "N RSE = 12.5 (25% range)" = as.integer(round(((CV/12.5)^2),0)), "N RSE = 20 (40% range)" =
                     as.integer(round(((CV/20)^2),0)), .groups = "drop") %>%
                #organize/format for output table (drop unneeded columns and rename things)
                select(-SD, -Count, -CV) %>% relocate(RSE, .after=Mean) %>% relocate(Lclass, .after=Species.Name) %>% 
                #now omit Lclass values larger than the biggest length class with a CPUE>0
                  group_by(Species.Name) %>% mutate(last_nonzero = max(which(Mean > 0))) %>%
                  filter(row_number() <= last_nonzero) %>% select(-last_nonzero) %>% 
                rename(Species = Species.Name, "Size Class" = Lclass)%>%
                rename_with(~ effort_unit_name, .cols = "Mean") #need rename_with to use variable for new name
              return(cpuesizetable)
          }
        }
      })
      
        #create CPUE by PSD size or length class table to display
        output$cpuebysize <- renderTable(digits = 2, spacing = "xs", na=" ", {
          if(CPUEsizeToggle() == "PSD"){
              if(any(selData()$Species.Code %in% gabel$Species.Code)){
                if(input$cpuesize == TRUE){
                  withProgress(message = "Doing Math for You",min=0,max=10,value=1, {
                    cpuesizetable()
                  })
                }
              } #don't need else here as I deal with not having spp with PSD classes in CPUEbySizeTable renderUI below
          }else{ #If using CPUE by length class
            if(input$cpuesize == TRUE){
              withProgress(message = "Doing Math for You",min=0,max=10,value=1, {
                cpuesizetable()
              })
            }
          }
        })
        
        #handle interface for above size reference tables and CPUE by PSD size class so can hide if no spp has PSD size classes
        output$CPUEbySizeTable <- renderUI({
          if(CPUEsizeToggle() == "PSD"){
            if(any(selData()$Species.Code %in% gabel$Species.Code)){
              tagList(
                h5("Values in the table below are catch rates within PSD ranges. If you would like to know the CPUE for
                   fish a given size and larger, simply sum the CPUE values for the ranges of interest. For example, to
                   get the average CPUE of Preferred and larger fish, sum the CPUE from P-M, M-T, and Trophy+."),
                hr(),
                h4(helpText("CPUE by PSD Size Category")),
                tableOutput("cpuebysize"),
                hr(),
                checkboxInput("show_psd_refs", "Show PSD Size Category References for all species in above table", FALSE),
                uiOutput("psd_references")
              )
            }else{ #not a spp with PSD size class
              helpText("There are no species caught in this sample that have defined PSD size classes, 
                       so CPUE cannot be viewed in this way. You can press the button above to view by inch or mm
                       size class instead.")
            }
          }else{# CPUE by inch/mm class tagList 
            tagList(
              h5("If you would like to know the CPUE for fish of a given size and larger, simply sum the CPUE values for
              the ranges of interest. For example, to get the average CPUE 15 inch and larger fish, sum the CPUE from 15-15.9,
                 16-16.9,17-17.9, etc. through the largest length class."),
              hr(),
              h4(helpText(paste0("CPUE by ",cpueValues$units," Class"))),
              tableOutput("cpuebysize")
            )
          }
        })
        
        # Create a reactive expression for displaying the PSD size references
        output$psd_references <- renderUI({
          if(input$show_psd_refs==TRUE) {
            tagList(
              hr(),
              h4(helpText("PSD Size Category References (mm)")),
              tableOutput("sizerefmm"),
              h4(helpText("PSD Size Category References (in)")),
              tableOutput("sizerefin")
            )
          } else {
            NULL
          }
        })
    
    ##Catch Tab Download buttons##########################
      # Downloadable csv of total cpue table
        output$downcpue1 <- downloadHandler(
          filename = function() {
            paste(input$selectlake,input$selectyear,input$selectgear, "totalcpue", "csv", sep = ".")
          },
          content = function(file) {
            write.csv(cpuetable(), file, row.names = FALSE)
          }
        )
        #second version needed so can show/hide with div's in ui.r (using 2 versions of button to make this easy)
        output$downcpue2 <- downloadHandler(
          filename = function() {
            paste(input$selectlake,input$selectyear,input$selectgear, "totalcpue", "csv", sep = ".")
          },
          content = function(file) {
            write.csv(cpuetable(), file, row.names = FALSE)
          }
        )
      # Downloadable csv of total cpue table
      output$downcpuebysize <- downloadHandler(
        filename = function() {
          paste(input$selectlake,input$selectyear,input$selectgear, "cpuebysize", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(cpuesizetable(), file, row.names = FALSE)
        }
      )
       
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Single Species Analysis Tab#########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
  #sample summary line
    output$sampleline <- renderText({
      #retrieve name(s)
      lake <- lakename()
      year <- yearname()
      gear <- gearname()
      species <- speciesname()
      
      sampleline <- paste(lake, year, gear, species, sep = " - ")
    })
  


  #display text showing matched or unmatched age dataset and sample size
    #show uploaded data in use
    output$uploadedAgeData <- renderText({
      if(input$loadageCheck == TRUE){
        uploadedAgeData <- as.character("User Uploaded Independent Dataset for Age Data")
      }
    })
    #display text showing matched dataset in green
    output$ageDataMatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$toggleCodeName == TRUE){
            if(all(input$selectspecies == input$selagespp) &&
               all(selDataspp()$Lake.Code == c(input$selagelake)) &&
               all(selDataspp()$Year == c(input$selageyears))){
              ageDataMatch <- as.character("Matched Age Dataset was used")
            }
        }else{
            if(all(input$selectspecies == input$selagespp) &&
               all(selDataspp()$lake.Name_Code == c(input$selagelake)) &&
               all(selDataspp()$Year == c(input$selageyears))){
              ageDataMatch <- as.character("Matched Age Dataset was used")
            }
        }
      }
    })
    #display text showing not a matched age dataset in red
    output$ageDataNoMatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$toggleCodeName == TRUE){
          if(any(input$selectspecies != input$selagespp) ||
             any(selDataspp()$Lake.Code != input$selagelake) ||
             any(selDataspp()$Year != input$selageyears)){
            ageDataNoMatch <- as.character("Age Dataset was NOT a Match to Sample Dataset")}
        }else{
          if(any(input$selectspecies != input$selagespp) ||
             any(selDataspp()$lake.Name_Code != input$selagelake) ||
             any(selDataspp()$Year != input$selageyears)){
            ageDataNoMatch <- as.character("Age Dataset was NOT a Match to Sample Dataset")}
        } 
      }
    })
    #Age dataset sample size display
    output$ageDataCount <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        ageDataCount <- length(selageDatafinal()$Age)
        ageDataCount <- paste(as.character("Age data sample size: N = "), ageDataCount, sep = "")
      }  
    })
    output$sampleDataSppCount <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp) && 
         !is.null(input$selectspecies)){
            sampleDataSppCount <- length(!is.na(selDataspp()$TL_mm))
            sampleDataSppCount <- paste(as.character("Sampling data sample size: N = "), 
                                    sampleDataSppCount, sep = "")
      }  
    })
    
  ##Length Frequency Histogram####################################
    #create lengthbin numericInput
    output$lengthbin_ui <- renderUI({
      req(selDataspp())  
      range_diff <- max(selDataspp()$TL_mm, na.rm = TRUE) - min(selDataspp()$TL_mm, na.rm = TRUE)
      if (input$inchLF==FALSE) {# units in mm
          default_val <- if (range_diff <= 500) {
            10 #use default bin of 10 if fish span no more than 500 mm
          } else if (range_diff <= 1199) {
            25 #use bin=25 if 501-1199mm
          } else {
            50 #use bin=50 for fish >1200
          }
      }else{ #units are inches, but will still evaluate sizes using TL_mm for break points
        default_val <- if (range_diff <= 500) {
          0.5 #use default bin of 0.5" if fish span no more than 500 mm
        } else if (range_diff <= 1199) {
          1 #use bin=1" if 501-1199mm
        } else {
          2 #use bin=2"0 for fish >=1200
        }
      }
      numericInput(inputId = "lengthbin", label = "Length Bin Grouping (mm)", value = default_val,
        min = 1, max = 200, width = '200px')
    })
    
    #Deal with conversion between inches and mm
      inchLFupdate <- reactiveValues(in_mm_LF = FALSE) #use as flag to prevent figure updating too soon
      observe(inchLFupdate$Lbin  <- input$lengthbin) #
    observeEvent(input$inchLF, {
      if (!is.null(input$inchLF)) {
        if (input$inchLF==TRUE) {# Convert to inches 
          new_value <- input$lengthbin / 25.4
          new_label <- "Length Bin Grouping (inch)"
          inchLFupdate$in_mm_LF = TRUE
        } 
        if (input$inchLF==FALSE) {# Convert from inch to mm
          new_value <- input$lengthbin * 25.4
          new_label <- "Length Bin Grouping (mm)"
          inchLFupdate$in_mm_LF = FALSE
        }
        # Update the value in lengthbin numericInput box
        updateNumericInput(session, "lengthbin", value = new_value, label = new_label)
        inchLFupdate$Lbin <- new_value #used as flag to indicate this converting of length bin box is complete
      }
    }, ignoreInit = TRUE) #tells this to ignore on initialization
    
    #calculate minor and major x-axis breaks that fall along bin sizes
    LF_breaks <- reactive({
      req(!is.null(input$inchLF))
      #create L-freq data (duplicate code in function below)
        if(inchLFupdate$in_mm_LF == TRUE){
          LFreqData <- selDataspp() %>% mutate(TL_in = TL_mm/25.4)
          x_var <- quo(TL_in) #saves text of variable name as quosure so I can ref x_var and get TL_in if using inches and TL_mm if using mm
        } else {
          LFreqData <- selDataspp()
          x_var <- quo(TL_mm)
        }
      max_val <- max(pull(LFreqData, !!x_var), na.rm = TRUE) #uses x_var quosure so it pulls max length from whichever column (TL_mm or TL_inch) is in the current units
      min_val <- min(pull(LFreqData, !!x_var), na.rm = TRUE) #uses x_var quosure so it pulls max length from whichever column (TL_mm or TL_inch) is in the current units
      current_unit <- input$inchLF #get current units selected by user...just here to force reactive to update if units change
          bin_size <- inchLFupdate$Lbin  # get current bin size...and ensure this updates if units change by putting it after above unit line
      
      # Define break logic based on unit system
      if(input$inchLF == T) { #inches
        steps <- c(1, 2, 5, 10) #Preferred division breaks for inches
      } else { #mm units
        steps <- c(10, 25, 50, 75, 100, 150, 200) #Prefer division breaks for mm
      }
      # Calculate optimal step size for major breaks
      raw_step <- ((max_val - min_val) / 7)  # Aim for ~7 major breaks for range of lengths displayed...this calculates exact step size needed for 7 breaks
      step <- steps[which.min(abs(steps - raw_step))] #find preferred division closest to but smaller than raw_step
      step <- max(round(step / bin_size) * bin_size, bin_size)# Ensure step is multiple of bin size and is no smaller than bin size
      
      # Generate major breaks (using step) and minor breaks (using bin size)
      major_minor_brks <- list(major = seq(0, ceiling(max_val / step) * step, by = step),
           minor = seq(0, max_val + bin_size, by = bin_size))
      
      return(major_minor_brks)
    })
    
    #function to make figure (making function works better than a reactive better for downloading)
    lfplot <- function(){
      input$inchLF #add dependency
      req(inchLFupdate$Lbin, LF_breaks())
      if(input$lengthfrequency == TRUE){
      #build ggplot length frequency
        if(inchLFupdate$in_mm_LF == TRUE){
          LFreqData <- selDataspp() %>% mutate(TL_in = TL_mm/25.4)
          x_var <- quo(TL_in) #saves the name of the length variable as quosure so when I graph I don't need separate code for both mm and inch versions...this x_var will be read as TL_mm if I'm in mm and TL_inch if I"'m in inches
          x_label <- "Total Length (inch)"
        } else {
          LFreqData <- selDataspp()
          x_var <- quo(TL_mm)
          x_label <- "Total Length (mm)"
        }
        
        req(nrow(LFreqData) > 0)
        ##Build ggplot##
          #create function to decide if decimal places are needed on x-axis
            custom_labels <- function(x){sapply(x, function(val){#x is each x-axis label...which is then fed to the sapply function as "val"
                if (is.na(val)) return(NA_character_) #look for NA's and return text string of NA if found...will avoid crashing program
                val_rounded <- round(val,1) #round to 1 decimal...needed for format to work properly
                if (val_rounded %% 1 == 0) { #check to see if val_rounded is an integer...if so, returns nsmall=0 (integer format), if not return nsmall=1 (1 decimal format)
                  format(val_rounded, nsmall = 0)
                } else {
                  format(val_rounded, nsmall = 1)
                }
            })}
        Lfreq <- ggplot(data=LFreqData, aes(x=!!x_var)) +
          geom_histogram(aes(y = after_stat(count/sum(count) * 100), fill="bar_fill", color="bar_outline"), binwidth=inchLFupdate$Lbin, boundary=0,
                         closed="left") +
          scale_fill_manual(values = c("bar_fill" = "grey80"), guide = "none") +
          scale_color_manual(values = c("bar_outline" = "black"), guide = "none") +
          scale_y_continuous(name="Percent of Fish", expand=expansion(mult=c(0,0.05)),
                             labels = function(x) paste0(x, "%")) +
          scale_x_continuous(name=x_label, breaks = LF_breaks()$major, minor_breaks = LF_breaks()$minor,labels = custom_labels,
                             guide = guide_axis(minor.ticks = TRUE),expand=expansion(mult=c(0.05,0.05))) +
          coord_cartesian(clip = "off")+
          theme_cowplot(font_size = 20)+
            theme(axis.ticks.length = unit(9, "pt"), axis.minor.ticks.length = rel(0.4),
                  axis.minor.ticks.x.bottom = element_line(colour = 'grey35'), axis.ticks=element_line(linewidth=0.5),
                  axis.minor.ticks=element_line(linewidth=0.1), plot.margin = margin(t = 3, r = 18, b = 3, l = 5))###size vs linewidth
         #above theme statements sometimes produces warning about theme element not in hierarchy, but has to come after 
         #cowplot or won't work...so I'm suppressing the warning where this graph gets printed with suppressWarnings()
        if(input$addSPPfig == TRUE){
          plot_build <- ggplot_build(Lfreq)# Get the plot build information
          hist_data <- plot_build$data[[1]] # Extract histogram data
          hist_data$height_percent <- hist_data$count / sum(hist_data$count) * 100 # Calculate percentage heights
          # Find the best location for annotation
            n_bins <- nrow(hist_data)
            # estimate size of annotation (species name) and convert this to how many bins wide it is on X-axis
              text_width_inch <- convertWidth(stringWidth(speciesname()), "inches", valueOnly = TRUE)
                x_range <- diff(layer_scales(Lfreq)$x$range$range)
                plot_width_inch <- convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)
                text_width_x_units <- (text_width_inch / plot_width_inch) * x_range
                bins_spanned <- text_width_x_units / inchLFupdate$Lbin
                annotation_width <- ceiling(bins_spanned)
            height_threshold <- 0.8 * max(hist_data$height_percent) #how high a bar is before considering it in the way for the annotation
           #iterate through each group of bin widths representing annotation width starting 1 bin width right of the y-axis to leave a little space as min to left of spp name before Y-axis
            best_score <- Inf
            current_start <- round(1+annotation_width/2,0) #set this to starting bin
            current_end <- current_start
            rangeList <- data.frame(start=NULL,end=NULL)
            for(i in 1:(n_bins - annotation_width + 1)) {
              region <- hist_data[i:(i+annotation_width-1), ]
              # Count bins over threshold and weight by their heights
                bins_over_threshold <- region$height_percent > height_threshold
                weighted_count <- sum(region$height_percent[bins_over_threshold] / 100)
              if(weighted_count < best_score) {
                  current_start <- i
                  current_end <- i
                  best_score <- weighted_count
                  range_ended <- FALSE
              } else if(weighted_count == best_score) {
                if(range_ended ==FALSE){
                  current_end <- i
                }else{
                  current_start <- i
                  current_end <- i
                  range_ended <- FALSE
                }
              }else{
                  rangeList <- rbind(rangeList, data.frame(start=current_start, end=current_end))
                  range_ended <- TRUE
              }
            }
            #need to rbind result of last loop  if it did not trigger above
              rangeList <- rbind(rangeList, data.frame(start=current_start, end=current_end))
            # Calculate the center of the best region
              if(length(rangeList)>0){
                rangeList <- rangeList %>% mutate(length = end - start + 1) %>% 
                  filter(length==max(length))
                best_start <- rangeList$start[1]#specify 1st element in data frame in casethere is more than one (i.e., forcing an rbind to this at end of loop might duplicate last row in some cases)
                best_end <- rangeList$end[1]
                center_bin <- (best_start + best_end) / 2
                best_x <- hist_data$x[round(center_bin)]
              } else {
                best_x <- mean(hist_data$x)  # Fallback if no best region found
              }
              
            # print(paste("annotation_width: ",annotation_width, "unit/bin: ", (hist_data$x[10]-hist_data$x[1])/10))
          best_y <- max(hist_data$height_percent) * 0.96  # Place at 90% of max height
          Lfreq <- Lfreq +
          #add spp label at upper right
            annotate("text", x = best_x, y = best_y, label = speciesname(), hjust = 0.5,  #hjust=0.5 is centered
                     vjust = 1, color = "black", size = 9)
        }
      return(Lfreq)
      } 
    }
    
  #render the length frequency plot
  output$lengthhist <- renderPlot(bg="transparent",{
    if(input$lengthfrequency == TRUE){
      withProgress(message = "Doing Math for You",min=0,max=10,value=1, {
        # lfplot()
        suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
          lfplot <- lfplot() +
            scale_fill_manual(values = "#404688FF", guide = "none") +#481F70FF
            scale_color_manual(values = "#FDE725FF", guide = "none")
        })
        suppressWarnings(print(lfplot)) #suppress warnings about axis.minor.ticks not being in cowplot theme
      })
    }
  })
  
  #create table with length frequency data
  lftable <- reactive({
    if(input$lengthfrequency == TRUE){
    ##CODE TO MATCH GGPLOT VERSION
    #this uses same binning method the ggplot code above does so they will match...change these
    #together when ready to roll out ggplot version of this histogram (still need to deal with inches/mm)
      if(inchLFupdate$in_mm_LF == TRUE){
        LFreqData <- selDataspp() %>% mutate(TL_in = TL_mm/25.4)
        lftable <- data.frame(bin = cut(LFreqData$TL_in,
            breaks = seq(floor(min(LFreqData$TL_in)/inchLFupdate$Lbin)*inchLFupdate$Lbin,
            ceiling(max(LFreqData$TL_in)/inchLFupdate$Lbin)*inchLFupdate$Lbin,
            by = inchLFupdate$Lbin), right = FALSE)) %>%
          group_by(bin) %>%
          summarise(count = n(), percentage = n()/nrow(LFreqData) * 100) %>%
          arrange(bin)
        return(lftable)
      }
      if(inchLFupdate$in_mm_LF == FALSE){
        lftable <- data.frame(bin = cut(selDataspp()$TL_mm,
            breaks = seq(floor(min(selDataspp()$TL_mm)/inchLFupdate$Lbin)*inchLFupdate$Lbin,
            ceiling(max(selDataspp()$TL_mm)/inchLFupdate$Lbin)*inchLFupdate$Lbin,
            by = inchLFupdate$Lbin), right = FALSE)) %>%
          group_by(bin) %>%
          summarise(count = n(), percentage = n()/nrow(selDataspp()) * 100) %>%
          arrange(bin)
        return(lftable)
      }
    }
  })
  
  #download button for length frequency table data
  output$downlftable <- downloadHandler(
    filename = function() {
      paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
            "lengthfreq", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(lftable(), file, row.names = FALSE)
    }
  )

  ##PSD Tables####################################################
  
    #reactive statements to create table with Gabelhouse names needed for psd category and psd value tables
    #Creating these as reactive statements will save processing power as they are used multiple times and this
    #will save the answer for later re-use.
      gabelseldata <- reactive({
            gabelseldata <- merge(selDataspp(), gabel, by="Species.Code", all.x = TRUE)
      })
    
      gabelname <- reactive({
            gabelname <- as.character(gabelseldata()[1,"Gabelhouse.Name"])
            return(gabelname)
      })
  
    #psd category length reference table
    output$psdvaltable <- renderTable(digits=1, rownames=FALSE, spacing="xs", {
      if(!(selDataspp()$Species.Code[1] %in% gabel$Species.Code)){#if species.code is not gabel file, should not return table (causes error if no matching PSD size classes that are defined)
        standardeq <- NULL
      }else{
        if(input$psd == TRUE){
          psdvals <- as.data.frame(psdVal(gabelname()))
          colnames(psdvals) <- "(mm)"
          names <- rownames(psdvals)
          psdvals <- mutate(psdvals, "(in)" = psdvals$`(mm)`/25.4)
          psdvals <- as.data.frame(t(psdvals))
          psdvals["TL"] <- row.names(psdvals)
          psdvals <- psdvals[c(7,1:6)]
          colnames(psdvals)[2:7] <- names
          psdvals <- psdvals
        }
      }
    })
 
    #actual psd values table
    psdfinal <- reactive({
      # write.csv(gabelseldata(),"TestOutput_gabelseldata.csv", row.names = F)
      if(!(selDataspp()$Species.Code[1] %in% gabel$Species.Code)){#if species.code is not in gabel file, should not return table (causes error if no matching PSD size classes that are defined)
         standardeq <- NULL
      }else{
        if(input$psd == TRUE){
          psdvals <- as.data.table(psdVal(gabelname())) #do I need this?
          psdCalc <- as.data.table(psdCalc(~TL_mm, gabelseldata(), gabelname(), units="mm",
                     method="multinomial", what="all", showIntermediate = FALSE), keep.rownames = "PSDabrev") 
          # Rename PSD-Q to PSD if it exists
          if ("PSD-Q" %in% psdCalc$PSDabrev) {
            psdCalc[PSDabrev == "PSD-Q", PSDabrev := "PSD"]
          }
          setnames(psdCalc, old = c("Estimate", "95% LCI", "95% UCI"),
                   new = c("PSD Value", "L 95% CI", "U 95% CI"), skip_absent = TRUE)
          # Merge with SortPSDabrv to ensure all categories are present and allow us to sort properly
          psdCalc <- merge(SortPSDabrv, psdCalc, by = "PSDabrev", all.x = TRUE)
          # Fill missing Estimate values with 0 for PSD Value, leave CI columns as NA
          if ("PSD Value" %in% names(psdCalc)) {
            set(psdCalc, which(is.na(psdCalc[["PSD Value"]])), "PSD Value", 0)
          }
          # Sort and remove unneeded columns
          setorder(psdCalc, sortOrder)
          psdfinal <- psdCalc[, .(PSDabrev, `PSD Value`, `L 95% CI`, `U 95% CI`)]
          if(speciesname()=="Gizzard Shad"){
            psdfinal <- psdfinal[PSDabrev != "PSD-P" & PSDabrev != "PSD-M" & PSDabrev != "PSD-T" & PSDabrev != "PSD Q-P" &
                       PSDabrev != "PSD P-M" & PSDabrev != "PSD M-T"]
          }
          psdfinal <- as.data.frame(psdfinal)
          rownames(psdfinal) <- psdfinal$PSDabrev
          psdfinal$PSDabrev <- NULL
          if ("PSD-T" %in% rownames(psdfinal)) { #create PSD T+ category at bottom of table
            psd_t_row <- psdfinal["PSD-T", , drop = FALSE]  # Keep as data.frame
            rownames(psd_t_row) <- "PSD T+"  # Rename row
            psdfinal <- rbind(psdfinal, psd_t_row)  # Append to bottom
          }
          return(psdfinal)
        }
      }
    })
  #render psd table
  output$psdtable <- renderTable(rownames=TRUE, spacing="xs",digits=0,{
    if(input$psd == TRUE){
      psdfinal()
    }
  })
  
  #create conditional helptext for PSD output to hide it if PSD has not been defined for a given spp
  output$PSDoutputDetails <- renderUI({
    if(!(selDataspp()$Species.Code[1] %in% gabel$Species.Code)){
      helpText({paste("There is no PSD information to display because ", speciesname(), " does not have defined PSD size classes")})
    }else{
      tagList(
        h4(helpText(textOutput("speciesref"))),
        tableOutput("psdtable"),
        downloadButton("downpsd", "PSD Table"),
        br(),
        br(),
        br(),
        helpText("References of lengths for PSD size categories"),
        tableOutput("psdvaltable"),
      )
    }
  })
    
  ##Relative Weight Table#########################################
    
    #create standard weight equation for reference
    output$standardequation <- renderTable(rownames = FALSE, digits = 3,{
      if(input$wr == TRUE){
        if(!(selDataspp()$Species.Code[1] %in% wsnames$Species.Code)){#if species.code is not in wsnames or not a single spp, should not return table (causes error if no matching Ws data)
          standardeq <- NULL
        }else{
          standard <- merge(selDataspp(), wsnames, by="Species.Code", all.x = TRUE)
          wsName <- as.character(standard[1,"wsname"]) 
          standardeq <- wsVal(wsName)
          standardeq <- standardeq[c(1,3,4,7:10)]
          colnames(standardeq) <- c("Species","Model Type","Reference Percentile","Min.TL",
                                    "Intercept (a')","Slope (b)","Source")
          standardeq <- mutate(standardeq, Min.TL = as.integer(Min.TL))
        }
        return(standardeq)
      }
    })  
  
    #create conditional helptext for standard weight equation above so we can hide it if no Ws exists for spp
    output$WsEqDetail <- renderUI({
      if(!(selDataspp()$Species.Code[1] %in% wsnames$Species.Code)){
        helpText({paste("Relative weight can only be calculated for species with a standard weight equation.  
                You selected '", speciesname(), "',  which does not have a defined standard weight.")})
      }else{
        tagList(
          hr(),
          helpText("Details of standard weight equation"),
          tableOutput("standardequation")
        )
      }
    })
    
    table <- reactive({
      if(input$wr == TRUE){
        if(!(selDataspp()$Species.Code[1] %in% wsnames$Species.Code)){#if species.code is not in wsnames or not a single spp, should not return table (causes error if no matching Ws data)
          table <- NULL
        }else{
            wrpsdjoindata <- merge(gabelseldata(), wsnames, by="Species.Code", all.x = TRUE)
            #Calculate relative weight and append to selected data
            wrvalue <- wrAdd(Wt_g ~ TL_mm + wsname, units="metric", data = wrpsdjoindata)
            wrpsdjoindata$wrvalue <- wrvalue
            if((selDataspp()$Species.Code[1] %in% gabel$Species.Code)){#only calculate by PSD size class if PSD categories are defined
              #Calculate psd size category and append to selected data
              psdval <- psdAdd(TL_mm~Gabelhouse.Name, units="mm", data=wrpsdjoindata)
              wrpsdjoindata$psdvalue <- psdval
              #calculated mean, sd, and count aggregated by psd size class
              wrdfs <- list(
                aggwrmeans = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, mean, na.action = na.omit),
                aggwrsds = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, sd, na.action = na.omit),
                aggwrcounts = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, length, na.action = na.omit)
              )
              joinedall <- cbind(wrdfs$aggwrmeans, wrdfs$aggwrsds[2], wrdfs$aggwrcounts[2])
              colnames(joinedall)[2:4] <- c("Mean", "St.Dev.", "Num")
              
              #Calculate standard error and upper/lower confidence levels
              witherror <- mutate(joinedall, Std.Error = (St.Dev./sqrt(Num)))
              calculate <- mutate(witherror, Lower.95.CI = Mean - (1.96*Std.Error),
                                  Upper.95.CI = Mean + (1.96*Std.Error),
                                  CV = (St.Dev./Mean)*100)
              calculate <- calculate[c(1,2,4,8,5,6,7)]
              colnames(calculate) <- c("Size Category","Mean Wr","Count","CV","SE","L 95% CI","U 95% CI")
              wtable <- calculate
           }else{
              wtable <- NULL
           }
          
          #Calculate overall Wr
          alldata <- wrpsdjoindata[!(is.na(wrpsdjoindata$wrvalue)),]
          allmean <- mean(alldata$wrvalue, na.rm = TRUE)
          allsd <- sd(alldata$wrvalue, na.rm = TRUE)
          allcount <- length(alldata$wrvalue)
          overall <- as.character("Overall")
          all <- data.frame(overall,allmean,allsd,allcount)
          colnames(all) <- c("Size Category","Mean","St.Dev.","Num")
          all <- mutate(all, Std.Error = (St.Dev./sqrt(Num)))
          all <- mutate(all, Lower.95.CI = Mean - (1.96*Std.Error),
                        Upper.95.CI = Mean + (1.96*Std.Error),
                        CV = (St.Dev./Mean)*100)
          all <- all[c(1,2,4,8,5,6,7)]
          colnames(all) <- c("Size Category","Mean Wr","Count","CV","SE","L 95% CI","U 95% CI")
          
          table <- rbind(all, wtable)
        }
        return(table)
      }
    })
    
    #render the relative weight table
    output$wrtable <- renderTable(digits=2, rownames=FALSE, spacing="xs", {
      if(input$wr == TRUE){
        table()
      }
    }) 
    
    #renderUI for ui.r code to display hr(), Wr table, and download button only if not species = 108 (all crappie spp)
    output$wrTableAndButton <- renderUI({
      if(speciesname() == "All crappie spp. combined"){
        NULL
      }else{
        tagList(
          hr(),
          tableOutput("wrtable"),
          downloadButton("downwr", "Relative Weight Table")
        )
      }
    })
    
  ##Length-Weight Regression Plot###################################
    lwreg <- function(){
      if(input$lwregression == TRUE){
          completelw <- subset(selDataspp(),!is.na(Wt_g)) #remove records with NA weight
          model <- lm(log10(Wt_g) ~ log10(TL_mm), data = completelw)
          eq_label <- sprintf("y = %.4f + %.4fx", coef(model)[1], coef(model)[2])# Create label for equation, note %.4f indicates 4 decimal rounding
          lwreg <- ggplot(data = completelw, aes(x = log10(TL_mm), y = log10(Wt_g))) +
            geom_point(shape = 1, size = 4) +
            geom_smooth(aes(fill = "ci_band"),formula = y ~ x, method = "lm", se = T, color = "black", linewidth = 1.5) +
            scale_fill_manual(values = c("ci_band" = "grey80"), guide = "none")+
            annotate("text", x = log10(max(completelw$TL_mm)), y = log10(min(completelw$Wt_g))*1.01, 
                     label = eq_label, hjust = 1, vjust = 0, size = 7, parse = F)+
            labs(x = "Log TL (mm)", y = "Log Weight (g)") +
            theme_cowplot(font_size = 20) 
          #add spp label if user clicked box
          if(input$addSPPfig == TRUE){
            lwreg <- lwreg +
                #add spp label at upper left
                annotate("text", x = log10(min(completelw$TL_mm)), y = log10(max(completelw$Wt_g)), 
                        label = speciesname(), hjust = 0, vjust = 1, color = "black", size = 9) 
          }
          return(lwreg)
      }
    }
   
  #Render LW regression plot
    output$lwplot <- renderPlot(bg="transparent", {
      if(input$lwregression == TRUE){ 
        # print(lwreg())
        suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
          lwreg2 <- lwreg()+
          scale_fill_manual(values = c("ci_band" = "#FDE725FF"), guide = "none")
        })
        print(lwreg2)
      }
    })
    
  #Output the length-weight regression coefficients
    output$lwcoef <- renderTable(digits=3, rownames = TRUE, {
      if(input$lwregression == TRUE){
        #if weight is NA, remove entire record
          completelw <- subset(selDataspp(),!is.na(Wt_g)) 
        #take log of each TL and wt
          varlength <- log10(selDataspp()$TL_mm)
          weight <- log10(selDataspp()$Wt_g)
        #build regression model
          lwmodel <- lm(weight~varlength, na.action = na.omit)
        #Pull out regression coefficients and make matrix for table
          coef1 <- coefficients(lwmodel)
          coefmatrix <- matrix(coef1, nrow=2, ncol=1, byrow=TRUE,
                               dimnames=list(c("Intercept", "a'"), c("Coefficients")))
          round(coefmatrix,3)
      }
    })
    
  ##Download buttons##################
      #Download png of length frequency plot
      output$downlfplot <- downloadHandler(
        filename = function(){ 
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "LFplot", "png", sep = ".")},
        content = function(file){
          # png(file, width = 600, height = 400)
          png(file, width = 800, height = 453)
          print(lfplot())
          dev.off()
        })
      
      #Downloadable csv of PSD table
      output$downpsd <- downloadHandler(
        filename = function() {
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "psd", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(psdfinal(), file, row.names = TRUE)
        }
      )
      
      #Downloadable csv of relative weight table
      output$downwr <- downloadHandler(
        filename = function() {
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "Wr", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(table(), file, row.names = FALSE)
        }
      )
      
      #Download png of length weight regression plot
      output$downLWregplot <- downloadHandler(
        filename = function(){ 
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "LWregplot", "png", sep = ".")},
        content = function(file){
          png(file, width = 600, height = 450)
          print(lwreg())
          dev.off()
        })
      
      
    ##Max length and weight stats#%%%%%%%%%%%%%%%%%%%%%%
      output$maxspptab <- renderTable(digits = 2,{
        if(input$max == TRUE){
          #pull max length and weight from selected data
          sppmax <- as.data.frame(max(selDataspp()$TL_mm, na.rm = NA))
          sppmaxw <- as.data.frame(max(selDataspp()$Wt_g, na.rm = NA))
          sppmax["maxw"] <- sppmaxw
          sppmaxtable <- as.data.frame(sppmax)
          colnames(sppmaxtable)[1] <- "maxl"
          #calculate max length and weight in inches and pounds and round all columns as appropriate
          sppmaxunits <- mutate(sppmaxtable,`Max TL (mm)` = as.character(round(maxl,0)),
                                `Max Wt (g)` = as.character(round(as.integer(maxw), 0)),
                                `Max TL (in)` = as.character(round(maxl * 0.0393701, 1)),
                                `Max Wt (lbs)` = as.character(round(maxw * 0.00220462, 1))
                              )
          sppmaxfinal <- sppmaxunits[, c("Max TL (mm)", "Max Wt (g)", "Max TL (in)", "Max Wt (lbs)")] # Select only formatted columns
          return(sppmaxfinal)
        }
      })
      
      #show or hide max length/wt metrics
      observeEvent(input$max,{
        if(input$max == T){
          show("MaxLnWt")
        }else{
          hide("MaxLnWt")
        }
      })
      
      
      
  ##Population Dynamics Column###############################################
    
    #Calculate observed age-length key ###
      alk_bin <- reactive({
        req(selageDatafinal())
        #create variable for length groupings based on max TL in sample (10,15,or 20 mm)
        if((max(selageDatafinal()$TL_mm)/30)>=20){
          w <- 20
        }
        if((max(selageDatafinal()$TL_mm)/30)>=15 & (max(selageDatafinal()$TL_mm)/30)<19.9999){
          w <- 15
        }
        if((max(selageDatafinal()$TL_mm)/30)<14.9999){
          w <- 10
        }
        return(w)
        })
      
      alkobserved <- reactive({
        req(alk_bin())
      #create length category variable
      agelencat <- lencat(~TL_mm, w=alk_bin(), startcat = 0, right = FALSE, as.fact = TRUE, drop.levels = FALSE,
                          use.names = FALSE, data = selageDatafinal())
      #count frequency of ages within each length category - create proportion table
      alkfreq <- xtabs(~LCat+Age, data = agelencat)
      alk <- prop.table(alkfreq, margin = 1)
      }) 
      
  ###Calculate multinomial logistic regression age-length key#############################
    
    alkmlr <- reactive({
      req(alk_bin())
      #create length category variable
        agedlencat <- lencat(~TL_mm, w=alk_bin(), startcat = 0, right = FALSE, data = selageDatafinal())
      #run multiple logistic regression
        mlr <- multinom(Age~LCat, data = agedlencat, maxit=500)
      #find min and max length category for sequence (rownames)
        minlencatage <- 0
        maxlencatage <- max(agedlencat$LCat)
        lens <- seq(minlencatage,maxlencatage,alk_bin())
      #Make age-length key (alk) from multinom regression results
        alkmlr1 <- predict(mlr, data.frame(LCat=lens), type = "probs")
        row.names(alkmlr1) <- lens
        alkmlr <- alkmlr1
    })
    
    ###Create dataset with predicted ages for the sample data (fields c(TL_mm, LCat, Age))##########
      #start by creating default checked status for checkbox that extrapolates ALK to younger ages
      observe({updateCheckboxInput(session, inputId = "extrapAge",
                value = if(sum(selageDatafinal()$Age < 1, na.rm=T) >= 3){TRUE}else{FALSE})
          #if number of age-0 fish aged is <3, leaves unchecked and does NOT extrapolate beyond aged fish
         })
      agesample <- reactive({
        req(alk_bin())
        #make length categories for sample dataset return just TL_mm, LCat, Wt_g, Age, Number.of.individuals
        sampledata <- lencat(~TL_mm, w=alk_bin(), startcat = 0, right = FALSE, data = selDataspp()) %>% 
          select(any_of(c("TL_mm", "LCat", "Wt_g", "Age", "Number.of.individuals")))
        if(!"Age" %in% colnames(sampledata)){
          sampledata <-sampledata %>% mutate(Age=NA_integer_)
          }
        #make length categories for aged fish dataset
        agedlencat2 <- lencat(~TL_mm, w=alk_bin(), startcat = 0, right = FALSE, data = selageDatafinal())
        #find min and max length categories within MLR ALK
        if(input$extrapAge == FALSE){
          minlencatage2 <- min(agedlencat2$LCat, na.rm = T)
        }else{
          #below allows us to use smallest fish observed, whether from age or sample data. Should not produce
          #any issues because multinomial regression will assume same pattern follows for smaller fish, but probably should
          #not be done if there are huge discrepancies between smallest aged fish and smallest field-sampled fish.
          #Also would be problem if smallest age fish was still above where age-0 fish are observed.
          minlencatage2 <- min(min(agedlencat2$LCat, na.rm = T), min(sampledata$LCat, na.rm = T))
        }
        maxlencatage2 <- max(agedlencat2$LCat)
        
        #filter sample data set to exclude NA lengths and any fish out of bounds of MLR age length key
        sampledatafinal <- filter(sampledata, LCat>(minlencatage2-1), LCat<(maxlencatage2+1), !is.na(LCat)) %>% 
          mutate(TL_mm=as.numeric(TL_mm), Age=as.numeric(Age), LCat=as.numeric(LCat))
        #separate sampledatafinal into aged and unaged data frames
        sampleDatFinAged <- filter(sampledatafinal, !is.na(Age))
        sampleDatFinUnaged <- filter(sampledatafinal, is.na(Age))
        #apply ages to unaged sample then rejoin with aged fish
        agesampleEstimated <- alkIndivAge(alkmlr(), Age~TL_mm, data = sampleDatFinUnaged)
        agesample <- rbind(sampleDatFinAged, agesampleEstimated)
      })
      
      #render table of fish with assigned ages
      output$agedfishtable <- downloadHandler(
        filename = function() {
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "agedFishTable", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(agesample(), file, row.names = F)
        }
      )
      
  ###ALK Plot information#############################################
    #Code for handling toggle button to change plot type
      alkPlotType <- reactiveVal("LenAgeAreaPlot")
        observeEvent(input$alkPlotToggle, {
          if (alkPlotType() == "LenAgeAreaPlot") {
            alkPlotType("alkbub")
            updateActionButton(session, "alkPlotToggle", label = "Change ALK plot to length-age area plot")
          } else {
            alkPlotType("LenAgeAreaPlot")
            updateActionButton(session, "alkPlotToggle", label = "Change ALK plot to bubble plot")
          }
      })
    #Code for changing plot from observed to smooth ALK
      Obs_SmoothALK <- reactiveVal("Observed")
      observeEvent(input$smoothALKToggle, {
        if (Obs_SmoothALK() == "Observed") {
          Obs_SmoothALK("Smoothed")
          updateActionButton(session, "smoothALKToggle", label = "Change figure to observed ALK")
        } else {
          Obs_SmoothALK("Observed")
          updateActionButton(session, "smoothALKToggle", label = "Change figure to smoothed ALK")
        }
      })
      
      #add toggle to remove age labels from plot area on ALK area plot
      observeEvent(alkPlotType(),{
        if(alkPlotType()=="alkbub"){
          hide("ageTextChkbx")
        }else{#only show if area plot is used
          show("ageTextChkbx")
        }
      })
      
    #create message about smoothed or observed and extrapolated ages above ALK plot
      output$Sm_obs_extrapMessage <- renderText(
        paste0("ALK figure is showing ", toupper(Obs_SmoothALK()), " data with ", #toupper converts text strings to all caps
               if(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed"){
                 "ages EXTRAPOLATED to smaller lengths than were aged. "
               }else{
                 "NO EXTRAPOLATING of ages (note: extrapolation only available with smoothed data). "
               },
               "Regardless of what is displayed, all analyses use the smoothed ALK to prevent errors from missing data.")
      )
      
    #Length-Age Plots from ALK data    
      LenAgePlot <- reactive({
        req(alkmlr(), alkobserved(), agesample(), alk_bin())
        if(Obs_SmoothALK() == "Smoothed"){
          ALK <- as.data.frame(alkmlr()) 
          tallALK <-  rownames_to_column(ALK) %>% 
            pivot_longer(cols = !c(rowname), names_to = "Ages", values_to = "Proportion") %>% 
            mutate(TL_mm = as.numeric(rowname), 
               Ages = as.character(Ages),  # Ensure Ages is character
               Ages = ifelse(Ages == "" | Ages == "0", "0", Ages))  # Replace empty or "0" with "0"
        }else{
          tallALK <- as.data.frame(alkobserved()) %>% 
            mutate(TL_mm = as.numeric(as.character(LCat)), Proportion = Freq,
                   Ages = as.character(Age),  # Ensure Ages is character
                   Ages = ifelse(Ages == "" | Ages == "0", "0", Ages))  # Replace empty or "0" with "0"
        } 
        #insert missing age classes, if they exist
          min_age <- min(as.numeric(as.character(tallALK$Ages)))
          max_age <- max(as.numeric(as.character(tallALK$Ages)))
          all_ages <- as.character(min_age:max_age)
          cnt_ages <- length(all_ages)#count of number of age classes involved
          tallALK <- tallALK %>%
            complete(TL_mm, Ages = factor(all_ages, levels = all_ages),
                     fill = list(Proportion = 0)) %>%
            mutate(Ages = factor(Ages, levels = rev(all_ages))) #ages need to be factor for ggplot column area graph
          
        #deal with scaling and positioning information
          minAgedTL <- min(selageDatafinal()$TL_mm, na.rm = TRUE)  
          maxTL <- max(selageDatafinal()$TL_mm, na.rm = TRUE) 
          TL_range <- maxTL - minAgedTL
          min_diff <- min(diff(sort(unique(tallALK$TL_mm)))) #determine spacing between length groups (i.e., bin width) for column width
          if(!(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed")){
            bins <- unique(tallALK$TL_mm)
            trunkPt <- max(bins[bins<= minAgedTL])
            tallALK <- tallALK %>% filter(TL_mm >= trunkPt & Proportion!= 0) #removing proportions ==0 prevents all age labels from printing on all bars 
            min_diff <- min(diff(sort(unique(tallALK$TL_mm)))) #determine spacing between length groups for column width
          }
          
        #set X-axis breaks (fish length)
          steps <- if(alk_bin()==10){c(20, 25, 50, 75, 100, 150, 200) #Prefer division breaks for mm
            }else if(alk_bin()==15){c(30, 60, 90, 150)
            }else{c(40, 60, 80, 100, 160, 200) }
          raw_step <- (TL_range / 6)  # Aim for ~6 major breaks for range of lengths displayed...this calculates exact step size needed for 7 breaks
          step <- steps[which.min(abs(steps - raw_step))] #find preferred division closest to but smaller than raw_step
          step <- max(round(step / alk_bin()) * alk_bin(), alk_bin())# Ensure step is multiple of bin size and is no smaller than bin size

          # Generate major breaks (using step) and minor breaks (using bin size)
          major_minor_brks <- list(major = seq(0, ceiling(maxTL / step) * step, by = step),
                                   minor = seq(0, maxTL + alk_bin(), by = alk_bin()))
          
      ###ggplot for area plot bar ALK###
        ###area column plot
        if(alkPlotType()=="LenAgeAreaPlot"){
            #prep age data for cumulative Y-values expressed as ymin and ymax
            tallALK_stacked <- tallALK %>%
              group_by(TL_mm) %>%
              arrange(-Ages, .by_group = TRUE) %>%  # make sure ages are ordered consistently
              mutate(yminProp = c(0, head(cumsum(Proportion), -1)), ymaxProp = cumsum(Proportion)) %>%
              ungroup()
            #build ggplot
            LenAgePlot <- ggplot(data=tallALK_stacked, aes(ymin=yminProp, ymax=ymaxProp, fill=Ages)) +
              geom_rect(aes(xmin=TL_mm, xmax = TL_mm + min_diff))+ #xmin and xmax set left and right side of bars
              scale_y_continuous(expand=expansion(mult=0), breaks=scales::breaks_width(0.1), 
                                 labels = scales::percent) +
              labs(y="% of observed fish", x="Length (mm)")+
              scale_fill_viridis_d(option = "viridis", direction = -1) + #option = "magma", includes purple-orange-yellow
              theme_cowplot(font_size = 20)
            #adjust x-axis scaling on area column plot based on checkbox to extrapolate to smaller ages
              if(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed"){
                LenAgePlot2 <-LenAgePlot +
                  scale_x_continuous(expand=expansion(mult=0,0.5), breaks = major_minor_brks$major,
                     minor_breaks = major_minor_brks$minor, guide = guide_axis(minor.ticks = TRUE))+
                  theme(axis.ticks.length = unit(9, "pt"), axis.minor.ticks.length = rel(0.4),
                        axis.minor.ticks.x.bottom = element_line(colour = 'grey35'), axis.ticks=element_line(linewidth=0.5),
                        axis.minor.ticks=element_line(linewidth=0.1))
              }else{
                LenAgePlot2 <- LenAgePlot +
                  scale_x_continuous(expand = expansion(mult = c(0.04, 0)),
                   limits = c(trunkPt - (min_diff/2), NA),  #set left X-axis to 1/2 bin width under trunkPt so length class at trunkPt is not cut off
                   breaks = major_minor_brks$major, minor_breaks = major_minor_brks$minor, 
                   guide = guide_axis(minor.ticks = TRUE)
                  )+
                  theme(axis.ticks.length = unit(9, "pt"), axis.minor.ticks.length = rel(0.4),
                        axis.minor.ticks.x.bottom = element_line(colour = 'grey35'), axis.ticks=element_line(linewidth=0.5),
                        axis.minor.ticks=element_line(linewidth=0.1))
              }
            if(input$addSPPfig == TRUE){
              if(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed"){
                x_lab <- min(tallALK$TL_mm) + (max(tallALK$TL_mm) - min(tallALK$TL_mm))*0.03
              }else{
                x_lab <- trunkPt + (max(tallALK$TL_mm) - minAgedTL)*0.03
              }
              #label spp name at upper left
              LenAgePlot2 <- LenAgePlot2+
                geom_shadowtext(aes(x = x_lab, y = 0.95, label = speciesname()), color = "white",
                                bg.color = "black", bg.r = 0.07, hjust = 0, vjust = 1, size = 9)
            }
            if(alkPlotType()=="LenAgeAreaPlot" & input$ageTextFig==T){
                #create function to read color fill so I can dynamically change font colors
                get_text_color <- function(Ages, allAges, fill_colors) {
                  colors <- fill_colors[match(Ages,allAges)]# Match each age to it's color
                  rgb_values <- col2rgb(colors)# Convert the colors to rgb
                  # Extract RGB values as a vector
                    r <- rgb_values[1, ]
                    g <- rgb_values[2, ]
                    b <- rgb_values[3, ]
                  luminance <- (0.299 * r + 0.587 * g + 0.114 * b) / 255 #calculate luminance from RGB colors
                    ifelse(luminance < 0.5, "black", "lightgrey")
                }
                # Extract the fill colors from the plot data and calculate text color using above function
                  # allAges <- unique(tallALK$Ages)
                  allAges <- rev(levels(tallALK$Ages))
                  numAges = length(allAges)
                  fill_colors <- viridis(numAges, direction = -1)
                #now apply text labels with appropriate colors
                LenAgePlot3 <- LenAgePlot2 +
                  geom_text(aes(x = TL_mm + min_diff / 2,  # centers the text in the bin
                      y = (yminProp+ymaxProp) / 2,        # vertically centers in the bar
                      label = Ages, color = get_text_color(Ages, allAges, fill_colors)), size = 5) +
                  scale_color_identity() # Tells ggplot our "black" and "lightgrey" values from get_text_color() is an actual color and not a data series
              return(LenAgePlot3)
            }else{
              return(LenAgePlot2)
            }
              
        ###Age Length Key Bubble Plot
        }else if(alkPlotType()=="alkbub"){
          # Determine number of age classes for Y-axis scaling
            tallALK$Age_num <- as.numeric(as.character(tallALK$Ages))
            n_ages <- diff(range(tallALK$Age_num, na.rm = TRUE))+1
            label_interval <- if(n_ages <= 20) {1} else if(n_ages <= 40) {2} else if(n_ages <=60) {3} else {4}
          #build ggplot
            LenAgePlot <- ggplot(data=tallALK,mapping=aes(x=TL_mm,y=Age_num)) +
              geom_point(mapping=aes(size=Proportion,color=Ages))+ #keep color = Ages so Ages is factor unlike Age_num
              scale_y_continuous(name = "Age (yr)", breaks = seq(min(tallALK$Age_num, na.rm = TRUE),
                 max(tallALK$Age_num, na.rm = TRUE), by = label_interval), minor_breaks = seq(min(tallALK$Age_num, na.rm = TRUE),
                 max(tallALK$Age_num, na.rm = TRUE), by = 1),
                guide = guide_axis(minor.ticks = TRUE)
                ) +
              labs(y="Age (yr)", x="Length (mm)")+
              scale_color_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE))+
              theme_cowplot(font_size = 20)+
              background_grid(major = "xy", minor = "x", size.major = 0.3, size.minor = 0.1, 
                              color.major = "white", color.minor = "gray97")+
              theme(legend.position = "none", panel.background = element_rect(fill = "gray80", colour = NA)) #remove symbol key & shade plot background
            
            #adjust x-axis scaling on area column plot based on checkbox to extrapolate to smaller ages
            if(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed"){
              LenAgePlot2 <-LenAgePlot +
                scale_x_continuous(expand=expansion(mult=0.03, 0.04), breaks = major_minor_brks$major, 
                     minor_breaks = major_minor_brks$minor,guide = guide_axis(minor.ticks = TRUE))+
                theme(axis.ticks.length = unit(9, "pt"), axis.minor.ticks.length = rel(0.4),
                      axis.minor.ticks.x.bottom = element_line(colour = 'grey35'), axis.ticks=element_line(linewidth=0.5),
                      axis.minor.ticks=element_line(linewidth=0.1), axis.minor.ticks.y.left = element_line(colour = 'grey35'))
            }else{
              LenAgePlot2 <- LenAgePlot +
               scale_x_continuous(expand = expansion(mult = c(0.03, 0.04)), limits = c(trunkPt, NA), 
                    breaks = major_minor_brks$major, minor_breaks = major_minor_brks$minor,
                    guide = guide_axis(minor.ticks = TRUE))+
               theme(axis.ticks.length = unit(9, "pt"), axis.minor.ticks.length = rel(0.4),
                    axis.minor.ticks.x.bottom = element_line(colour = 'grey35'), axis.ticks=element_line(linewidth=0.5),
                    axis.minor.ticks=element_line(linewidth=0.1))
            }
            if(input$addSPPfig == TRUE){
              #calculate location for spp name along x-axis
                if(input$extrapAge == TRUE & Obs_SmoothALK() == "Smoothed"){
                  x_lab <- min(tallALK$TL_mm) + (max(tallALK$TL_mm) - min(tallALK$TL_mm))*0.03
                }else{
                  x_lab <- trunkPt + (max(tallALK$TL_mm) - minAgedTL)*0.03
                }
              #label spp name at upper left
              LenAgePlot2 <- LenAgePlot2+
                annotate("text", x = x_lab, y = cnt_ages-0.5,label = speciesname(), hjust = 0, 
                         vjust = 0.5, color = "black", size = 9)
            }
        }
    })
      
    #render ALK Length-Age Area Plot or Bubble Plot
        output$alkplot <- renderPlot(bg="transparent",{
        if(input$agelengthkey == TRUE){
          suppressWarnings( #needed to prevent console warning about axis.minor.ticks not in cowplot
              print(LenAgePlot())
          )
        }
      })
      
        
  ##ALK download buttons#####################
      #Download png of ALK bubble plot
      output$downALKplot <- downloadHandler(
        filename = function(){ 
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "ALKplot", "png", sep = ".")},
        content = function(file){
          png(file, width = 600, height = 450)
            print(LenAgePlot())
          dev.off()
        })
      
    #Download png of Age Frequency plot
    output$downafplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "AgeFreq", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        print(agefreq())
        dev.off()
      })
    
  #Downloadable csv of observed age-length key
  output$obsALK <- downloadHandler(
    filename = function() {
      paste(input$selectlake,input$selectyear,input$selectspecies,
            "obsALK", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(alkobserved(), file, row.names = TRUE)
    }
  )
  
  #Downloadable csv of smoothed age-length key
  output$smoothALK <- downloadHandler(
    filename = function() {
      paste(input$selectlake,input$selectyear,input$selectspecies,
            "smoothALK", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(alkmlr(), file, row.names = TRUE)
    }
  )
  
  #hide/show age-length key info
  observeEvent(input$agelengthkey,{
    if(input$agelengthkey == T){
      show("alk")
    }else{
      hide("alk")
    }
  })
  
  
  ###Age-Frequency Histogram################################################
  ageXaxis <- reactive({
    maxsampage <- max(agesample()$Age)
    minsampage <- min(agesample()$Age)
    if(maxsampage-minsampage>80){
      use_labels <- seq(minsampage, maxsampage, by = 5)
    }else if(maxsampage-minsampage>60){
      use_labels <- seq(minsampage, maxsampage, by = 4)
    }else if(maxsampage-minsampage>40){
      use_labels <- seq(minsampage, maxsampage, by = 3)
    }else if(maxsampage-minsampage>20){
      use_labels <- seq(minsampage, maxsampage, by = 2)
    }else{
      use_labels <- seq(minsampage, maxsampage, by = 1)
    }
    list( 
      minsampage = minsampage,
      maxsampage = maxsampage,
      use_labels = use_labels
    )
  })
    agefreq <- function(){#leaving this as a function rather than a reactive so it can be used in download function for graph
      #calculate age frequency and add missing age classes
        agefreqDf <- as.data.frame(xtabs(~Age, data = agesample())) %>% mutate(Age=as.numeric(as.character(Age)))
        allAges <- data.frame(Age = seq(ageXaxis()$minsampage, ageXaxis()$maxsampage, by=1))
        #find sum of all ages from sample - agesample reactive function
          total <- sum(agefreqDf$Freq)
          agefreqDf <- mutate(agefreqDf, Freq = Freq/total*100)
        #insert missing age classes
          agefreqDf <- left_join(allAges,agefreqDf, by="Age") %>% 
            mutate(Freq=case_when(is.na(Freq)~0,TRUE~Freq))
      #Make figure
      agefreqHist <- ggplot(data=agefreqDf, aes(x=Age, y=Freq))+
        geom_bar(aes(fill = "bar_fill", color = "bar_outline"), 
                 stat="identity", width=1) +
        scale_fill_manual(values = c("bar_fill" = "grey80"), guide = "none") +
        scale_color_manual(values = c("bar_outline" = "black"), guide = "none") +
        labs(y="% of observed fish", x="Age (yr)")+
        scale_y_continuous(expand = expansion(mult = c(0, .1)))+
        scale_x_continuous(breaks = ageXaxis()$use_labels, minor_breaks = allAges, guide = guide_axis(minor.ticks = TRUE))+
        theme(axis.ticks.length.minor = rel(0.5))+
        theme_cowplot(font_size = 20)
      if(input$addSPPfig == TRUE){
        agefreqHist <- agefreqHist+
          #add spp label at upper right
          annotate("text", x = ageXaxis()$maxsampage + 0.5, y = Inf,#max(agefreqDf$Freq, na.rm = T)*1.1, 
                   label = speciesname(), hjust = 1, vjust = 1,
                   color = "black", size = 9)
      }
      return(agefreqHist)
    }
    
    #render age frequency histogram
    output$agefreqhist <- renderPlot(bg="transparent",{
      if(input$agefreq == TRUE){
      # agefreq()
      #length frequency graph is not working with this same color changing approach (but below works perfectly)
      #As such, I'm going with black and white figures for now and will address this at some point in the future
        suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
          agefreqHist <- agefreq() +
            scale_fill_manual(values = "#404688FF", guide = "none") +#481F70FF
            scale_color_manual(values = "#FDE725FF", guide = "none")
        })
      print(agefreqHist)
      }
    })
    
    #Download png of Age Frequency plot
    output$downafplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "AgeFreq", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        print(agefreq())
        dev.off()
      })
    
    #hide/show age frequency information
    observeEvent(input$agefreq,{
      if(input$agefreq == T){
        show("ageFreqInfo")
      }else{
        hide("ageFreqInfo")
      }
    })
    
    
    ##von Bertalanffy Growth Model#################################################
    
    #deal with unit conversion with input$inch check box (mutate TL mm to inches)
      aged <- reactive({
        if(input$inch == FALSE){
          aged <- agesample()
        }
        if(input$inch == TRUE){
          aged <- data.frame(mutate(agesample(), TL_mm = TL_mm/25.4))
        }
        return(aged)
      })
    
    #fit VB curve with nls package
    vonBfun <<- vbFuns() #defines this variable globally, this is needed for the predFit() function to work when building ggplot of VB curve
    fitvonB <- reactive({
      tryCatch({  #will use this to trap any convergence issues in nls() and return text rather than VB curve plot
        #define vonB starting coefficients
          vonBstarts <- list(Linf=max(aged()$TL_mm, na.rm = TRUE), K=0.3, t0=0)
        #define function
          vonBfun <- vbFuns()
        #use nls package to fit vonB curve - nls.control() controls iteration parameters
          fitvonB <- nls(TL_mm~vonBfun(Age,Linf,K,t0), data = aged(), start = vonBstarts,#need to explicitly use stat::nls or call from predFit function from investr refuses to run
                       control = nls.control(maxiter = 200, minFactor = 1/1000000,
                                             printEval = FALSE, warnOnly = TRUE))
        #handle logic for tryCatch
          if (fitvonB$convInfo$isConv) {
            return(fitvonB)
          } else {
            warning("Model did not converge")
            return(NULL)
          }
      }, error = function(e) {
        warning(paste("Error in nls fitting:", e$message))
        return(NULL)
      })
    })
    
  ###von Bert Plot (mean length-at-age)  ################################################
    #generate VB plot and save as object
    lengthplot <- reactive({
            #define FSA von Bert function
              vonBfun <- vbFuns()
      #initially handle error if nls() did not converge when fitting VB curve
      if (is.null(fitvonB())) {
        return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "Von Bertalanffy growth model did not converge.",
                      size = 8) +
             theme_void())
      }

        if(input$growth == TRUE){
          #aggregate mean of each age
            sumfish <- aggregate(TL_mm~Age, data = aged(), mean)
            #create age sequence from max and min of aged for seq() next
              minsampageData <- min(aged()$Age)
              minsampage <- -1 #build extended VB line back to -1 yr to show full curve
              maxsampageData <- max(aged()$Age)
              maxsampage <- maxsampageData + 1 #extend VB line 1 year forward
              ageslineData <- seq(minsampageData, maxsampageData, length.out = 200)#sequence of "ages" to use for solid VB line
              ageslineAll <- seq(minsampage, maxsampage, length.out = 200)#sequence of "ages" to use for dashed (extrapolated) line
            #use vonB equation to predict ages for line on plot
              vbPI <- data.frame(Age=ageslineAll, investr::predFit(fitvonB(), data.frame(Age=ageslineAll), #uses predFIt from investr package...this call prevents having to load the whole package for this one call to one function
                             interval = "prediction"))
              vbCI <- data.frame(Age=ageslineAll, investr::predFit(fitvonB(), data.frame(Age=ageslineAll), #uses predFIt from investr package...this call prevents having to load the whole package for this one call to one function
                             interval = "confidence"))
              MLA <- vonBfun(ageslineAll, Linf = coef(fitvonB()))
              MLAdata <- vonBfun(ageslineData, Linf = coef(fitvonB()))
            #create data frame of the two above arrays
              VBlineDF <- data.frame(TL_mm=MLA, Age=ageslineAll) #this is the expanded version (for extrapolated line)
              VBlineDFdata <- data.frame(TL_mm=MLAdata, Age=ageslineData) #this is the one matching the actual data

            #create labels to skip every other age if <20 age classes exist
              if(maxsampage-minsampage>80){
                use_labels <- seq(minsampage+1, maxsampage, by = 5)
              }else if(maxsampage-minsampage>60){
                use_labels <- seq(minsampage+1, maxsampage, by = 4)
              }else if(maxsampage-minsampage>40){
                use_labels <- seq(minsampage+1, maxsampage, by = 3)
              }else if(maxsampage-minsampage>20){
                use_labels <- seq(minsampage+1, maxsampage, by = 2)
              }else{
                use_labels <- seq(minsampage+1, maxsampage, by = 1)
              }

          if(input$inch == FALSE){ 
            vbplot <- ggplot(aged(), aes(x=Age, y=TL_mm))+
              geom_ribbon(data=vbPI, aes(x=Age, Y=NULL, ymin=lwr, ymax=upr, fill="95% Prediction Int"), inherit.aes = FALSE)+ #set color to gray85 in scale_color_manual
              geom_ribbon(data=vbCI, aes(x=Age, Y=NULL, ymin=lwr, ymax=upr, fill="95% Confidence Int"), inherit.aes = FALSE)+#set color to gray65 in scale_color_manual
              geom_point(data=sumfish, aes(color = 'Mean Length'), shape=17, size=6)+ #color= forces this into legend
              geom_point(shape=1, size=3,aes(color = 'Individual Fish')) +
              geom_line(data=VBlineDF, y=MLA, linewidth = 1.25, linetype="dashed")+#expanded dashed line from age -1 to max age+1
              geom_line(data=VBlineDFdata, y=MLAdata, aes(color='von Bert'), linewidth = 1.25, linetype="solid")+ #color= forces this into legend
              scale_color_manual(name = NULL, values = c('Mean Length' = 'black', 'Individual Fish' = 'black',
                                 'von Bert' = 'black'), breaks = c('Mean Length', 'Individual Fish', 'von Bert')) +
              scale_fill_manual(name = NULL, values = c('95% Prediction Int' = 'gray80', '95% Confidence Int' = 'gray60')) +
              guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
              labs(y="Total Length (mm)", x="Age (yr)")+
              scale_x_continuous(breaks = use_labels, minor_breaks = seq(minsampage+1, maxsampage, by = 1),
                                 guide = guide_axis(minor.ticks = TRUE), expand = expansion(mult = c(0, 0.05)))+
              scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+ #will limit Y start at 0 using coord_cartesian below...this sets no pads at bottom but 5% padd at top
              theme(axis.ticks.length.minor = rel(0.5))+
              theme_cowplot(font_size = 20)+
              theme(legend.position = "inside", legend.position.inside = c(0.65, 0.20), legend.text=element_text(size=17),
                    legend.spacing.y = unit(0, "cm") )+
              coord_cartesian(ylim = c(0, NA), xlim=c(-1,NA), clip="on") #needed to avoid truncating CI if it does not fit the Y-axis' scaling
          }
          if(input$inch == TRUE){

            vbplot <- ggplot(aged(), aes(x=Age, y=TL_mm))+
              geom_ribbon(data=vbPI, aes(x=Age, Y=NULL, ymin=lwr, ymax=upr, fill="95% Prediction Int"), inherit.aes = FALSE)+ #set color to gray85 in scale_color_manual
              geom_ribbon(data=vbCI, aes(x=Age, Y=NULL, ymin=lwr, ymax=upr, fill="95% Confidence Int"), inherit.aes = FALSE)+#set color to gray65 in scale_color_manual
              geom_point(data=sumfish, aes(color = 'Mean Length'), shape=17, size=6)+ #color= forces this into legend
              geom_point(shape=1, size=3,aes(color = 'Individual Fish')) +
              geom_line(data=VBlineDF, y=MLA, linewidth = 1.25, linetype="dashed")+#expanded dashed line from age -1 to max age+1
              geom_line(data=VBlineDFdata, y=MLAdata, aes(color='von Bert'), linewidth = 1.25, linetype="solid")+ #color= forces this into legend
              scale_color_manual(name = NULL, values = c('Mean Length' = 'black', 'Individual Fish' = 'black',
                                                         'von Bert' = 'black'), breaks = c('Mean Length', 'Individual Fish', 'von Bert')) +
              scale_fill_manual(name = NULL, values = c('95% Prediction Int' = 'gray80', '95% Confidence Int' = 'gray60')) +
              guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
              labs(y="Total Length (in)", x="Age (yr)")+
              scale_x_continuous(breaks = use_labels, minor_breaks = seq(minsampage+1, maxsampage, by = 1),
                                 guide = guide_axis(minor.ticks = TRUE), expand = expansion(mult = c(0, 0.05)))+
              scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+ #will limit Y start at 0 using coord_cartesian below...this sets no pads at bottom but 5% padd at top
              theme(axis.ticks.length.minor = rel(0.5))+
              theme_cowplot(font_size = 20)+
              theme(legend.position = "inside", legend.position.inside = c(0.65, 0.20), legend.text=element_text(size=17),
                    legend.spacing.y = unit(0, "cm") )+
              coord_cartesian(ylim = c(0, NA), xlim=c(-1,NA), clip="on")
          }
          if(input$addSPPfig == TRUE){
            vbplot <- vbplot+
              #label spp name at upper left
              annotate("text", x = minsampage + ((maxsampage - minsampage) *0.02),
                       y = max(aged()$TL_mm) * 0.98, label = speciesname(),
                       hjust = 0, vjust = 1, color = "black", size = 9)
          }

          return(vbplot)
          }
    })

    #render VB mean length at age plot
    output$meanlengthplot <- renderPlot(bg="transparent", {
      if(input$growth == TRUE){
        suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
          lengthplot <- lengthplot() +
              scale_fill_manual(name = NULL, values = c('95% Prediction Int' = '#FDE725FF', '95% Confidence Int' = '#7AD151FF')) #404688FF
        })
        print(lengthplot)
          }
    })
    
    #Download png of Growth Metrics plot
    output$downmeanplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "MeanLength", "png", sep = ".")},
      content = function(file){
        png(file, width = 675, height = 450)
        print(lengthplot())
        dev.off()
      })
    
    observeEvent(input$CI_PIinfo,{
      showModal(modalDialog(
        title = "Additional Information about Confidence Intervals and Perdiction Intervals",
        HTML(
        "The lime-green band is the 95% confidence interval for the von Bertalanffy curve line.  This illustrates
        the uncertainty von Bertalanffy trend line for mean length at age 
        (i.e., the true VB trend line could be anywhere within this shaded band). The CI band is generally small 
        as the waythe 3 VB parameters change the curvature of the line can only have so much flexibility and still 
        come close to the mean length of each age class.  <br>
        The yellow band is the 95% prediction interval. This is much wider and reflects where 95% of observed 
        mean lengths occur. The vertical space in this band at any point can be though of as the reasonable range 
        of lengths at that age (i.e., the band will cover 95% of the individual observed fish lengths)."),
        easyClose = TRUE,
        footer = modalButton("Close")))
    })
    
    #show or hide VB length at age plot information
    observeEvent(input$vonbert,{
      if(input$vonbert == T){
        show("vonbertInfo")
      }else{
        hide("vonbertInfo")
      }
    })
    
        
  ###Mean length-at-age table###
    lengthtable <- reactive({
      if(input$meanlength == TRUE){
        #summarize up the sample according to designated ages
          meanlength <- aggregate(TL_mm~Age, data = aged(), mean)
          countlength <- aggregate(TL_mm~Age, data = aged(), length)
          selength <- aggregate(TL_mm~Age, data = aged(), se)
          sdlength <- aggregate(TL_mm~Age, data = aged(), sd)
        #Join datasets together
          meanlength <- cbind(meanlength,countlength,selength,sdlength)
          meanlength <- meanlength[c(1,2,4,6,8)]
          colnames(meanlength) <- c("Age","Mean","Count","SE","SD")
        #calculate confidence interval and CV
          meanlength <- mutate(meanlength, LCI = Mean - (1.96*SE), UCI = Mean + (1.96*SE),
                             CV = (SD/Mean)*100)
          meanlength <- meanlength[c(1,2,3,8,4,6,7)]
        #rename column names for output
          colnames(meanlength)[2:7] <- c("Mean TL", "Count", "CV","SE","L 95% CI","U 95% CI")
        #make Age an integer, gets rid of decimals
          lengthtable <- mutate(meanlength, Age = as.integer(Age))
      }
    })
    #render mean length-at-age table
    output$meanlengthtable <- renderTable(spacing="xs",{
      if(input$meanlength == TRUE){
        lengthtable()
      }
    })     
    
    #produce downloadable table of mean length at age
    output$downML <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "MeanLength", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(lengthtable(), file, row.names = FALSE)
      }
    )
    
    #hide/show mean length at age table
    observeEvent(input$meanlength,{
      if(input$meanlength == T){
        show("meanLengthAge")
      }else{
        hide("meanLengthAge")
      }
    })
  
  ###von Bert Coefficient Table########################################################
    coeftable <- reactive({
      if(input$vonbert == TRUE){
        if(is.null(fitvonB())) { #if nls() did not converge and produce VB equation
          return(NULL)
        }
        #call coefficients from the nls model
          coef <- as.data.frame(coef(fitvonB()))
          colnames(coef) <- "Estimate"
        #call confidence intervals for the coefficients
          confint <- as.data.frame(confint(fitvonB(), level = 0.95)) 
          colnames(confint)[1:2] <- c("LCI","UCI")
        #bring the confidence intervals into the same table as coefficients
          coef["L 95% CI"] <- confint$LCI
          coef["U 95% CI"] <- confint$UCI
          return(coef)
      }
    })
    
    #render table of von bert coefficients
    output$vonBcoef <- renderTable(digits = 3, spacing="xs", rownames = TRUE, {
      if(input$vonbert == TRUE && !is.null(coeftable())) {
        coeftable()
      }
    })
    #if nls failed to produce von Bert coefficients, display error
    output$vonBerror <- renderUI({
      if(input$vonbert == TRUE && is.null(coeftable())) {
        h4("Von Bertalanffy growth model did not converge so equation parameters cannot be displayed.") }
    })

    #Downloadable csv of von Bert coefficients
    output$downvonBcoef <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "vonBcoef", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(coeftable(), file, row.names = TRUE)
      }
    )
    
    ###Mean weight-at-age table##################################################
    weighttable <- reactive({
      if(input$meanweight == TRUE){
        if(input$inch == FALSE){
          aged <- agesample()
        }
        if(input$inch == TRUE){
          aged <- mutate(agesample(), Wt_g = Wt_g*0.00220462)
        }
        
        #summarize up the sample according to designated ages
        meanwt <- aggregate(Wt_g~Age, data = aged, mean)
        countwt <- aggregate(Wt_g~Age, data = aged, length)
        sewt <- aggregate(Wt_g~Age, data = aged, se)
        sdwt <- aggregate(Wt_g~Age, data = aged, sd)
        #Join datasets together
        meanweight <- cbind(meanwt,countwt,sewt,sdwt)
        meanweight <- meanweight[c(1,2,4,6,8)]
        colnames(meanweight) <- c("Age","Mean","Count","SE","SD")
        #calculate confidence interval and CV
        meanweight <- mutate(meanweight, LCI = Mean - (1.96*SE), UCI = Mean + (1.96*SE),
                             CV = (SD/Mean)*100)
        meanweight <- meanweight[c(1,2,3,8,4,6,7)]
        #rename column names for output
        colnames(meanweight)[2:7] <- c("Mean Weight", "Count", "CV","SE","L 95% CI","U 95% CI")
        #make Age an integer, gets rid of decimals
        weighttable <- mutate(meanweight, Age = as.integer(Age))
      }
    })
    #render mean weight-at-age table
    output$meanweighttable <- renderTable(spacing="xs",{
      if(input$meanweight == TRUE){
        weighttable()
      }
    })
    #show/hide mean wt-at-age table
    observeEvent(input$meanweight,{
      if(input$meanweight == T){
        show("meanWeightAge")
      }else{
        hide("meanWeightAge")
      }
    })
    
    
    #Downloadable csv of mean weight at ages
    output$downMW <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "MeanWeight", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(weighttable(), file, row.names = FALSE)
      }
    )
    
  ###Catch Curve plot and mortality table#######################################
  
    #show or hide all buttons/outputs for mort, theorMaxAge, nat mort, and recruitement
    observeEvent(input$mort,{
      if(input$mort == T){
        show("mortMetrics")
      }else{
        hide("mortMetrics")
      }
    })
    
    #define maxcatchvector, maxcatchage, and maxsampage that will be updated by mortCatch reactive statement
    maxCatch <- reactiveValues(
      maxcatchvector =  NULL,
      maxcatchage =  NULL,
      maxsampage = NULL
    )
    #create data needed to fit catch curve
    mortCatch <- reactive({
      if(input$mort == TRUE){
        #add up catch frequency
          catch <- as.data.frame(xtabs(~Age+Age, data = agesample()))
        #change age to numeric, add 1 to frequency (avoid log(1))
          catch <- mutate(catch, Age = as.numeric(as.character(Age)), Freq = Freq+1)
          #set object to age at which max catch is obtained (descending limb) for ages2use argument
            maxCatch$maxcatchvector <- catch[which.max(catch$Freq),]
            maxCatch$maxcatchage <- as.numeric(maxCatch$maxcatchvector[1,1])
          #set object for ages2use argument
            maxCatch$maxsampage <- max(catch$Age)
        return(catch)
      }
    })
    
    #fit catch curve
    mort <- reactive({
      if(input$mort == TRUE){
        #calculate mortality with FSA
        mort <- catchCurve(Freq~Age, data = mortCatch(), ages2use = maxCatch$maxcatchage:maxCatch$maxsampage,
                           weighted = TRUE, parm = "lm")
      }
    })
    
    ##Build catch curve plot
    catchplot <- function(){#cannot do with reactive statement or plot will not download
      if(input$mort == TRUE){
        limb <- data.frame(age=mort()$age.e, logAbund=mort()$log.catch.e, weights=mort()$weights.e) #data on descending limb
        allAges <- data.frame(age=mort()$age, logAbund=log(mort()$catch)) #data for all ages (including those before descending limb)
        
        #set up range of age classes and determine if axis can handle displaying all or every other
        maxsampage <- max(allAges$age)
        minsampage <- min(allAges$age)
            if(maxsampage-minsampage>80){
              use_labels <- seq(0, maxsampage, by = 5)
            }else if(maxsampage > 60){
              use_labels <- seq(0, maxsampage, by = 4)
            }else if(maxsampage > 40){
              use_labels <- seq(0, maxsampage, by = 3)
            }else if(maxsampage > 20){
              use_labels <- seq(0, maxsampage, by = 2)
            }else{
              use_labels <- seq(0, maxsampage, by = 1)
            }
        
        catchplot <- ggplot(data = limb, aes(x = age, y = logAbund)) +
          geom_smooth(aes(weight = weights, fill = "ci_band"), formula = y~x, method = "lm", se = TRUE, 
                      color = "black", linewidth = 1.25, alpha = 1)+
          scale_fill_manual(values = c("ci_band" = "grey80"), guide = "none") +
          geom_point(data=allAges, aes(x=age,y=logAbund), shape = 21, fill = "white", size = 4)+
          geom_point(shape = 21, fill = "black", size = 4)+
          labs(y="ln(Abundance)", x="Age (yr)")+
          scale_x_continuous(breaks = use_labels, minor_breaks = seq(0, maxsampage, by=1), 
                             guide = guide_axis(minor.ticks = TRUE)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01))) + 
          coord_cartesian(ylim = c(0, NA), xlim = c(0, maxsampage)) + #needed to set x and y axis range here to properly scale confidence band
          theme(axis.ticks.length.minor = rel(0.5))+
          theme_cowplot(font_size = 20)+
          annotate("text", x = 0, y = -Inf, hjust = 0, vjust = -1, size = 6, #vjust here sets bottom for all 3 lines
                   label = paste("Z ==", round(coef(mort())[["Z"]], 3)), parse = TRUE) +
          annotate("text", x = 0, y = -Inf, hjust = 0, vjust = -2.7, size = 6, #vjust here moves A line up or down only
                   label = paste("A ==", round(coef(mort())[["A"]], 1), "*'%'"), parse = TRUE) +
          annotate("text", x = 0, y = -Inf, hjust = 0, vjust = -2.9, size = 6, #vjust here sets the top of the 3 lines (Z line)
                   label = paste("R^2 ==", round(summary(mort()$lm)$r.squared, 2)), parse = TRUE)
        #add spp label if check box indicates user wants it
        if(input$addSPPfig == TRUE){
          catchplot <- catchplot+
            #add spp label at upper right
            annotate("text", x = maxsampage, y = Inf, 
                     label = speciesname(), hjust = 1, vjust = 1,
                     color = "black", size = 9)
        }
        return(catchplot)
      }
    }

    output$catchcurve <- renderPlot(bg="transparent" ,{
      if(input$mort == TRUE){
        # print(catchplot())
        suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
          catchplot <- catchplot()+
            scale_fill_manual(values = c("ci_band" = "#FDE725FF"), guide = "none" )
        })
        print(catchplot)
      }
    })  
      
  #build mortality table from catch curve analysis
  mortfinal <- reactive({
    if(input$mort == TRUE){
      #calculate summary and conf intervals for estimates
        mortest <- cbind(summary(mort()), confint(mort()))
      #don't return t-value and p-value (columns 3-4)
        morttable <- as.data.frame(mortest[,c(1,2,5,6)])
      #rename columns
        colnames(morttable)[1:4] <- c("Mortality Rate", "Standard Error", "L 95% CI", "U 95% CI")
        rownames(morttable)[1:2] <- c("Instantaneous Z", "Annualized A (%)")
        mortfinal <- morttable
    }
  })
  
  #render mortality table
  output$mortalitytable <- renderTable(digits = 2,spacing="xs",rownames = TRUE,{
    if(input$mort == TRUE){
      mortfinal()
    }
  })  
  
  #####
    #Download png of Catch Curve
    output$downmort <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "CatchCurve", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        print(catchplot())
        dev.off()
      })
  #####
      
  #Theoretical max age
  Theor_max_age <- reactive({
    if(input$mort == TRUE){
      catchCurveParms <- coef(mort(), parm=c("lm"))
      TMA <- round(log(1)-(unname(catchCurveParms[1])/unname(catchCurveParms[2])),1)
          #catchCurveParms[1] is intercept and catchCurveParms[2] is slope from catch curve
          #unname gets rid of named value from catchCurveParms
      L_CI_TMA <- round(log(1)-(unname(-catchCurveParms[1])/confint(mort())[1,2]),1) #replace slope with U_CI
      U_CI_TMA <- if(-confint(mort())[1,1] > 0){">500"}#if value in table is negative...is going to produce negative age est but really means very old age (infinity), which is what you get as Z approaches 0
                  else{round(log(1)-(unname(catchCurveParms[1])/-confint(mort())[1,1]),1)} #replace Z slope with L_CI
      theorMaxAge <- as.data.frame(cbind(TMA, L_CI_TMA, U_CI_TMA))
      colnames(theorMaxAge)[1:3] <- c("Estimate", "L 95% CI", "U 95% CI")
      rownames(theorMaxAge)[1] <- "Theoretical Maximum Age (yr)"
      return(theorMaxAge)
    }
  })
  
output$TheorMaxAge <- renderTable(digits = 2,spacing="xs",rownames = TRUE,{
  if(input$mort == TRUE){
    Theor_max_age()
  }
})  
  
  #Max Observed Age
  obsMaxAge <- reactive({
    if(input$mort == TRUE){
      max(selageDatafinal()$Age, na.rm = T)
    }
  })
  
  #render Max Observed Age output
  output$ObsMaxAge <- renderText({
    if(input$mort == TRUE){
      as.character(paste("Observed Maximum Age = ", obsMaxAge(), " Yr", sep = ""))
    }
  })  

  #Estimate natural mortality rates
  
  #build modal for explaining natural mort estimation
  observeEvent(input$NatMort_MoreInfo, {
    showModal(modalDialog(
      title = "Additional Information about natural mort estimation",
      "Estimates of natural mortality (M) are based on the fact that M often correlates with theoretical 
       max age (Hoenig NLS approach) or von Bertalanffy values (Pauly NLS-T approach).  This will not work
       well for all populations, but is an established method in the field.  Using these estimates of natural
       mortality, fishing
       mortality is then calculated as total mortality minus natural mortality. The Hoenig NLS method should
       be preferred when available as it produces the most consistently accurate results (Then et al.
       2015, ICES J. Mar. Sci. 72:82-92), but the Pauly NLS-T method can be used in cases where theoretical 
       maximum age cannot be calcualted
       if von Bertalanffy curve parameters are available.  If fishing mortality is negative, it is
       a sign that the estimated natural mortality was already larger than total mortality...suggesting
       the estimated natural mort was not a good fit to these data (and/or that most mortality is
       caused by natural mortality).",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #build natural mort table
  natMortTable <- reactive({
    if(input$mort == TRUE){
      if (is.null(fitvonB())) {
        est_M_table <- metaM("HoenigNLS", #calc instantaneous nat mort from metaM function in FSA package
                             tmax=as.numeric(Theor_max_age()[1,1])) 
        est_M_table2 <- data.frame(method = "Hoenig NLS (Then et al. 2015)", M=round(est_M_table, digits = 3)) %>% 
          select(method, M) %>% 
          mutate("Est. Inst. Fishing Mort. (F)" = round(mortfinal()[1,1] - M, digits = 3),
                 "Inst. Total Mort. (Z)" = round(mortfinal()[1,1], digits = 3),
                 "Ann. total Mort. (A)" = as.character(paste(round(mortfinal()[2,1], digits = 1), "%", sep = "")),
                 "Est. Ann. Nat. Mort. (v)" = as.character(paste(round(
                   M * mortfinal()[2,1] / mortfinal()[1,1], digits = 1), "%", sep = "")),
                 "Est. Exploitation / Ann. Fish. Mort. (u)" = as.character(paste(round(
                   mortfinal()[2,1] - (M * mortfinal()[2,1] / mortfinal()[1,1]), digits = 1), "%", sep = ""))
          ) %>% 
          rename(Method = method, "Est. Inst. Nat. Mort (M)" = M) 
        est_M_table2$message <- "Pauly NLS-T approach could not be calculated because von Bert curve did not converge"
        return(est_M_table2)
      }else{
        vbParms <- coeftable()[,1, drop=F]
        est_M_table <- metaM(c("HoenigNLS", "PaulyLNoT"), #calc instantaneous nat mort from metaM function in FSA package
                             Linf= (vbParms[1,]/10), K=vbParms[2,], t0=vbParms[3,], tmax=as.numeric(Theor_max_age()[1,1])) %>% 
          mutate(method = case_when(method == "HoenigNLS" ~ "Hoenig NLS",
                                    method == "PaulyLNoT" ~ "Pauly NLS-T"),
                 M = round(M, digits = 3)) %>% 
          select(method, M) %>% 
          mutate("Est. Inst. Fishing Mort. (F)" = round(mortfinal()[1,1] - M, digits = 3),
                 "Inst. Total Mort. (Z)" = round(mortfinal()[1,1], digits = 3),
                 "Ann. total Mort. (A)" = as.character(paste(round(mortfinal()[2,1], digits = 1), "%", sep = "")),
                 "Est. Ann. Nat. Mort. (v)" = as.character(paste(round(
                          M * mortfinal()[2,1] / mortfinal()[1,1], digits = 1), "%", sep = "")),
                 "Est. Exploitation / Ann. Fish. Mort. (u)" = as.character(paste(round(
                          mortfinal()[2,1] - (M * mortfinal()[2,1] / mortfinal()[1,1]), digits = 1), "%", sep = ""))
                ) %>% 
          rename(Method = method, "Est. Inst. Nat. Mort (M)" = M) 
      }
    }
  })
  
  #render natural mortality table
  output$natMortalityTable <- renderTable(spacing="xs",rownames = FALSE, align = "c",{
    if(input$mort == TRUE){
        table_data <- natMortTable()
        if("message" %in% names(table_data)) {
          # If message exists, add it as a new row
          rbind(table_data[, !names(table_data) %in% "message"], 
                c(table_data$message, rep("", ncol(table_data) - 1)))
        } else {
          table_data
        }
    }
  })  

  #####
   #Downloadable csv of mortality table
    output$downmorttable <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "mortTable", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(mortfinal(), file, row.names = TRUE)
      }
    )
       
    #Downloadable csv of estimated natural mortality table
    output$downNatmorttable <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "EstNatMortTable", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(natMortTable(), file, row.names = TRUE)
      }
    )
#####
 #Recruitment indexes####
    YCstrength <- reactive({
      if(length(unique(input$selectyear))==1){#can only calc YCstrength if all fish collected in same year (so year identifies cohort)
        YCstrength <- data.frame(age = mort()$age.e, yrclass = as.numeric(input$selageyears) - mort()$age.e,
          ycs = rstudent(mort()$lm)) %>% mutate(combinedX = paste0("Age-", age, "  (", yrclass, ")"))
    return(YCstrength)
    }
  })
  #build figure
  YCstrengPlot <- function(){
      if(input$mort == TRUE & nrow(filter(YCstrength(), !is.nan(ycs)))>0){
        if(length(unique(YCstrength()$combinedX))>6){
          axis_guide <- guide_axis(n.dodge=1) #no line wrapping of x-axis labels
        }else{axis_guide <- guide_axis(n.dodge=2)} #wrap x-axis labels on 2 rows
        ggplot(YCstrength(), aes(x = reorder(combinedX, -yrclass), y = ycs)) +
          geom_bar(stat = "identity", aes(fill = "bar_fill", color = "bar_outline")) +
          geom_hline(yintercept = 0, linetype = "longdash", color = "black", linewidth = 0.3) +
          scale_fill_manual(values = c("bar_fill" = "grey80"), guide = "none") +
          scale_color_manual(values = c("bar_outline" = "black"), guide = "none") +
          labs(x = "Year Class", y = "Year class strength (std dev)",) +
          theme_cowplot(font_size = 20)+
          theme(axis.text.x = element_text(angle = 65, hjust = 1))
      }
  }
  #render YCstrength plot
  output$YCstrengthPlt <- renderUI({
    if(length(input$selectyear)!=1){ #cannot get cohort year if not one year of data.  Age data can be multiple years as it assumes length-age relationship constant, but sampling data must only include 1 yr
      return(div(style = "text-align: left; width: 100%;", 
                 "Year-class strength plot can only be created if one and only one year of data is selected."))
    }else{
        req(YCstrength())
        if(input$mort == TRUE){
            if(nrow(filter(YCstrength(), !is.nan(ycs)))>0){
              renderPlot(bg="transparent", height = 500,{ #height 500 makes plot area a nice size despite the large x-axis tick label space neededre
                suppressMessages({ #used to prevent console output about there already being a fill and this will replace the fill
                  YCstrengthPlot <- YCstrengPlot()+
                  scale_fill_manual(values = "#404688FF", guide = "none") +#481F70FF
                    scale_color_manual(values = "#FDE725FF", guide = "none")
                })
                print(YCstrengthPlot)
              })
            }else{
              return(div(style = "text-align: left; width: 100%;", 
                         "SE value from catch is curve not available, so recruitment indexes cannot be calculated"),br())
            }
        }
    }
  })
  
  #Download png of Growth Metrics plot
  output$downYCstrengthPlt <- downloadHandler(
    filename = function(){ 
      paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
            "RecruitVar", "png", sep = ".")},
    content = function(file){
      png(file, width = 675, height = 450)
      print(YCstrengPlot())
      dev.off()
    })
  
  
  #dialog box with more info about recruitment
  observeEvent(input$YCstrength_MoreInfo, {
    showModal(modalDialog(
      title = "Additional Information about recruitement indexes",
      "The figure presents year class strength estimates using standardized residuals from the weighted catch
      curve (Maciena 1997, Fish. Res. 32:115-121). Given these are standardized values, they center on 
      zero and scale in units of standard deviations.  So a year class strength value of -1.5 indicates a 
      cohort with abundance that is 1.5 standard deviations below the average value.  Keep in mind that in 
      a normal-shaped distribution, +/- 2 SD is roughly 95% of the data and +/- 3 SD is about 99.7% of the 
      data, so values outside these ranges indicate significant deviations from typical year class strength. 
      The R-squared value from the catch curve can also be used as an index of recruitment variability 
      (Isermann et al. 2002, NAJFM 22:1124–1135) across all year classes.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Percentile tab calculations##############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  #load initial data
  percentileData <- reactive({
    percentileData <- read.fst("PercentileData.fst", as.data.table = T)
    return(percentileData)
      })
    
  ##Calculate Selectize boxes(populate w/ gear/spp from Select Sample tab)

      #code creates a line break
        output$lineBrk <- renderUI({HTML("<br/>")})
        
    #Create N_agedMin slider on server side to get max age info
        maxN_aged <- reactive({max(percentileData()$N_aged, na.rm=TRUE)})
        output$NAgedMinSlider <- renderUI({sliderInput(inputId = "N_agedMin", label = 
               "Min # of aged fish required before using a survey's growth/mortality metrics in percentiles", 
               min = 30, max = maxN_aged(), sep = "", step=1, value = 150)
        })
        
      #Percentiles selectize box
        output$percentileInptBox <- renderUI({
            selectizeInput("percentileInpt", "To customize percentiles produced, type desired values in this box",
                "Percentiles:",choices = c(paste(1:99,rep("%",99),sep="")), multiple = TRUE, 
                options = list(placeholder = "click/type here"))
        })
     
          #Process input of percentileInptBox to establish which percentile values to calculate
            PercToProp = data.frame(prop = (c(seq(0.01,0.99, by= 0.01)))) %>% 
              mutate(perc = paste(prop  *100, "%", sep = ""))
            #create reactive value selPerctls() to store either default or custom proportions
            selPerctls <- reactive({
              if(is.null(input$percentileInpt)){
                selPerctls <- c(0.05,0.25,0.5,0.75,0.95)#sets default percentages
              }else{   #unless user specified percentiles to use
                #read input$percentleInp and add Prop from PercToProp table
                input_perc <- merge(data.frame(perc = input$percentileInpt), PercToProp, by = "perc", all.x = TRUE)
                selPerctls <- as.numeric(input_perc$prop) #use proportions as output
              }
                return(sort(selPerctls))
            }) 
                
    #Create N_SurveyMin slider on server side to get min # surveys to use and set it to # percentiles requested by default
      N_perc_columns <- reactive({length(selPerctls())}) 
      output$min_survey <- renderUI({sliderInput(inputId = "N_SurveyMin", label = 
            "Min # of surveys for which to calculate percentiles (effects all percentile tables)", 
            min = 3, max = 100, sep = "", step=1, value = N_perc_columns()*2)
      })
        
  #Function to convert gear.Name_Code to Gear.Code
  gearNameCode_toCode <- reactive({
    req(input$selPercGear)
         gear <- data.table(input$selPercGear)
          colnames(gear) <- "gear.Name_Code"
          gear <- merge(gear, gearinfo, by="gear.Name_Code", all.x=T)
          gearname <- sort.default(gear$Gear.Code)
      })
  #Function to convert species.Code_name to Species.Code
  SpeciesCodeName_toCode <- reactive({
         species <- data.table(input$selPercSpp)
          colnames(species) <- "species.Code_Name"
          species <- merge(species, speciesinfo, by="species.Code_Name", all.x=T)
          gearname <- sort.default(species$Species.Code)
      })
  #Function to convert lake.Name_Code to Species.Code
  LakeNameCode_toCode <- reactive({
         lake <- data.table(input$selLakeCodePerc)
          colnames(lake) <- "lake.Name_Code"
          lake <- merge(lake, lakeinfo, by="lake.Name_Code", all.x=T)
          lakename <- sort.default(lake$Lake.Code)
      })
  
  #used to determine if multiple selected gears are from the same gear family (will be allowed for PSD and mort, but not CPUE
  #percentile tables)
  gearFamilies <- reactive({
    req(gearNameCode_toCode())
    gearCodesUsed <- data.table(unique(gearNameCode_toCode()))
    colnames(gearCodesUsed) <- "Gear.Code" 
    gearCodesUsed2 <- merge(gearCodesUsed, gearinfo, by="Gear.Code")
    gearFamilies <- unique(gearCodesUsed2$Gear.Family)  
    return(gearFamilies)
  })
  
  #Initialize code-based selectize boxes and establish server-side processing of code box choices
    #Server-side processing of selectize boxes
      observe({
            updateSelectizeInput(session, "selPercGear", 
                choices =  sort.default(unique(percentileData()$gear.Name_Code)), 
                selected = if(input$toggleCodeName == TRUE){
                              gear <- data.table(as.numeric(as.character(input$selectgear)))
                              colnames(gear) <- "Gear.Code"
                              gear <- merge(gear, gearinfo, by="Gear.Code", all.x=T)
                              gearcode <- sort.default(gear$gear.Name_Code)
                            }else{
                                input$selectgear
                            }, server = TRUE)})
      observe({updateSelectizeInput(session, "selPercSpp", choices = 
              sort.default(unique(percentileData()$species.Code_Name)), server = TRUE)})
      observe({updateSelectizeInput(session, "selLakeCodePerc", 
              choices = sort.default(percentileData()$lake.Name_Code), server = TRUE)})
      

    ##Filter percentileData for selected parameters####
    #Filter main table based on lake, years, gears, region, and/or lake
    selPercData <- reactive({
      req(percentileData())
      if(input$tabs != "Select Sample"){#Prevent running while on data selection tab to speed it up
      selPercData <- percentileData()
        if(!is.null(input$selPercGear)){selPercData <- selPercData[selPercData$Gear.Code %in% c(gearNameCode_toCode()),]}
        if(!is.null(input$selPercSpp)){selPercData <- selPercData[selPercData$Species.Code %in% c(SpeciesCodeName_toCode()),]}
        if(!is.null(input$perYrs)){selPercData <- selPercData[selPercData$Year %in% c(input$perYrs[1]:input$perYrs[2]),]}
        if(!is.null(input$selRegionPerc)){selPercData <- selPercData[selPercData$ODWC.Region %chin% c(input$selRegionPerc),]}
        if(!is.null(input$selLakeCodePerc)){selPercData <- selPercData[selPercData$Lake.Code %chin% c(LakeNameCode_toCode()),]}
        return(selPercData)
      }else{return(NULL)}
    })
      
        output$selectedPercTable <- DT::renderDataTable({
          req(selData())
                  withProgress(message = "Creating table of selected data", min=0,max=10,value=1,{
                    incProgress(10)
                    expr=selPercData() 
                  })
          })

 ###Calculate percentiles for various metrics##############################
 
  #create reactive values for unit conversion variables
      units <- reactiveValues(
        MaxTlWt = FALSE,
        MLA = FALSE,
        MWtA = FALSE
        )
      
      #statements to change units$ values above based on status of the check boxes they track
      observeEvent(input$Max_TL_WT_inch_lb,{
        if(input$Max_TL_WT_inch_lb==TRUE){
          units$MaxTlWt <- TRUE
        }else{
          units$MaxTlWt <- FALSE
        }
      })  
      
      observeEvent(input$MLA_inch,{
        if(input$MLA_inch==TRUE){
          units$MLA <- TRUE
        }else{
          units$MLA <- FALSE
        }
      })

      observeEvent(input$MWtA_lb,{
        if(input$MWtA_lb==TRUE){
          units$MWtA <- TRUE
        }else{
          units$MWtA <- FALSE
        }
      })
    
  #Max TL and Wt table calculations
    maxTL_WT_percTable <- reactive({
      req(selPercData())
      # maxTL_Wt <- selPercData() %>% group_by(Species.Code) %>%
      maxTL_Wt <- selPercData() %>%
          select(SurveyID:Species.Code, maxTL:N_maxWt) %>% 
          # pivot_longer(cols = c(maxTL, maxWt), names_to = "Metric", values_to = "Values")
          data.table::melt(measure.vars = c("maxTL", "maxWt"), variable.name = "Metric", value.name = "Values")
      
      maxTL_Wt <- if(units$MaxTlWt==TRUE){#converts length from mm to inches and weight from g to Lb
        maxTL_Wt %>% mutate(Values = case_when(Metric=="maxTL" ~ Values * 0.0393701,
                                               Metric=="maxWt" ~ Values * 0.00220462))
      }else{
        maxTL_Wt
      }
  
      Nsurveys <- maxTL_Wt %>% subset(!is.na(Values)) %>% 
        .[, .("# Surveys"=.N), by=c("Species.Code", "Metric")] #count (.N) by Spp & metric groupings
          
      maxTL_WT_percMetrics <- maxTL_Wt %>% group_by(Species.Code, Metric) %>%
        group_modify(~{quantile(.x$Values, probs = selPerctls(), na.rm = TRUE) %>% tibble::enframe()}) %>%
        mutate(value2 = case_when(
          units$MaxTlWt==FALSE & Metric == "maxTL" ~ as.character(round(value, 0)),
          units$MaxTlWt==FALSE & Metric == "maxWt" ~ as.character(format(round(value, 0))),
          units$MaxTlWt==TRUE & Metric == "maxTL" ~ as.character(round(value, 1)),
          units$MaxTlWt==TRUE & Metric == "maxWt" ~ as.character(format(round(value, 2)))
        )) %>%
        select(-value) %>%
        pivot_wider(names_from = name, values_from = value2) %>%
        merge(Nsurveys, by = c("Species.Code", "Metric"), all.x = TRUE) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x = TRUE) %>% 
        relocate(Species.Name) %>%
        mutate(Metric=recode(Metric, "maxTL" = "Maximum TL", "maxWt" = "Maximum Weight")) %>% #rename metric values
        rename("Species Name" = "Species.Name") %>%
        arrange(Metric, "Species Name") %>%
        ungroup() %>% as.data.frame() %>% select(-Species.Code) 
      
      maxTL_WT_percMetrics <- maxTL_WT_percMetrics[complete.cases(maxTL_WT_percMetrics[,"# Surveys"]) &
        maxTL_WT_percMetrics$"# Surveys" >= input$N_SurveyMin,] #remove rows with # Surveys = NA or those with < min # surveys allowed
        #note subset() does not work well for above statements...not sure why
      
      maxTL_WT_percMetrics
    })
 
  #CPUE percentile table calculation
    SortCPUE <- reactive({SortPSD %>% mutate(CPUEmetric = paste("CPUE", SortPSD$PSDname, sep=""),
        cat_title = paste("CPUE-", SortPSD$PSDname, sep = "")) %>%
        mutate(cat_title = replace(cat_title, cat_title == "CPUE-", "Overall CPUE")) %>% select(-PSDname)
    })

    CPUEpercTable <- reactive({
      req(SortCPUE, selPercData(), input$N_SurveyMin)
      
      CPUEperc <- reactive({selPercData() %>% group_by(Species.Code) %>% 
        select(SurveyID:Nsites, CPUE:CPUEtrophy, -maxTL, -N_maxTL,-maxWt, -N_maxWt) %>% 
        pivot_longer(cols = CPUE:CPUEtrophy, names_to = "CPUEmetric", values_to="CPUEVal") 
      })
        
      Nsurveys <- reactive({
        req(CPUEperc())
        CPUEperc() %>% group_by(Species.Code,CPUEmetric) %>% 
        summarise("N_Surveys"=n(),.groups = "drop_last")
      })
      
     CPUEpercMetrics <- CPUEperc() %>%
        merge(Nsurveys(), by = c("Species.Code", "CPUEmetric"), all.x=T) %>% 
        filter(N_Surveys >= input$N_SurveyMin) %>%
        group_by(Species.Code, CPUEmetric, N_Surveys) %>% 
        group_modify(~{quantile(.x$CPUEVal, probs = selPerctls(), na.rm = TRUE) %>%tibble::enframe()}) %>%  
        pivot_wider(names_from = name,values_from = value) %>%
        merge(SortCPUE(), by="CPUEmetric", all.x=T) %>% arrange(Species.Code, sortOrder) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by="Species.Code", all.x=T) %>% 
        relocate(Species.Name) %>%
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
        select(-sortOrder, -CPUEmetric, -Species.Code) %>%
        relocate(cat_title, .after = Species.Name) %>%
        mutate(cat_title = case_when(cat_title == "CPUE-substock" ~ "CPUE < Stock",
                                     cat_title == "CPUE-stock" ~ "CPUE S-Q",
                                     cat_title == "CPUE-quality" ~ "CPUE Q-P",
                                     cat_title == "CPUE-preferred" ~ "CPUE P-M",
                                     cat_title == "CPUE-memorable" ~ "CPUE M-T",
                                     cat_title == "CPUE-trophy" ~ "CPUE Trophy+",
                                     TRUE ~ cat_title)) %>% 
        rename("Species Name" = "Species.Name", "CPUE Category" = "cat_title", "# Surveys" = "N_Surveys") 
    })

    #PSD percentile calculations
    PSDpercTable <- reactive({
      req(selPercData(), input$N_SurveyMin)
      PSDperc <- selPercData() %>% group_by(Species.Code) %>% 
        select(SurveyID:Nsites, NfishStock:PSD.T) %>% 
        pivot_longer(cols = PSD_S.Q:PSD.T, names_to = "PSDmetric", values_to="PSDVal") %>% 
        drop_na(PSDVal) %>% filter(NfishStock >= input$N_PSDMin)
      
      Nsurveys <- PSDperc %>% group_by(Species.Code,PSDmetric) %>% 
        summarise("N_Surveys"=n(),.groups = "drop_last")
      
      PSDsortOrder <- data.frame(PSDmetric=c("PSD", "PSD.P", "PSD.M", "PSD.T", 
                                   "PSD_S.Q", "PSD_Q.P", "PSD_P.M", "PSD_M.T"), sortOrder=1:8)
      PSDpercMetrics <- PSDperc %>% 
        merge(Nsurveys, by=c("Species.Code","PSDmetric"), all.x=T) %>% 
        filter(N_Surveys >= input$N_SurveyMin) %>% 
        group_by(Species.Code, PSDmetric, N_Surveys) %>% 
        group_modify(~{quantile(.x$PSDVal, probs=selPerctls(), na.rm=TRUE) %>%tibble::enframe()}) %>% 
        pivot_wider(names_from = name,values_from=value) %>% 
        merge(PSDsortOrder, by="PSDmetric", all.x=T) %>% arrange(Species.Code, sortOrder) %>% 
        merge(select(speciesinfo, Species.Code, Species.Name), by="Species.Code") %>% relocate(Species.Name) %>% 
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>% 
        mutate("PSD Metric" = recode(PSDmetric, PSD_S.Q = "PSD S-Q", PSD_Q.P = "PSD Q-P", PSD_P.M = 
           "PSD P-M", PSD_M.T = "PSD M-T", PSD.P = "PSD-P", PSD.M = "PSD-M", PSD.T = "PSD-T")) %>% 
        relocate("PSD Metric", .after = Species.Name) %>% 
        select(-sortOrder, -PSDmetric, -Species.Code) 
    })
    
    #Wr percentile calculations
    WrpercTable <- reactive({
      req(selPercData(), input$N_SurveyMin, input$Min_Wr_N)
        selPercDataWr <- selPercData() %>% select(SurveyID:Nsites, Species.Code, Nfish, N_Wr:Wr.trophy) %>%
          rename(N_Wr.total = N_Wr, Wr.total=Wr) %>%
          pivot_longer(cols = N_Wr.total:Wr.trophy, names_to = c(".value", "PSD_cat"), names_sep = "\\.", 
                       values_drop_na = TRUE)
                        #note \\ is needed in names_sep to identify the period is not a function but is the ASCI period

        selPercDataWr$Wr[selPercDataWr$N_Wr < input$Min_Wr_N] <- NA #remove Wr values with too few fish so it does not skew percentile

        WrPerc <- subset(selPercDataWr, !is.na(Wr)) #drop NA's

        NsurveysWr <- WrPerc %>% group_by(Species.Code, PSD_cat) %>%
          summarise("N_Surveys"=n(),.groups = "drop_last")

        SortWr <- data.frame(PSD_cat=c("total","substock","stock","quality","preferred","memorable","trophy"),
                                          sortOrder=1:7)

        WrPercMetrics <- WrPerc %>%
          merge(NsurveysWr, by = c("Species.Code","PSD_cat"), all.x=T) %>%
          filter(N_Surveys >= input$N_SurveyMin) %>% group_by(Species.Code, PSD_cat, N_Surveys) %>%
          group_modify(~{quantile(.x$Wr, probs = selPerctls(),na.rm=TRUE) %>%tibble::enframe()}) %>%
          pivot_wider(names_from = name, values_from = value) %>%
          merge(SortWr, by = "PSD_cat", all.x=T) %>% arrange(Species.Code, sortOrder) %>%
          merge(select(speciesinfo,Species.Code, Species.Name), by = "Species.Code", all.x=T) %>% 
          relocate(Species.Name) %>%
          relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
          mutate("Wr Metric" = recode(PSD_cat, total = "Wr all sizes", substock = "Wr < stock size", stock =
                          "Wr stock-quality size", quality = "Wr quality-preferred size", memorable =
                          "Wr memorable-preferred size", preferred = "Wr preferred-memorable size",
                          Trophy = "Wr > trophy size")) %>%
          relocate("Wr Metric", .after = Species.Name) %>%
          select(-sortOrder, -PSD_cat, -Species.Code)
    })
    
    
    #Mean TL at age (MLA) percentile calculations
    MLApercTable <- reactive({
      req(selPercData(), input$N_SurveyMin, input$N_agedMin)
      selPercDataMLA <- selPercData() %>% select(SurveyID:Nsites, N_aged, matches(c("MeanTL_Age", "MeanN_Age"))) %>%
              pivot_longer(cols = matches(c("MeanTL_Age", "MeanN_Age")), names_to = c(".value", "Age"),
                           names_sep = "_", values_drop_na = TRUE) %>%
              filter(MeanN >= input$Min_N_at_Age & N_aged >= input$N_agedMin)
      
      SortAge <- data.frame(Age = unique(selPercDataMLA$Age))  %>%
              mutate(sortOrder = as.numeric(sub("Age.", "", Age))) %>% #extract the age number by substituting null for the string "Age."
              arrange(sortOrder)
      
      selPercDataMLA <- if(units$MLA==TRUE){#converts length from mm to inches
        selPercDataMLA %>% mutate(MeanTL = MeanTL * 0.0393701)
      }else{
        selPercDataMLA
      }
      
      NsurveysMLA <- selPercDataMLA %>%
        subset(!is.na(MeanTL)) %>%
        group_by(Species.Code, Age) %>%
        summarise("N_Surveys"=n(),.groups = "drop_last")

      #Create MLA percentile table
      MLAPercMetrics <- selPercDataMLA %>%
        merge(NsurveysMLA, by = c("Species.Code", "Age"), all.x=T) %>%
        filter(N_Surveys >= input$N_SurveyMin) %>%
        group_by(Species.Code, Age, N_Surveys) %>%
        group_modify(~{quantile(.x$MeanTL, probs = selPerctls(), na.rm = TRUE) %>% tibble::enframe()}) %>%
        mutate(value = case_when(units$MLA==FALSE ~ as.character(round(value, digits = 0)), 
                                  TRUE ~ as.character(round(value, digits = 1)))) %>%
        pivot_wider(names_from = name, values_from = value) %>%
        merge(SortAge, by = "Age", all.x=T) %>% arrange(Species.Code, sortOrder) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x=T) %>% relocate(Species.Name) %>%
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
        mutate("Age class" = sub("\\.", "-", Age)) %>%  #replace periods with - in the age class column
        relocate("Age class", .after = Species.Name) %>%
        select(-sortOrder, -Age, -Species.Code)
    })
    
    #Mean Wt at age (MWtA) percentile calculations
    MWtApercTable <- reactive({
      req(selPercData(), input$N_SurveyMin, input$N_agedMin)
      MWtApercTable <- selPercData() %>% select(SurveyID:Nsites, N_aged, matches(c("MeanWt_Age", "MeanN_Age"))) %>%
        pivot_longer(cols = matches(c("MeanWt_Age", "MeanN_Age")), names_to = c(".value", "Age"),
                     names_sep = "_", values_drop_na = TRUE) %>%
        filter(MeanN >= input$Min_N_at_Age & N_aged >= input$N_agedMin)
      
      SortAge <- data.frame(Age = unique(MWtApercTable$Age))  %>%
        mutate(sortOrder = as.numeric(sub("Age.", "", Age))) %>% #extract the age number by substituting null for the string "Age."
        arrange(sortOrder)
      
      MWtApercTable <- if(units$MWtA==TRUE){#converts weight from g to Lb
        MWtApercTable %>% mutate(MeanWt = MeanWt * 0.00220462)
      }else{
        MWtApercTable
      }
      
      NsurveysMWtA <- MWtApercTable %>%
        subset(!is.na(MeanWt)) %>%
        group_by(Species.Code, Age) %>%
        summarise("N_Surveys"=n(),.groups = "drop_last")
      
      #Create mean Wt at age percentile table
      MWtAPercMetrics <- MWtApercTable %>%
        merge(NsurveysMWtA, by = c("Species.Code", "Age"), all.x=T) %>%
        filter(N_Surveys >= input$N_SurveyMin) %>%
        group_by(Species.Code, Age, N_Surveys) %>%
        group_modify(~{quantile(.x$MeanWt, probs = selPerctls(), na.rm = TRUE) %>% tibble::enframe()}) %>%
        mutate(value = case_when(units$MWtA==FALSE ~ as.character(round(value, digits = 0)), TRUE ~
               as.character(round(value, digits = 2)))) %>%
        pivot_wider(names_from = name, values_from = value) %>%
        merge(SortAge, by = "Age", all.x=T) %>% arrange(Species.Code, sortOrder) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x=T) %>% 
        relocate(Species.Name) %>%
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
        mutate("Age class" = sub("\\.", "-", Age)) %>%  #replace periods with "-" in the age class column
        relocate("Age class", .after = Species.Name) %>%  
        select(-sortOrder, -Age, -Species.Code) #%>% 
    })
    
    MortPercTable <- reactive({
      req(selPercData(), input$N_SurveyMin, input$N_agedMin, input$Min_Mort_R2)
      MortPercTable <- selPercData() %>% select(SurveyID:Nsites, N_aged, matches(c("MeanN_Age")), Z:A) %>%
        filter(N_aged >= input$N_agedMin & R2 >= input$Min_Mort_R2)

       NsurveyMort <- MortPercTable %>%
        subset(!is.na(A)) %>%
        group_by(Species.Code) %>%
        summarise("N_Surveys"=n(),.groups = "drop_last")

       MortPercMetrics <- MortPercTable %>%
        merge(NsurveyMort, by = c("Species.Code"), all.x=T) %>%
        filter(N_Surveys >= input$N_SurveyMin) %>%
        group_by(Species.Code, N_Surveys) %>%
         group_modify(~{quantile(.x$A, probs = selPerctls(), na.rm = TRUE) %>% tibble::enframe()}) %>%
        mutate(value = as.character(paste(round(value, digits = 0), "%", sep = ""))) %>%
        pivot_wider(names_from = name, values_from = value) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x=T) %>% 
        relocate(Species.Name) %>%
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
        select(-Species.Code) %>% 
        filter(!is.na(Species.Name)) #this basically replaces the search for NA values in first row on renderTable
    })

    
  ###Make percentile output tables##############################
    ###Max TL and Wt output table
      output$Max_TL_Wt_perc <- renderTable(spacing = "xs", {
        req(maxTL_WT_percTable(), input$N_SurveyMin)
        if(!is.null(input$selPercGear) | !is.null(input$selPercSpp)){
          if(rowSums(is.na(maxTL_WT_percTable()[1,])) < ncol(maxTL_WT_percTable())){#only show if first row does not have NA in all columns
            maxTL_WT_percTable()
          }else{
            NULL
          }
        }
      })
    #render text over table or message explaining why no output is given
    maxTL_WTtext <- reactive({
        if(!is.null(input$selPercGear) | !is.null(input$selPercSpp)){
          if(rowSums(is.na(maxTL_WT_percTable()[1,])) < ncol(maxTL_WT_percTable())){#only show if first row does not have NA in all columns
            if(units$MaxTlWt==FALSE){
              maxTL_WTtext <- as.character("Maximum TL (mm) and Weight (g) Percentiles")
            }else{
              maxTL_WTtext <- as.character("Maximum TL (inches) and Weight (lbs) Percentiles")
            }
          }else{
            maxTL_WTtext <- as.character("Maximum TL and Weight Percentiles - insufficent data exists to calculate percentiles, please select different parameters.")
          }
        }else{
          maxTL_WTtext <- NULL
        }
      return(maxTL_WTtext)
      })
      
    #display above text
      output$Max_TL_Wt_Text <- renderText({
        maxTL_WTtext()
      })
      
      #Downloadable Max TL and Wt table
      output$downMax_TL_Wt_perc <- downloadHandler(
          filename = function() {
            paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                  "_MaxTL_Wt_perc", "csv", sep = ".")
          },
          content = function(file) {
            write.csv(maxTL_WT_percTable(), file, row.names = FALSE)
          }
      )
      #MaxTL_WT table download button server-side rendering so it can be conditional on maxTL_WT_percTable()
      output$Max_TL_Wt_percRendered <- renderUI({
        req(maxTL_WT_percTable())
        if(rowSums(is.na(maxTL_WT_percTable()[1,])) < ncol(maxTL_WT_percTable())){#only show if first row does not have NA in all columns
          downloadButton("downMax_TL_Wt_perc","Max TL & Wt Table")
        }
      })
      #create conditional checkbox for changing units...will only show if table is rendered. 
      output$MaxTlWtUnits <- renderUI({
          if(rowSums(is.na(maxTL_WT_percTable()[1,])) < ncol(maxTL_WT_percTable())){#only show if first row does not have NA in all columns
              checkboxInput("Max_TL_WT_inch_lb", "Display Measurements in English Units (inches & lbs)",
                      value = units$MaxTlWt) #not setting value=FALSE prevents turning off inches as table re-renders
          }
      })
      #conditionally create help text about unit conversion
      output$MaxTlWtHelpText <- renderUI({
        if(rowSums(is.na(maxTL_WT_percTable()[1,])) < ncol(maxTL_WT_percTable())){#only show if first row does not have NA in all columns
          helpText("Default is metric (mm and g). Checkbox changes units to inches and lbs.")
        }
      })
      
    ###Put CPUE percentile data in CPUEpercTable into output table
      output$CPUEperc <- renderTable(digits = 2, spacing = "xs", {
        req(CPUEpercTable())
        if(!is.null(input$selPercGear)){
          if(length(input$selPercGear) == 1){
            if(rowSums(is.na(CPUEpercTable()[1,])) < ncol(CPUEpercTable())){#only show if first row does not have NA in all columns
              CPUEpercTable()
            }
          }
        }
      })
      
      output$CPUEpercText <- renderText({
        if(length(input$selPercGear) != 1){
            CPUEtext <- as.character("CPUE Percentiles are only relvant within a single gear type.  Please select one and only one gear to see results.")
         }else if(rowSums(is.na(CPUEpercTable()[1,])) < ncol(CPUEpercTable())){#only show if first row does not have NA in all columns 
            CPUEtext <- as.character("CPUE Percentiles")
        }else{
            CPUEtext <- as.character("CPUE Percentiles - insufficent data exists to calculate percentiles, please select different parameters.")
        }
      })
      
      #Downloadable CPUE perc table
      output$downCPUEperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_CPUEperc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(CPUEpercTable(), file, row.names = FALSE)
        }
      )
      output$CPUEpercRendered <- renderUI({
        req(CPUEpercTable())
        if(rowSums(is.na(CPUEpercTable()[1,])) < ncol(CPUEpercTable())){#only show if first row does not have NA in all columns 
          downloadButton("downCPUEperc","CPUE Table")
        }
      })
      
    ###Put PSD percentile data in PSDpercTable into output table
      output$PSDperc <- renderTable(digits = 0, spacing = "xs", {
        req(PSDpercTable())
        if(!is.null(input$selPercGear)){
            if(length(gearFamilies()) == 1){#allow user to combine gears within gear families only
              if(rowSums(is.na(PSDpercTable()[1,])) < ncol(PSDpercTable())){#only show if first row does not have NA in all columns 
                PSDpercTable()
              }
            }
        } 
      })
      
      output$PSDpercText <- renderText({
        req(gearFamilies())
        if(length(gearFamilies()) != 1){#allow user to combine gears within gear families only
            CPUEtext <- as.character("PSD Percentiles - because most gears are strongly size biased, PSDs are only comparable within a single gear type.  Please select one and only one gear to see results.")
          }else if(rowSums(is.na(PSDpercTable()[1,])) < ncol(PSDpercTable())){#only show if first row does not have NA in all columns 
            PSDtext <- as.character("PSD Percentiles")
          }else{
            PSDtext <- as.character("PSD Percentiles - insufficent data exists to calculate percentiles, please select different parameters.")
          }
      })
      
      #Downloadable PSD perc table
      output$downPSDperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_PSDperc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(PSDpercTable(), file, row.names = FALSE)
        }
      )
      output$PSDpercRendered <- renderUI({
        req(PSDpercTable())
        if(rowSums(is.na(PSDpercTable()[1,])) < ncol(PSDpercTable())){#only show if first row does not have NA in all columns 
          downloadButton("downPSDperc","PSD Table")
        }
      })
      
    ###Put Wr percentile data from WrpercTable into output table
      output$Wrperc <- renderTable(digits = 0, spacing = "xs", {
        req(WrpercTable())
          if(rowSums(is.na(WrpercTable()[1,])) < ncol(WrpercTable())){#only show if first row does not have NA in all columns 
            WrpercTable()
          }
      })
      output$WrpercText <- renderText({
        if(rowSums(is.na(WrpercTable()[1,])) < ncol(WrpercTable())){#only show if first row does not have NA in all columns 
           Wrtext <- as.character("Wr Percentiles")
          }else{
            Wrtext <- as.character("Wr Percentiles - insufficent data exists to calculate percentiles, please select different parameters.")
          }
      })
      
      #Downloadable Wr perc table
      output$downWrperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_Wr_perc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(WrpercTable(), file, row.names = FALSE)
        }
      )
      output$WrpercRendered <- renderUI({
        req(WrpercTable())
        if(rowSums(is.na(WrpercTable()[1,])) < ncol(WrpercTable())){#only show if first row does not have NA in all columns 
          downloadButton("downWrperc","Wr Table")
        }
      })
      
     ###Put MLA (mean length at age) percentile data from  MLApercTable into output table
      output$MLAperc <- renderTable(spacing = "xs", {
        req(MLApercTable())
          if(rowSums(is.na(MLApercTable()[1,])) < ncol(MLApercTable())){#only show if first row does not have NA in all columns 
            MLApercTable()
          }
      })
      output$MLApercText <- renderText({
        if(rowSums(is.na(MLApercTable()[1,])) < ncol(MLApercTable())){#only show if first row does not have NA in all columns 
            if(units$MLA==FALSE){
              Wrtext <- as.character("Mean Length-at-Age Percentiles (mm TL)")
            }else{
              Wrtext <- as.character("Mean Length-at-Age Percentiles (inches TL)")
            }
          }else{
            Wrtext <- as.character("Mean Length-at-Age Percentiles - insufficent data exists to calculate percentiles, please select different survey criteria or data quality parameters.")
          }
      })
      
      #Downloadable MLA perc table
      output$downMLAperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_MLAperc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(MLApercTable(), file, row.names = FALSE)
        }
      )
      #conditionally render the MLA download button
      output$MLApercRendered <- renderUI({
        req(MLApercTable())
        if(rowSums(is.na(MLApercTable()[1,])) < ncol(MLApercTable())){
          downloadButton("downMLAperc","TL @ Age Table")
        }
      })
      #create conditional checkbox for changing units...will only show if table is rendered. 
      output$MLA_Units <- renderUI({
          if(rowSums(is.na(MLApercTable()[1,])) < ncol(MLApercTable())){#only show if first row does not have NA in all columns
              checkboxInput("MLA_inch", "Display Measurements in English Units (inches)",
                      value = units$MLA) #not setting value=FALSE prevents turning off inches as table re-renders
          }
      })
      #conditionally render the helper text for unit conversion
      output$MLAhelpText <- renderUI({
        req(MLApercTable())
        if(rowSums(is.na(MLApercTable()[1,])) < ncol(MLApercTable())){
          helpText("Default is metric (mm). Checkbox changes units to inches.")
        }
      })
      
    ###Put mean wt at age (MWtA) percentile data from  MWtApercTable into output table
      output$MWtAperc <- renderTable(spacing = "xs", {
        req(MWtApercTable())
            if(rowSums(is.na(MWtApercTable()[1,])) < ncol(MWtApercTable())){#only show if first row does not have NA in all columns 
                MWtApercTable()
            }
          })
      output$MWtApercText <- renderText({
           if(rowSums(is.na(MWtApercTable()[1,])) < ncol(MWtApercTable())){#only show if first row does not have NA in all columns 
               if(units$MWtA==FALSE){
                  Wrtext <- as.character("Mean Weight-at-Age Percentiles (g)")
                }else{
                  Wrtext <- as.character("Mean Weight-at-Age Percentiles (lbs)")
                }
              }else{
                Wrtext <- as.character("Mean Weight-at-Age Percentiles - insufficent data exists to calculate percentiles, please select different survey criteria or data quality parameters.")
              }
      })
      #Downloadable MWtA perc table
      output$downMWtAperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_MWtAperc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(MWtApercTable(), file, row.names = FALSE)
        }
      )
      #conditionally render the MWtA download button
      output$MWtApercRendered <- renderUI({
        req(MWtApercTable())
        if(rowSums(is.na(MWtApercTable()[1,])) < ncol(MWtApercTable())){
          downloadButton("downMWtAperc","Wt @ Age Table")
        }
      })
      #create conditional checkbox for changing units...will only show if table is rendered. 
      output$MWtA_Units <- renderUI({
          if(rowSums(is.na(MWtApercTable()[1,])) < ncol(MWtApercTable())){#only show if first row does not have NA in all columns
              checkboxInput("MWtA_lb", "Display Measurements in English Units (pounds)",
                      value = units$MWtA) #not setting value=FALSE prevents turning off inches as table re-renders
          }
      })
      #conditionally render the helper text for unit conversion (still need to work on how to hide the unit checkbox)
      output$MWtAhelpText <- renderUI({
        req(MWtApercTable())
        if(rowSums(is.na(MWtApercTable()[1,])) < ncol(MWtApercTable())){
          helpText("Default is metric (g). Checkbox changes units to pounds.")
        }
      })
      
    ###Put Mortality percentile data in MortPercTable into output table
      output$Mortperc <- renderTable(spacing = "xs", {
        req(MortPercTable())
        if(!is.null(input$selPercGear)){
            if(length(gearFamilies()) == 1){#allow user to combine gears within gear families only
            if(rowSums(is.na(MortPercTable()[1,])) < ncol(MortPercTable())){#only show if first row does not have NA in all columns 
              MortPercTable()
            }
          }
        }
      })
      output$MortpercText <- renderText({
        # if(length(input$selPercGear) != 1){
        if(length(gearFamilies()) != 1){#allow user to combine gears within gear families only
            CPUEtext <- as.character("Annualized Mortality Percentiles - Because most gears are strongly size biased, mortality estimates are only comparable within a single gear type.  Please select one and only one gear to see results.")
        }else if(rowSums(is.na(MortPercTable()[1,])) < ncol(MortPercTable())){#only show if first row does not have NA in all columns 
            CPUEtext <- as.character("Annualized Mortality Percentiles")
        }else{
            CPUEtext <- as.character("Annualized Mortality Percentiles - insufficent data exists to calculate percentiles, please select different parameters.")
        }
      })
      
      #Downloadable Mort perc table
      output$downMortperc <- downloadHandler(
        filename = function() {
          paste(input$selPercGear,input$selPercSpp,input$perYrs, input$selRegionPerc, input$selLakeCodePerc,
                "_Mort_perc", "csv", sep = ".")
        },
        content = function(file) {
          write.csv(MortPercTable(), file, row.names = FALSE)
        }
      )
      output$MortrpercRendered <- renderUI({
        req(MortPercTable())
        if(rowSums(is.na(MortPercTable()[1,])) < ncol(MortPercTable())){#only show if first row does not have NA in all columns 
          downloadButton("downMortperc","Mortality Table")
        }
      })
      


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
#stocking information Tab########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  stockinfo <- reactive({
     
    withProgress(message = "Loading Data", min=0,max=10,value=1, {
      
      # stockinfo <- stockingData[stockingData$Year >= input$stockrange[1] & stockingData$Year <= input$stockrange[2],]
      stockinfo <- stockingData[Year >= input$stockrange[1] & Year <= input$stockrange[2]]
      
    if(!is.null(input$stocklake)){ 
      stockinfo <- stockinfo[stockinfo$Water.Body %chin% c(input$stocklake),]
    }
    if(!is.null(input$stockspp)){
      stockinfo <- stockinfo[stockinfo$Species %chin% c(input$stockspp),]
    }
      stockinfo <- stockinfo %>% mutate(Number=format(round(as.numeric(Number),0), big.mark = ","))
      stockinfo <- stockinfo %>% select(1:11)
      colnames(stockinfo) <- c("Year", "Date Stocked", "Water Body", "Stocking Site", "Species", "Number Stocked",
                               "Weight (lb)",  "Size (in)", "Fish/lb", "Hatchery (origin)", "Mortality (%)")
      return(stockinfo)
    })
  })


  #render stocking info table
    output$stocktable <- DT::renderDataTable(
      if(!is.null(input$stocklake) | !is.null(input$stockspp))
        stockinfo(), rownames = FALSE,
      options = list(pageLength = 50)
    )
  
  #Downloadable csv of stocking information table
  output$downstocking <- downloadHandler(
    filename = function() {
      paste("stockingInfo", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(stockinfo(), file, row.names = FALSE)
    }
  )
  
}
    