#Packages to be installed and loaded############################
library(shiny)
# library(profvis)Used for profiling to see errors and find slow steps in code
library(dplyr) #had used join() but I changed most to dplyr left_join and remaining ones plyr::join.  Now just need join_all
library(tidyr)
library(FSA)
library(nnet)#multinomial regression for ALK's
library(nlstools) #von Bert curv fitting 
library(tibble)
library(rlang) #is_empty() which catches NULL values, but also other forms of null like conditions not caught by is.null()
library(data.table) #used for fread, which is faster way to load .csv files
library(fst)#for loading fst saved files
# library(periscope)#trying this to make downloadable figures


#.csv's to read in#############################################
gearinfo <- read.fst("gearinfo.fst", as.data.table = TRUE)
lakeinfo <- read.fst("lakeinfo.fst", as.data.table = TRUE)
speciesinfo <- read.fst("speciesinfo.fst", as.data.table = TRUE)
gabel <- read.fst("gabelhousenames.fst", as.data.table = TRUE)
wsnames <- read.fst("WSnames.fst", as.data.table = TRUE)
ageData_file <- read.fst("compiledagedata.fst", as.data.table = TRUE)
stockingData <- read.fst("stockingData.fst", as.data.table = TRUE)
SortPSD <- data.table(PSDname=c("","substock", "stock","quality", "preferred", "memorable", "trophy"),
                      sortOrder=c(1,2,3,4,5,6,7), key="sortOrder")
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

    #if user uploads data, automatically check the "load your own data" box. 
    observeEvent(input$loadedSampleData, {
      if(!is.na(input$loadedSampleData)){
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
          
        #deal with user-uploaded data  
       }else if(input$selectedData =="loadedSampleData"){
         if(is.null(input$loadedSampleData)){
           removeModal()
           showModal(NoDataLoaded) 
         }else{
         mainDataRead <- as.data.table(fread(input$loadedSampleData$datapath))
            mainDataRead[mainDataRead == "."] <- NA
            mainDataRead <- mutate(mainDataRead, TL_mm = as.numeric(as.character(TL_mm)),
                              Wt_g = as.numeric(as.character(Wt_g))) %>% 
                merge(select(lakeinfo, Lake.Code, Lake.Name), by="Lake.Code", all.x = TRUE) %>%
                merge(select(gearinfo, Gear.Code, Gear.Name), by="Gear.Code", all.x = TRUE) %>%
                unite(Lake.Name, Lake.Code, col="lake.Name_Code", sep = " - ", remove = F) %>%
                unite(Gear.Name, Gear.Code, col="gear.Name_Code", sep = " - Code ", remove = F) %>%
                merge(select(speciesinfo, Species.Code, Species.Name, species.Code_Name), by="Species.Code", all.x=TRUE) %>% 
                relocate(lake.Name_Code, gear.Name_Code, species.Code_Name, .after = last_col()) %>% 
                setkey(Lake.Code, Year, Gear.Code, Month)
            output$dataBeingUsed <- renderText({
              return(paste("<span style=\"color:green\">Using uploaded sample data</span>"))
            })
           mainData$df_data <- mainDataRead
           setkey(mainData$df_data, Lake.Code, Year, Gear.Code, Month)
           removeModal()
         }
       }else{
          #if a check box was used, load associated data file accordingly
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
    #Adds ability to upload files greater than 5 MB (max now 200 MB)
      options(shiny.maxRequestSize=200*1024^2)
      
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
            # req(LakeAvail())
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
        if(is_empty(input$selectmonth)){
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
      req(selDataspp(), input$selagelake, input$selageyears, input$selagespp, input$selectspecies, 
          input$toggleCodeName)
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp) &&
         length(input$selagelake) != 0 && length(input$selageyears) != 0  && length(input$selagespp) != 0 &&
         length(input$selectspecies) !=0){
             if(input$toggleCodeName == TRUE){
                if(input$selectspecies == input$selagespp &&
                   selDataspp()$Lake.Code %chin% c(input$selagelake) &&
                   selDataspp()$Year %in% c(input$selageyears)
                   ){
                    yesmatch <- as.character("Matched Age Dataset")
                    return(yesmatch)
                }  
             }else{
               if(input$selectspecies == input$selagespp &&
                  selDataspp()$lake.Name_Code %chin% c(input$selagelake) &&
                  selDataspp()$Year %in% c(input$selageyears)
                  ){
                 yesmatch <- as.character("Matched Age Dataset")
                 return(yesmatch)
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
            if(input$selectspecies != input$selagespp ||
               selDataspp()$Lake.Code != input$selagelake ||
               selDataspp()$Year != input$selageyears)
              {
                nomatch <- as.character("Not a Matched Age Dataset")
                return(nomatch)
            }
          }else{
            if(input$selectspecies != input$selagespp ||
               selDataspp()$lake.Name_Code != input$selagelake ||
               selDataspp()$Year != input$selageyears){
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

     #Create agedata either from Use user-supplied data (based on check box) or built-in data (if box not checked)
      agedata <- reactive(if(input$loadageCheck == TRUE){
          uploadAgeData <- input$loadedageData
              if(is.null(uploadAgeData)){
                return(NULL)
              }
              agedata <- fread(uploadAgeData$datapath)
              agedata <- agedata %>% 
                merge(select(lakeinfo, Lake.Code, Lake.Name), by = "Lake.Code", all.x=T) %>%
                merge(select(gearinfo, Gear.Code, Gear.Name), by.x = "Gear", by.y = "Gear.Code", all.x=T) %>%
                merge(select(speciesinfo, Species.Code, Species.Name), by = "Species.Code", all.x=T) %>%
                unite(Lake.Name, Lake.Code, col="lake.Name_Code", sep = " - ", remove = F) %>%
                unite(Gear.Name, Gear, col="gear.Name_Code", sep = " - Code ", remove = F) %>%
                unite(Species.Code, Species.Name, col="species.Code_Name", sep = " - ", remove = F) %>%
                relocate(lake.Name_Code, gear.Name_Code, species.Code_Name, .after = last_col()) %>%
                as.data.table()
              agedata <- setkey(agedata, Lake.Code, Year, Gear, Species.Code)
        }else{
          agedata <- ageData_file
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
                    selageDatafinal <- mutate(selageDatafinal, TLmm = as.numeric(as.character(TLmm)),
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
    
    #Selected sample summary###
      output$lakename3 <- renderText({lakename()})
      output$year3 <- renderText({yearname()})
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
                                 dimnames = list(c(),c("Species", "Number")))
        }
      })
      
    ###Total Effort Table###
      output$toteffort <- renderTable(digits=1, spacing="xs", rownames=FALSE, {
        req(selData())
        if(input$totaleffort == TRUE){
          if(selData()$Gear.Code[[1]] <= 40){ #netting and seining (use effort)
            aggeffort <- aggregate(Effort ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
          }else{ #electrofishing (use gear length)
            aggeffort <- aggregate(Gear.Length ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
          }  
          colnames(aggeffort)[3] <- "Effort"
          effortbygear <- aggregate(Effort ~ Gear.Code, data=aggeffort, sum, na.action=NULL)
          listofgearcodes <- effortbygear$Gear.Code
          listofgearnames <- as.data.table(unique(selData()$Gear.Code))
          colnames(listofgearnames) <- "Gear.Code"
          listofgearnames <- merge(listofgearnames,gearinfo,by="Gear.Code", all.x=T)
          listofgearnames <- as.character(listofgearnames[,2])
          table <- cbind(effortbygear,listofgearnames)
          table <- table[c(1,3,2)]
          colnames(table)[1:3] <- c("Gear Code"," Gear Name", "Effort")
          namedtable <- table
        }
      })
      
    ##Total CPUE Table##################################################
    # Dan already optomized using data.table approach
      cpuetable <- reactive({
        req(selData())
        if(input$cpue == TRUE){
          #Aggregate into total catch by species per sample (also return gear code and effort) - transform SampleID to character
          aggselData <- as.data.table(aggregate(Number.of.individuals ~ SampleID*Species.Name+Gear.Code, 
                                  data=selData(), sum))
          aggselDatach <- aggselData %>% mutate_if(is.factor, as.character)
          #Add zeroes for samples that didn't catch a particular species - FSA
          selDatazero <- as.data.table(addZeroCatch(as.data.frame(aggselDatach), "SampleID", "Species.Name", #must be data.fram as addZeroCatch won't work on data
                                                    "Number.of.individuals") %>% 
            filter(Species.Name != "No fish in sample"))
          
        #Pull out effort from selData and join to dataset
          #Calculate CPUE based on gear code
          if(aggselData$Gear.Code[[1]]  >= 41){ #electrofishing; get value from 1st row of Gear.Code (col 3), and see if >=41
            lengthbysample <- as.data.table(aggregate(Gear.Length ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, lengthbysample, by="SampleID", all.x=TRUE)
            samplecpue <- (mutate(allcpue, CPUE = (Number.of.individuals*60)/Gear.Length))
          } 
          if(aggselData$Gear.Code[[1]] <= 40 & aggselData[1,3] != 10){ #netting; get value from 1st row of Gear.Code (col 3), and see if >=41
            effortbysample <- as.data.table(aggregate(Effort ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, effortbysample, by="SampleID", all.x=TRUE)
            samplecpue <- (mutate(allcpue, CPUE = (Number.of.individuals*24)/Effort))
          }   
          if(aggselData$Gear.Code[[1]] == 10){ #seining; get value from 1st row of Gear.Code (col 3), and see if >=41
            effortbysample <- as.data.table(aggregate(Effort ~ SampleID, selData(), mean))
            allcpue <- merge(selDatazero, effortbysample, by="SampleID", all.x=TRUE)
            samplecpue <- as.data.table(mutate(allcpue, CPUE = (Number.of.individuals*1076)/Effort))
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
          setnames(cpuetable, c(1:9), c("Species","Mean","Count","RSE","SE","L 95% CI",
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
    })
    
    #Make reference table for PSD groupings (in) 
    output$sizerefin <- renderTable(digits=1, rownames=TRUE, spacing="xs", na = " ",{
      if(input$cpuesize == TRUE){
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
    })
            
    ##CPUE by PSD Size Category Calculations#############################
    cpuesizetable <- reactive({
      if(input$cpuesize == TRUE){
        
        #read in data...I'm doing this to avoid running selData() 2x (once for noTL and again to make cpuesizetable)
          selData <- selData()
        
        #Find spp for which no TL data were taken
          noTL <- selData %>% group_by(Species.Code) %>% summarise(non_na_count = sum(!is.na(TL_mm)), 
            .groups = "drop") %>% subset(non_na_count == 0)
          
        #Add Spp names using Gabelhouse.Name spelling, make sure everything is character data type (no factors),
          cpuesizetable <-merge(selData, gabel, by = "Species.Code", all.x=T) %>% mutate_if(is.factor, as.character) %>% 
              
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
            mutate(Effort2 = case_when(Gear.Code >= 41 ~ 60/Gear.Length,  #EF gears (Number.of.individuals*60)/Gear.Length where gear length is in min. = #/hr
                           Gear.Code <= 40 & Gear.Code != 10 ~ 24/Effort,  #net gears (Number.of.individuals*24)/Effort where effort is number of net nights= #/24h
                           Gear.Code == 10 ~ 1076/Effort)) %>%  #Seine (Number.of.individuals*1076)/Effort...not sure how this works, but should make #/ft^2

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
            select(-SD, -Count, -CV) %>% relocate(RSE, .after=Mean) %>% rename(Species = Species.Name, "Size Category" = psdval)
      }
    })
    
    #create CPUE by PSD size table to display
    output$cpuebysize <- renderTable(digits = 2, spacing = "xs", na=" ", {
      if(input$cpuesize == TRUE){
        withProgress(message = "Doing Math for You",min=0,max=10,value=1, {
          cpuesizetable()
        })
      }
    })
   
    
    ##Catch Tab Download buttons##########################
      # Downloadable csv of total cpue table
      output$downcpue <- downloadHandler(
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
          if(input$selectspecies == input$selagespp &&
             selDataspp()$Lake.Code %chin% c(input$selagelake) &&
             selDataspp()$Year %in% c(input$selageyears)){
            ageDataMatch <- as.character("Matched Age Dataset was used")
          }
        }else{
          if(input$selectspecies == input$selagespp &&
             selDataspp()$lake.Name_Code %chin% c(input$selagelake) &&
             selDataspp()$Year %in% c(input$selageyears)){
            ageDataMatch <- as.character("Matched Age Dataset was used")
          }
        }
      }
    })
    #display text showing not a matched age dataset in red
    output$ageDataNoMatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$toggleCodeName == TRUE){
          if(input$selectspecies != input$selagespp ||
             selDataspp()$Lake.Code != input$selagelake ||
             selDataspp()$Year != input$selageyears){
            ageDataNoMatch <- as.character("Age Dataset was NOT a Match to Sample Dataset")}
        }else{
          if(input$selectspecies != input$selagespp ||
             selDataspp()$lake.Name_Code != input$selagelake ||
             selDataspp()$Year != input$selageyears){
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
    
  ##Length Frequency Histogram####################################
    lfplot <- function(){
      if(input$lengthfrequency == TRUE){
        #set plot margins
        par(mar=c(5,5,1,4)+0.1)
        maxtl <- max(selDataspp()$TL_mm, na.rm = na.omit)
        xaxis <- seq(0,(maxtl+30), input$lengthbin)
        #histogram function with slider input as "number of bins"
        if(xaxis[2]!=25.4){
          lfhist <- hist(selDataspp()$TL_mm, breaks = xaxis, plot = FALSE)
          lfhist$counts=lfhist$counts/sum(lfhist$counts)*100
          lfplot <- plot(lfhist, main = NULL, xlab="Total Length (mm)", ylab="% of Observed Fish",
               cex.lab = 1.5, cex.axis = 1.4)
        }
        if(xaxis[2]==25.4){
          selDatain <- mutate(selDataspp(), TL_mm = TL_mm/25.4)
          maxin <- max(selDatain$TL_mm, na.rm = na.omit)
          xaxisin <- seq(0,(maxin+1), 1)
          lfhist <- hist(selDatain$TL_mm, breaks = xaxisin, plot = FALSE)
          lfhist$counts=lfhist$counts/sum(lfhist$counts)*100
          lfplot <- plot(lfhist, main = NULL, xlab="Total Length (in)", ylab="% of Observed Fish",
               cex.lab = 1.5, cex.axis = 1.4)
        }
      } 
    }
  #render the length frequency plot
  output$lengthhist <- renderPlot(bg="transparent",{
    if(input$lengthfrequency == TRUE){
      withProgress(message = "Doing Math for You",min=0,max=10,value=1, {
        lfplot()
      })
    }
  })
  
  #create table with length frequency data
  lftable <- reactive({
    if(input$lengthfrequency == TRUE){
      maxtl <- max(selDataspp()$TL_mm, na.rm = na.omit)
      xaxis <- seq(0,(maxtl+30), input$lengthbin)
      
   #histogram function with slider input as "number of bins"
      if(xaxis[2]!=25.4){
        lfhist <- hist(selDataspp()$TL_mm, breaks = xaxis, plot = FALSE)
        lfhist$counts=lfhist$counts/sum(lfhist$counts)*100
      }
      if(xaxis[2]==25.4){
        selDatain <- mutate(selDataspp(), TL_mm = TL_mm/25.4)
        maxin <- max(selDatain$TL_mm, na.rm = na.omit)
        xaxisin <- seq(0,(maxin+1), 1)
        lfhist <- hist(selDatain$TL_mm, breaks = xaxisin, plot = FALSE)
        lfhist$counts=lfhist$counts/sum(lfhist$counts)*100
      }
      bins <- data.frame(lfhist$breaks)
      nada <- max(as.numeric(rownames(bins)))
      bins <- bins[-nada,]
      percent <- data.frame(lfhist$counts)
      lftable <- cbind(bins, percent)
      colnames(lftable) <- c("Length Bin", "Percent")
      lftable <- lftable
      
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
    })
 
    #actual psd values table
    psdfinal <- reactive({
      if(input$psd == TRUE){
        psdvals <- as.data.table(psdVal(gabelname()))
        #calculate psd's from FSA function
        psdCalc <- psdCalc(~TL_mm, gabelseldata(), gabelname(), units="mm", 
                           method="multinomial", what="all", showIntermediate = FALSE)
        colnames(psdCalc)[1:3] <- c("PSD Value", "L 95% CI", "U 95% CI")
        psdfinal <- as.data.frame(psdCalc)
      }
    })
  #render psd table
  output$psdtable <- renderTable(rownames=TRUE, spacing="xs",digits=0,{
    if(input$psd == TRUE){
      psdfinal()
    }
  })
    
  ##Relative Weight Table#########################################
    
    #create standard weight equation for reference
    output$standardequation <- renderTable(rownames = FALSE, digits = 3,{
      if(input$wr == TRUE){
        standard <- merge(selDataspp(), wsnames, by="Species.Code", all.x = TRUE)
        wsName <- as.character(standard[1,"wsname"]) 
        standardeq <- wsVal(wsName)
        standardeq <- standardeq[c(1,3,4,7:10)]
        colnames(standardeq) <- c("Species","Model Type","Reference Percentile","Min.TL",
                                  "Intercept","Slope","Source")
        standardeq <- mutate(standardeq, Min.TL = as.integer(Min.TL))
      }
    })  
    
    table <- reactive({
      if(input$wr == TRUE){
        wrpsdjoindata <- merge(gabelseldata(), wsnames, by="Species.Code", all.x = TRUE)
        #Calculate psd size category and append to selected data
        psdval <- psdAdd(TL_mm~Gabelhouse.Name, units="mm", data=wrpsdjoindata)
        wrpsdjoindata$psdvalue <- psdval
        #Calculate relative weight and append to selected data
        wrvalue <- wrAdd(Wt_g ~ TL_mm + wsname, units="metric", data = wrpsdjoindata)
        wrpsdjoindata$wrvalue <- wrvalue
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
        colnames(calculate) <- c("Size Category","Mean","Count","CV","SE","L 95% CI","U 95% CI")
        wtable <- calculate
        
        #Calculate overal Wr
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
        colnames(all) <- c("Size Category","Mean","Count","CV","SE","L 95% CI","U 95% CI")
        table <- rbind(wtable, all)
      }
    })
    
    #render the relative weight table
    output$wrtable <- renderTable(digits=2, rownames=FALSE, spacing="xs", {
      if(input$wr == TRUE){
        table()
      }
    }) 
    
  ##Length-Weight Regression Plot###################################
    lwreg <- function(){
      if(input$lwregression == TRUE){
        #if weight is NA, remove entire record
          completelw <- subset(selDataspp(),!is.na(Wt_g)) 
        #take log of each TL and wt
          varlength <- log10(completelw$TL_mm)
          weight <- log10(completelw$Wt_g)
        #build regression model
          lwmodel <- lm(weight~varlength)
        #Set plot margins
          par(mar=c(5,5,1,4)+0.1)
          plot(varlength, weight, xlab="logTL (mm)", ylab="logWeight (g)",
               cex.lab = 1.5, cex.axis = 1.4)
        #Fit regression line to plot
          lines(varlength, fitted(lwmodel))
      }
    }
   
  #Render LW regression plot
    output$lwplot <- renderPlot(bg="transparent", {
      if(input$lwregression == TRUE){ 
        lwreg()
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
          png(file, width = 600, height = 400)
          lfplot()
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
          lwreg()
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
          #calculate max length and weight in inches and pounds
          sppmaxunits <- mutate(sppmaxtable, maxw = as.integer(maxw),
                                maxlin = maxl*0.0393701, maxwpound = maxw*0.00220462)
          colnames(sppmaxunits)[1:4] <- c("Max TL (mm)", "Max Wt (g)", "Max TL (in)", "Max Wt (lbs)")
          sppmaxfinal <- sppmaxunits
   
        }
      })
      
      
  ##Population Dynamics Column###############################################
    
    #Calculate observed age-length key ###
      
      alkobserved <- reactive({
      #create variable for length groupings based on max TL in sample (10,15,or 20 mm)
      if((max(selageDatafinal()$TLmm)/30)>=20){
        w <- 20
      }
      if((max(selageDatafinal()$TLmm)/30)>=15 & (max(selageDatafinal()$TLmm)/30)<19.9999){
        w <- 15
      }
      if((max(selageDatafinal()$TLmm)/30)<14.9999){
        w <- 10
      }
      #create length category variable
      agelencat <- lencat(~TLmm, w=w, startcat = 0, right = FALSE, as.fact = TRUE, drop.levels = FALSE,
                          use.names = FALSE, data = selageDatafinal())
      #count frequency of ages within each length category - create proportion table
      alkfreq <- xtabs(~LCat+Age, data = agelencat)
      alk <- prop.table(alkfreq, margin = 1)
      round(alk,3)
      }) 
      
  ###Calculate multinomial logistic regression age-length key#############################
    
    alkmlr <- reactive({
    #create variable for length groupings based on max TL in sample (10,15,or 20 mm)
      if((max(selageDatafinal()$TLmm)/30)>=20){
        w <- 20
      }
      if((max(selageDatafinal()$TLmm)/30)>=15 & (max(selageDatafinal()$TLmm)/30)<20){
        w <- 15
      }
      if((max(selageDatafinal()$TLmm)/30)<14.9999){
        w <- 10
      }
      #create length category variable
        agedlencat <- lencat(~TLmm, w=w, startcat = 0, right = FALSE, data = selageDatafinal())
      #run multiple logistic regression
        mlr <- multinom(Age~LCat, data = agedlencat, maxit=500)
      #find min and max length category for sequence (rownames)
        minlencatage <- min(agedlencat$LCat)
        maxlencatage <- max(agedlencat$LCat)
        lens <- seq(minlencatage,maxlencatage,w)
      #Make predictions with mlr
        alkmlr1 <- predict(mlr, data.frame(LCat=lens), type = "probs")
        row.names(alkmlr1) <- lens
        alkmlr <- alkmlr1
    })
    
    ###Create dataset with predicted ages for the sample data (fields c(TL_mm, LCat, Age))##########
    
    agesample <- reactive({
      #create variable for length groupings based on max TL in sample (10,15,or 20 mm)
      if((max(selageDatafinal()$TLmm)/30)>=20){
        w <- 20
      }
      if((max(selageDatafinal()$TLmm)/30)>=15 & (max(selageDatafinal()$TLmm)/30)<20){
        w <- 15
      }
      if((max(selageDatafinal()$TLmm)/30)<15){
        w <- 10
      }
      #make length categories for sample dataset, create Age field of NA's, return just those 4 fields
        samplencat <- lencat(~TL_mm, w=w, startcat = 0, right = FALSE, data = selDataspp())
        samplencat["Age"] <- NA
        sampledata <- samplencat[c("TL_mm", "LCat", "Wt_g", "Age", "Number.of.individuals")]
      #find min and max length categories within MLR ALK
        agedlencat2 <- lencat(~TLmm, w=w, startcat = 0, right = FALSE, data = selageDatafinal())
        minlencatage2 <- min(agedlencat2$LCat)
        maxlencatage2 <- max(agedlencat2$LCat)
      #filter sample dataset to exclude any fish out of bounds of MLR age length key, also if length=NA
        sampledata2 <- filter(sampledata, LCat>(minlencatage2-1), LCat<(maxlencatage2+1), !is.na(LCat))
        sampledatafinal <- transform(sampledata2, TL_mm=as.numeric(TL_mm), Age=as.numeric(Age), LCat=as.numeric(LCat))
      #apply ages to sample
        agesample <- alkIndivAge(alkmlr(), Age~TL_mm, data = sampledatafinal)
      #repeat for each "Number.of.individuals"
        agesample <- subset(agesample, Number.of.individuals > 0)
    })
      
      #render table of fish with assigned ages
      output$agedfishtable <- downloadHandler(
        filename = function() {
          paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
                "agedFishTable", "csv", sep = "_")
        },
        content = function(file) {
          write.csv(agesample(), file, row.names = TRUE)
        }
      )
      
  ###Age Length Key Bubble Plot#############################################
  
    alkbub <- reactive({
    # alkbub <- function(){
      if(input$agelengthkey == TRUE){

    #make bubble plot representation
        par(mar=c(5,5,1,4)+0.1)
        alkbubplot <- alkPlot(alkobserved(), type="bubble", xlab="Total Length (mm)",
                              cex.lab = 1.5, cex.axis = 1.4)
        return(alkbubplot)
    }
  })
    # }
    
  #render ALK bubble plot
      output$alkplot <- renderPlot(bg="transparent",{
      if(input$agelengthkey == TRUE){
        alkbub()
      }
    })
      
    #Download png of ALK bubble plot
    output$downALKplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "ALKplot", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        # alkbub()#cannot use reactive statement here...just exports blank figure.  I could ref alkbub()
        # if I made that a function(){} rather than reactive({})...not sure why this is the case.
        par(mar=c(5,5,1,4)+0.1)
        alkbubplot <- alkPlot(alkobserved(), type="bubble", xlab="Total Length (mm)",
                              cex.lab = 1.5, cex.axis = 1.4)
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
  
  ###Age-Frequency Histogram################################################
    
    agefreq <- function(){#leaving this as a function rather than a reactive so it can be used
      #in dowload function
      if(input$agefreq == TRUE){
        #find sum of all ages from sample - agesample reactive function
          agefreq <- as.data.frame(xtabs(~Age, data = agesample()))
          total <- sum(agefreq$Freq)
          agefreq <- mutate(agefreq, Freq = Freq/total*100)
        #Set margins around plot
          par(mar=c(5,5,1,4)+0.1)
        #make histogram using barplot function
          agefreq <- barplot(agefreq$Freq, space = .01, names.arg = agefreq$Age,
                               xlab = "Age", ylab = "% of Observed Fish", col = "transparent",
                               cex.lab = 1.5, cex.names = 1.4, cex.axis = 1.4)
      }
    }
    
    #render age frequency histogram
    output$agefreqhist <- renderPlot(bg="transparent",{
      if(input$agefreq == TRUE){
      agefreq()
      }
    })
    
    #Download png of Age Frequency plot
    output$downafplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "AgeFreq", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        agefreq()
        dev.off()
      })
    
    ##von Bertalanffy Growth Model#################################################
    
    #deal with unit conversion with input$inch check box (mutate TL mm to inches)
    aged <- eventReactive(input$inch,{
      if(input$inch == FALSE){
        aged <- agesample()
      }
      if(input$inch == TRUE){
        aged <- data.frame(mutate(agesample(), TL_mm = TL_mm/25.4))
      }
      return(aged)
    })
    
    #fit VB curve with nls package
    fitvonB <- reactive({
      #define vonB starting coefficients
        vonBstarts <- list(Linf=max(aged()$TL_mm, na.rm = TRUE), K=0.3, t0=0)
      #define function
        vonBfun <- vbFuns()
      #use nls package to fit vonB curve - nls.control() controls iteration parameters
        fitvonB <- nls(TL_mm~vonBfun(Age,Linf,K,t0), data = aged(), start = vonBstarts,
                     control = nls.control(maxiter = 200, minFactor = 1/1000000,
                                           printEval = FALSE, warnOnly = TRUE))
    })
    
  ###Mean length-at-age plot and von Bert Plot################################################
    
    lengthplot <- function(){#using function instead of reactive here because we cannot make a downloadable
      #figure using a reactive statement, and process of building this plot is too complex...just easier
      #to have the code below given once rather than recreating it within the download function
      if(input$growth == TRUE){
        #aggregate mean of each age
          sumfish <- aggregate(TL_mm~Age, data = aged(), mean)
        
        #von Bert line
          #define FSA function
            vonBfun <- vbFuns()
          #find max and min of aged for seq() next
            minsampage <- min(aged()$Age)
            maxsampage <- max(aged()$Age)
          #sequence of "ages" to use to plot curve
            agesline <- seq(minsampage, maxsampage, length.out = 200)
          #use vonB equation to predict ages for line on plot
            vBline <- vonBfun(agesline, Linf = coef(fitvonB()))
        #find range of age and TL to define plot boundaries
          xlimits <- range(aged()$Age)
          ylimits <- range(aged()$TL_mm)
        #set margins around plot
          par(mar=c(6,5,1,4)+0.1)
        
        #plot 
        if(input$inch == FALSE){
          plot(TL_mm~Age, data = aged(), pch = 1, xlab="Age", ylab="Total Length (mm)",
               cex.lab = 1.5, cex.axis = 1.4)
        #plot a dashed line through mean length-at-ages
          points(TL_mm~Age, data = sumfish, pch = 17, cex = 2.2)
          lines(vBline~agesline, lwd=2)
          legend('bottomright',c('Mean Length','von Bert'),
                 lty=c(NA,1),pch=c(17,NA), lwd=2, cex=1.3, pt.cex = 2, ncol = 1)
        }
        if(input$inch == TRUE){
          plot(TL_mm~Age, data = aged(), pch = 1, xlab="Age", ylab="Total Length (in)",
               cex.lab = 1.5, cex.axis = 1.4)
        #plot a dashed line through mean length-at-ages
          points(TL_mm~Age, data = sumfish, pch = 17, cex = 2.2)
          lines(vBline~agesline, lwd=2)
          legend('bottomright',c('Mean Length','von Bert'),
                 lty=c(NA,1),pch=c(17,NA), lwd=2, cex=1.3, pt.cex = 2, ncol = 1)
        }
      }
    }
    
    #render mean length at age plot
    output$meanlengthplot <- renderPlot(bg="transparent", {
      if(input$growth == TRUE){
        lengthplot()
      }
    })
    
    #Download png of Growth Metrics plot
    output$downmeanplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "MeanLength", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        lengthplot()
        dev.off()
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
    
  ###von Bert Coefficient Table########################################################
    
    coeftable <- reactive({
      if(input$vonbert == TRUE){
        #call coefficients from the nls model
          coef <- as.data.frame(coef(fitvonB()))
          colnames(coef) <- "von Bert Coefficients"
        #call confidence intervals for the coefficients
          confint <- as.data.frame(confint(fitvonB(), level = 0.95)) 
          colnames(confint)[1:2] <- c("LCI","UCI")
        #bring the confidence intervals into the same table as coefficients
          coef["L 95% CI"] <- confint$LCI
          coef["U 95% CI"] <- confint$UCI
          coeftable <- coef
      }
    })
    
    #render table of von bert coefficients
    output$vonBcoef <- renderTable(digits = 3,spacing="xs",rownames = TRUE,{
      if(input$vonbert == TRUE){
        coeftable()
      }
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
    
  ###Catch Curve plot and mortality table#######################################
  
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
    
    ##Build catch curve plot
    catchplot <- function(){#cannot do with reactive statement or plot will not download
      if(input$mort == TRUE){
          #r2 value for catch curve (weighted)
          limb <- mortCatch()[!(mortCatch()$Age<maxCatch$maxcatchage), ]
          model <- lm(log(Freq)~Age, data = limb)
          limb <- mutate(limb, wts = predict(model))
          wtdModel <- lm(log(Freq)~Age, data = limb, weights = wts)
          r2 <- summary(wtdModel)$adj.r.squared
          r2label = bquote(italic(R)^2 == .(format(r2, digits = 3)))
       
        #set margins around plot
          par(mar=c(6,5,2,2)+0.1)
        #plot catch curve
          catchplot <- plot(mort(), pos.est = "bottomleft", cex.lab = 1.5, cex.axis = 1.4, 
                          cex.est = 1.5, main = r2label, cex.main = 1.5, ylab = "ln(catch)")
      }
    }

    output$catchcurve <- renderPlot(bg="transparent" ,{
      if(input$mort == TRUE){
        catchplot()
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
      
  #build mortality table from catch curve analysis
  mortfinal <- reactive({
    if(input$mort == TRUE){
      #calculate summary and conf intervals for estimates
        mortest <- cbind(summary(mort()), confint(mort()))
      #don't return t-value and p-value
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
  
    #Download png of Catch Curve
    output$downmort <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "CatchCurve", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        catchplot()
        dev.off()
      })
      
  #Theoretical max age
  Theor_max_age <- reactive({
    if(input$mort == TRUE){
      catchCurveParms <- coef(mort(), parm=c("lm"))
      theorMaxAge <- round(log(1)-(unname(catchCurveParms[1])/unname(catchCurveParms[2])),1)
          #catchCurveParms[1] is intercept and catchCurveParms[2] is slope from catch curve
          #unname gets rid of named value from catchCurveParms
      names(theorMaxAge) <- "Theoretical Maximum Age"
      return(theorMaxAge)
    }
  })

  #render Theor. Max Age output
  output$TheorMaxAge <- renderText({
    if(input$mort == TRUE){
      as.character(paste("Theoretical Maximum Age = ", round(Theor_max_age(), digits = 1), " Yr", sep = ""))
    }
  })  
  
  #Max Observed Age
  obsMaxAge <- reactive({
    if(input$mort == TRUE){
      max(selageDatafinal()$Age)
    }
  })
  
  #render Max Observed Age output
  output$ObsMaxAge <- renderText({
    if(input$mort == TRUE){
      as.character(paste("Observed Maximum Age = ", obsMaxAge(), " Yr", sep = ""))
    }
  })  

  #Estimate natural mortality rates
  natMortTable <- reactive({
    if(input$mort == TRUE){
      vbParms <- coeftable()[,1, drop=F]
      est_M_table <- metaM(c("HoenigNLS", "PaulyLNoT"), #calc instantaneous nat mort from metaM function in FSA package
                           Linf= (vbParms[1,]/10), K=vbParms[2,], t0=vbParms[3,], tmax=Theor_max_age()) %>% 
        mutate(method = case_when(method == "HoenigNLS" ~ "Hoenig NLS (Then et al. 2015)",
                                  method == "PaulyLNoT" ~ "Pauly NLS-T (Then et al. 2015)"),
               M = round(M, digits = 3)) %>% 
        mutate("Est. Inst. Fishing Mort. (F)" = round(mortfinal()[1,1] - M, digits = 3),
               "Instantaneous Total Mort. (Z)" = round(mortfinal()[1,1], digits = 3),
               "Annualized total Mort. (A)" = as.character(paste(round(mortfinal()[2,1], digits = 1), "%", sep = "")),
               "Est. Annual. Nat. Mort. (v)" = as.character(paste(round(
                        M * mortfinal()[2,1] / mortfinal()[1,1], digits = 1), "%", sep = "")),
               "Est. Exploitation / Annual. Fish. Mort. (u)" = as.character(paste(round(
                        mortfinal()[2,1] - (M * mortfinal()[2,1] / mortfinal()[1,1]), digits = 1), "%", sep = ""))
              ) %>% 
        rename(Method = method, "Est. Inst. Nat. Mort (M)" = M) 
    }
  })
  
  #render natural mortality table
  output$natMortalityTable <- renderTable(spacing="xs",rownames = FALSE, align = "c",{
    if(input$mort == TRUE){
      natMortTable()
    }
  })  

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

 
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Percentile tab calculations##############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  #load initial data
  percentileData <- reactive({
    # percentileData <- read.fst("PercentileData.fst", as.data.table = T) %>%
    #   mutate(maxTL=as.numeric(maxTL))
    percentileData <- read.fst("PercentileData.fst", as.data.table = T)
    return(percentileData)
      })
    
  ##Calculate Selectize boxes(populate w/ gear/spp from Select Sample tab)

      #code creates a line break
        output$lineBrk <- renderUI({HTML("<br/>")})
        # output$lineBrk <- renderUI({HTML("\n")})
        # output$lineBrk <- renderUI({HTML("<br>")})
        
    #Create N_agedMin slider on server side to get max age info
        maxN_aged <- reactive({max(percentileData()$N_aged, na.rm=TRUE)})
        output$NAgedMinSlider <- renderUI({sliderInput(inputId = "N_agedMin", label = 
               "Min # of aged fish required before using a survey's growth/mortality metrics in percentiles", 
               min = 30, max = maxN_aged(), sep = "", step=1, value = 150)
        })
        
      #Percentiles selectize box
        output$percentileInptBox <- renderUI({
            selectizeInput("percentileInpt", "To customize percentiles produced, type desired values in this box",
                "Percentiles:",choices = c(paste(1:99,rep("%",99),sep="")),multiple = TRUE, 
                options = list(placeholder = "click/type here"))
        })
     
          #Process input of percentileInptBox to establish which percentile values to calculate
            PercToProp = data.frame(prop = (c(seq(0.01,0.99, by= 0.01)))) %>% 
              mutate(perc = paste(prop  *100, "%", sep = ""))
            selPerctls <- Vectorize(reactive({if(is.null(input$percentileInpt)){
                selPerctls <- c(0.05,0.25,0.5,0.75,0.95)#sets default percentages
              }else{   #unless user specified percentiles to use
                selPerctls <- as.numeric(unlist((merge(data.table(input$percentileInpt), PercToProp, 
                                                       by=c("input.percentileInpt"="perc"), all.x=T) %>%
                 select(-"input.percentileInpt"))))}})) #reads from percentile selectize box and converts to proportion.
                
    #Create N_SurveyMin slider on server side to get min # surveys to use and set it to # percentiles requested by default
      N_perc_columns <- reactive({length(selPerctls())}) 
      output$min_survey <- renderUI({sliderInput(inputId = "N_SurveyMin", label = 
            "Min # of surveys for which to calculate percentiles (effects all percentile tables)", 
            min = 3, max = 100, sep = "", step=1, value = N_perc_columns()*2)
      })
        
  #Function to convert gear.Name_Code to Gear.Code
  gearNameCode_toCode <- reactive({
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
                              # gear <- data.table(as.character(input$selectgear))
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
      # if(input$tabs == "Statewide Percentiles"){#only run when on percentiles tab
      if(input$tabs != "Select Sample"){#Prevent running while on data selection tab to speed it up
      # selPercData <- merge(percentileData(), (lakeinfo %>% select(Lake.Code,ODWC.Region)), 
      #                      by = "Lake.Code", all.x=T) #add ODWC>Region to percentileData...but now save fst with this already present
      selPercData <- percentileData()
        if(!is.null(input$selPercGear)){selPercData <- selPercData[selPercData$Gear.Code %in% c(gearNameCode_toCode()),]}
        if(!is.null(input$selPercSpp)){selPercData <- selPercData[selPercData$Species.Code %in% c(SpeciesCodeName_toCode()),]}
        if(!is.null(input$perYrs)){selPercData <- selPercData[selPercData$Year %in% c(input$perYrs[1]:input$perYrs[2]),]}
        if(!is.null(input$selRegionPerc)){selPercData <- selPercData[selPercData$ODWC.Region %chin% c(input$selRegionPerc),]}
        if(!is.null(input$selLakeCodePerc)){selPercData <- selPercData[selPercData$Lake.Code %chin% c(LakeNameCode_toCode()),]}
        return(selPercData)
      }else{return(NULL)}
    })
      
      ####temp
      
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
        # group_by(Species.Code, Metric) %>% #subset is faster than filter here, I benchmarked
        # summarise("# Surveys"=n(),.groups = "drop_last")
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
        
      # Nsurveys <- CPUEperc() %>% group_by(Species.Code,CPUEmetric) %>% 
      #   summarise("N_Surveys"=n(),.groups = "drop_last")
      Nsurveys <- reactive({
        req(CPUEperc())
        CPUEperc() %>% group_by(Species.Code,CPUEmetric) %>% 
        summarise("N_Surveys"=n(),.groups = "drop_last")
      })
      
     CPUEpercMetrics <- CPUEperc() %>%
        merge(Nsurveys(), by = c("Species.Code", "CPUEmetric"), all.x=T) %>% 
        filter(N_Surveys >= input$N_SurveyMin) %>%
        #   ## replace N_SurveyMin with input$N_SurveyMin above in shiny app
        group_by(Species.Code, CPUEmetric, N_Surveys) %>% 
        group_modify(~{quantile(.x$CPUEVal, probs = selPerctls(), na.rm = TRUE) %>%tibble::enframe()}) %>%  
        pivot_wider(names_from = name,values_from = value) %>%
        merge(SortCPUE(), by="CPUEmetric", all.x=T) %>% arrange(Species.Code, sortOrder) %>%
        merge(select(speciesinfo, Species.Code, Species.Name), by="Species.Code", all.x=T) %>% 
        relocate(Species.Name) %>%
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>%
        select(-sortOrder, -CPUEmetric, -Species.Code) %>%
        relocate(cat_title, .after = Species.Name) %>%
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
            #need to replace Min_Wr_N with input$Min_Wr_N or whatever function produces this value in shiny app

        WrPerc <- subset(selPercDataWr, !is.na(Wr)) #drop NA's

        NsurveysWr <- WrPerc %>% group_by(Species.Code, PSD_cat) %>%
          summarise("N_Surveys"=n(),.groups = "drop_last")

        SortWr <- data.frame(PSD_cat=c("total","substock","stock","quality","preferred","memorable","trophy"),
                                          sortOrder=1:7)

        WrPercMetrics <- WrPerc %>%
          merge(NsurveysWr, by = c("Species.Code","PSD_cat"), all.x=T) %>%
            ## replace N_SurveyMin with input$N_SurveyMin below in shiny app
          filter(N_Surveys >= input$N_SurveyMin) %>% group_by(Species.Code, PSD_cat, N_Surveys) %>%
            ##replace selPerctls with selPerctls() below in shiny app
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
        ## replace N_SurveyMin with input$N_SurveyMin above in shiny app
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
            #need to replace N_agedMin with input$N_agedMin and Min_Mort_R2 with input$Min_Mort_R2 in shiny app

       NsurveyMort <- MortPercTable %>%
        subset(!is.na(A)) %>%
        group_by(Species.Code) %>%
        summarise("N_Surveys"=n(),.groups = "drop_last")

       MortPercMetrics <- MortPercTable %>%
        merge(NsurveyMort, by = c("Species.Code"), all.x=T) %>%
        filter(N_Surveys >= input$N_SurveyMin) %>%
          ## replace N_SurveyMin with input$N_SurveyMin above in shiny app
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
          # if(length(input$selPercGear) == 1){
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
    