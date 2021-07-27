#Packages to be installed and loaded############################
library(shiny)
library(plyr);library(dplyr) #had used join() but I changed most to dplyr left_join and remaining ones plyr::join.  Now just need join_all
library(tidyr)
library(FSA)
library(nnet)
library(nlstools)  
library(tibble)
library(bslib) #adds theme engine with theme picker

#.csv's to read in#############################################
mainData <- read.csv("Main_database_name_abrev.csv")#just used by Dan for testing purposes (speeds things up)
#mainData <- read.csv("Main_database_name.csv")
gearinfo <- read.csv("gearinfo.csv")
lakeinfo <- read.csv("lakeinfo.csv")
yearmonthinfo <- read.csv("yearmonthinfo.csv")
speciesinfo <- read.csv("speciesinfo.csv")
gabel <- read.csv("gabelhousenames.csv")
wsnames <- read.csv("WSnames.csv")
agedata <- read.csv("compiledagedata.csv")
stockingData <- read.csv("stockingdata.csv")
#percentileData <- read.csv("E:/ODWC Shiny stuff/Oklahoma Fishery Analysis Application/PercentileData.csv")#Loading this down with the percentile tab to save loading time
SortPSD <- data.frame(PSDname=c("","substock","stock","quality","preferred","memorable","trophy"),sortOrder=c(1,2,3,4,5,6,7))
    #above creates table with psd size classes and a sort order variable to use for sorting PSD by size rather 
    #than alphabetical.  Note first size class is blank so it can be used to sort the "total" category first (i.e.,
    #when reporting CPUE by size class, this allows total CPUE to be first, then substock CPUE, stock, etc.)


  function(input, output, session) {
    
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Select Sample tab code##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #bs_themer() #adds bslib theme selecter
    
    #hide red "Loading Data" when .csv's are loaded
      hide(id = "loading-content", anim = TRUE, animType = "fade")
    #Adds ability to upload files greater than 5 MB (max now 100 MB)
      options(shiny.maxRequestSize=100*1024^2)
    
    #Server-side processing of selectize boxes
      observe({updateSelectizeInput(session, "selectlake", choices = lakeinfo$Lake.Code, server = TRUE)})
      observe({updateSelectizeInput(session, "selectyear", choices = yearmonthinfo$Year, server = TRUE)})
      observe({updateSelectizeInput(session, "selectmonth", choices = yearmonthinfo$Month, server = TRUE)})
      observe({updateSelectizeInput(session, "selectgear", choices = gearinfo$Gear.Code, server = TRUE)})
      
      observe({updateSelectizeInput(session, "selectlakename", choices = sort.default(lakeinfo$Lake.Name), server = TRUE)})
      observe({updateSelectizeInput(session, "selectyearname", choices = yearmonthinfo$Year, server = TRUE)})
      observe({updateSelectizeInput(session, "selectmonthname", choices = yearmonthinfo$Month, server = TRUE)})
      observe({updateSelectizeInput(session, "selectgearname", choices = sort.default(gearinfo$Gear.Name), server = TRUE)})
      
    #Create selected data table for catch analysis using input from user in the selectizeInput boxes (no species selected) 
      
      #filter data from the selectizeInput values
          selData <- reactive({
            if(input$loadCheck == FALSE){
              selData <- mainData 
              if(!is.null(input$selectlake)){ 
                selData <- selData[selData$Lake.Code %in% c(input$selectlake),]
              }
              if(!is.null(input$selectyear)){
                selData <- selData[selData$Year %in% c(input$selectyear),]
              }
              if(!is.null(input$selectmonth)){
                selData <- selData[selData$Month %in% c(input$selectmonth),]
              }
              if(!is.null(input$selectgear)){ 
                selData <- selData[selData$Gear.Code %in% c(input$selectgear),]
              }
              selData 
            }
            else{
              selData <- input$loadedData
              if(is.null(selData)){
                return(NULL)
              }
              selData <- read.csv(selData$datapath)
              selData[selData == "."] <- NA
              selData <- mutate(selData, TL_mm = as.numeric(as.character(TL_mm)),
                                Wt_g = as.numeric(as.character(Wt_g)))
              selData
            }
          })

  ##create final selected data table and download button####
    #output selected data table
        output$selectedDataTable <- DT::renderDataTable({
                  withProgress(message = "Filtering Database", min=0,max=10,value=1,{
                    incProgress(10)
                    expr=selData()
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
  
  ##Functions to grab codes from names and vice versa and syncing selection boxes for codes/names####
    #codes to names
      lakename <- function(){
        lake <- as.data.frame(as.character(input$selectlake))
        colnames(lake) <- "Lake.Code"
        lake <- left_join(lake,lakeinfo,by="Lake.Code")
        lakename <- as.character(lake[,2])
      }
      yearname <- function(){
        yearname <- input$selectyear
      }
      monthname <- function(){
        monthname <- input$selectmonth
      }
      gearname <- function(){
        gear <- as.data.frame(as.character(input$selectgear))
        colnames(gear) <- "Gear.Code"
        gear <- plyr::join(gear,gearinfo,by="Gear.Code")
        gearname <- as.character(gear[,2])
      }
      speciesname <- function(){
        species <- as.data.frame(as.character(input$selectspecies))
        colnames(species) <- "Species.Code"
        species <- plyr::join(species,speciesinfo,by="Species.Code")
        speciesname <- as.character(species[,2])
      }
      
    #names to codes
      lakecode <- function(){
        lake <- as.data.frame(as.character(input$selectlakename))
        colnames(lake) <- "Lake.Name"
        lake <- left_join(lake,lakeinfo,by="Lake.Name")
        lakecode <- as.character(lake[,2])
      }
      yearcode <- function(){
        yearcode <- input$selectyearname
      }
      monthcode <- function(){
        monthcode <- input$selectmonthname
      }
      gearcode <- function(){
        gear <- as.data.frame(as.character(input$selectgearname))
        colnames(gear) <- "Gear.Name"
        gear <- left_join(gear,gearinfo,by="Gear.Name")
        gearcode <- as.character(gear[,2])
      }
      speciescode <- function(){
        species <- as.data.frame(as.character(input$selectspeciesname))
        colnames(species) <- "Species.Name"
        species <- left_join(species,speciesinfo,by="Species.Name")
        speciescode <- as.character(species[,2])
      }
      availyear <- function(){
        availyear <- sort.default(unique(selData()$Year))
      }
  
      #Update selectize inputs to names when boxes with codes get a new value
      observe({
        updateSelectizeInput(session, "selectlakename", selected = lakename())
        updateSelectizeInput(session, "selectyearname", selected = yearname())
        updateSelectizeInput(session, "selectmonthname", selected = monthname())
        updateSelectizeInput(session, "selectgearname", selected = gearname())
       })
      #Update selectize inputs to codes when boxes with names get a new value
      observe({
        updateSelectizeInput(session, "selectlake", selected = lakecode())
        updateSelectizeInput(session, "selectyear", selected = yearcode())
        updateSelectizeInput(session, "selectmonth", selected = monthcode())
        updateSelectizeInput(session, "selectgear", selected = gearcode())
      })
      
  ##Summarize selected data in center  box####
    #Provide reference for full lake and gear names to right of selectize inputs (Data Selection Tab)
    output$lakename <- renderText({lakename()})
    output$year <- renderText({yearname()})
    output$month <- renderText({monthname()})
    output$gearname <- renderText({gearname()})
    
    output$useUploadedsamp <- renderText({
      if(input$loadCheck == TRUE){
        useUploadedsamp <- as.character("Using uploaded sample data")
      }
    })


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Select Types of Analyses tab ##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  ##Create dynamic UI's for species selection that populate with options available from selected sample####
    
    #SSP data Species code selection box using server-side choices processing to match available spp
      observe({
        withProgress(message = "Loading Species Options Data", min=0,max=10,value=1, {
          callspecies <- sort.default(unique(selData()$Species.Code))
            updateSelectizeInput(session, "selectspecies", choices = callspecies,
              server = TRUE)
        })
      })
    
    #SSP data Species name selection box using server-side choices processing to match available spp
      observe({
        withProgress(message = "Loading Data", min=0,max=10,value=1, {
          #below is more efficient (and uses dplyr rather than plyr) but I'm going back to old to debug some errors
          # callspecies <- as.data.frame(unique(selData()$Species.Code))
          # colnames(callspecies) <- "Species.Code"
          # callspecies <- as.character(unlist(left_join(callspecies,speciesinfo,by="Species.Code") %>% 
          #                     select(Species.Name)))
          callspecies <- unique(selData()$Species.Code)
          callspecies <- as.data.frame(as.character(callspecies))
          colnames(callspecies) <- "Species.Code"
          callspecies <- plyr::join(callspecies,speciesinfo,by="Species.Code")
          callspecies <- as.character(callspecies[,2])
          updateSelectizeInput(session, "selectspeciesname", choices = callspecies,
                server = TRUE)
        })
      })
    
    #observe functions to fill spp name in box when spp code is entered
      observe({
        updateSelectizeInput(session, "selectspeciesname", selected = speciesname())
      })
    #Observer function to fill spp code in box when spp name is entered
      observe({
        updateSelectizeInput(session, "selectspecies", selected = speciescode())
      })
      
    #Create selected SSP data table for single species analysis on Select Analysis tab
      selDataspp <- reactive({
        selDataspp <- selData()[selData()$Species.Code == input$selectspecies,]
        selDataspp <- selDataspp[rep(row.names(selDataspp), selDataspp$Number.of.individuals),]
            #Above line repeats each row by number of times in Number of individuals column
      })
      
    # Create downloadable csv of selected species dataset
        output$downloadsppData <- downloadHandler(
          filename = function() {
            paste(input$selectlake,input$selectyear,input$selectgear,input$selectspecies,"sppdata.csv", sep = ".")
          },
          content = function(file) {
            write.csv(selDataspp(), file, row.names = FALSE)
          }
        )
      
    #Age data - species selection box choices and default value set to species selected for SSP data at top of page
      observe({
        withProgress(message = "Loading Data",min=0,max=10,value=1, {
        callagespecies <- sort.default(unique(selData()$Species.Code))
        updateSelectizeInput(session, "selagespp", choices = callagespecies,
            selected = input$selectspecies, server = TRUE)
        })
      })
    
    #Age data - lake selection box built here (server.r) so only values matching selected sample are in drop down list
      # output$selagelake <- renderUI({
      #   withProgress(message = "Loading Data",min=0,max=10,value=1, {
      #     tempAgeSpp <- agedata
      #         if(!is.null(input$selagespp)){
      #           tempAgeSpp <- tempAgeSpp[tempAgeSpp$Species.Code %in% c(input$selagespp),]
      #         }
      #     callagelake <- sort.default(unique(tempAgeSpp$Lake.Code))
      #     selectizeInput("selagelake", "Select Lake Code(s):", choices = callagelake,
      #                    multiple = TRUE, options = list(placeholder = "click/type here"),
      #                    selected = input$selectlake)
      #   })
      # })
        observe({
          withProgress(message = "Loading Data",min=0,max=10,value=1, {
            tempAgeSpp <- agedata
            if(!is.null(input$selagespp)){
              tempAgeSpp <- tempAgeSpp[tempAgeSpp$Species.Code %in% c(input$selagespp),]
            }
            callagelake <- sort.default(unique(tempAgeSpp$Lake.Code))
            updateSelectizeInput(session, "selagelake", choices = as.character(callagelake),
              selected = input$selectlake, server = TRUE)
          })
        })
        #above server-side processing causes mutate error with selageDatafinal() down below that appears to be
        #due to processing on null object.  Seems there is some out-of-sequence processing here as it does
        #produce the right selageDatafinal project, but throws this error while processing along the way. Goes
        #away if I go back to ui.r rendering of this selectize box.
    
    #Age data - years selection box built here (server.r) so only values matching selected sample are in drop down list
      # output$selageyears <- renderUI({
      #   withProgress(message = "Pairing Age Dataset",min=0,max=10,value=1, {
      #     callageyears <- sort.default(selageData2()$Year)
      #     selectizeInput("selageyears", "Select Year(s):", choices = callageyears,
      #                    multiple = TRUE, options = list(placeholder = "click/type here"),
      #                    selected = input$selectyear)
      #   })
      # })
       observe({
          withProgress(message = "Pairing Age Dataset",min=0,max=10,value=1, {
            callageyears <- sort.default(selageData2()$Year)
            updateSelectizeInput(session, "selageyears", choices = callageyears,
              selected = input$selectyear, server = TRUE)
            #below is more efficient, but I'm going back to old version to debug error
            # updateSelectizeInput(session, "selageyears", choices = sort.default(selageData2()$Year),
            #   selected = input$selectyear, server = TRUE)
          })
       })
      #note above code causes error:
          #Warning: Error in [.data.frame: undefined columns selected
          #[No stack trace available]
      #I suspect this is a data type issue (factor vs character or such) but probably is not related to Year
      #The old ui.r rendering of the selectizeInput does not have this error, but I can't see any reason it
      #would be different, so may be a timing issue of when something processes when I move to server-side
      #processing.  
      #When I pick species, I also get a mutate error about a null object with server side processing of both selageyears
      #and selagelake that goes away if I go back to ui.r rendering of selectize boxes.  In the end, the app works
      #despite these errors (further suggesting these are short term errors caused by processing something out of
      #order such that it processes on an empty dataframe)
         
    #Reminder to select species above in orange
      output$nospp <- renderText({
        if(is.null(input$selagespp)){
          if(input$loadageCheck == FALSE){
            nospp <- as.character("Select Species Above")
          }
        }
      })
      
    #Display text showing no paired data in orange
      output$nodata <- renderText({
        if(is.null(input$selagelake) || is.null(input$selageyears)){
          if(input$loadageCheck == FALSE){
            nodata <- as.character("No Paired Age Data")
          }
        }
      })
      

  ##Test if selected age dataset matches the selected sample dataset or if uploaded data being used################### 
      
    #display text showing matched age dataset in green
    output$yesmatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$selectspecies == input$selagespp &&
           selDataspp()$Lake.Code %in% c(input$selagelake) &&
           selDataspp()$Year %in% c(input$selageyears)){
          if(input$loadageCheck == FALSE){
            yesmatch <- as.character("Matched Age Dataset")
          }
        }  ##add else statement to say "Matched Age Dataset Using Uploaded Data"
      }
    })
      
    #display text showing not a matched age dataset in red
    output$nomatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$selectspecies != input$selagespp ||
           selDataspp()$Lake.Code != input$selagelake ||
           selDataspp()$Year != input$selageyears){
          if(input$loadageCheck == FALSE){
            nomatch <- as.character("Not a Matched Age Dataset")
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
  
      #first filter by species selected by user
        selageData <- reactive({
          if(input$loadageCheck == FALSE){
            selageData <- agedata
            if(!is.null(input$selagespp)){
              selageData <- selageData[selageData$Species.Code %in% c(input$selagespp),]
              selageData
            }
            # #####################################################
            # ###next line add if else for ==TRUE and load user data as selageData...Dan's first attempt to modify this to make uploaded age data filter also...but I need to check if this even affects final project or jsut used to display table at bottom of tab.
            # else{
            #   #selageData <- NA#added to blank out dataframe so can see if new data loaded yet
            #   selageData <- input$loadedageData
            #   if(is.null(selageData)){
            #     return(NULL)
            #   }
            #   selageData <- read.csv(selageData$datapath)
            #   selageData <- mutate(selageData, TLmm = as.numeric(as.character(TLmm)),
            #                        Age = as.numeric(as.character(Age)))
            #   selageData <- selageData
            # }
            # #################
            # #End of what Dan added
          }
        })
        
        #Next, filter by lake selected by user
          selageData2 <- reactive({
            if(input$loadageCheck == FALSE){
            selageData2 <- selageData()
            if(!is.null(input$selagelake)){
              selageData2 <- selageData2[selageData2$Lake.Code %in% c(input$selagelake),]
              selageData2
            }
            }
          })
  
      #Finally, filter by year selected by user
        selageDatafinal <- reactive({
          if(input$loadageCheck == FALSE){
            selageDatafinal <- selageData2()
            if(!is.null(input$selageyears)){
              selageDatafinal <- selageDatafinal[selageDatafinal$Year %in% c(input$selageyears),]
              selageDatafinal <- mutate(selageDatafinal, TLmm = as.numeric(as.character(TLmm)),
                                        Age = as.numeric(as.character(Age)))
              selageDatafinal
            }
          }
          #####Dan moved below block up.
          else{
            selageDatafinal <- input$loadedageData
            if(is.null(selageDatafinal)){
              return(NULL)
            }
            selageDatafinal <- read.csv(selageDatafinal$datapath)
            selageDatafinal <- mutate(selageDatafinal, TLmm = as.numeric(as.character(TLmm)),
                                      Age = as.numeric(as.character(Age)))
            selageDatafinal
          }
          #%%%%%%%%%%%%%%%%%%%%%%%%%%
        })
  
  
    #output selected age data
    output$selectedageData <- DT::renderDataTable({
      withProgress(message = "Pairing Age Dataset", min=0, max=10,value = 1,{
        incProgress(10)
        expr=selageDatafinal()
      })
    })
    
        
  ##Selected sample summary and download info####  
    
    #Age dataset sample size display
      output$agecount <- renderText({
        if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
          agecount <- length(selageDatafinal()$Age)
          agecount <- paste(as.character("N = "), agecount, sep = "")
        }
        #######Dan tried commenting block below out for uploading data
        else{
          if(input$loadageCheck == TRUE){
            agecount <- length(selageDatafinal()$Age)
            agecount <- paste(as.character("N = "), agecount, sep = "")
          }
        }
        #######################################################
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
        species <- as.character(input$selagespp)
        speciesco <- as.data.frame(species)
        colnames(speciesco) <- "Species.Code"
        fullsname <- plyr::join(speciesco,speciesinfo,by="Species.Code")
        speciesname1 <- as.character(fullsname[,2])
      })
      output$agelake <- renderText({
        lake <- as.character(input$selagelake)
        lakeco <- as.data.frame(lake)
        colnames(lakeco) <- "Lake.Code"
        fullname <- plyr::join(lakeco,lakeinfo,by="Lake.Code")
        lakename1 <- as.character(fullname[,2])
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
        if(input$abiotic == TRUE){
          #We should be able to just aggregate by mean and take mins/maxs from it because they only take one measurement/sample
          aggpool <- aggregate(Pool.Elevation ~ SampleID, data=selData(), mean, na.action=NULL)  
            poolmean <- mean(aggpool$Pool.Elevation, na.rm=TRUE)
            poolmin <- min(aggpool$Pool.Elevation, na.rm=TRUE)
            poolmax <- max(aggpool$Pool.Elevation, na.rm=TRUE)
          aggtemp <- aggregate(Surface.Temp ~ SampleID, data=selData(), mean, na.action=NULL)
            tempmean <- mean(aggtemp$Surface.Temp, na.rm=TRUE)
            tempmin <- min(aggtemp$Surface.Temp, na.rm=TRUE)  
            tempmax <- max(aggtemp$Surface.Temp, na.rm=TRUE)
          aggsecchi <- aggregate(Secchi ~ SampleID, data=selData(), mean, na.action=NULL)
            secchimean <- mean(aggsecchi$Secchi, na.rm=TRUE)
            secchimin <- min(aggsecchi$Secchi, na.rm=TRUE)
            secchimax <- max(aggsecchi$Secchi, na.rm=TRUE)
          aggcond <- aggregate(Conductivity ~ SampleID, data=selData(), mean, na.action=NULL)
            condmean <- mean(aggcond$Conductivity, na.rm=TRUE)
            condmin <- min(aggcond$Conductivity, na.rm=TRUE)
            condmax <- max(aggcond$Conductivity, na.rm=TRUE)
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
        if(input$samplesize == TRUE){
          numspp <- left_join(selData(), speciesinfo, by="Species.Code")
          numspp2 <- aggregate(Number.of.individuals ~ Species.Name, numspp, sum)
          sppchar <- as.character(numspp2$Species.Name)
          numsppmatrix <- matrix(c(sppchar, numspp2$Number.of.individuals),
                                 ncol=2,
                                 dimnames = list(c(),c("Species", "Number")))
        }
      })
      
    ###Total Effort Table###
      output$toteffort <- renderTable(digits=1, spacing="xs", rownames=FALSE, {
        if(input$totaleffort == TRUE){
          if(selData()$Gear.Code <= 40){ #netting and seining (use effort)
            aggeffort <- aggregate(Effort ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
          }else{ #electrofishing (use gear length)
            aggeffort <- aggregate(Gear.Length ~ SampleID+Gear.Code, data=selData(), mean, na.action=NULL)
          }  
          colnames(aggeffort)[3] <- "Effort"
          effortbygear <- aggregate(Effort ~ Gear.Code, data=aggeffort, sum, na.action=NULL)
          listofgearcodes <- effortbygear$Gear.Code
          if(input$loadCheck == FALSE){ 
            listofgearnames <- gearname()
          }else{
            listofgearnames <- as.data.frame(unique(selData()$Gear.Code))
            colnames(listofgearnames) <- "Gear.Code"
            listofgearnames <- left_join(listofgearnames,gearinfo,by="Gear.Code")
            listofgearnames <- as.character(listofgearnames[,2])
          }
          table <- cbind(effortbygear,listofgearnames)
          table <- table[c(1,3,2)]
          colnames(table)[1:3] <- c("Gear Code"," Gear Name", "Effort")
          namedtable <- table
        }
      })
      
    ##Total CPUE Table##################################################
      cpuetable <- reactive({
        if(input$cpue == TRUE){
          #Aggregate into total catch by species per sample (also return gear code and effort) - transform SampleID to character
          joinselData <- left_join(selData(),speciesinfo,by="Species.Code")
          aggselData <- aggregate(Number.of.individuals ~ SampleID*Species.Name+Gear.Code, 
                                  data=joinselData, sum)
          aggselDatach <- aggselData %>% mutate_if(is.factor, as.character)
          #Add zeroes for samples that didn't catch a particular species - FSA
          selDatazero <- addZeroCatch(aggselDatach, "SampleID", "Species.Name", "Number.of.individuals")
          
        #Pull out effort from selData and join to dataset
          #Calculate CPUE based on gear code
          if(aggselData$Gear.Code >= 41){ #electrofishing
            lengthbysample <- aggregate(Gear.Length ~ SampleID, selData(), mean)
            allcpue <- left_join(selDatazero, lengthbysample, by="SampleID")
            samplecpue <- as.data.frame(mutate(allcpue, CPUE = (Number.of.individuals*60)/Gear.Length))
          } 
          if(aggselData$Gear.Code <= 40 & aggselData$Gear.Code != 10){ #netting
            effortbysample <- aggregate(Effort ~ SampleID, selData(), mean)
            allcpue <- left_join(selDatazero, effortbysample, by="SampleID")
            samplecpue <- as.data.frame(mutate(allcpue, CPUE = (Number.of.individuals*24)/Effort))
          }   
          if(aggselData$Gear.Code == 10){ #seining
            effortbysample <- aggregate(Effort ~ SampleID, selData(), mean)
            allcpue <- left_join(selDatazero, effortbysample, by="SampleID")
            samplecpue <- as.data.frame(mutate(allcpue, CPUE = (Number.of.individuals*1076)/Effort))
          }
          
          #aggregate by species and create dataframes for mean, sd, and count 
          cpuedfs <- list( 
            meancpue = aggregate(CPUE ~ Species.Name, data=samplecpue, mean, na.action = na.omit),
            sdcpue = aggregate(CPUE ~ Species.Name, data=samplecpue, sd, na.action = na.omit),
            countcpue = aggregate(CPUE ~ Species.Name, data=samplecpue, length, na.action = na.omit)
          ) 
          
          #join all previous dataframes together - rename columns
          cpuejoinall <- join_all(cpuedfs, by="Species.Name", type="left")
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
          cpuetable <- cpuetable[c(1,2,4,8,5,6,7,10,11)]
          colnames(cpuetable) <- c("Species","Mean","Count","RSE","SE","L 95% CI",
                                   "U 95% CI","N RSE = 12.5 (25% range)","N RSE = 20 (40% range)")
          cpuetable <- cpuetable[order(cpuetable$Species),]
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
        sp <- data.frame(unique(selData()$Species.Code))
        colnames(sp) <- "Species.Code"
        sp <- left_join(sp,gabel,by="Species.Code")
        sp <- left_join(sp, speciesinfo, by="Species.Code")
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
        sp <- data.frame(unique(selData()$Species.Code))
        colnames(sp) <- "Species.Code"
        sp <- left_join(sp,gabel,by="Species.Code")
        sp <- left_join(sp, speciesinfo, by="Species.Code")
        sp <- mutate(sp, Gabelhouse.Name = as.character(Gabelhouse.Name))
        sp <- sp[complete.cases(sp),]
        
        match.fun(psdVal)
        buildtablemm <- data.frame(lapply(sp$Gabelhouse.Name, psdVal))
        colnames(buildtablemm) <- sp$Species.Name
        
        buildtablein <- as.data.frame(buildtablemm)
        buildtablein <- mutate_all(buildtablein, funs(./25.4))#funs depreciated. I think the fix is just to put a ~ in front of the
          #function given there is just one or use list() in place of funs() if you have multiple. so this should become
          #mutate_all(buildtablein, ~(./25.4)), but I don't have time to test right now...just noting for future update.
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
          noTL <- selData %>% group_by(Species.Code) %>% summarise(non_na_count = sum(!is.na(TL_mm))) %>% 
            subset(non_na_count == 0)
          
        #Add Spp names using Gabelhouse.Name spelling, make sure everything is character data type (no factors),
          cpuesizetable <-left_join(selData,gabel, by = "Species.Code") %>% mutate_if(is.factor, as.character) %>% 
              
          #add PSD size classes to each row based on fish lengths/spp
            mutate(psdval = psdAdd(TL_mm ~ Gabelhouse.Name, verbose = FALSE)) %>%  
            
          #sum up # fish by spp and psd size class
            group_by(SampleID, Gear.Code, Effort, Gear.Length, Species.Code, psdval) %>%
              summarise(numbCaught = sum(Number.of.individuals), .groups = "drop") %>%
  
          #force 0 into numbCaught variable when spp or size class was not caught at a given sample station (best to convert into a tibble first, then back into dataframe)
            as_tibble() %>%
              complete(nesting(SampleID, Gear.Code, Effort, Gear.Length), Species.Code, psdval, fill = list(numbCaught=0)) %>%
              as.data.frame()  %>%
  
          #Create proper effort based on gear type as "Effort 2" so can multiply # caught by this to get CPUE
            mutate(Effort2 = case_when(Gear.Code >= 41 ~ 60/Gear.Length,  #EF gears (Number.of.individuals*60)/Gear.Length where gear length is in min. = #/hr
                           Gear.Code <= 40 & Gear.Code != 10 ~ 24/Effort,  #net gears (Number.of.individuals*24)/Effort where effort is number of net nights= #/24h
                           Gear.Code == 10 ~ 1076/Effort)) %>%  #Seine (Number.of.individuals*1076)/Effort...not sure how this works, but should make #/ft^2

          #Calculate CPUE by SampleID
            group_by(SampleID, Effort2, Species.Code, psdval) %>% 
            summarise(CPUEsite = numbCaught*Effort2, .groups = "drop") %>%

          #Remove species code 98 (no species caught) or data with no PSD size class or TL
            left_join(gabel, by = "Species.Code") %>% subset(!is.na(Gabelhouse.Name) & !is.na(psdval) &
               !Species.Code %in% noTL$Species.Code) %>% select(-Gabelhouse.Name) %>%
  
          #Add Species.Name and drop Species.Code
            left_join(speciesinfo, by = "Species.Code") %>% select(-Species.Code) %>% relocate(Species.Name) %>%
  
          #average catch rates across sites (still within psdval groupings) and create RSE/95%CI/etc.
            group_by(Species.Name, psdval) %>% summarise(Mean = mean(CPUEsite), SD = sd(CPUEsite), Count = n(), 
               SE=if(SD==0){NA}else{ (SD/sqrt(Count))},  #make SE missing if SD was zero...prevents 95% CI that is 0-0 (and not correct)
               "L 95% CI" = Mean - (1.96*SE), "U 95% CI" = Mean + (1.96*SE), RSE = (SE/Mean)*100, CV = (SD/Mean)*100,
               "N RSE = 12.5 (25% range)" = as.integer(round(((CV/12.5)^2),0)), "N RSE = 20 (40% range)" =
               as.integer(round(((CV/20)^2),0))) %>%

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
        if(input$selectspecies == input$selagespp &&
           selDataspp()$Lake.Code %in% c(input$selagelake) &&
           selDataspp()$Year %in% c(input$selageyears)){
          #if(input$loadageCheck == FALSE){
          ageDataMatch <- as.character("Matched Age Dataset was used")
          #}
        }
      }
    })
    #display text showing not a matched age dataset in red
    output$ageDataNoMatch <- renderText({
      if(!is.null(input$selagelake) && !is.null(input$selageyears) && !is.null(input$selagespp)){
        if(input$selectspecies != input$selagespp ||
           selDataspp()$Lake.Code != input$selagelake ||
           selDataspp()$Year != input$selageyears){
          if(input$loadageCheck == FALSE){
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
      # else if (input$loadageCheck == TRUE){
      #   ageDataCount <- length(selageDatafinal()$Age)
      #   ageDataCount <- paste(as.character("Age data sample size: N = "), ageDataCount, sep = "")
      # }
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
          lfhist$counts=lfhist$counts/sum(lfhist$counts)
          lfplot <- plot(lfhist, main = NULL, xlab="Total Length (mm)", ylab="Relative Frequency (%)",
               cex.lab = 1.5, cex.axis = 1.4)
        }
        if(xaxis[2]==25.4){
          selDatain <- mutate(selDataspp(), TL_mm = TL_mm/25.4)
          maxin <- max(selDatain$TL_mm, na.rm = na.omit)
          xaxisin <- seq(0,(maxin+1), 1)
          lfhist <- hist(selDatain$TL_mm, breaks = xaxisin, plot = FALSE)
          lfhist$counts=lfhist$counts/sum(lfhist$counts)
          lfplot <- plot(lfhist, main = NULL, xlab="Total Length (in)", ylab="Relative Frequency (%)",
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
        lfhist$counts=lfhist$counts/sum(lfhist$counts)
      }
      if(xaxis[2]==25.4){
        selDatain <- mutate(selDataspp(), TL_mm = TL_mm/25.4)
        maxin <- max(selDatain$TL_mm, na.rm = na.omit)
        xaxisin <- seq(0,(maxin+1), 1)
        lfhist <- hist(selDatain$TL_mm, breaks = xaxisin, plot = FALSE)
        lfhist$counts=lfhist$counts/sum(lfhist$counts)
      }
      bins <- data.frame(lfhist$breaks)
      nada <- max(as.numeric(rownames(bins)))
      bins <- bins[-nada,]
      frequency <- data.frame(lfhist$counts)
      lftable <- cbind(bins, frequency)
      colnames(lftable) <- c("Length Bin", "Frequency")
      lftable <- lftable
      
    }
  })
  
  #download button for length frequency table data
  #Downloadable csv of PSD table
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
    #psd category length reference table
    output$psdvaltable <- renderTable(digits=1, rownames=FALSE, spacing="xs", {
      if(input$psd == TRUE){
        # selDataspp <- selDataspp()
        # write.csv(selDataspp,"test_selDataspp.csv")
        gabelseldata <- left_join(selDataspp(),gabel,by="Species.Code")
        gabelname <- as.character(gabelseldata[1,20])
        # gabelname <- as.character(left_join(as.data.frame(input$selectspecies),gabel,by="Species.Code"))
        #     write.csv(gabelname,test_gabelname.csv)
        #                           #%>% select(-selectspecies))
        gabelname <- as.character(gabelseldata[1,20])
        psdvals <- as.data.frame(psdVal(gabelname))
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
        gabelseldata <- left_join(selDataspp(),gabel,by="Species.Code")
        gabelname <- as.character(gabelseldata[1,20])
        #calculate psd's from FSA function
        psdCalc <- psdCalc(~TL_mm, gabelseldata, gabelname, units="mm", 
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
        standard <- left_join(selDataspp(),wsnames,by="Species.Code")
        
        wsName <- as.character(standard[1,20]) 
        standardeq <- wsVal(wsName)
        standardeq <- standardeq[c(1,3,4,7:10)]
        colnames(standardeq) <- c("Species","Model Type","Reference Percentile","Min.TL",
                                  "Intercept","Slope","Source")
        standardeq <- mutate(standardeq, Min.TL = as.integer(Min.TL))
      }
    })  
    
    table <- reactive({
      if(input$wr == TRUE){
        #Join the gabelhouse and weight standard names used in FSA code
        psdjoindata <- left_join(selDataspp(),gabel,by="Species.Code")
        wrpsdjoindata <- left_join(psdjoindata,wsnames,by="Species.Code")
        #Calculate psd size category and append to selected data
        psdval <- psdAdd(TL_mm~Gabelhouse.Name, units="mm", data=wrpsdjoindata)
        wrpsdjoindata$psdvalue <- psdval
        colnames(wrpsdjoindata)[22] <- "psdvalue"
        #Calculate relative weight and append to selected data
        wrvalue <- wrAdd(Wt_g~TL_mm+wsname, units="metric",data = wrpsdjoindata)
        wrpsdjoindata$wrvalue <- wrvalue
        colnames(wrpsdjoindata)[23] <- "wrvalue"
        #calculated mean, sd, and count aggregated by psd size class
        wrdfs <- list(
          aggwrmeans = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, mean, na.action = na.omit),
          aggwrsds = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, sd, na.action = na.omit),
          aggwrcounts = aggregate(wrvalue~psdvalue, data=wrpsdjoindata, length, na.action = na.omit)
        )
        joinedall <- join_all(wrdfs, by="psdvalue",type="left")
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
      if((max(selageDatafinal()$TLmm)/30)>20){
        w <- 20
      }
      if((max(selageDatafinal()$TLmm)/30)>15 & (max(selageDatafinal()$TLmm)/30)<19.9999){
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
      if((max(selageDatafinal()$TLmm)/30)>20){
        w <- 20
      }
      if((max(selageDatafinal()$TLmm)/30)>15 & (max(selageDatafinal()$TLmm)/30)<19.9999){
        w <- 15
      }
      if((max(selageDatafinal()$TLmm)/30)<14.9999){
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
      
  ###Age Length Key Bubble Plot#############################################
    
    alkbub <- function(){
      if(input$agelengthkey == TRUE){

    #make bubble plot representation
        par(mar=c(5,5,1,4)+0.1)
        alkbubplot <- alkPlot(alkobserved(), type="bubble", xlab="Total Length (mm)",
                              cex.lab = 1.5, cex.axis = 1.4)
    }
  }
    
  #render ALK bubble plot
    output$alkplot <- renderPlot(bg="transparent",{
      if(input$agelengthkey == TRUE){
        alkbub()
      }
    })
    
  ###Age-Frequency Histogram################################################
    
    agefreq <- function(){
      if(input$agefreq == TRUE){
        #find sum of all ages from sample - agesample reactive function
        agefreq <- as.data.frame(xtabs(~Age, data = agesample()))
        total <- sum(agefreq$Freq)
        agefreq <- mutate(agefreq, Freq = Freq/total)
        #Set margins around plot
        par(mar=c(5,5,1,4)+0.1)
        #make histogram using barplot function
        agefreq <- barplot(agefreq$Freq, space = .01, names.arg = agefreq$Age,
                               xlab = "Age", ylab = "Relative Frequency (%)", col = "transparent",
                               cex.lab = 1.5, cex.names = 1.4, cex.axis = 1.4)
      }
    }
    
    #render age frequency histogram
    output$agefreqhist <- renderPlot(bg="transparent",{
      if(input$agefreq == TRUE){
      agefreq()
      }
    })
    
    ##von Bertalanffy Growth Model#################################################
    
    fitvonB <- reactive({
      
      #if inch checkbox is true, mutate TL mm to inches
      if(input$inch == FALSE){
        aged <- agesample()
      }
      if(input$inch == TRUE){
        aged <- mutate(agesample(), TL_mm = TL_mm/25.4)
      }
      #define vonB starting coefficients
      vonBstarts <- list(Linf=max(aged$TL_mm, na.rm = TRUE), K=0.3, t0=0)
      #define function
      vonBfun <- vbFuns()
      #use nls package to fit vonB curve - nls.control() controls iteration parameters
      fitvonB <- nls(TL_mm~vonBfun(Age,Linf,K,t0), data = aged, start = vonBstarts,
                     control = nls.control(maxiter = 200, minFactor = 1/1000000,
                                           printEval = FALSE, warnOnly = TRUE))
    })
    
  ###Mean length-at-age plot and von Bert Plot################################################
    
    lengthplot <- function(){
      if(input$growth == TRUE){
        #if inch checkbox is true, mutate TL mm to inches
        if(input$inch == FALSE){
          aged <- agesample()
        }
        if(input$inch == TRUE){
          aged <- mutate(agesample(), TL_mm = TL_mm/25.4)
        }
        #aggregate mean of each age
        sumfish <- aggregate(TL_mm~Age, data = aged, mean)
        
        #von Bert line
        #define FSA function
        vonBfun <- vbFuns()
        #find max and min of aged for seq() next
        minsampage <- min(aged$Age)
        maxsampage <- max(aged$Age)
        #sequence of "ages" to use to plot curve
        agesline <- seq(minsampage, maxsampage, length.out = 200)
        #use vonB equation to predict ages for line on plot
        vBline <- vonBfun(agesline, Linf = coef(fitvonB()))
        #find range of age and TL to define plot boundaries
        xlimits <- range(aged$Age)
        ylimits <- range(aged$TL_mm)
        #set margins around plot
        par(mar=c(6,5,1,4)+0.1)
        
        #plot 
        if(input$inch == FALSE){
          plot(TL_mm~Age, data = aged, pch = 1, xlab="Age", ylab="Total Length (mm)",
               cex.lab = 1.5, cex.axis = 1.4)
          #plot a dashed line through mean length-at-ages
          points(TL_mm~Age, data = sumfish, pch = 17, cex = 2.2)
          lines(vBline~agesline, lwd=2)
          legend('bottomright',c('Mean Length','von Bert'),
                 lty=c(NA,1),pch=c(17,NA), lwd=2, cex=1.3, pt.cex = 2, ncol = 1)
        }
        if(input$inch == TRUE){
          plot(TL_mm~Age, data = aged, pch = 1, xlab="Age", ylab="Total Length (in)",
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
  ###Mean length-at-age table###
    lengthtable <- reactive({
      if(input$meanlength == TRUE){
        #if inch checkbox is true, mutate TL mm to inches
        if(input$inch == FALSE){
          aged <- agesample()
        }
        if(input$inch == TRUE){
          aged <- mutate(agesample(), TL_mm = TL_mm/25.4)
        }
        #summarize up the sample according to designated ages
        meanlength <- aggregate(TL_mm~Age, data = aged, mean)
        countlength <- aggregate(TL_mm~Age, data = aged, length)
        selength <- aggregate(TL_mm~Age, data = aged, se)
        sdlength <- aggregate(TL_mm~Age, data = aged, sd)
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
    
  ###von Bert Coefficient Table########################################################
    
    coeftable <- function(){
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
    }
    
    #render table of von bert coefficients
    output$vonBcoef <- renderTable(digits = 3,spacing="xs",rownames = TRUE,{
      if(input$vonbert == TRUE){
        coeftable()
      }
    })
    
  ###Catch Curve plot and mortality table#######################################
    
    catchplot <- function(){
      if(input$mort == TRUE){
        #add up catch frequency
        catch <- as.data.frame(xtabs(~Age+Age, data = agesample()))
        #change age to numeric, add 1 to frequency (avoid log(1))
        catch <- mutate(catch, Age = as.numeric(as.character(Age)), Freq = Freq+1)
        #set object to age at which max catch is obtained (descending limb) for ages2use argument
        maxcatchvector <- catch[which.max(catch$Freq),]
        maxcatchage <- as.numeric(maxcatchvector[1,1])
        #set object for ages2use argument
        maxsampage <- max(catch$Age)
        #calculate mortality with FSA
        mort <- catchCurve(Freq~Age, data = catch, ages2use = maxcatchage:maxsampage,
                           weighted = TRUE, parm = "lm")
        #r2 value for catch curve (weighted)
        limb <- catch[!(catch$Age<maxcatchage), ]
        model <- lm(log(Freq)~Age, data = limb)
        limb <- mutate(limb, wts = predict(model))
        wtdModel <- lm(log(Freq)~Age, data = limb, weights = wts)
        r2 <- summary(wtdModel)$adj.r.squared
        r2label = bquote(italic(R)^2 == .(format(r2, digits = 3)))
        
        #set margins around plot
        par(mar=c(6,5,2,2)+0.1)
        #plot catch curve
        catchplot <- plot(mort, pos.est = "bottomleft", cex.lab = 1.5, cex.axis = 1.4, 
                          cex.est = 1.5, main = r2label, cex.main = 1.5, ylab = "ln(catch)")
      }
    }

    output$catchcurve <- renderPlot(bg="transparent" ,{
      if(input$mort == TRUE){
        catchplot()
      }
    })  
    
  mortfinal <- function(){
    if(input$mort == TRUE){
      #add up catch frequency
      catch <- as.data.frame(xtabs(~Age+Age, data = agesample()))
      #change age to numeric, add 1 to frequency (avoid log(1))
      catch <- mutate(catch, Age = as.numeric(as.character(Age)), Freq = Freq+1)
      #set object to age at which max catch is obtained (descending limb) for ages2use argument
      maxcatchvector <- catch[which.max(catch$Freq),]
      maxcatchage <- as.numeric(maxcatchvector[1,1])
      #set object for ages2use argument
      maxsampage <- max(catch$Age)
      #calculate mortality with FSA
      mort <- catchCurve(Freq~Age, data = catch, ages2use = maxcatchage:maxsampage,
                         weighted = TRUE, parm = "lm")
      #calculate summary and conf intervals for estimates
      mortest <- cbind(summary(mort), confint(mort))
      #don't return t-value and p-value
      morttable <- as.data.frame(mortest[,c(1,2,5,6)])
      #rename columns
      colnames(morttable)[1:4] <- c("Mortality Rate", "Standard Error", "L 95% CI", "U 95% CI")
      rownames(morttable)[1:2] <- c("Instantaneous Z", "Annualized A (%)")
      mortfinal <- morttable
    }
  }
  
  #render mortality table
  output$mortalitytable <- renderTable(digits = 2,spacing="xs",rownames = TRUE,{
    if(input$mort == TRUE){
      mortfinal()
    }
  })  

  ###Population Dynamics column downloads###################
    
    #Download png of ALK bubble plot
    output$downALKplot <- downloadHandler(
      filename = function(){ 
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "ALKplot", "png", sep = ".")},
      content = function(file){
        png(file, width = 600, height = 450)
        alkbub()
        dev.off()
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
    
    #Downloadable csv of mean length at ages
    output$downML <- downloadHandler(
      filename = function() {
        paste(input$selectlake,input$selectyear,input$selectgear, input$selectspecies,
              "MeanLength", "csv", sep = ".")
      },
      content = function(file) {
        write.csv(lengthtable(), file, row.names = FALSE)
      }
    )
    
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
    
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Percentile tab calculations##############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #create variables to indicate if the tab just loaded for first time...used for server-side processing of choices in selectize boxes
  # FirstRun <- observeEvent(input$tabs,{
  #   if(input$tabs == "Statewide Percentiles"){
  #     #data.frame(GR=TRUE)
  #     #IterationCnt()
  #   }
  # })
  
  #load initial data
  percentileData <- reactive({
    percentileData <- read.csv("PercentileData.csv")  #use this when we go live
    #percentileData <- read.csv("PercData2005.csv")  #Dan using this for testing to save time...only data >2005
      percentileData
      })
  
  ###Calculate Selectize boxes(populate w/ gear/spp from Select Sample tab)
  
      #set up loading screen for waiter package
        waiting_screen <- tagList(
          #spin_flower(),#spin_loaders(id=32, color = "red", style = NULL),#spin_ball(),
          spin_whirly(),
          #need to move text down or spinner ends up on top of it, so add two br()
          h4(br(),br(),"Please wait while the app loads the percentile data...this may take several second")
        ) 
       
      #code creates a line break
        output$lineBrk <- renderUI({HTML("<br/>")})
        
    #Create N_agedMin slider on server side to get max age info
        maxN_aged <- reactive({max(percentileData()$N_aged, na.rm=TRUE)})
        output$NAgedMinSlider <- renderUI({sliderInput(inputId = "N_agedMin", label = 
               "Min # of aged fish used for growth/mortality metrics", min = 20, max = maxN_aged(), sep = "", step=1, value = 50)
        })
        
      #Percentiles selectize box
        output$percentileInptBox <- renderUI({
          waiter_show(html = waiting_screen, color = "#33E8FF") #puts up loading screen from waiter package
                on.exit(waiter_hide()) #removes loading screen once renderUI complete
            selectizeInput("percentileInpt", "To customize percentiles produced, type desired values in this box",
                "Percentiles:",choices = c(paste(1:99,rep("%",99),sep="")),multiple = TRUE, 
                options = list(placeholder = "click/type here"))
        })
     
          #Process input of percentileInptBox to establish which percentile values to calculate
            PercToProp = data.frame(prop = (c(seq(0.01,0.99, by= 0.01)))) %>% 
              mutate(perc = paste(prop  *100, "%", sep = ""))
            selPerctls <- Vectorize(reactive({if(is.null(input$percentileInpt)){
                selPerctls <- c(0.05,0.15,0.25,0.5,0.75,0.85,0.95)#sets default percentages
              }else{   #unless user specified percentiles to use
                selPerctls <- as.numeric(unlist((left_join(data.frame(input$percentileInpt),PercToProp,by=c("input.percentileInpt"="perc")) %>%
                 select(-"input.percentileInpt"))))}})) #reads from percentile selectize box and converts to proportion.
                
    #Create N_SurveyMin slider on server side to get min # surveys to use and set it to # percentiles requested by default
      N_perc_columns <- reactive({length(selPerctls())}) #this is not working
      output$min_survey <- renderUI({sliderInput(inputId = "N_SurveyMin", label = 
            "Min # of surveys for which to calculate percentiles", 
            #min = 3, max = 100, sep = "", step=1, value = 10)
            min = 3, max = 100, sep = "", step=1, value = N_perc_columns())
      })
        
  
  ##cross-populate selectize boxes with Code and Names selections####
    
    #Functions to grab codes from names and vice versa and syncing selection boxes for codes/names
    
      #Functions for converting codes to names
        gearnameperc <- function(){  
          gear <- as.data.frame(as.character(input$selPercGear))
          colnames(gear) <- "Gear.Code"
          gear <- plyr::join(gear,gearinfo,by="Gear.Code")
          gearname <- as.character(gear[,2])
        }
        sppnameperc <- function(){
          species <- as.data.frame(as.character(input$selPercSpp))
          colnames(species) <- "Species.Code"
          species <- plyr::join(species,speciesinfo,by="Species.Code")
          speciesname <- as.character(species[,2])
        }
        lakenameperc <- function(){
          lake <- as.data.frame(as.character(input$selLakeCodePerc))
          colnames(lake) <- "Lake.Code"
          lake <- plyr::join(lake,lakeinfo,by="Lake.Code")
          speciesname <- as.character(lake[,2])
        }
        
      #Functions for converting Names to codes
        gearcodeperc <- function(){  
          gear <- as.data.frame(as.character(input$selPercGearNm))
          colnames(gear) <- "Gear.Name"
          gear <- plyr::join(gear,gearinfo,by="Gear.Name")
          gearcode <- as.character(gear[,2])
        }
        speciescodeperc <- function(){  
          species <- as.data.frame(as.character(input$selPercSppNm))
          colnames(species) <- "Species.Name"
          species <- plyr::join(species,speciesinfo,by="Species.Name")
          speciescode <- as.character(species[,2])
        }
        lakecodeperc <- function(){
          lake <- as.data.frame(as.character(input$selLakeNamePerc))
          colnames(lake) <- "Lake.Name"
          lake <- plyr::join(lake,lakeinfo,by="Lake.Name")
          lakecode <- as.character(lake[,2])
        }
        
    #update selectize boxes from Code to Name and server-side processing choices (is faster than ui.r)
    observe({withProgress(message = "Loading Cross-pop Code to Name Data",min=0,max=10,value=1, {
      if(is.null(input$selPercGear)){
          updateSelectizeInput(session, "selPercGearNm", selected = NULL, choices = sort.default(gearinfo$Gear.Name),
          server = TRUE)
        }else{
          updateSelectizeInput(session, "selPercGearNm", selected = gearnameperc())
        }
      if(is.null(input$selPercSpp)){
          updateSelectizeInput(session, "selPercSppNm", selected = NULL, choices = sort.default(speciesinfo$Species.Name), 
          server = TRUE)
        }else{
          updateSelectizeInput(session, "selPercSppNm", selected = sppnameperc())
        }  
      if(is.null(input$selLakeCodePerc)){
          updateSelectizeInput(session, "selLakeNamePerc", selected = NULL, choices = sort.default(lakeinfo$Lake.Name),
          server = TRUE)
      }else{
        updateSelectizeInput(session, "selLakeNamePerc", selected = lakenameperc())
      }
    })})
    
    #update selectize boxes from Name to Code (and server-side processing of code box choices)
       
      #create Gear Code observer function to set choices and default selected item on first run
      observe({withProgress(message = "Loading Cross-pop Name to Code Data",min=0,max=10,value=1, {
         updateSelectizeInput(session, "selPercGear", choices = sort.default(gearinfo$Gear.Code),
            selected = input$selectgear, server = TRUE)
        # updateSelectizeInput(session, "selPercGear", choices = sort.default(percentileData()$Gear.Code),
        #      selected = input$selectgear, server = TRUE) #option using available gear codes in percentileData
      })})
      
      #create Species Code observer function to set choices and default selected item on first run
      observe({withProgress(message = "Loading Cross-pop Name to Code Data",min=0,max=10,value=1, {
         updateSelectizeInput(session, "selPercSpp", choices = sort.default(speciesinfo$Species.Code),
            selected = input$selectspecies, server = TRUE)
        # updateSelectizeInput(session, "selPercSpp", choices = sort.default(percentileData()$Species.Code),
        #     selected=input$selectspecies, server = TRUE)
      })})
      
      #create Lake Code observer function to set choices on first run
      observe({withProgress(message = "Loading Cross-pop Name to Code Data",min=0,max=10,value=1, {
        updateSelectizeInput(session, "selLakeCodePerc", choices = sort.default(lakeinfo$Lake.Code),
            selected = ,server = TRUE) 
      })})
      
    #create observer function to set value to name boxes if value in code boxes change
      observe({
        updateSelectizeInput(session, "selPercGear", selected = gearcodeperc())
        updateSelectizeInput(session, "selPercSpp", selected = speciescodeperc())
        updateSelectizeInput(session, "selLakeCodePerc", selected = lakecodeperc())
      })
        
    
    ##Filter percentileData for selected parameters####
    #Filter main table based on lake, years, gears, region, and/or lake
    selPercData <- reactive({
      selPercData <- left_join(percentileData(),(lakeinfo %>% select(Lake.Code,ODWC.Region)),by = "Lake.Code")
      if(!is.null(input$selPercGear)){selPercData <- selPercData[selPercData$Gear.Code %in% c(input$selPercGear),]}
      if(!is.null(input$selPercSpp)){selPercData <- selPercData[selPercData$Species.Code %in% c(input$selPercSpp),]}
      if(!is.null(input$perYrs)){selPercData <- selPercData[selPercData$Year %in% c(input$perYrs[1]:input$perYrs[2]),]}
      if(!is.null(input$selRegionPerc)){selPercData <- selPercData[selPercData$ODWC.Region %in% c(input$selRegionPerc),]}
      if(!is.null(input$selLakeCodePerc)){selPercData <- selPercData[selPercData$Lake.Code %in% c(input$selLakeCodePerc),]}
      selPercData
    })
      
    #Create age-related data based on N_agedMin slider input
    selPercData_age <- reactive({
      selPercData_age <- subset(percentileData(), N_aged >= input$N_agedMin)
    })
    #Create sort files for all tables that include PSD values
    #SortPSD <- data.frame(PSDname=c("","substock","stock","quality","preferred","memorable","trophy"),sortOrder=c(1,2,3,4,5,6,7))
      ###moved above line to top of program as we will use for regular SSP table sorting when adding Trophy category back into code

    
 ###Calculate percentiles for various metrics##############################
 
  #Max TL and Wt table calculations
    maxTL_WT_percTable <- reactive({
      maxTL_Wt <- selPercData() %>% group_by(Species.Code) %>% 
          select(SurveyID:Species.Code, maxTL:N_maxWt) %>% 
          pivot_longer(cols = c(maxTL, maxWt), names_to = "Metric", values_to="Values")
  
      Nsurveys <- maxTL_Wt %>% subset(!is.na(Values)) %>% group_by(Species.Code,Metric) %>%
        summarise("# Surveys"=n(),.groups = "drop_last")
          
      maxTL_WT_percMetrics <- maxTL_Wt%>% group_by(Species.Code, Metric) %>%
        group_modify(~{quantile(.x$Values, probs = selPerctls(), na.rm = TRUE) %>% tibble::enframe()}) %>%
        mutate(value2 = case_when(Metric == "maxTL" ~ as.character(round(value, 0)),
                                Metric == "maxWt" ~ as.character(format(round(value, 1), nsmall = 1)))) %>%
        select(-value) %>%
        pivot_wider(names_from = name, values_from = value2) %>%
        left_join(Nsurveys, by = c("Species.Code", "Metric")) %>%
        left_join(speciesinfo, by = "Species.Code") %>% relocate(Species.Name) %>%
        mutate(Metric=recode(Metric, "maxTL" = "Maximum TL (mm)", "maxWt" = "Maximum Weight (g)")) %>% #rename metric values
        rename("Species Name" = "Species.Name") %>%
        arrange(Metric, "Species Name") %>%
        ungroup() %>% as.data.frame() %>% select(-Species.Code) 
      
      maxTL_WT_percMetrics <- maxTL_WT_percMetrics[complete.cases(maxTL_WT_percMetrics[,"# Surveys"]) &
        maxTL_WT_percMetrics$"# Surveys" >= input$N_SurveyMin,] #remove rows with # Surveys = NA or those with < min # surveys allowed
        #note subset() does not work well for above statements...not sure why
      
      maxTL_WT_percMetrics
    })
 
    
  #CPUE percentile table calculation
    SortCPUE <- SortPSD %>% mutate(metric_cat = paste("CPUE", SortPSD$PSDname, sep=""), 
        cat_title = paste("CPUE-", SortPSD$PSDname, sep = "")) %>% 
        mutate(cat_title = replace(cat_title, cat_title == "CPUE-", "Overall CPUE")) %>% select(-PSDname)

    CPUEpercTable <- reactive({
      if(!is.null(input$selPercGear)){
        withProgress(message = "Loading CPUE table Data", min = 0, max = 10, value = 1, {
          CPUEperc <- selPercData() %>% select(SurveyID:Nsites, CPUE:CPUEtrophy) %>% group_by(Species.Code) %>% 
            pivot_longer(cols = CPUE:CPUEtrophy, names_to = "metric_cat", values_to="metricVal")
          
          Nsurveys <- CPUEperc %>% subset(!is.na(metricVal)) %>% group_by(Species.Code, metric_cat) %>% 
            summarise("# Surveys" = n(), .groups = "drop_last")
          
          CPUEpercMetrics <- CPUEperc %>% 
            group_by(Species.Code, metric_cat) %>% 
            group_modify(~{quantile(.x$metricVal, probs=selPerctls(), na.rm = TRUE) %>% 
                tibble::enframe()}) %>% 
                  pivot_wider(names_from = name, values_from = value) %>% 
                  left_join(Nsurveys, by = c("Species.Code", "metric_cat")) %>% 
                  left_join(SortCPUE, by = "metric_cat") %>% arrange(Species.Code, sortOrder) %>% 
                  left_join(speciesinfo, by = "Species.Code") %>% relocate(Species.Name, cat_title) %>% 
                  ungroup() %>% 
                  select(-sortOrder, -metric_cat, -Species.Code) %>% 
                  rename("Species Name" = "Species.Name", "CPUE category" = "cat_title")
            #note above group_modify() iterates a function on groups of data, but returns an atomic vector that needs to be parsed into
            #columns by enframe() to make a data fame
          CPUEpercTable <- CPUEpercMetrics[complete.cases(CPUEpercMetrics[,"# Surveys"]) & #remove NA's
                                             CPUEpercMetrics$"# Surveys" >= input$N_SurveyMin,] #remove rows based on too few surveys
        })
      }
    })

    #PSD percentile calculations
    PSDpercTable <- reactive({
      PSDperc <- selPercData() %>% group_by(Species.Code) %>% 
        select(SurveyID:Nsites, NfishStock:PSD.T) %>% 
        pivot_longer(cols = PSD_S.Q:PSD.T, names_to = "PSDmetric", values_to="PSDVal") %>% 
        drop_na(PSDVal) %>% filter(NfishStock >= input$N_PSDMin)
      
      Nsurveys <- PSDperc %>% group_by(Species.Code,PSDmetric) %>% 
        summarise("N_Surveys"=n(),.groups = "drop_last")
      
      PSDsortOrder <- data.frame(PSDmetric=c("PSD_S.Q", "PSD_Q.P", "PSD_P.M", "PSD_M.T",
                                   "PSD", "PSD.P", "PSD.M", "PSD.T"), sortOrder=1:8)
      PSDpercMetrics <- PSDperc %>% 
        left_join(Nsurveys, by=c("Species.Code","PSDmetric")) %>% 
        filter(N_Surveys >= input$N_SurveyMin) %>% 
        group_by(Species.Code, PSDmetric, N_Surveys) %>% 
        group_modify(~{quantile(.x$PSDVal, probs=selPerctls(), na.rm=TRUE) %>%tibble::enframe()}) %>% 
        pivot_wider(names_from = name,values_from=value) %>% 
        left_join(PSDsortOrder, by="PSDmetric") %>% arrange(Species.Code, sortOrder) %>% 
        left_join(speciesinfo, by="Species.Code") %>% relocate(Species.Name) %>% 
        relocate(N_Surveys, .after = last_col()) %>% ungroup() %>% 
        mutate("PSD Metric" = recode(PSDmetric, PSD_S.Q = "PSD S-Q", PSD_Q.P = "PSD Q-P", PSD_P.M = 
           "PSD P-M", PSD_M.T = "PSD M-T", PSD.P = "PSD-P", PSD.M = "PSD-M", PSD.T = "PSD-T")) %>% 
        relocate("PSD Metric", .after = Species.Name) %>% 
        select(-sortOrder, -PSDmetric, -Species.Code) 
    })

  ###Make percentile output tables##############################

    #Max TL and Wt output table
      output$Max_TL_Wt_perc <- renderTable(spacing = "xs", {
        if(!is.null(input$selPercGear) | !is.null(input$selPercSpp)){
          if(nrow(selPercData() )>3){
            maxTL_WT_percTable()
          }
        }
      })
      output$Max_TL_Wt_Text <- renderText({
        if(!is.null(input$selPercGear) | !is.null(input$selPercSpp)){
            CPUEtext <- as.character("Maximum TL (mm) and Weight (g) Percentiles")
        }
       })
      
    #Put CPUE percentile data in CPUEpercTable into output table
      output$CPUEperc <- renderTable(digits = 2, spacing = "xs", {
        if(!is.null(input$selPercGear)){
          if(nrow(selPercData() )>3){
            CPUEpercTable()
          }
        }
      })
      output$CPUEpercText <- renderText({
        if(!is.null(input$selPercGear)){
          if(nrow(selPercData() )>3){
            CPUEtext <- as.character("CPUE Percentiles")
          }else{
            CPUEtext <- as.character("CPUE Percentiles - insufficent data exists to calculate percentiles, please select different parameters")
          }
        }
      })
      
    #Put PSD percentile data in PSDpercTable into output table
      output$PSDperc <- renderTable(digits = 0, spacing = "xs", {
        if(!is.null(input$selPercGear)){
          if(nrow(selPercData() )>3){
            PSDpercTable()
          }
        } 
      })
      output$PSDpercText <- renderText({
        if(!is.null(input$selPercGear)){
          if(nrow(selPercData() )>3){
            PSDtext <- as.character("PSD Percentiles")
          }else{
            PSDtext <- as.character("PSD Percentiles - insufficent data exists to calculate percentiles, please select different parameters")
          }
        }
      })
      
    #need to decide how to handle single-species analysis...group by species or only provide output if 1 spp selected
    
      #maybe need to make reverse slider to select min # surveys for all metrics...default = 5 but go down to 3?
      #Also might want a min # fish/sample for all but CPUE metric?...but maybe not...adds noise but adds N too and probably unbiased
      #Maybe is bigger issue for max TL and WT?
    #Wr by PSD
    #Max TL...need to change percentileData to do this?
    #Max Wt
    #mean length at ages
    #
  
# Note we can change the location of the withProgress box using code at
# https://stackoverflow.com/questions/35037230/change-style-and-position-of-the-message-box-generated-by-withprogress

#################
      
##Stuff Dan used for testing/developing this tab#  
      output$firstRun <- renderText(input$N_agedMin)#{paste("GearRun=",GearRun)})
      #output$firstRun <- renderText({paste("first run=",FirstRun$GR)})
      #selPercData2 <- reactive({selPercData2 <- selPercData() })#has 84 columns w/o ODWC.Region  %>% select(!Nfish:A)
  #Stuff for debugging using table outputs  #output$percentileTable <- renderTable(CPUEpercTable())
  #output$perYrsTabl <- renderTable(input$perYrs)
  output$selPercDataTbl <- DT::renderDataTable(selPercData())
  output$selPercAgedTbl <- DT::renderDataTable(selPercData_age() %>% 
           relocate(N_aged))
  #output$test <- renderTable(data.frame(input$percentileInpt))
  # output$test <- renderTable(data.frame(selPerctls()))
  # output$test2 <- renderTable(data.frame(PercToProp))
  #  output$test <- renderTable(data.frame(selPercData2()))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
#stocking information Tab########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  stockinfo <- reactive({
     
    withProgress(message = "Loading Data", min=0,max=10,value=1, {
      
      stockinfo <- stockingData[stockingData$Year >= input$stockrange[1] & stockingData$Year <= input$stockrange[2],]

    if(!is.null(input$stocklake)){ 
      stockinfo <- stockinfo[stockinfo$Water.Body %in% c(input$stocklake),]
    }
    if(!is.null(input$stockspp)){
      stockinfo <- stockinfo[stockinfo$Species %in% c(input$stockspp),]
    }
      stockinfo <- stockinfo[3:8]
      colnames(stockinfo) <- c("Date Stocked", "Water Body", "Species", "Number Stocked", "Size (in)",
                               "Hatchery (origin)")
      stockinfo <- stockinfo
    })
  })
 
  #render stocking info table
  output$stocktable <- renderTable(digits = 2,spacing="xs",rownames = FALSE,{
    if(!is.null(input$stocklake) | !is.null(input$stockspp))  
      stockinfo()
  }) 
  
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
    

