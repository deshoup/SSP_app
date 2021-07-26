#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
library(DT)
library(V8)#package needed to refresh page...only used if we upload csv files
#library(dplyr)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #Javascript needed to refresh page...will only need on file upload screen

#.csv's to read in#############################################
gearinfo <- read.csv("gearinfo.csv")
lakeinfo <- read.csv("lakeinfo.csv")
yearmonthinfo <- read.csv("yearmonthinfo.csv")
speciesinfo <- read.csv("speciesinfo.csv")
stockingData <- read.csv("stockingdata.csv")


fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("OK Fishery Analysis App")),
    useShinyjs(),

  # Application title with ODWC logos
  titlePanel(
    wellPanel(
        fluidRow(
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="150px")),
        column(6, align="center", h2("Oklahoma Fishery Analysis Application"),
               hr(), 
               h5("Created by Dray D. Carl and Daniel E. Shoup")),
        column(3, align="center",img(src="osulogo.png", height="auto", width="180px"))
        )
    ),
    windowTitle = "OK Fishery Analysis App" #this text is what appears as browser title
  ),
  
 # Menu structure with main functions of app. Each "tabPanel" line makes another tab with code for each page underneath
  tabsetPanel(id="tabs", type = c("tabs"),
    
             ########################################################
             #Select Sample Tab
             ########################################################
             tabPanel("Select Sample",
              hr(),  
                fluidRow(
                  #Code from left select section of main data selection tab
                  column(4,
                    sidebarPanel(width = 12,
                      h4("Filter Dataset by Codes", align="center"),
                      hr(),
                      selectizeInput(
                        "selectlake", "Lake Code(s):", choices = lakeinfo$Lake.Code,   
                          multiple = TRUE, options = list(placeholder = "click/type here")
                      ),
                      selectizeInput(
                        "selectyear", "Year(s):", choices = yearmonthinfo$Year,
                        multiple = TRUE, options = list(placeholder = "click/type here")
                      ),
                      selectizeInput(
                        "selectmonth", "Month(s):", choices = yearmonthinfo$Month,
                        multiple = TRUE, options = list(placeholder = "click/type here")
                      ),
                      selectizeInput(
                        "selectgear", "Gear Code(s):", choices = gearinfo$Gear.Code,
                          multiple = TRUE, options = list(placeholder = "click/type here")
                      ),
                      helpText("All codes can be referenced in the SSP Manual tab.")
                    )
                  ),
                 
                #Data selection summary - renderText to show full names of lakes, gears, species
                 column(4, align = "center",
                  mainPanel(width = 12, 
                    wellPanel(        
                      h4("Sample Selection Summary"),
                      hr(),
                        h4(textOutput("lakename")),
                        h4(textOutput("year")),
                        h4(textOutput("month")),
                        h4(textOutput("gearname")),
                        span(textOutput("useUploadedsamp"), style="color:green"),
                      #ui side for loading initial .csv's
                      div(id = "loading-content",
                          h1("Uploading Data..."),
                          style="color:red"),
                      hr(),
                      h4(helpText("Minimum of Lake, Year, and Gear inputs must be selected. Multiple selections are allowed
                                 within each field.")),
                      h4(helpText("Filtered dataset will appear below.")),
                      hr(),
                      downloadButton("downloadData", "Download Selected Sample Data")
                    ), 
                    wellPanel(
                      h4("Alternatively, upload your own sample data"),
                      helpText("Must have correct .csv format and column headings"),
                      fileInput("loadedData", "Upload Sample Data"),
                      checkboxInput("loadCheck", "Check to use uploaded sample data", value = FALSE)
                    )
                  )
                ),
                 column(4,
                  sidebarPanel(width = 12,
                    h4("Filter Dataset by Names", align="center"),
                    hr(),
                    selectizeInput(
                      "selectlakename", "Lake Name(s):", choices = sort.default(lakeinfo$Lake.Name),   
                      multiple = TRUE, options = list(placeholder = "click/type here")
                    ),
                    selectizeInput(
                      "selectyearname", "Year(s):", choices = yearmonthinfo$Year,
                      multiple = TRUE, options = list(placeholder = "click/type here")
                    ),
                    selectizeInput(
                      "selectmonthname", "Month(s):", choices = yearmonthinfo$Month,
                      multiple = TRUE, options = list(placeholder = "click/type here")
                    ),
                    selectizeInput(
                      "selectgearname", "Gear Name(s):", choices = sort.default(gearinfo$Gear.Name),
                      multiple = TRUE, options = list(placeholder = "click/type here")
                    )
                  )
                 )
                ),
                #code with output from widgets showing data to be analyzed
                hr(),
                fluidRow(
                  textOutput("printGear"),
                  DT::dataTableOutput("selectedDataTable")
                  )
              ),
             
             ########################################################
             #Select Types of Analyses Tab
             ########################################################
              tabPanel("Select Analyses",
                      hr(),  
                      sidebarPanel(width=12,
                        fluidRow(    
                          column(4,
                            wellPanel(
                              h4("Catch Analyses Metrics", align="center"),
                              hr(),
                              wellPanel(
                                checkboxInput("abiotic", "Abiotic Metrics", value = TRUE), 
                                checkboxInput("samplesize", "Sample Size", value = TRUE),
                                checkboxInput("totaleffort", "Total Effort", value = TRUE),
                                checkboxInput("cpue", "CPUE", value = TRUE),
                                checkboxInput("cpuesize", "CPUE by Size Category", value=TRUE)
                              ),
                              helpText("Catch Analysis Output tab includes all species.")
                            ),
                            wellPanel(
                              h4("Selected Sample Data", align="center"),
                              hr(),
                                h4(textOutput("lakename3")),
                                h4(textOutput("year3")),
                                h4(textOutput("month3")),
                                h4(textOutput("gearname3")),
                                h4(textOutput("speciesname3")),
                              hr(),
                              h4("Selected Age Data", align="center"),
                              hr(),
                                h4(textOutput("agespp")),
                                h4(textOutput("agelake")),
                                h4(textOutput("ageyear"))
                            )
                          ),
                          column(4,
                            wellPanel(
                              h4("Single Species Analyses Selection", align="center"),
                              hr(),
                                  uiOutput("speciesselection"),
                                  uiOutput("speciesnameselection"),
                                  helpText("Species must be selected for Single Species Analysis Output"),
                                downloadButton("downloadsppData", "Download Selected Species Data"),
                              hr(),
                              h4("Select Age Dataset"),
                              hr(),
                                span(textOutput("yesmatch"), style="color:green"),
                                span(textOutput("nomatch"), style="color:red"),
                                span(textOutput("nospp"), style="color:orange"),
                                span(textOutput("nodata"), style="color:orange"),
                                span(textOutput("useUploadedage"), style="color:green"),
                                h4(textOutput("agecount")),
                                hr(),
                                  uiOutput("selagespp"),
                                  uiOutput("selagelake"),
                                  uiOutput("selageyears"),
                                  helpText("All three selection boxes are required to select age dataset."),
                                  helpText("Multiple selections are allowed in Lake and Year fields."),
                                  helpText("Age dataset will appear below."),
                                downloadButton("downagedata", "Download Selected Age Data")
                            )
                           ),
                           column(4,
                            wellPanel(
                              h4("Single Species Analyses Metrics", align="center"),
                                hr(),
                                h4("Length and Weight Metrics"),
                                hr(),
                                checkboxInput("lengthfrequency", "Length Frequency", value = TRUE),
                                checkboxInput("psd", "PSD Table", value = TRUE),
                                checkboxInput("wr", "Relative Weight Table", value = TRUE),
                                checkboxInput("lwregression", "Length-Weight Regression", value = TRUE),
                                checkboxInput("max", "Max Length and Weight", value = TRUE),
                                hr(),
                                h4("Population Dynamics"),
                                hr(),
                                  checkboxInput("agelengthkey", "Age-Length Key", value = TRUE),
                                  checkboxInput("agefreq", "Age-Frequency Histogram", value = TRUE),
                                  checkboxInput("growth", "Growth Metrics Plot", value = TRUE),
                                  checkboxInput("meanlength", "Mean Length-at-Age", value = TRUE),
                                  checkboxInput("meanweight", "Mean Weight-at-Age", value = TRUE),
                                  checkboxInput("vonbert", "Von Bertalanffy Equation", value = TRUE),
                                  checkboxInput("mort", "Catch Curve (Mortality)", value = TRUE),
                              hr(),
                              h4("Alternatively, upload your own age data"),
                              helpText("Must have correct .csv format and column headings"),
                              fileInput("loadedageData", "Upload Age Data"),
                              checkboxInput("loadageCheck", "Check to use uploaded age data", value = FALSE)
                            )
                          )
                        )  
                      ),
                      DT::dataTableOutput("selectedageData")
                ),            
             
             ########################################################
             #Catch Analysis Output Tab
             ########################################################             
              tabPanel("Catch Analyses",
                    hr(),
                      mainPanel(width=12,
                        fluidRow(
                          column(4,
                            wellPanel(
                              h4("Sampling Event Summary", align="center"),
                              hr(),
                              h4(textOutput("lakename2")),
                              h4(textOutput("year2")),
                              h4(textOutput("month2")),
                              h4(textOutput("gearname2"))
                            ),
                            wellPanel(
                              h4("Abiotic Metrics", align="center"),
                              hr(),
                              tableOutput("abioticsum")     
                            ),
                            wellPanel(
                              h4("Sample Size and Effort", align="center"),
                              hr(),
                                h4(helpText("Sample Sizes")),
                                tableOutput("totsamplesize"),
                              hr(),
                                h4(helpText("Total Effort")),
                                tableOutput("toteffort")
                            )
                          ),
                          column(8,
                            wellPanel(
                              h4("Catch per Unit Effort", align="center"),
                              hr(),
                              h4("Total CPUE"), 
                              hr(),
                                tableOutput("totcpue"),
                              hr(),
                              h4("CPUE by Size Category"),
                              hr(),
                              h4(helpText("Size Category References (mm)")),
                                tableOutput("sizerefmm"),
                              h4(helpText("Size Category References (in)")),
                                tableOutput("sizerefin"),
                              hr(),
                                h4(helpText("CPUE by Size Category")),
                                tableOutput("cpuebysize"),
                              hr(),
                              h4("Catch Analysis Tab Downloads"),
                              downloadButton("downcpue", "Total CPUE Table"),
                              downloadButton("downcpuebysize", "CPUE by Size Table")
                            )       
                          )
                        )
                      )
              
             ),            
             
             ########################################################
             #Single Species Analysis Output
             ########################################################
             tabPanel("Single Species Analyses", value="analyze",
               hr(),       
                mainPanel(width=12, 
                  fluidRow(
                    column(6,  
                      wellPanel(
                        h3(textOutput("sampleline")),
                            span(textOutput("uploadedAgeData"), style="color:orange"),
                            span(textOutput("ageDataMatch"), style="color:green"),
                            span(textOutput("ageDataNoMatch"), style="color:red"),
                            textOutput("ageDataCount")
                      ),
                      wellPanel(
                        h3("Length and Weight Metrics"),
                          hr(),
                            h4("Length-Frequency Histogram"),
                            plotOutput("lengthhist"),
                            numericInput("lengthbin", "Length Bin Grouping (mm)", value = 10, min = 1,
                                         max = 200, width = '180px'),
                            helpText("Enter desired length bin grouping: default is 10 mm."),
                            helpText("Enter 25.4 mm for 1-inch groupings"),
                          hr(),
                          downloadButton("downlfplot","Length-Frequency Plot"),
                          downloadButton("downlftable", "Length-Frequency Tabular Data"),
                          hr(),
                            h4("Proportional Size Distribution (PSD)"),
                          hr(),
                            h4(helpText(textOutput("speciesref"))),
                            helpText("References of lengths for PSD size categories"),
                            tableOutput("psdvaltable"),
                            tableOutput("psdtable"),
                          hr(),
                            h4("Relative Weight (Wr)"),
                          hr(),
                            helpText("Details of standard weight equation"),
                            tableOutput("standardequation"),
                          hr(),
                            tableOutput("wrtable"),
                          hr(),
                            h4("Length-Weight Regression"),
                            plotOutput("lwplot"),
                            tableOutput("lwcoef"),
                          hr(),
                            h4("Max Length and Weight"),
                          hr(),
                            tableOutput("maxspptab"),
                          hr(),
                          h4("Length-Weight Downloads"),
                            downloadButton("downpsd", "PSD Table"),
                            downloadButton("downwr", "Relative Weight Table"),
                            downloadButton("downLWregplot","Length-Weight Regression Plot")
                      )
                    ),
                    column(6,
                      wellPanel(
                        h3("Population Dynamics"),
                          hr(),
                            h4("Selected Age-Length Key"),
                            plotOutput("alkplot"),
                          hr(),
                            downloadButton("obsALK","Observed Age-Length Key"),
                            downloadButton("smoothALK","Smooth Age-Length Key"),
                            downloadButton("downALKplot","Age-Length Key Plot"),
                          hr(),
                            h4("Age Frequency Histogram"),
                            plotOutput("agefreqhist"),
                          hr(),
                            h4("Growth Metrics"),
                            helpText("Mean Length-at-Age and von Bertalanffy Growth Equation"),
                          hr(),
                            h4(checkboxInput("inch", "Display Measurements in English Units (in & lbs)", value = FALSE)),
                            helpText("Default is metric (mm and g). Checkbox changes units to in. and lbs."),
                            plotOutput("meanlengthplot"),
                            helpText("Mean Length-at-Age"),
                            tableOutput("meanlengthtable"),
                            helpText("Mean Weight-at-Age"),
                            tableOutput("meanweighttable"),
                            helpText("von Bertalanffy Growth Equation"),
                            tableOutput("vonBcoef"),
                          hr(),
                            h4("Catch Curve (Mortality)"),
                            plotOutput("catchcurve"),
                            tableOutput("mortalitytable"),
                          hr(),
                          h4("Population Dynamics Downloads"),
                            downloadButton("downafplot","Age-Frequency Plot"),
                            downloadButton("downmeanplot","Growth Metrics Plot"),
                            downloadButton("downML", "Mean Length-at-Age Table"),
                            downloadButton("downMW", "Mean Weight-at-Age Table"),
                            downloadButton("downvonBcoef", "von Bert Equation Table"),
                            downloadButton("downmort", "Catch Curve Plot"),
                            downloadButton("downmorttable", "Mortality Table")
                            
                      )
                    )
                  )
                )
              ),
             ###################################################
             #Percentiles tab
             ###################################################
              tabPanel("Statewide Percentiles",
                       hr(),
                       column(4,
                          sidebarPanel(width = 12,
                            #req(percentileData),
                             h4("Select parameters for percentile calculations", align="center"),
                             hr(),

                             #selectize boxes for parameterizing percentile calculations
                              #gear and species selectize populated with gear/spp from sample selection data
                                 div(style="display:inline-block",uiOutput("selGearPerc")),#code for gear code selectizeInput on server side to populate with gear from analysis tab
                                 div(style="display:inline-block",
                                     selectizeInput("selPercGearNm", "Gear Name(s):", choices = sort.default(gearinfo$Gear.Name),
                                          multiple = TRUE, options = list(placeholder = "click/type here"))),  #gear name selectize
                                 uiOutput("lineBrk"), #create hard return
                                 div(style="display:inline-block",uiOutput("selSppPerc")), #select Species code processed on server.r
                                 div(style="display:inline-block",selectizeInput("selPercSppNm", "Species Name(s):", width = "100%",
                                    choices = sort.default(speciesinfo$Species.Name),multiple = TRUE, options = list(placeholder =
                                    "click/type here"))),  #select species name

                              #date slider
                               # sliderInput("perYrs", "Year Range", min = min(percentileData$Year),
                               #             max = max(percentileData$Year, na.rm = FALSE), sep = "", step=1,
                               #             value = c(2010, max(percentileData$Year, na.rm = TRUE))),
                               sliderInput("perYrs", "Year Range", min = 1980,
                                         max = as.numeric(format(Sys.Date(), "%Y")), sep = "", step=1,
                                         value = c(2010, as.numeric(format(Sys.Date(), "%Y")))),

                              #Region selection box
                                # selectizeInput("selRegionPerc", "Management Region(s):", choices = dplyr::distinct(lakeinfo,ODWC.Region),
                                #     multiple = TRUE, options=list(placeholder = "click/type here")),
                                selectizeInput("selRegionPerc", "Management Region(s):", choices = unique(lakeinfo$ODWC.Region),
                                    multiple = TRUE, options=list(placeholder = "click/type here")),

                              #lake code and lake name boxes (will move lake code box to server.r side like did for gear code/name)
                               div(style="display:inline-block",
                                   selectizeInput("selLakePerc", "Lake Code(s):", choices = lakeinfo$Lake.Code,multiple = TRUE, options =
                                    list(placeholder = "click/type here"))
                                  ),
                               div(style="display:inline-block",
                                   selectizeInput("selectlakename", "Lake Name(s):", choices = sort.default(lakeinfo$Lake.Name),multiple =
                                     TRUE, options = list(placeholder = "click/type here"))
                                  ),

                              #Custome percentiles to calculate
                               uiOutput("percentileInptBox"), #create input box for specifying desired percentiles...code generated on server.r side

                            helpText("All codes can be referenced in the SSP Manual tab.")
                                )
                         ),

                       # #create loading content message
                        div(id = "loading-content-percTab", h1("Reading in percentile data, please wait..."),style="color:red"),
                      # uiOutput("condPanel"),
                       column(8,
                              wellPanel(
                                # h4("PercToProp"),
                                # tableOutput("test2"),
                                # h4("Percentiles df"),
                                # tableOutput("test"),


                                h3("Percentile Values"),
                                hr(),
                                tableOutput("CPUEperc"),
                                h3("testing raw data"),
                                hr(),
                                #tableOutput("selPercDataTbl")
                                #DT::dataTableOutput("selPercDataTbl")

                              )
                      )
                
              ),
             ###################################################
             #Stocking info tab
             ###################################################
              tabPanel("Stocking Information",
                       hr(),
                       mainPanel(width=12,
                         fluidRow(
                           column(4,
                             wellPanel(
                               h4("Use this tab to find out what was stocked in a particular water body.  This feature
                                    may complement analyses from other tabs and aid management decisions."),
                               helpText("Use criteria selection boxes below to filter stocking information. Table will
                                        appear in the right panel and is available to download with button below."),
                               helpText(" Multiple selections are allowed in each selection box."),
                               hr(),
                               h3("Stocking Information Criteria"),
                               hr(),
                               sliderInput("stockrange", "Year Range", min = 1931,
                                           max = max(stockingData$Year, na.rm = FALSE), sep = "",
                                           value = c(max(stockingData$Year, na.rm = TRUE)-10,
                                                     max(stockingData$Year, na.rm = TRUE))),
                               selectizeInput(
                                    "stocklake", "Water Body", choices = sort.default(unique(stockingData$Water.Body)),
                                    multiple = TRUE, options = list(placeholder = "click/type here")
                                ),
                               selectizeInput(
                                 "stockspp", "Species", choices = sort.default(unique(stockingData$Species)),
                                 multiple = TRUE, options = list(placeholder = "click/type here")
                               ),
                               hr(),
                               downloadButton("downstocking", "Selected Stocking Information Table")
                             )
                           ),
                           column(8,
                             wellPanel(
                               h3("Stocking Information Table"),
                               hr(),
                                tableOutput("stocktable")
                             )
                           )
                         )
                       )
              ), 
             #############################################
             #User's guide tab
             #############################################
               tabPanel("User's Guide",
                        hr(),
                        mainPanel(width=12,
                          fluidRow(
                            column(2),
                            column(8, align="center",
                                  tags$iframe(style="height:700px; width:100%",
                                              src="user.guide.ofaa.pdf#page=1&view=FitH")
                            )      
                          )        
                        )
               ),
             ########################################
             #SSP Manual tab
             ########################################
               tabPanel("SSP Manual",
                        hr(),
                        mainPanel(width=12,
                                  fluidRow(
                                    column(2),
                                    column(8, align="center",
                                           tags$iframe(style="height:700px; width:100%",
                                                       src="odwc.ssp.ofaa.pdf#page=1&view=FitH")
                                    )      
                                  )        
                        )
               ),
             #################################
             #Acknowledgements tab
             #################################
              tabPanel("Acknowledgements",
                      hr(),
                      mainPanel(width=12,
                        fluidRow(
                          column(6,
                            wellPanel(
                               align = "center",
                               h3("Funded by the Oklahoma Department of Wildlife Conservation"),
                               img(src="ODWClogo.gif", height="auto", width="28%"),
                               h3("Project F-50-R"),
                              hr(),
                               h4("Special thanks to:"),
                               h4("Ashley Nealis"),
                               h4("Kurt Kuklinski"),
                              hr(),
                               h4("Helpful input provided by:"),
                               h4("Josh Johnston"),
                               h4("Jason Schooley"),
                               h4("Cliff Sager"),
                               h4("Garrett Johnson")
                            )
                          ),
                          column(6,
                             wellPanel(
                              h3("R Packages Used:"),
                              hr(),
                               h4("FSA"),
                               h5("Ogle, D.H. 2017. FSA: Fisheries Stock Analysis. R package version 0.8.17."),
                               helpText("When using output derived from this app, please cite the above
                                        FSA Package. Almost every metric used a function from this package."),
                               h4("shiny"),
                               h5("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web
                                 Application Framework for R. R package version 1.0.5."),
                               h4("shinyjs"),
                               h5("Dean Attali (2017). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds.
                                 R package version 0.9.1."),
                               h4("V8"),
                               h5("Jeroen Ooms (2017). V8: Embedded JavaScript Engine for R. R package version 1.5."),
                               h4("dplyr"),
                               h5("Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller (2018). dplyr: A Grammar of Data
                                  Manipulation. R package version 0.7.5."),
                               h4("plyr"),
                               h5("Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of
                                 Statistical Software, 40(1), 1-29."),
                               h4("tidyr"),
                               h5("Hadley Wickham and Lionel Henry (2017). tidyr: Easily Tidy Data with 'spread()' and 'gather()'
                                 Functions. R package version 0.7.2."),
                               h4("nnet"),
                               h5(" Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition.
                                Springer, New York. ISBN 0-387-95457-0"),
                               h4("nlstools"),
                               h5("Florent Baty, Christian Ritz, Sandrine Charles, Martin Brutsche, Jean-Pierre Flandrois,
                                Marie-Laure Delignette-Muller (2015). A Toolbox for Nonlinear Regression in R: The Package
                                nlstools. Journal of Statistical Software, 66(5), 1-21.")

                             )
                           )
                        )
                      )
              )
           )
        )   
    
   
  