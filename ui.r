#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
library(DT)
library(V8)#package needed to refresh page...only used if we upload csv files
library(waiter) #produces spinning icon loading screen for percentiles tab (may want to do on main tab too?)
library(bslib) #Theming package for Shiny

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" #Javascript needed to refresh page...will only need on file upload screen
thematic::thematic_shiny(font = "auto")

#.csv's to read in#############################################
gearinfo <- read.csv("gearinfo.csv")
lakeinfo <- read.csv("lakeinfo.csv")
yearmonthinfo <- read.csv("yearmonthinfo.csv")
speciesinfo <- read.csv("speciesinfo.csv")
stockingData <- read.csv("stockingdata.csv")

#start main ui.r code#########
fluidPage(
  #theme = bs_theme(),  #use bslib theme package...add customizations in parentheses
  use_waiter(),  #establish waiter function for loading screen
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("OK Fishery Analysis App")),
    useShinyjs(),
  
  tags$head(tags$style(".shiny-progress {top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px !important; color: blue;font-size: 20px;font-style: italic;}")),
  #tags$head(tags$style(".shiny-progress {top: 50%;left: 50%;margin-top: -100px !important;margin-left: -250px; color: blue;font-size: 20px;font-style: italic;}")),
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Application title with ODWC logos#####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Select Sample Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Select Sample",
      hr(),  
        fluidRow(
          #Code from left select section of main data selection tab
          column(4,
            sidebarPanel(width = 12,
              h4("Filter Dataset by Codes", align="center"),
              hr(),
              
            #choices for below selectize boxes are processed in server.r
              # selectizeInput(
              #   "selectlake", "Lake Code(s):", choices = lakeinfo$Lake.Code,   
              #     multiple = TRUE, options = list(placeholder = "click/type here")
              # ),
              selectizeInput(
                "selectlake", "Lake Code(s):", choices = NULL,   
                  multiple = TRUE, options = list(placeholder = "click/type here")
              ),
              # selectizeInput(
              #   "selectyear", "Year(s):", choices = yearmonthinfo$Year,
              #   multiple = TRUE, options = list(placeholder = "click/type here")
              # ),
              selectizeInput(
                "selectyear", "Year(s):", choices = NULL,
                multiple = TRUE, options = list(placeholder = "click/type here")
              ),
              # selectizeInput(
              #   "selectmonth", "Month(s):", choices = yearmonthinfo$Month,
              #   multiple = TRUE, options = list(placeholder = "click/type here")
              # ),
              selectizeInput(
                "selectmonth", "Month(s):", choices = NULL,
                multiple = TRUE, options = list(placeholder = "click/type here")
              ),
              # selectizeInput(
              #   "selectgear", "Gear Code(s):", choices = gearinfo$Gear.Code,
              #     multiple = TRUE, options = list(placeholder = "click/type here")
              # ),
               selectizeInput(
                "selectgear", "Gear Code(s):", choices = NULL,
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
            #choices for below selectize boxes are processed in server.r
            # selectizeInput(
            #   "selectlakename", "Lake Name(s):", choices = sort.default(lakeinfo$Lake.Name),   
            #   multiple = TRUE, options = list(placeholder = "click/type here")
            # ),
            selectizeInput(
              "selectlakename", "Lake Name(s):", choices = NULL,   
              multiple = TRUE, options = list(placeholder = "click/type here")
            ),
            # selectizeInput(
            #   "selectyearname", "Year(s):", choices = yearmonthinfo$Year,
            #   multiple = TRUE, options = list(placeholder = "click/type here")
            # ),
            selectizeInput(
              "selectyearname", "Year(s):", choices = NULL,
              multiple = TRUE, options = list(placeholder = "click/type here")
            ),
            # selectizeInput(
            #   "selectmonthname", "Month(s):", choices = yearmonthinfo$Month,
            #   multiple = TRUE, options = list(placeholder = "click/type here")
            # ),
            selectizeInput(
              "selectmonthname", "Month(s):", choices = NULL,
              multiple = TRUE, options = list(placeholder = "click/type here")
            ),
            # selectizeInput(
            #   "selectgearname", "Gear Name(s):", choices = sort.default(gearinfo$Gear.Name),
            #   multiple = TRUE, options = list(placeholder = "click/type here")
            # )
            selectizeInput(
              "selectgearname", "Gear Name(s):", choices = NULL,
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Select Analyses Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Select Analyses",
              hr(),  
              sidebarPanel(width=12,
                fluidRow(    
                  
                  #catch metrics & selected sample column
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
                  
                  #Single Species Analyses Selection column
                  column(4,
                    wellPanel(
                      h4("Single Species Analyses Selection", align="center"),
                      hr(),
                          selectizeInput("selectspecies", "Select Species Code:", choices = NULL,
                              multiple = TRUE, options = list(placeholder = "click/type here")),
                          selectizeInput("selectspeciesname", "Select Species Name:", choices = NULL,
                              multiple = TRUE, options = list(placeholder = "click/type here")),
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
                          selectizeInput("selagespp", "Select Species Code:", choices = NULL,
                               multiple = TRUE, options = list(placeholder = "click/type here")),
                          # uiOutput("selagelake"),
                          selectizeInput("selagelake", "Select Lake Code(s):", choices = NULL,
                               multiple = TRUE, options = list(placeholder = "click/type here")),
                          # uiOutput("selageyears"),
                          selectizeInput("selageyears", "Select Year(s):", choices = NULL,
                              multiple = TRUE, options = list(placeholder = "click/type here")),
                          helpText("All three selection boxes are required to select age dataset."),
                          helpText("Multiple selections are allowed in Lake and Year fields."),
                          helpText("Age dataset will appear below."),
                        downloadButton("downagedata", "Download Selected Age Data")
                    )
                   ),
                  
                  #Single Species Analyses Metrics checkbox column
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Catch Analysis Output Tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%             
      tabPanel("Catch Analyses",
            hr(),
              mainPanel(width=12,
                fluidRow(
                  
                  #Sample summary column
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
                  
                  #main data column on right
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Single Species Analysis Output##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Single Species Analyses", value="analyze",
       hr(),       
        mainPanel(width=12, 
          fluidRow(     
            
            #Length & weight and size structure colom on left
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
            
            #Age and growth column on right
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Percentiles tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Statewide Percentiles", 
         hr(),
         mainPanel(width=12,
           fluidRow(
             #parameter selection column on left
             column(4,
                #sidebarPanel(width = 12,
                  wellPanel(
                     h4("Select survey criteria for calculating percentiles", align="center"),
                     hr(),
                     
                     ###selectize boxes for parameterizing percentile calculations
                     
                      #gear code, choices on server.r
                        div(style="display:inline-block",
                            selectizeInput("selPercGear", "Gear Code(s):", choices = NULL,
                                 multiple = TRUE, options = list(placeholder = "click/type here"))
                          ),
                   
                      #gear name, choices on server.r
                        div(style="display:inline-block",
                            selectizeInput("selPercGearNm", "Gear Name(s):", choices = NULL, 
                                 multiple = TRUE, options = list(placeholder = "click/type here"))
                          ),
      
                      uiOutput("lineBrk"), #create hard return
      
                      #select Species code, choices processed on server.r
                        div(style="display:inline-block",#uiOutput("selSppPerc")), 
                            selectizeInput("selPercSpp", "Species Code(s):", choices = NULL,
                                  multiple = TRUE, options = list(placeholder = "click/type here"))
                          ),
                
                      #select species name, choices processed on server.r
                        div(style="display:inline-block",selectizeInput("selPercSppNm", "Species Name(s):", width = "100%",
                            choices = NULL,multiple = TRUE, options = list(placeholder ="click/type here"))
                          ),  
                  
                      #Year slider
                       sliderInput("perYrs", "Year Range", min = 1980,
                           max = as.numeric(format(Sys.Date(), "%Y")), sep = "", step=1,
                           value = c(2010, as.numeric(format(Sys.Date(), "%Y")))),
      
                      #Region selection box
                        selectizeInput("selRegionPerc", "Management Region(s):", choices = unique(lakeinfo$ODWC.Region),
                            multiple = TRUE, options=list(placeholder = "click/type here")),
      
                      #Lake code, choices processed on server.r
                         div(style="display:inline-block",
                             selectizeInput("selLakeCodePerc", "Lake Code(s):", choices = NULL, multiple = TRUE, 
                                options = list(placeholder = "click/type here"))
                            ),
                     
                       #Lake name, choices processed on server.r
                         div(style="display:inline-block",
                             selectizeInput("selLakeNamePerc", "Lake Name(s):", choices = NULL,multiple =
                                  TRUE, options = list(placeholder = "click/type here"))
                            ),
                     helpText("All codes can be referenced in the SSP Manual tab."),
                  ),
                  
                  #Percentile customization
                  wellPanel(
                   #Custom percentiles to calculate
                       uiOutput("percentileInptBox"), #create input box for specifying desired percentiles...code generated on server.r side
                   ),
                  
                  #criteria for deciding data quality required in percentile calculations
                  wellPanel(
                     h4("Set minimum data quality parameters determining how much data is required to include surveys in percentile calculations", align="center"),
                     hr(),
                     
                      #Slider for min # surveys on which to calculate percentiles
                          #Set up css info for a reverse highlighting of slider (blue right of the marker)
                           tags$style(HTML("
                                  #reverseSlider .irs-bar {border-top: 1px solid #ddd;
                                    border-bottom: 1px solid #ddd;background: linear-gradient(to bottom, #DDD -50%, 
                                    #FFF 150%);
                                  }
                                  #reverseSlider .irs-bar-edge {
                                    border: 1px solid #ddd;
                                    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                    border-right: 0;
                                  }
                                  #reverseSlider .irs-line {
                                    background: #428bca;
                                    border: 1px solid #428bca;
                                  }
                                ")),
                          div(id = "reverseSlider",
                              uiOutput("min_survey")
                              # sliderInput(inputId = "N_SurveyMin", label = 
                              #   "Min # of surveys for which to calculate percentiles", 
                              #   min = 3, max = 100, sep = "", step=1, value = 10)
                           ),
                            #note: selPerctls() has list of percentiles to use
                     
                      #Slider for min # stock size fish before calculate PSD
                          div(id = "reverseSlider",
                              sliderInput(inputId = "N_PSDMin", label = 
                                "Min # of stock-size fish required before including a survey's PSD value percentile calculation", 
                                min = 10, max = 100, sep = "", step=1, value = 40)
                                #95% CI's are +/- 25 PSD units with N=25 stock fish...so default to no smaller than 25 on this.
                                #with N=40, CI is +/- 19...probably more realistic for default table with 5 percentiles.
                                #See Gustafson 1998 NAJFM 8:139-141
                           ),
                     
                      #Slider for min # Wr values for Wr percentiles
                          div(id = "reverseSlider",
                              sliderInput(inputId = "Min_Wr_N", label = 
                                       "Min # of Wr values required before including survey's average Wr in percentile calculation", 
                                     min = 3, max = 100, sep = "", step=1, value = 5)
                          ),
                     
                      #Slider for min # aged fish...calculated on server side to have max value
                          div(id = "reverseSlider",
                              uiOutput("NAgedMinSlider")
                           ),
                                     
                     #Slider for min # fish in age class for mean length/wt at age percentiles
                          div(id = "reverseSlider",
                              sliderInput(inputId = "Min_N_at_Age", label = 
                                    "Min # fish required in an age class before using it for mean length/weight at age percentile calculation", 
                                     min = 3, max = 100, sep = "", step=1, value = 5)
                          ),
                     
                     #Slider for min R^2 for using mortality data
                          div(id = "reverseSlider",
                              sliderInput(inputId = "Min_Mort_R2", label = div(HTML(
                                "Min R<sup>2</sup> from catch curve before using survey for mortality percentile calculation")), 
                                 min = 0.65, max = 1.0, sep = "", step=0.01, value = 0.80)
                          ),
                     
                     helpText("Note: a survey is a combination of data from all sites sampled with the same gear, lake, and year.")
                     
                  ),
                   
                #)
              ),
             
            # Column with percentile tables in it
             column(8,
                conditionalPanel(
                  condition = "input.selPercGear != '' || input.selPercSpp != '' ", #hide output until gear or spp selected
                    wellPanel(
                        span(textOutput("Max_TL_Wt_Text"),style="font-size:175%"),
                        tableOutput("Max_TL_Wt_perc"),
                        h4(checkboxInput("Max_TL_WT_inch_lb", "Display Measurements in English Units (in & lbs)", value = FALSE)),
                        helpText("Default is metric (mm and g). Checkbox changes units to inches and lbs.")
                        
                    ),
                    wellPanel(
                      span(textOutput("CPUEpercText"),style="font-size:175%"),
                      tableOutput("CPUEperc")
                    ),
                    wellPanel(
                      span(textOutput("PSDpercText"),style="font-size:175%"),
                      tableOutput("PSDperc")
                    ),
                    wellPanel(
                      span(textOutput("WrpercText"),style="font-size:175%"),
                      tableOutput("Wrperc")
                    ),
                    wellPanel(
                      span(textOutput("MLApercText"),style="font-size:175%"),
                      tableOutput("MLAperc"),
                      h4(checkboxInput("MLA_inch", "Display Measurements in English Units (inches)", value = FALSE)),
                      helpText("Default is metric (mm). Checkbox changes units to inches.")
                      
                    ),
                    wellPanel(
                      span(textOutput("MWtApercText"),style="font-size:175%"),
                      tableOutput("MWtAperc"),
                      h4(checkboxInput("MWtA_lb", "Display Measurements in English Units (pounds)", value = FALSE)),
                      helpText("Default is metric (g). Checkbox changes units to pounds.")
                    ),
                  
                    wellPanel(
                      span(textOutput("MortpercText"),style="font-size:175%"),
                      tableOutput("Mortperc")
                    )
                ),
                      
                      #stuff to delete after done debugging
                      
                      # h3("testing raw data"),
                      # hr(),
                      # DT::dataTableOutput("selPercDataTbl"),
                      #   #tableOutput("selPercDataTbl")
                      
                      # h3("testing age data"),
                      # DT::dataTableOutput("selPercAgedTbl")
                      # 
                      # h3("testing sortAge table"),
                      # DT::dataTableOutput("test"),
                      
              )
           )
         ),
         
      ),
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Stocking info tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Stocking Information",
           hr(),
           mainPanel(width=12,
             fluidRow(
               
               #selection criteria panel on left
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
               
               #Data pannel on right
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##User's guide tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##SSP Manual tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
     
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     ##Acknowledgements tab##########
     #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      tabPanel("Acknowledgements",
          hr(),
          mainPanel(width=12,
            fluidRow(
              
              #funding pannel on left
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
              
              #Packages pannel on right
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
    ############
   
  