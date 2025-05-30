#Packages to be installed and loaded############################
library(shiny)
library(shinyjs)
# library(profvis) #for profiling
library(DT)
library(fst)
library(data.table)
# library(periscope)#trying this to make downloadable figures

#.csv's to read in#############################################
lakeinfo <- read.fst("lakeinfo.fst", as.data.table = TRUE)
stockingData <- read.fst("stockingData.fst", as.data.table = TRUE)

#start main ui.r code#########
fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "icon", type = "image/png", href = "ODWClogo.gif"),
            tags$title("OK Fishery Analysis App")),
  
  tags$head(tags$style(".shiny-progress {color:blue; font-size:200%; font-style: italic;}")),
  tags$head(tags$style(".shiny-notification {position:fixed; top:40% ;left:30%; font-size:200%; 
                       width:40%; opacity:0.95; z-index:9999;}")),

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Application title with ODWC logos#####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  titlePanel(
    wellPanel(
        fluidRow(
          
   # profvis_ui("profiler"), adds button to start profiling for debugging and speed measurement
        column(3,align="center", img(src="ODWClogo.gif", height="auto", width="100px")),
        column(6, align="center", tags$b(h2(HTML(paste0(
          "OK Fishery Analysis App<sup style='font-size: 0.3em; vertical-align: super;'>© ",
          paste0("2018-",format(Sys.Date(), "%Y")),"</sup>")))),
               hr(), 
               h5("Created by Daniel E. Shoup and Dray D. Carl")),
              #below line vertically centers OSU logo...sets height to 110 px
               tags$style(HTML('
                      .verticalcenter {
                      display: table-cell;                      
                      height: 110px;
                      vertical-align: middle;
                      }')),
        column(3, align="center", img(src="osulogo.png", height="auto", width="auto",class="verticalcenter"))
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
          
            # profvis_ui("profiler"),
    
            #Code from left select section of main data selection tab...right side select boxes below in diff section
            column(6,
              sidebarPanel(width = 12,
                h4("Select criteria for data to analyze", align="center"),
                checkboxInput("toggleCodeName", "Check box to filter by codes, uncheck to filter by name-code", value = FALSE),
                hr(),
             
                uiOutput("selectLakeBox"),
              
                uiOutput("selectYearBox"),
              
                uiOutput("selectMonthBox"),
                
                uiOutput("selectGearBox"),
                
                actionButton("clearBoxes","Clear all criteria"),
              )
            ),
           
          #Data selection summary - renderText to show full names of lakes, gears, species
           column(6, align = "center",
            mainPanel(width = 12, 
              wellPanel(        
                h4("Sample Selection Summary"),
                hr(),
                  # h4(textOutput("clearBoxesButton")),
                  h4(textOutput("lakename")),
                  h4(textOutput("year")),
                  h4(textOutput("month")),
                  h4(textOutput("gearname")),
                
                  htmlOutput("dataBeingUsed"), #replaces a textOutput so I can style color based on value
                #ui side for loading initial .csv's
                div(id = "loading-content",
                    h1("Uploading Data..."),
                    style="color:red"),
                hr(),
                h4(helpText("Minimum of Lake, Year, and Gear inputs must be selected. Multiple selections are allowed
                           within each field.")),
                h4(helpText("Filtered dataset will appear below.")),
                hr(),
                textOutput("currentDataID"),
                actionButton("change","Change the data being used"),
                hr(),
                downloadButton("downloadData", "Download Selected Sample Data")
              ), 
             
            )
          ),
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
                  #Selected sample column & box to upload age data
                  column(4, 
                    wellPanel(
                         h4("Alternatively, upload your own age data"),
                         helpText("Must have correct .csv format and column headings"),
                         fileInput("loadedageData", "Upload Age Data"),
                         checkboxInput("loadageCheck", "Check to use uploaded age data", value = FALSE),
                         # hr()
                    ),
                    wellPanel(
                      h4("Selected Sample Data", align="center"),
                      hr(),
                        h4(textOutput("lakename3")),
                        h4(textOutput("year3")),
                        h4(textOutput("monthRange")),
                        # h4(textOutput("month3")),
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
                  
                  #Single-Species Analyses Selection column
                  column(4,
                    wellPanel(
                      h4("Single-Species Analyses Selection", align="center"),
                      hr(),
                          
                          checkboxInput("toggleSppCodeName", 
                                "Check box to filter by code-name, uncheck to filter by name", value = FALSE),
                          selectizeInput("selectspecies", "Select Species from Sampling Data to Analyze:", choices = NULL,
                              multiple = T, options = list(placeholder = "click/type here")),
                          helpText("Species above must be selected for Single-Species Analysis and will dictate the
                                   species used in the sampling data"),
                        downloadButton("downloadsppData", "Download Selected Species Sampling Data"),
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

                      
                          selectizeInput("selagespp", "Select Species from age data below to pair with the  
                                         species selected above from the sampling data for analysis:", choices = NULL,
                               multiple = TRUE, options = list(placeholder = "click/type here")),
                          selectizeInput("selagelake", "Select Lake Code(s) to pick age data for analysis:", choices = NULL,
                               multiple = TRUE, options = list(placeholder = "click/type here")),
                          selectizeInput("selageyears", "Select Year(s) to pick age data for analysis:", choices = NULL,
                              multiple = TRUE, options = list(placeholder = "click/type here")),
                          helpText("All three selection boxes are required to select age dataset and are
                                   selected hierarchiaclly (species, then lake, then year, and finaly month)."),
                          helpText("Multiple selections are allowed in Lake, Year, and Month fields. However,
                                   month data should not span a long enough interval that fish growth would 
                                   bias the length-age relationship. By default the app looks for age data
                                   within the same 3 month windows as the sampling data (i.e., it is from
                                   the same season or quarter of the year)"),
                          helpText("Age dataset will appear below once all three boxes are populated."),
                        downloadButton("downagedata", "Download Selected Age Data")
                    )
                   ),
                  
                  
                  
                  #Metrics checkbox column
                   column(4,
                    wellPanel(
                    h4("Catch Analyses Metrics (all spp)", align="center"),
                    hr(),
                      checkboxInput("abiotic", "Abiotic Metrics", value = TRUE), 
                      checkboxInput("samplesize", "Sample Size", value = TRUE),
                      checkboxInput("totaleffort", "Total Effort", value = TRUE),
                      checkboxInput("cpue", "CPUE", value = TRUE),
                      checkboxInput("cpuesize", "CPUE by Size Category", value=TRUE)
                    ),
                  wellPanel(
                      h4("Single-Species Analyses Metrics", align="center"),
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
                          checkboxInput("vonbert", "Von Bertalanffy Equation", value = TRUE),
                          checkboxInput("meanlength", "Mean Length-at-Age", value = TRUE),
                          checkboxInput("meanweight", "Mean Weight-at-Age", value = TRUE),
                          checkboxInput("mort", "Mortality, theoretical max age, and recruitement metrics", value = TRUE)
                      
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
                      # h4(textOutput("month2")),
                      h4(textOutput("monthRange")),
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
                      hidden(
                        div(id="CPUEbyPSD_length",
                          hr(),
                          h4("CPUE by Size Category"),
                          
                          actionButton("CPUEbySize_PSD_inch", "Change table to CPUE by inch or mm class"),
                            hidden(
                              div(id="CPUEbyLength",
                                fluidRow(
                                  hr(),
                                  column(2,numericInput("CPUElengthbin", "Bin width (inch)",
                                            value = 2,min = 0.1, max = 20, width = '200px')), 
                                  column(3,numericInput("CPUEMinBin", "Starting Bin", value = NULL, #set value server side
                                                        min = 0, max = 1270, width = '350px')),
                                  column(5,checkboxInput("InchCPUE", "Uncheck to give length bin in mm", 
                                                         value = TRUE)) 
                                  # uiOutput("CPUEMinBin"),
                                )
                              )
                            ),
                          uiOutput("CPUEbySizeTable"),
                        )
                      ),
                      hr(),
                      h4("Catch Analysis Tab Downloads"),
                      div(id="noCPUEbyLength_downld",
                        downloadButton("downcpue1", "Total CPUE Table"),
                      ),
                      div(id="CPUEbyLength_downld",
                        downloadButton("downcpue2", "Total CPUE Table"),
                        downloadButton("downcpuebysize", "CPUE by Size Table")
                      )
                    )       
                  )
                )
              )
      
     ),            
     
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Single-Species Analysis Output##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     tabPanel("Single-Species Analyses", value="analyze",
       hr(),       
        mainPanel(width=12, 
          fluidRow(     
            
            #Length & weight and size structure column on left
            column(6,  
              wellPanel(
                h3(textOutput("sampleline")),
                    span(textOutput("uploadedAgeData"), style="color:orange"),
                    span(textOutput("ageDataMatch"), style="color:green"),
                    span(textOutput("ageDataNoMatch"), style="color:red"),
                    textOutput("ageDataCount"),
                    textOutput("sampleDataSppCount"),
                    checkboxInput("addSPPfig", "Check to add species name to figures", value = TRUE)
              ),
              wellPanel(
                h3("Length and Weight Metrics"),
                  hr(),
                    h4("Length-Frequency Histogram"),
                    plotOutput("lengthhist"),
                checkboxInput("inchLF", "Display length-frequency in inches (checked) or mm (unchecked)", 
                                 value = FALSE),
                helpText("Default is metric (mm). Checkbox changes units to inches"),
                    uiOutput("lengthbin_ui"),
                    helpText("Enter desired length bin grouping: default is 10 mm (0.4 inch)."),
                  br(),
                  downloadButton("downlfplot","Length-Frequency Plot"),
                  downloadButton("downlftable", "Length-Frequency Tabular Data"),
                  hr(),
                    h4("Proportional Size Distribution (PSD)"),
                  hr(),
                    uiOutput("PSDoutputDetails"),
                # br(),
                  hr(),
                    h4("Relative Weight (Wr)"),
                    uiOutput("wrTableAndButton"),
                    uiOutput("WsEqDetail"),#produces text for "Details of standard weight equation" (or error message if spp does not have Ws equation)
                  br(),
                  hr(),
                    h4("Length-Weight Regression"),
                    plotOutput("lwplot"),
                    downloadButton("downLWregplot","Length-Weight Regression Plot"),
                  br(),
                  br(),
                    tableOutput("lwcoef"), #length-weight coefficients and regression plot
                  hr(),
                div(id="MaxLnWt",
                      h4("Max Length and Weight"),
                    hr(),
                      tableOutput("maxspptab")
                )
              )
            ),
            
            #Age and growth column on right
            column(6,
              wellPanel(
                h3("Population Dynamics"),
                  hr(),
                  div(id="alk",
                    h4("Age-Length Key"),
                        fluidRow(
                          column(3, actionButton("smoothALKToggle", "Change figure to smoothed ALK"), 
                                 style = "margin-right: 60px;"),
                          column(3, actionButton("alkPlotToggle", "Change figure to bubble plot")),
                        ),
                        textOutput("Sm_obs_extrapMessage"),
                        h4(checkboxInput("extrapAge", "Extrapolate smoothed age-length key to fish smaller than smallest individual aged.",
                                         value = F)),#server-side updateCheckboxInput will override this once age data are loaded
                        helpText("Extrapoation should only be done in cases where you aged enough age-0 fish (i.e., you found the size below which fish are >50% age-0)."),
                        plotOutput("alkplot"),
                        div(id="ageTextChkbx",
                          checkboxInput("ageTextFig", "Uncheck to remove age values from plot bars", 
                                  value = T)
                        ),
                        uiOutput("ageFigTextBox"),
                        downloadButton("downALKplot","Age-Length Key Plot"),
                        downloadButton("obsALK","Observed Age-Length Key (table)"),
                        downloadButton("smoothALK","Smooth Age-Length Key (table)"),
                        downloadButton("agedfishtable", "Table with ages assigned to fish"),
                      hr()
                  ),
                  div(id="ageFreqInfo",
                        h4("Age Frequency Histogram"),
                        plotOutput("agefreqhist"),
                        downloadButton("downafplot","Age-Frequency Plot"),
                      br(),
                      br(),
                      br()
                  ),
                    h3("Growth Metrics"),
                    h4(checkboxInput("inch", "Display Growth Metrics in English Units (in & lbs)", value = FALSE)),
                    helpText("Default is metric (mm and g). Checkbox changes units to in. and lbs."),
                  br(),
                  div(id="vonbertInfo",
                      h4("von Bertalanffy Figure"),
                        plotOutput("meanlengthplot"),
                        fluidRow(
                        downloadButton("downmeanplot","von Bertalanffy plot"),
                        actionButton("CI_PIinfo", "Click for info about CI and prediction interval")
                        ),
                      br(),
                      br(),
                      br(),
                      h4("von Bertalanffy Growth Equation"),
                        tableOutput("vonBcoef"),
                        uiOutput("vonBerror"), #handles nls fitting errors in VB equation
                        downloadButton("downvonBcoef", "von Bert Equation Table"),
                      br(),
                      br(),
                      br()
                  ),
                  div(id="meanLengthAge",
                      h4("Mean Length-at-Age"),
                        tableOutput("meanlengthtable"),
                        downloadButton("downML", "Mean Length-at-Age Table"),
                      br(),
                      br(),
                      br()
                  ),
                  div(id="meanWeightAge",
                      h4("Mean Weight-at-Age"),
                        tableOutput("meanweighttable"),
                        downloadButton("downMW", "Mean Weight-at-Age Table"),
                      br(),
                      br(),
                      br()
                  ),
                  div(id="mortMetrics",  
                        h4("Catch Curve (Mortality)"),
                        plotOutput("catchcurve"),
                      helpText("Shaded band in figure above is 95% confidence interval for the slope. True slope (mortality) could be 
                               any angle that fits within this shaded range."),
                        downloadButton("downmort", "Catch Curve Plot"),
                      br(),
                      br(),
                      hr(),
                        tableOutput("mortalitytable"),
                        downloadButton("downmorttable", "Mortality Table"),
                      br(),
                      br(),
                      hr(),
                    tableOutput("TheorMaxAge"),                  
                    h5(textOutput("ObsMaxAge")),
                    
                      hr(),
                        h4("Estimates of natural mortality (and resulting fishing mortality)"),
                        helpText("Note: If fishing mortality is negative, it is 
                                 a sign that 1) estimate of natural or total mortality was not very accurate or 2) exploitation (fishing mortality) was 
                                 small and accounts for most of total mortality"),
                        actionButton("NatMort_MoreInfo", "More Info"),
                        tableOutput("natMortalityTable"),
                        helpText(""),
                    downloadButton("downNatmorttable", "Est Nat Mortality Table"),
                    hr(),
                    h4("Year class strength (Maceina 1997 Fish Res 32:115-121)"),
                    actionButton("YCstrength_MoreInfo", "More Info about yr class strength"),
                    uiOutput("YCstrengthPlt"),
                    downloadButton("downYCstrengthPlt", "Year class strength plot")
                  )
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
                        div(#style="display:inline-block",
                            selectizeInput("selPercGear", "Select Gear:", choices = NULL,
                                 multiple = TRUE, options = list(placeholder = "click/type here"))
                          ),
                   
                      #select Species code, choices processed on server.r
                        div( 
                            selectizeInput("selPercSpp", "Select Species:", choices = NULL,
                                  multiple = TRUE, options = list(placeholder = "click/type here"))
                          ),
                
                      #Year slider
                       sliderInput("perYrs", "Year Range", min = 1980,
                           max = as.numeric(format(Sys.Date(), "%Y")), sep = "", step=1,
                           value = c(2010, as.numeric(format(Sys.Date(), "%Y")))),
      
                      #Region selection box
                        selectizeInput("selRegionPerc", "Management Region(s):", choices = unique(lakeinfo$ODWC.Region),
                            multiple = TRUE, options=list(placeholder = "click/type here")),
      
                      #Lake code, choices processed on server.r
                         div(#style="display:inline-block",
                             selectizeInput("selLakeCodePerc", "Select Lake(s)", choices = NULL, multiple = TRUE,
                                options = list(placeholder = "click/type here"))
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
                           ),
                            #note: selPerctls() has list of percentiles to use
                     
                      #Slider for min # stock size fish before calculate PSD
                          div(id = "reverseSlider",
                              sliderInput(inputId = "N_PSDMin", label = 
                                "Min # of stock-size fish required before including a survey's PSD value percentile calculation
                                (only effects PSD percentile table)", 
                                min = 10, max = 100, sep = "", step=1, value = 40)
                                #95% CI's are +/- 25 PSD units with N=25 stock fish...so default to no smaller than 25 on this.
                                #with N=40, CI is +/- 19...probably more realistic for default table with 5 percentiles.
                                #See Gustafson 1998 NAJFM 8:139-141
                           ),
                     
                      #Slider for min # Wr values for Wr percentiles
                          div(id = "reverseSlider",
                              sliderInput(inputId = "Min_Wr_N", label = 
                                       "Min # of Wr values required before including survey's average Wr in percentile calculation 
                                       (only effects Wr percentile table)", 
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
                          fluidRow(
                            column(5,
                              h4(uiOutput("MaxTlWtUnits"))),#render check box for changing units
                            column(1,
                              uiOutput("Max_TL_Wt_percRendered")), #Download button
                            ),
                          uiOutput("MaxTlWtHelpText") #render message about changing units
                    ),
                    wellPanel(
                      span(textOutput("CPUEpercText"),style="font-size:175%"),
                      tableOutput("CPUEperc"),
                      uiOutput("CPUEpercRendered") #render download button
                    ),
                    wellPanel(
                      span(textOutput("PSDpercText"),style="font-size:175%"),
                      tableOutput("PSDperc"),
                      uiOutput("PSDpercRendered")#render download button
                    ),
                    wellPanel(
                      span(textOutput("WrpercText"),style="font-size:175%"),
                      tableOutput("Wrperc"),
                      uiOutput("WrpercRendered")#render download button
                    ),
                    wellPanel(
                      span(textOutput("MLApercText"),style="font-size:175%"),
                      tableOutput("MLAperc"),
                      fluidRow(
                        column(5,
                                uiOutput("MLA_Units")),#render check box for changing units
                        column(1,
                               uiOutput("MLApercRendered")),#render download button
                      ),
                      uiOutput("MLAhelpText") #render message about changing units
                    ),
                    wellPanel(
                      span(textOutput("MWtApercText"),style="font-size:175%"),
                      tableOutput("MWtAperc"),
                      fluidRow(
                        column(5,
                               uiOutput("MWtA_Units")),#render check box for changing units
                        column(1,
                               uiOutput("MWtApercRendered")),#render download button
                      ),
                      uiOutput("MWtAhelpText") #render message about changing units
                    ),
                  
                    wellPanel(
                      span(textOutput("MortpercText"),style="font-size:175%"),
                      tableOutput("Mortperc"),
                      uiOutput("MortrpercRendered")#render download button
                    )
                ),
              )
           )
         ),
              #use below line to display selected percentile data table at bottom of page
              # DT::dataTableOutput("selectedPercTable")
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
                   sliderInput("stockrange", "Year Range", min = min(stockingData$Year, na.rm = FALSE), step = 1,
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
                    # tableOutput("stocktable")
                   DT::dataTableOutput("stocktable")
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
               #tags$iframe(style="height:700px; width:100%",#1300
               tags$iframe(style="height:1300px; width:100%",
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
                   h4("Garrett Johnson"),
                   h4("Ashley Nealis"),
                   h4("Michael Holley"),
                   h4("Jeremy Duck")
                )
              ),
              
              #Packages pannel on right
              column(6,
                 wellPanel(
                  h3("R Packages Used:"),
                  hr(),
                   h4("FSA"),
                   h5("Ogle, D.H., J.C. Doll, P. Wheeler, and A. Dinno. (2022). FSA: Fisheries Stock Analysis. 
                      R package version 0.9.3"),
                   helpText("When using output derived from this app, please cite the above
                            FSA Package. Almost every metric used a function from this package."),
                   h4("shiny"),
                   h5("Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, 
                      Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for 
                      R. R package version 1.7.1."),
                   h4("shinyjs"),
                   h5("Dean Attali (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. 
                      R package version 2.1.0."),
                   h4("dplyr"),
                   h5("Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2022). dplyr: A Grammar of Data Manipulation. 
                      R version 1.0.9."),
                   h4("tidyr"),
                   h5("Hadley Wickham and Maximilian Girlich (2022). tidyr: Tidy Messy Data. R package version 1.2.0."),
                   h4("nnet"),
                   h5("Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition.
                    Springer, New York. ISBN 0-387-95457-0"),
                   h4("nlstools"),
                   h5("Florent Baty, Christian Ritz, Sandrine Charles, Martin Brutsche, Jean-Pierre Flandrois,
                    Marie-Laure Delignette-Muller (2015). A Toolbox for Nonlinear Regression in R: The Package
                    nlstools. Journal of Statistical Software, 66(5), 1-21."),
                  h4("DT"),
                  h5("Yihui Xie, Joe Cheng and Xianying Tan (2022). DT: A Wrapper of the JavaScript Library 'DataTables'. 
                     R package version 0.23."),
                  h4("data.table"),
                  h5("Matt Dowle and Arun Srinivasan (2021). data.table: Extension of `data.frame`. R package version 1.14.2."),
                  h4("tibble"),
                  h5("Kirill Müller and Hadley Wickham (2021). tibble: Simple Data Frames. R package version 3.1.7."),
                  h4("rlang"),
                  h5("Lionel Henry and Hadley Wickham (2022). rlang: Functions for Base Types and Core R and 'Tidyverse' 
                     Features. R package version 1.0.2."),
                  h4("fst"),
                  h5("Mark Klik (2022). fst: Lightning Fast Serialization of Data Frames. R package version 0.9.8."),
                 )
               )
            )
          )
      )
   )
)   
