# -------------------------------------------------------------------------
# SETUP -------------------------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(pbapply)
library(data.table)
library(igraph)
library(ggiraph)
library(shinyWidgets)
library(ggraph)
library(shinyMatrix)
library(DT)
library(colourpicker)


# -------------------------------------------------------------------------
# DATA --------------------------------------------------------------------
# -------------------------------------------------------------------------
source("functions_fromAHgen.R")

WRCcols <- c("#CCB1B9", "#607EBC", "#77B7A6", "#64324D", "#413D4C")
extraCols <- c("#0D0A0B", "#FABE46")
WRCcols <- c(WRCcols, extraCols)
myGreys <- readRDS("myGreys.RDS")

# dl <- readRDS("genericAH.RDS")

definitions <- read.csv("USAH_Definitions_20210504.csv") %>% mutate(level = as.numeric(level), levelName = fct_inorder(levelName)) %>% select(-reference)

dl <- 
  list(dv = readRDS("templateGeneric.RDS")$vInfo %>% select(1:3) %>% mutate(levelName = fct_inorder(levelName)) %>% merge(definitions, by = c("level", "levelName", "vName"), all = TRUE) %>% drop_na(definition),
       de = readRDS("templateGeneric.RDS")$edgelist)

tmpKey <-
  dl$dv %>%
  select(level, levelName) %>%
  unique %>%
  mutate(tmp = c("l1FP", "l2VPM", "l3GF", "l4ORP", "l5PO"),
         label = paste0(level, " - ", levelName)) %>%
  select(-level, -levelName)








# -------------------------------------------------------------------------
# DATA - INPUTS -----------------------------------------------------------
# -------------------------------------------------------------------------
mat <- matrix(c(55,60,65,70,75,
                0.1,0.2,0.3,0.5,1,
                6,5,4,3.5,2),
              ncol = 3,
              dimnames = list(rows = paste0("Level ", unique(dl$dv$level)),
                              cols = c("Angle", "Increase radius", "Node size")))

pickerChoices <- list(
  
  "No selection" = "No selection",
  
  "Level 1 - Functional purposes" = dl$dv %>% filter(level == 1) %>% pull(vName),
  "Level 2 - Values and priority measures" = dl$dv %>% filter(level == 2) %>% pull(vName),
  "Level 3 - Generalised functions" = dl$dv %>% filter(level == 3) %>% pull(vName),
  "Level 4 - Object-related processes" = dl$dv %>% filter(level == 4) %>% pull(vName),
  "Level 5 - Physical objects" = dl$dv %>% filter(level == 5) %>% pull(vName)
  
)


# -------------------------------------------------------------------------
# UI SETUP ----------------------------------------------------------------
# -------------------------------------------------------------------------
ui <- fluidPage(
  
  br(),
  
  
  # BUTTON - NODE SETTINGS --------------------------------------------------
  sidebarLayout(
    
    
    # -------------------------------------------------------------------------
    # SIDEBAR -----------------------------------------------------------------
    # -------------------------------------------------------------------------
    sidebarPanel(width = 2,   
                 
                 tags$h2("AHexploreR V2.1"),
                 
                 fluidRow(
                   column(width = 2, dropdownButton(
                     
                     # SETTINGS
                     circle = TRUE, 
                     status = "primary", # Button colour
                     icon = icon("sitemap"), 
                     width = "500px",
                     tooltip = tooltipOptions(title = "Click to change network settings"),
                     
                     
                     
                     tags$h3("Network settings"),
                     
                     
                     tags$h5("Adjust arc angle, radius (distance between levels), node size and the colours of nodes in each level. Press update network to see changes."),
                     
                     fluidRow(
                       
                       
                       
                       column(width = 3, tags$h5(strong(paste0("Level"))), tags$body(tags$div(id="h5", style="padding:3%")),
                              fluidRow(tags$h5("1 - Functional purposes"), tags$body(tags$div(id="h5", style="padding:0.5%")), style=list("padding-left: 10%;")),
                              fluidRow(tags$h5("2 - Values and priority measures"), tags$body(tags$div(id="h5", style="padding:0.25%")), style=list("padding-left: 10%;")),
                              fluidRow(tags$h5("3 - Objective functions"), tags$body(tags$div(id="h5", style="padding:0.5%")), style=list("padding-left: 10%;")),
                              fluidRow(tags$h5("4 - Object-related processes"), tags$body(tags$div(id="h5", style="padding:0.25%")), style=list("padding-left: 10%;")),
                              fluidRow(tags$h5("5 - Physical objects"), tags$body(tags$div(id="h5", style="padding:0.5%")), style=list("padding-left: 10%;"))
                       ),
                       
                       column(width = 2, tags$h5(strong(paste0("Arc angle"))),
                              fluidRow(numericInput("angle1", label = NULL, value = 55, step = 5, min = 5, max = 180, width = "80%")),
                              fluidRow(numericInput("angle2", label = NULL, value = 60, step = 5, min = 5, max = 180, width = "80%")),
                              fluidRow(numericInput("angle3", label = NULL, value = 65, step = 5, min = 5, max = 180, width = "80%")),
                              fluidRow(numericInput("angle4", label = NULL, value = 70, step = 5, min = 5, max = 180, width = "80%")),
                              fluidRow(numericInput("angle5", label = NULL, value = 75, step = 5, min = 5, max = 180, width = "80%"))
                       ),
                       
                       column(width = 2, tags$h5(strong(paste0("Increase radius"))),
                              fluidRow(numericInput("r1", label = NULL, value = 0, step = 0.1, min = 0, max = 10, width = "80%")), 
                              fluidRow(numericInput("r2", label = NULL, value = 0, step = 0.1, min = 0, max = 10, width = "80%")), 
                              fluidRow(numericInput("r3", label = NULL, value = 0, step = 0.1, min = 0, max = 10, width = "80%")), 
                              fluidRow(numericInput("r4", label = NULL, value = 0.5, step = 0.1, min = 0, max = 10, width = "80%")), 
                              fluidRow(numericInput("r5", label = NULL, value = 1.0, step = 0.1, min = 0, max = 10, width = "80%"))
                       ),
                       
                       column(width = 2, tags$h5(strong(paste0("Node size"))),
                              fluidRow(numericInput("s1", label = NULL, value = 6, step = 0.1, min = 0.1, max = 10, width = "80%")), 
                              fluidRow(numericInput("s2", label = NULL, value = 5, step = 0.1, min = 0.1, max = 10, width = "80%")), 
                              fluidRow(numericInput("s3", label = NULL, value = 4, step = 0.1, min = 0.1, max = 10, width = "80%")), 
                              fluidRow(numericInput("s4", label = NULL, value = 3.5, step = 0.1, min = 0.1, max = 10, width = "80%")), 
                              fluidRow(numericInput("s5", label = NULL, value = 2, step = 0.1, min = 0.1, max = 10, width = "80%"))
                       ),
                       
                       column(width = 2, tags$h5(strong(paste0("Node colour"))),
                              
                              
                              # COLOURS
                              fluidRow(colourInput("col1", NULL, value = WRCcols[[1]], palette = "limited", allowedCols = WRCcols, closeOnClick = TRUE), style=list("padding-right: 0%;")),
                              fluidRow(colourInput("col2", NULL, value = WRCcols[[2]], palette = "limited", allowedCols = WRCcols, closeOnClick = TRUE), style=list("padding-right: 0%;")),
                              fluidRow(colourInput("col3", NULL, value = WRCcols[[3]], palette = "limited", allowedCols = WRCcols, closeOnClick = TRUE), style=list("padding-right: 0%;")),
                              fluidRow(colourInput("col4", NULL, value = WRCcols[[4]], palette = "limited", allowedCols = WRCcols, closeOnClick = TRUE), style=list("padding-right: 0%;")),
                              fluidRow(colourInput("col5", NULL, value = WRCcols[[5]], palette = "limited", allowedCols = WRCcols, closeOnClick = TRUE), style=list("padding-right: 0%;"))
                              
                       ),
                       
                       ),
                     
                     
                     # SUBMIT BUTTON
                     actionButton(inputId = "submitMatrix", label = "Update network", class = "btn-primary"),
                     hr(style = "border-top: 1px solid #D0D5DA;"),
                     
                     
                     # SLIDERS
                     tags$h5(strong(paste0("Live update:"))),
                     fluidRow(
                       column(width = 4, sliderInput("edgeSpacing", label = "Node spacing", min = 0, max = 1, value = c(1,1), step = 0.05, ticks = FALSE)),
                       column(width = 4, sliderInput("edgeAlpha", label = "Edge opacity", min = 0, max = 1, value = 0.4, step = 0.1, ticks = FALSE)),
                       column(width = 4, sliderInput("edgeGrey", label = "Edge grayness", min = 1, max = 9, value = 6, step = 1, ticks = FALSE))
                     ),
                     
                     # SWITCH
                     materialSwitch(inputId = "submitCircles", label = strong(paste("Toggle underlying circles:")), status = "primary", inline = TRUE)
                     
                   )),
                   
                   
                   column(width = 2, dropdownButton(
                     
                     # SETTINGS
                     circle = TRUE, 
                     status = "primary", # Button colour
                     icon = icon("expand-arrows-alt"), 
                     width = "200px",
                     tooltip = tooltipOptions(title = "Click to resize"),
                     
                     tags$h3("Resize plot"),
                     
                     # PLOT SIZE
                     numericInput("width", label = "Plot width (inches):", value = 1),
                     numericInput("height", label = "Plot height (inches):", value = 1)
                     
                   ))),
                 
                 hr(style = "border-top: 1px solid #D0D5DA;"),
                 
                 
                 # AH LEVELS
                 sliderInput("selectLevels", label = "Select level(s):", 
                             min = 1, max = 5, value = c(1,5), step = 1, ticks = FALSE),
                 hr(style = "border-top: 1px solid #D0D5DA;"),
                 
                 
                 # PICKER
                 pickerInput(inputId = "myNodes", label = "Select node subnetwork:", 
                             choices = pickerChoices, selected = "No selection", 
                             multiple = FALSE, 
                             options = list(`live-search` = TRUE, size = 10, style = "btn-primary")),
                 
                 
                 # SUBMIT BUTTON
                 actionButton(inputId = "myReset", label = "Reset nodes", class = "btn-primary"),
                 
                 hr(style = "border-top: 1px solid #D0D5DA;"),
                 
                 tags$h5(strong(paste0("Node subnetwork opacity:"))),
                 fluidRow(
                   column(width = 6, sliderInput("edgeSubAlpha", label = "Edges", 
                                                 min = 0, max = 1, value = 0.1, step = 0.01, ticks = FALSE)),
                   column(width = 6, sliderInput("nodeSubAlpha", label = "Nodes", 
                                                 min = 0, max = 1, value = 0.1, step = 0.01, ticks = FALSE))
                 ),
                 
                 markdown("###### *Developed by Annie Visser-Quinn (annievisserquinn@gmail.com) as part of the Water Resilient Cities project, funded by UKRI EPSRC, grant number EP/N030419/1. Maintained by David Morrison (dh48@hw.ac.uk). Source code is available via github: https://github.com/avisserquinn/AHExploreR.*")
                 
    ),
    
    
    # -------------------------------------------------------------------------
    # MAIN PANEL --------------------------------------------------------------
    # -------------------------------------------------------------------------
    mainPanel(width = 10,
              
              # TAGS - SIZE FORMATTING
              tags$body(tags$div(id="ppitest", style="width:0.75in;visible:hidden;padding:0px")),
              
              tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });'),
              
              
              # GGIRAPH OUTPUT
              tabsetPanel(type = "tabs",
                          tabPanel("Plot", br(), girafeOutput(outputId = "basePlot", width = "auto", height = "auto")),
                          tabPanel("Table - Nodes", br(), dataTableOutput(outputId = "tableNodes")),
                          tabPanel("Table - Edges", br(), dataTableOutput(outputId = "tableEdges")),
                          tabPanel("References", br(), markdown(
                            "- Abel, J.R. and Deitz, R. (2013) 'Do Big Cities Help College Graduates Find Better Jobs? Liberty Street Economics. Available at: [Do Big Cities Help College Graduates Find Better Jobs? -Liberty Street Economics (newyorkfed.org)](https://libertystreeteconomics.newyorkfed.org/2013/05/do-big-cities-help-college-graduates-find-better-jobs.html#.V3bs_ZMrK9Z) (Accessed: 03/05/2021)
                            - Adams, J., Greenwood, D., Thomashow, M. and Russ, A. (2016) 'Sense of place', The Nature of Cities. Available at: [https://www.thenatureofcities.com/2016/05/26/sense-of-place/](https://www.thenatureofcities.com/2016/05/26/sense-of-place/) (Accessed: 09/03/21).
                            - Allwinkle, S. and Cruickshank, P. (2011) Creating Smart-er Cities: An Overview. Journal of Urban Technology, 18:2, 1-18. [https://doi.org/10.1080/10630732.2011.601103](https://doi.org/10.1080/10630732.2011.601103)
                            - Arup (2015) 'City Resilience Framework'. Available at: [https://www.rockefellerfoundation.org/report/city-resilience-framework/](https://www.rockefellerfoundation.org/report/city-resilience-framework/)
                            - Beall, J. (2016) 'Who needs the other more: cities or universities?'. Available at: [https://www.britishcouncil.org/voices-magazine/who-needs-other-more-cities-or-universities](https://www.britishcouncil.org/voices-magazine/who-needs-other-more-cities-or-universities)
                            - Commission, E. (2020) The Future of Cities: Cities as Innovation Hubs. Available at: [https://urban.jrc.ec.europa.eu/thefutureofcities/cities-as-innovation-hubs#the-chapter](https://urban.jrc.ec.europa.eu/thefutureofcities/cities-as-innovation-hubs#the-chapter) (Accessed: 09/03/21).
                            - Council of Europe (2005) Council of Europe Framework Convention on the Value of Cultural Heritage for Society. Available at: [https://www.coe.int/en/web/conventions/full-list/-/conventions/rms/0900001680083746](https://www.coe.int/en/web/conventions/full-list/-/conventions/rms/0900001680083746) (Accessed: 09/03/21)
                            - Dunn, N. and Coulton, C. (2016) Future of health and healthcare provision in cities. Future of Cities: Working Paper. Foresight, Government Office for Science. Available at: [Future of health and healthcare provision in cities (publishing.service.gov.uk)](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/545772/gs-16-15-future-of-cities-health-healthcare-provision.pdf) (Accessed 04/05/2021).
                            - EEA (2015) Urban sustainability issues - What is a resource-efficient city?' Available at: [Urban sustainability issues - What is a resource-efficient city? (europa.eu)](https://www.eea.europa.eu/publications/resource-efficient-cities/file) (Accessed 04/05/2021).
                            - Exenberger, A. and Strobl, P. (2013) 'Introduction', in Globalisation and the City: Two Connected Phenomena in Past and Present [online]. Innsbruck: Innsbruck University Press.
                            - Foster, S. and Iaione, C. (2017) Ostrom in the City: Design Principles for the Urban Commons. The nature of cities. Available at: [Ostrom in the City: Design Principles for the Urban Commons - The Nature of Cities](https://www.thenatureofcities.com/2017/08/20/ostrom-city-design-principles-urban-commons/) (Accessed 04/05/2021).
                            - Goodland, R. (1995) 'The Concept of Environmental Sustainability', Annual Review of Ecology and Systematics, 26, pp. 1-24.
                            - Knapp, C. (2008) Making Multicultural Places. Project for Public Spaces. Available at: [Making Multicultural Places (pps.org)](https://www.pps.org/article/multicultural-places) Accessed (04/05/2021).
                            - Knox, P.L. (1996) 'Globalization and the world city hypothesis', Scottish Geographical Magazine, 112(2), pp. 124-126.
                            - LeGrand, L. and Malany, P.E. (2012) A reflection on the Importance of Settlements in Humanitarian Shelter Assistance. Available at: [https://www.alnap.org/help-library/a-reflection-on-the-importance-of-settlements-in-humanitarian-shelter-assistance](https://www.alnap.org/help-library/a-reflection-on-the-importance-of-settlements-in-humanitarian-shelter-assistance).
                            - Longley, R. (2020) 'Equity vs. Equality: What is the difference?'. Available at: [https://www.thoughtco.com/equity-vs-equality-4767021](https://www.thoughtco.com/equity-vs-equality-4767021).
                            - Lonsdale, J., Schweppenstedde, D., Van Stolk, C., Guerin, B., Hafner, M. (2015) 'One Place, One Budget? Approaches to pooling resources for public service transformation. RAND Corportation. Available at: [One Place, One Budget? Approaches to pooling resources for public service transformation | RAND](https://www.rand.org/pubs/research_reports/RR1017.html) (Accessed 04/05/2021).
                            - Massey, D. (2004) 'Geographies of responsibility', Geografiska Annaler: Series B, Human Geography, 86(1), pp. 5-18.
                            - Roberts, J. (2018) Urban Safety Project: Urban Safety and Security in Myanmar. The Asia Foundation.
                            - Russell, H., Smith, A. and Leverton, P. (2011) Sustaining cultural identity and a sense of place - new wine in old bottles or old wine in new bottles? The College of Estate Management.
                            - Smith, N. (2018) How Universities Make Cities Great. BloombergOpinion. Available at: [How Universities Make Cities Great - Bloomberg](https://www.bloomberg.com/opinion/articles/2018-03-06/how-universities-make-cities-great) (Accessed 04/05/021).
                            - UNDRR (2020) 'Hazard definition & classification review: Technical report'. 10/09/2020. Available at: https://www.undrr.org/publication/hazard-definition-and-classification-review.
                            - UNHCR (2020) Settlement in urban areas. Available at: https://emergency.unhcr.org/entry/36413/settlement-in-urban-areas (Accessed: 09/03/21).
                            - UNSDG (2019) Leaving no-one behind: A UNSDG Operational Guide for UN Country Teams (Interim Draft). Available at: [Interim-Draft-Operational-Guide-on-LNOB-for-UNCTs.pdf](https://unsdg.un.org/sites/default/files/Interim-Draft-Operational-Guide-on-LNOB-for-UNCTs.pdf) (Accessed 04/05/2021).
                            - World Bank (2017) 'Opportunities and Challenges of Urbanization: Planning for an Unprecedented Future'. Available at: [Opportunities and Challenges of Urbanization: Planning for an Unprecedented Future (worldbank.org)](https://www.worldbank.org/en/events/2017/09/25/opportunities-and-challenges-of-urbanization) (Accessed 03/05/2021).
                            - World Bank (2021) Inclusive Cities. Available at: [Inclusive Cities (worldbank.org)](https://www.worldbank.org/en/topic/inclusive-cities) (Accessed: 04/05/2021)."

                          ))
              )
              
    ) 
    
  )
  
)


# -------------------------------------------------------------------------
# SERVER ------------------------------------------------------------------
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  angles <- reactive({ c(input$angle1, input$angle2, input$angle3, input$angle4, input$angle5) })
  radius <- reactive({ c(input$r1, input$r2, input$r3, input$r4, input$r5) })
  size <- reactive({ c(input$s1, input$s2, input$s3, input$s4, input$s5) })
  
  DL <- reactive({
    
    DL <- list()
    DL$dv <- dl$dv %>% filter(level >= input$selectLevels[[1]] & level <= input$selectLevels[[2]])
    DL$de <- 
      dl$de %>% 
      separate(layer, c("tmp", "tmp2"), remove = FALSE) %>% 
      inner_join(tmpKey %>% 
                   mutate(fromLevel = str_sub(label,1,1) %>% as.numeric)) %>% 
      select(-tmp, -label) %>% 
      rename(tmp = tmp2) %>% 
      inner_join(tmpKey %>% 
                   mutate(toLevel = str_sub(label,1,1) %>% 
                            as.numeric)) %>% 
      select(-tmp, -label) %>% 
      select(layer, fromLevel, toLevel, from, to, weight) %>%
      filter(fromLevel >= input$selectLevels[[1]] & toLevel <= input$selectLevels[[2]]) %>%
      select(layer, from, to, weight)
    
    return(DL)
    
  })
  
  
  # REACTIVE
  toListen <- reactive({ 
    list(input$edgeGrey,
         input$edgeSpacing, 
         input$edgeAlpha,
         input$submitMatrix, 
         input$myNodes, 
         input$edgeSubAlpha, 
         input$nodeSubAlpha,
         input$submitCircles,
         input$width,
         input$height,
         input$selectLevels
    ) 
  })
  
  
  # OBSERVE REACTIVE
  observeEvent(toListen(),
               
               {
                 
                 pal <- c(input$col1, input$col2, input$col3, input$col4, input$col5)
                 
                 # GENERATE KEY
                 myKey <- 
                   dl$dv %>% 
                   select(level, levelName) %>% 
                   unique %>% 
                   mutate(angle = angles(),
                          addR = radius())#(input$myMatrix[,2] %>% as.numeric))
                 
                 myKey2 <- myKey
                 myKey <- myKey %>% slice(input$selectLevels[[1]]:input$selectLevels[[2]])
                 
                 
                 # GENERATE LAYOUT
                 vl <- visLayout(edgelist = DL()$de,
                                 vInfo = DL()$dv, 
                                 key = myKey, 
                                 spacing = list(min = input$edgeSpacing[[1]], max = input$edgeSpacing[[2]]))
                 
                 
                 # DATA
                 nodes <- vl$vertices
                 edges <- vl$edges
                 edgeAlpha <- input$edgeAlpha
                 edgeFade <- 0.2
                 edgeGrey <- myGreys[input$edgeGrey]
                 nodeAlpha <- 1
                 nodeFade <- 0.75
                 
                 
                 # SELECT NODE SUBNETWORK
                 if(input$myNodes != "No selection") { 
                   
                   vl <- visLayout(edgelist = dl$de,
                                   vInfo = dl$dv, 
                                   key = myKey2, 
                                   spacing = list(min = input$edgeSpacing[[1]], max = input$edgeSpacing[[2]]))
                   
                   vl <- vNetwork(input$myNodes, vl$vertices, vl$edges, direction = "both") 
                   subNodes <- vl$vertices %>% filter(level >= input$selectLevels[[1]] & level <= input$selectLevels[[2]])
                   subEdges <- vl$edges %>% filter(toLevel > input$selectLevels[[1]] & fromLevel < input$selectLevels[[2]])
                   edgeAlpha <- input$edgeSubAlpha
                   nodeAlpha <- input$nodeSubAlpha
                 }
                 
                 
                 # GENERATE BASE PLOT
                 # Base plot
                 basePlot <- ggplot() 
                 
                 # Underlying circles
                 if(input$submitCircles == TRUE) { 
                   
                   basePlot <- basePlot + 
                     
                     ggforce::geom_arc(data = myKey, 
                                       aes(x0 = 0, 
                                           y0 = 0, 
                                           r = level+addR, 
                                           start = (180-angle)*pi/180, 
                                           end = (180+angle)*pi/180),
                                       size = 1, 
                                       colour = myGreys[[9]], 
                                       linetype = "dotted", 
                                       alpha = 0.5)
                   
                 }
                 
                 # Add edges
                 basePlot <- basePlot + 
                   
                   geom_segment(data = 
                                  edges %>% 
                                  arrange(desc(layer)) %>% 
                                  arrange(from, to), 
                                
                                aes(x = x, 
                                    xend = xend, 
                                    y = y, 
                                    yend = yend),
                                
                                size = 0.2, 
                                colour = myGreys[input$edgeGrey], 
                                alpha = edgeAlpha,
                                show.legend = FALSE) 
                 
                 # Overplot edges
                 if(input$myNodes != "No selection") { 
                   
                   basePlot <- basePlot + 
                     
                     geom_segment(data = 
                                    subEdges %>% 
                                    arrange(desc(layer)) %>% 
                                    arrange(from, to), 
                                  
                                  aes(x = x, 
                                      xend = xend, 
                                      y = y, 
                                      yend = yend),
                                  
                                  size = 0.2, 
                                  colour = myGreys[input$edgeGrey-1], 
                                  alpha = input$edgeAlpha,
                                  show.legend = FALSE) 
                   
                 }
                 
                 # Add points
                 basePlot <- basePlot + 
                   
                   geom_point_interactive(data = 
                                            nodes, 
                                          
                                          aes(x = x, 
                                              y = y,
                                              fill = levelName,
                                              size = levelName,
                                              tooltip = vName, 
                                              data_id = vName),
                                          
                                          alpha = nodeAlpha,
                                          shape = 21, 
                                          colour = myGreys[6], 
                                          stroke = 0.25)
                 
                 # Overplot points
                 if(input$myNodes != "No selection") { 
                   
                   basePlot <- basePlot + 
                     
                     geom_point_interactive(data = 
                                              subNodes, 
                                            
                                            aes(x = x, 
                                                y = y,
                                                fill = levelName,
                                                size = levelName,
                                                tooltip = vName, 
                                                data_id = vName),
                                            
                                            alpha = nodeAlpha + nodeFade,
                                            shape = 21, 
                                            colour = myGreys[6], 
                                            stroke = 0.25)
                 }
                 
                 # Manipulate non-data ink
                 basePlot <- basePlot + 
                   
                   theme_void(base_size = 18, base_family = "sans-serif") +
                   
                   theme(legend.title = element_blank(),
                         legend.text = element_text(margin = margin(r = 25, unit = "pt")),
                         legend.text.align = 0,
                         legend.position = "top",
                         text = element_text(colour = myGreys[[2]])) +
                   
                   scale_size_manual(values = size()) +
                   scale_fill_manual(values = pal[input$selectLevels[[1]]:input$selectLevels[[2]]])
                 
                 basePlot <- basePlot + 
                   
                   guides(fill = guide_legend(nrow = 5, 
                                              ncol = 1, 
                                              override.aes = list(size = as.numeric(size())[input$selectLevels[[1]]:input$selectLevels[[2]]])), 
                          size = "none")
                 
                 basePlot <- basePlot + 
                   labs(caption = paste0("Generated ", Sys.Date(), 
                                         " using the AHexploreR @ waterresilientcities.shinyapps.io/AHexploreR",
                                         "\nWater Resilient Cities - EPSRC EP/N030419/1")) +
                   theme(plot.caption = element_text(colour = myGreys[[7]], size = 12))
                 
                 output$basePlot <- renderGirafe({ 
                   
                   girafe(ggobj = basePlot,
                          
                          width_svg = input$width,
                          height_svg = input$height,
                          
                          options = list(opts_sizing(rescale = FALSE),
                                         opts_tooltip(use_fill = TRUE,
                                                      delay_mouseover = 500),
                                         opts_hover(css = "stroke:#1B2631;"),
                                         opts_toolbar(pngname = "AHexploreR"),
                                         opts_selection(type = "single")
                          ))
                   
                 })
                 
                 
                 output$tableNodes <- renderDataTable(datatable(
                   dl$dv %>% 
                     filter(level >= input$selectLevels[[1]] & level <= input$selectLevels[[2]]) %>% 
                     select(Level = level, "Level name" = levelName, "Node name" = vName, "Definition" = definition), 
                   rownames = FALSE, 
                   selection = "none", 
                   options = list(pageLength = 25, 
                                  lengthMenu = c(25,50,100,250,500))))
                 
                 output$tableEdges <- renderDataTable(datatable(
                   vl$edges %>% 
                     filter(toLevel > input$selectLevels[[1]] & fromLevel < input$selectLevels[[2]]) %>%
                     select("Level from" = fromLevel, "Level to" = toLevel, "Edge from" = from, "Edge to" = to), 
                   rownames = FALSE, 
                   selection = "none", 
                   options = list(pageLength = 25, 
                                  lengthMenu = c(25,50,100,250,500))))
                 
               })
  
  
  # OBSERVE & UPDATE PICKERS
  observeEvent(input$basePlot_selected, {
    
    if (is.null(input$basePlot_selected)) {
      
      updatePickerInput(session = session, 
                        inputId = "myNodes", 
                        selected = "No selection"
      )
      
    } else { 
      
      updatePickerInput(session = session, 
                        inputId = "myNodes", 
                        selected = input$basePlot_selected %>% as.character
      )
      
    }
    
  })
  
  observeEvent(input$myReset, {
    
    updatePickerInput(session = session, 
                      inputId = "myNodes", 
                      selected = "No selection"
    )
    
  })
  
  output$selectedvar <- renderText({ (0.725*input$pltChange$width/input$pltChange$dpi) })
  output$selectedvar2 <- renderText({ (0.725*input$pltChange$height/input$pltChange$dpi) })
  
  observeEvent(input$pltChange, {
    
    value <- (0.725*input$pltChange$width/input$pltChange$dpi) %>% plyr::round_any(1, round)
    updateNumericInput(session = session, inputId = "width", value = value)
    
  })
  
  observeEvent(input$pltChange, {
    
    value <- (0.725*input$pltChange$height/input$pltChange$dpi) %>% plyr::round_any(1, round)
    updateNumericInput(session = session, inputId = "height", value = value)
    
  })
  
}


# -------------------------------------------------------------------------
# RUN APP -----------------------------------------------------------------
# -------------------------------------------------------------------------
shinyApp(ui, server)