options(shiny.maxRequestSize=30*1024^2) #increase size limit of uploaded files to 30MB

#load/install packages
if (!require('shiny')) install.packages('shiny'); library('shiny', quiet = T)
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse', quiet = T)
if (!require('DT')) install.packages('DT'); library('DT', quiet = T)
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer', quiet = T)
if (!require('viridis')) install.packages('viridis'); library('viridis', quiet = T)
if (!require('shinyjs')) install.packages('shinyjs'); library('shinyjs', quiet = T)
if (!require('ggdark')) install.packages('ggdark'); library('ggdark', quiet = T)
if (!require('scales')) install.packages('scales'); library('scales', quiet = T)

#load packages
#library(shiny)
#library(tidyverse)
#library(DT)
#library(RColorBrewer)
#library(viridis)
#library(shinyjs)
#library(ggdark)
#library(scales)

#function for calculating mean of F1/F2
calculateMeans <- function(input_data, input_facet) {
  if (input_facet == 'none') {
    input_data %>%
      group_by(vowel) %>%
      summarise(meanF1 = mean(F1), meanF2 = mean(F2))
  } else {
    input_data %>%
      group_by(vowel, xtraFactor) %>%
      summarise(meanF1 = mean(F1), meanF2 = mean(F2))
  }
}


############# UI #############

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(tags$style(
    HTML("
         #titlePanel {
           color: white;
           background: #0B517F;
           border-style: solid;
           border-size: 2px;
           border-radius: 20px;
           width: 99%;
           height: auto;
           padding: 10px 20px 20px 20px;
           margin: 10px 10px -10px 10px;
           display: inline-block;
         }
         
         #subtext {
         color: grey;
         font-style: italic;
         font-size: 12px;
         }
         
         hr {
          border-top: 1px solid #b4b4b4;
         }
         
         #titlePanel a {
         color: #a0e9f9;
         font-weight: bold;
         }
         
         #bannerLogo {
          display: inline-block;
          float: right;
         }
         
         .fa {
         vertical-align: middle;
         padding: 10px 10px 10px 0px;
         }
         
         #customColours {
          margin-top: -15px;
         }
         "))),
  
  tags$div(id="titlePanel",
           img(id='bannerLogo', src = 'mvp_logo.png', height = 170),
    titlePanel(windowTitle = 'Modern Vowel Plotter (MVP)', 
               title = HTML("<b>The Real MVP</b> ( <i><b>M</b>odern <b>V</b>owel <b>P</b>lotter</i> )")),
    HTML('<p>Upload a spreadsheet of vowel formant measurements in <b>.csv</b> format and this tool will generate vowel plots made using ggplot2.</p> <p>By default, the plotter tool will look for columns named <b>F1</b> and <b>F2</b> for your formant measurements, <b>vowel</b> for the vowel category labels, and <b>word</b> for the word labels, so it works best if your spreadsheet contains these column names. It will still work if your columns are labelled differently though - you just need to use the drop-down menus to specify which columns contain the various labels/values needed to plot.</p> <p style="float:left;">Developed by <b><a href="https://www.gbailey.uk/" target="_blank">George Bailey</a></b> (2022) at the University of York</p>'),
    ),
  
  br(), br(),
  sidebarLayout(
    sidebarPanel(
      
      tags$div(id = "dataSelectionPanel",
        h2("Data and column selection", style="color: #0B517F"),
        fileInput('datafile', "Upload your spreadsheet of formant values",
                  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        uiOutput('pickF1'), uiOutput('pickF2'), 
        uiOutput('pickWord'), uiOutput('pickVowel'), uiOutput('pickFacet'), 
        actionButton("gotoCustomisationPanel", "Go to customisation panel →",
                     icon = icon("paint-roller", style = "font-size: 30px;"),
                     style="color: #fff; background-color: #0B517F; font-size: 17px; width: 100%;")
        
      ),

      hidden(
        tags$div(id = "plotCustomisationPanel",
          h2("Plot customisation", style="color: #0B517F"),
          #h3("Select colours"),
          selectInput('colourPick', 'Choose a colour palette:', 
                      choices = c('Default', 'Set1', 'Spectral', 'Paired', 'Set3',
                                  'Viridis', 'Magma', 'Inferno', 'Plasma', 'Dark2', 'CUSTOM')),
          uiOutput('customColoursUI'),
          radioButtons('bgColourPick', 'Choose a background colour:', 
                      choices = c('white', 'dark mode', 'lighter grey', 'darker grey'), inline = T),
          
          #h3("Select plot elements"),
          tags$hr(),
          uiOutput("includeVowels"),
          
          tags$hr(),
          radioButtons('includeObs', label = strong('Plot individual tokens?'), 
                       choices = c('none', 'points', 'word labels'), selected = 'points', inline = T),
          uiOutput("obsSize"),
          uiOutput("obsAlpha"),
          
          tags$hr(),
          radioButtons('includeMeans', label = strong('Plot category mean?'), 
                       choices = c('none', 'points', 'category labels'), selected = 'none', inline = T),
          uiOutput("meanSize"),
          
          tags$hr(),
          radioButtons('includeEllipse', label = strong('Plot distribution?'), 
                       choices = c('none', 'ellipse'), selected = 'none', inline = T),
          uiOutput("ellipseAlpha"),
          
          tags$hr(),
          textInput("plotTitle", "Enter a plot title"),
          textInput("plotSubtitle", "Enter a plot subtitle"),
          
          actionButton("gotoDataPanel", "← Go back to data selection panel", 
                       icon = icon("database", style = "font-size: 30px;"),
                       style="color: #fff; background-color: #0B517F; font-size: 17px; width: 100%;")
        )
        ),
      
      #br(), br(),
      #HTML('<p>Developed by <a href="https://www.gbailey.uk/" target="_blank">George Bailey</a> (2022) at the University of York</p>'),
    width=4),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Plot", 
                 hidden(plotOutput('vwlPlot', height = '500px', width = '800px')), 
                 hidden(htmlOutput("plotText")),
                 uiOutput('ui.plotsave')
                 ),
        tabPanel("Data", DT::dataTableOutput("table"))
      )
    )
  )
)



########### SERVER ###########


server <- function(input, output, session) {

  observeEvent(input$gotoCustomisationPanel, {
    toggle('dataSelectionPanel')
    toggle('plotCustomisationPanel')
    show('vwlPlot')
    show('plotText')
    show('downloadPlot')
  })
  
  observeEvent(input$gotoDataPanel, {
    toggle('dataSelectionPanel')
    toggle('plotCustomisationPanel')
  })
  
  
  #display the plot download button (only once data is uploaded)
  output$ui.plotsave <- renderUI({
    if (is.null(filedata())) return()
    div(style = "display:inline-block; float:right; margin-right: 100px;", 
        hidden(
          downloadButton("downloadPlot", label = 'Download the plot', 
                       icon = icon('image', style = "font-size: 30px"),
                       style="color: #fff; background-color: #0D8012; font-size: 17px; width: 100%;")
        )
    )
  })
  
 
  #handles the data upload
  filedata <- reactive({
    req(input$datafile)
    
    read.csv(input$datafile$datapath, header = T)
  })
  
  output$includeVowels <- renderUI({
    if (is.null(filedata())) return()
    vowelCats <- plotdata()$vowel %>% unique() %>% as.character()
    
    checkboxGroupInput("vowelChoices", "Include the following categories:", 
                       choices = vowelCats, selected = vowelCats, inline = T)
  })
  
  ## COLUMN SELECTIONS ##
  
  output$pickF1 <- renderUI({
    tags$hr()
    colLabels <- c('', filedata() %>% colnames() %>% as.character())
    selectInput("F1Column", "F1 values are in:", 
                choices = colLabels, selected = ifelse('F1' %in% colLabels, 'F1', ''))
  })
  output$pickF2 <- renderUI({
    tags$hr()
    colLabels <- c('', filedata() %>% colnames() %>% as.character())
    selectInput("F2Column", "F2 values are in:", 
                choices = colLabels, selected = ifelse('F2' %in% colLabels, 'F2', ''))
  })
  output$pickVowel <- renderUI({
    tags$hr()
    colLabels <- c('', filedata() %>% colnames() %>% as.character())
    selectInput("vowelColumn", HTML('Group observations by:<br><span id="subtext">(each unique value in this column will be plotted as a separate colour-coded group)</span>'), 
                choices = colLabels, selected = ifelse('vowel' %in% colLabels, 'vowel', ''))
  })
  output$pickWord <- renderUI({
    tags$hr()
    colLabels <- c('', filedata() %>% colnames() %>% as.character())
    selectInput("wordColumn", "Word labels are in:", 
                choices = colLabels, selected = ifelse('word' %in% colLabels, 'word', ''))
  })
  output$pickFacet <- renderUI({
    tags$hr()
    colLabels <- c('none', filedata() %>% colnames() %>% as.character())
    selectInput("facetColumn", HTML('Optionally, choose a facet term:<br><span id="subtext">(each unique value in this column will be plotted in a separate panel)</span>'), 
                       choices = colLabels)
  })
  
  ## SLIDERS  ##
  
  output$obsSize <- renderUI({
    if (input$includeObs != 'none') {
      sliderInput('obsSize', "Size of token points/labels", 1, 10, value = 3, ticks = F)
    }
  })
  output$obsAlpha <- renderUI({
    if (input$includeObs != 'none') {
      sliderInput('obsAlpha', "Token point/label transparency", 0, 1, value = 1, step = 0.1, ticks = F)
    } 
  })
  output$meanSize <- renderUI({
    if (input$includeMeans != 'none') {
      sliderInput('meanSize', "Size of mean labels", 4, 30, value = 10, step = 2, ticks = F)
    }
  })
  output$ellipseAlpha <- renderUI({
    if (input$includeEllipse != 'none') {
      sliderInput('ellipseAlpha', "Ellipse transparency", 0, 1, value = 0.2, step = 0.1, ticks = F)
    } 
  })
  
  output$customColoursUI <- renderUI({
    if (input$colourPick == 'CUSTOM') {
      HTML(
        paste(
          '<b>Paste a comma-separated list of',
          plotdata() %>% filter(vowel %in% input$vowelChoices) %>% pull(vowel) %>% unique() %>% length(),
          '<a href="https://htmlcolorcodes.com/color-picker/" target="_blank">#hex colour codes</a> here to use as a custom colour palette:</b>',
          textInput('customColours', '')
          )
        )
      
    }
  })
  
  
  customPal <- reactive({
    n <- plotdata() %>% 
      filter(vowel %in% input$vowelChoices) %>%
      pull(vowel) %>% unique() %>% length()
    
    if (input$colourPick == 'Default') {
      pal <- hue_pal()(n)
    } else if (input$colourPick %in% c('Set1', 'Dark2', 'Spectral', 'Paired', 'Set3')) {
      pal <- brewer.pal(n, input$colourPick)
    } else if (input$colourPick %in% c('Viridis', 'Magma', 'Inferno', 'Plasma')) {
      pal <- viridis(n, opt = tolower(input$colourPick))
    } else if (input$colourPick == 'CUSTOM') {
      validate(
        need(str_count(input$customColours, '#') == n, "You haven't entered enough hex colour codes")
      )
      
      validate(
        need(str_detect(input$customColours, paste0("^((#([A-F0-9]{6}))[, ]*){", n, "}")), 
             "One or more of the hex codes are incorrectly formatted")
      )
      
      pal <- input$customColours %>%
        toupper() %>%
        str_split(',') %>%
        unlist() %>%
        str_trim()
      }
      
    pal
  })
  
  
  #download button
    output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('plot', '.png', sep='')
    },
    content=function(file){
      ggsave(filename = file, bg = 'white', width = 10, height = 7, dpi = 300, units = "in")
    },
    contentType='image/png')
  
    
    plotdata <- reactive({
      
      if (input$facetColumn == 'none') {
        data.frame(
          F1 = filedata()[[input$F1Column]],
          F2 = filedata()[[input$F2Column]],
          word = filedata()[[input$wordColumn]],
          vowel = filedata()[[input$vowelColumn]]
        )
      } else {
      data.frame(
        F1 = filedata()[[input$F1Column]],
        F2 = filedata()[[input$F2Column]],
        word = filedata()[[input$wordColumn]],
        vowel = filedata()[[input$vowelColumn]],
        xtraFactor = filedata()[[input$facetColumn]]
      )
      }
    })
    

  #plot panel
  output$vwlPlot <- renderPlot({
    req(filedata())
    
    basePlot <- plotdata() %>%
      filter(vowel %in% input$vowelChoices) %>%
      ggplot(aes(F2, F1, group = as.factor(vowel), colour = as.factor(vowel), fill = as.factor(vowel))) +
      #ggplot(aes_string(x = input$F2Column, y = input$F1Column,
      #                  group = input$vowelColumn, colour = input$vowelColumn)) +
      scale_x_reverse(position = "top") +
      scale_y_reverse(position = "right") +
      labs(caption = "Plot generated using the MVP (Modern Vowel Plotter), available at: https://grbails.shinyapps.io/vowel-plotter") +
      guides(colour=guide_legend(override.aes=list(size = 3, alpha = 1))) +
      scale_colour_manual(values = customPal()) +
      scale_fill_manual(values = customPal())
    
    if (input$bgColourPick == 'white') {
      basePlot <- basePlot +
        theme_bw() +
        theme(text = element_text(size = 15),
              plot.caption = element_text(color = "slategrey"))#, face = "italic"))
    } else if (input$bgColourPick == 'lighter grey') {
      basePlot <- basePlot +
        theme_gray() +
        theme(text = element_text(size = 15),
              plot.caption = element_text(color = "slategrey"))#, face = "italic"))
    } else if (input$bgColourPick == 'darker grey') {
      basePlot <- basePlot +
        theme_dark() +
        theme(text = element_text(size = 15),
              plot.caption = element_text(color = "slategrey"))#, face = "italic"))
    } else if (input$bgColourPick == 'dark mode') {
      basePlot <- basePlot +
        dark_theme_bw() +
        theme(text = element_text(size = 15),
              plot.caption = element_text(face = "italic"))
    }
    
    #plotting individual observations
    if (input$includeObs == 'none') {
      plotV1 <- basePlot +
        geom_point(size = 0, alpha = 0)
    } else if (input$includeObs == 'points') {
      plotV1 <- basePlot +
        geom_point(size = input$obsSize, alpha = input$obsAlpha)
    } else if (input$includeObs == 'word labels') {
      plotV1 <- basePlot +
        geom_text(aes(label = word), size = input$obsSize, alpha = input$obsAlpha)
    }
    
    #plotting ellipses
    if (input$includeEllipse == 'none') {
      plotV2 <- plotV1
    } else if (input$includeEllipse == 'ellipse') {
      plotV2 <- plotV1 +
        stat_ellipse(geom = 'polygon', aes(fill = as.factor(vowel)), 
                     alpha = ifelse(is.null(input$ellipseAlpha), 0.2, input$ellipseAlpha), 
                     show.legend = F) #+
      #scale_fill_manual(guide = 'none', values = alpha(customPal(), input$ellipseAlpha))
    }
    
    #plotting means
    if (input$includeMeans == 'none') {
      plotV3 <- plotV2
    } else if (input$includeMeans == 'points') {
      plotV3 <- plotV2 +
        geom_point(data = plotdata() %>% 
                     filter(vowel %in% input$vowelChoices) %>% 
                     calculateMeans(input$facetColumn),
                   aes(meanF2, meanF1, fill = as.factor(vowel)), colour = 'black', 
                   size = ifelse(is.null(input$meanSize), 10, input$meanSize), 
                   pch = 23,
                   show.legend = F)
    } else if (input$includeMeans == 'category labels') {
      plotV3 <- plotV2 +
        geom_label(data = plotdata() %>% 
                     filter(vowel %in% input$vowelChoices) %>% 
                     calculateMeans(input$facetColumn),
                   aes(meanF2, meanF1, label = vowel), fill = 'white', 
                   size = ifelse(is.null(input$meanSize), 10, input$meanSize), 
                   show.legend = F)
    }
    
        #plotting a facet
    if (input$facetColumn == 'none') {
      plotV4 <- plotV3
    } else {
      plotV4 <- plotV3 +
        #facet_wrap(as.formula(paste("~", xtraFactor)))
        facet_wrap(~xtraFactor)
    }
    
    #adding titles
    if (input$plotTitle == '' & input$plotSubtitle == '') {
      plotV5 <- plotV4
    } else if (input$plotTitle != ''& input$plotSubtitle == '') {
      plotV5 <- plotV4 +
        ggtitle(input$plotTitle)
    } else if (input$plotTitle == '' & input$plotSubtitle != '') {
        plotV5 <- plotV4 +
          ggtitle(input$plotTitle, subtitle = input$plotSubtitle)
    } else if (input$plotTitle != '' & input$plotSubtitle != '') {
      plotV5 <- plotV4 +
        ggtitle(input$plotTitle, subtitle = input$plotSubtitle)
    }
    
    #return final plot
    plotV5
    
  })
  
  output$plotText <- renderUI({ 
    req(filedata())
    
    HTML(
      paste("<br><br>Displaying", 
            plotdata() %>% filter(vowel %in% input$vowelChoices) %>% nrow(), 
            "data points from", 
            plotdata() %>% filter(vowel %in% input$vowelChoices) %>% pull(vowel) %>% unique() %>% length(),
            "category grouping(s)"
      )
      )
  })
  
  
  #table panel
  output$table <- renderDataTable(
    filedata()
    )

}

shinyApp(ui, server)
