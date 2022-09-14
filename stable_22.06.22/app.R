# initial -----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(jsonlite)
publ   <- readxl::read_xlsx("../DB1.xlsx", sheet = "publications")
lang <- "en"
publ_id <- "publ1"
TR <- read_json("www/translation1.json")
attach(.GlobalEnv, name = "lang")
tr0 <- function(b){pluck(b, lang)}

L <- readxl::read_xlsx("translation.xlsx") %>% 
    unite("key", type, key) %>% 
    transpose(.names = .$key)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    # h1("Faunistica"),
    
    # Publication info --------------------------------------------------------    

    
    uiOutput("publ"),
    uiOutput("plAdm"),
    uiOutput("plGeo"),
    uiOutput("event"),
    uiOutput("taxa"),
    uiOutput("abu"),
    HTML("<br>"),
    HTML("<br>"),
    p("data input page", align = "right"),
    title = "Faunistica: input data",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style1.css")),
    fluidRow(class="navbar",
             column(div(strong("Faunistica", `font-size` = "20pt")), width = 2),
             column(div(a("Home", href="#home")), class = "navup", width = 1),
             column(div(a("Input data", href="#news")), class = "navup", width = 2),
             column(div(a("Statistics", href="#news")), class = "navup", width = 2),
             column(div(), width = 1),
             column(div(a("LogIn", href="#news")), class = "navup", width = 1),
             column(div(
                 selectInput(
                     inputId = "language", 
                     label = NULL, 
                     choices = c("en", "ru"), 
                     selected = "en"
                 )), class = "navup", width = 1)
    )
    
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
    
    r <- function(X){ # translates text into current language
        sapply(X,function(s) L[[s]][[input$language]], USE.NAMES=FALSE)
    }
    
    output$publ <- renderUI({
        verticalLayout(
            verticalLayout(
                h3(r("publ_h_pub"), align = "center", class = "row2"), 
                if(str_detect(publ[publ$publ_id == publ_id,]$auth, "_")){
                    p(paste0(r("publ_aut2"), " ",
                             gsub("_", ", ", publ[publ$publ_id == publ_id,]$auth)))
                } else {
                    p(paste0(r("publ_aut1"), " ",
                             publ[publ$publ_id == publ_id,]$auth))
                },
                p(paste0(r("publ_name_publ"), " ", publ[publ$publ_id == publ_id,]$name)),
                p(paste0(r("publ_ref"), " ", publ[publ$publ_id == publ_id,]$ref)),
                actionButton("Change", r("publ_chg_publ")),
                HTML("<br>")
            )
        )
        
    })
    
    output$plAdm <- renderUI({
        verticalLayout(
            tags$hr(),
            h3(r("pl.adm_H_pl.adm"), align = "center"), # , align = "center"
            HTML("<br>"),
            flowLayout(
                textInput("Country",   r("pl.adm_adm0")),
                textInput("Region",    r("pl.adm_adm1")),
                textInput("District",  r("pl.adm_adm2")), 
                textInput("loc1", r("pl.geo_loc"))
            ),
            HTML("<br>"),
            flowLayout(actionButton("hold_adm",   r("serv_hold")), 
                       actionButton("unhold_adm", r("serv_unhold"))
            )
        )
    })

    
    output$plGeo <- renderUI({
        verticalLayout(
            tags$hr(),
            h3(r("pl.geo_h_pl.geo"), align = "center"), # , align = "center"
            HTML("<br>"),
            flowLayout(
                textInput("loc2", r("pl.geo_loc2")),
                textInput("rem",  r("pl.geo_geo.rem")),
                verticalLayout(
                    span(r("pl.geo_coord")),
                    splitLayout(
                        textInput("N", label = NULL, width = 300, 
                                  placeholder = r("pl.geo_filler")),
                        HTML("<b>N</b>"),
                        cellWidths = c("80%", "20%")
                    )
                ), 
                verticalLayout(
                    HTML("<br>"),
                    splitLayout(
                        textInput("E", label = NULL, width = 250, 
                                  placeholder = r("pl.geo_filler")), 
                        HTML("<b>E</b>"),
                        cellWidths = c("80%", "20%")
                    ),
                ),
            ),
            HTML("<br>"),
            flowLayout(actionButton("hold_geo",   r("serv_hold")), 
                       actionButton("unhold_geo", r("serv_unhold"))
            )
        )
        
    })
    
    output$event <- renderUI({
        verticalLayout(
            tags$hr(),
            h3(r("event_h_event"), align = "center"), # , align = "center"
            HTML("<br>"),
            flowLayout(
                textInput("Habitat", r("event_hab")),
                dateInput("dat", r("event_date"), startview = "month",),
                textInput("Effort", r("event_effort"),
                          placeholder = r("event_effort_fill")),
                textInput("event_rem", r("event_event_rem"))
            ), 
            HTML("<br>"),
            flowLayout(actionButton("event_Hevent",  r("serv_hold")), 
                       actionButton("event_UHevent", r("serv_unhold"))
            )
        )
        
    })
    
    # Taxa UI ------------------------------------------------------------------ 
    output$taxa <- renderUI({
        verticalLayout(
            tags$hr(),
            h3(r("taxa_h_taxa"), align = "center", textcolor = "red"), 
            HTML("<br>"),
            flowLayout(
                textInput("Fam", r("taxa_fam")),
                textInput("Gen", r("taxa_gen")),
                textInput("Sp",  r("taxa_sp")),
                checkboxInput("sp_n.def", r("taxa_n_def")),
            ),
            HTML("<br>"),
            flowLayout(actionButton("hold_taxa", r("serv_hold")), 
                       actionButton("unhold_taxa", r("serv_unhold"))
            )
        )
    })
    
    output$abu <- renderUI({
        verticalLayout(
            tags$hr(),
            h3(r("taxa_abu"), align = "center", textcolor = "red"),
            HTML("<br>"),
            flowLayout(
                numericInput("mmm", r("taxa_mmm"), value = 0, min = 0, max = 999),
                numericInput("fff", r("taxa_fff"), value = 0, min = 0, max = 999),
                numericInput("jjj", r("taxa_jjj"), value = 0, min = 0, max = 999),
                textInput("ind_rem",r("taxa_ind_rem"))
            ),
            HTML("<br>"),
            actionButton("submit", r("serv_subm"))
        )
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
