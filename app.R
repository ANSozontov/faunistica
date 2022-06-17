# initial -----------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(jsonlite)
species<- readxl::read_xlsx("../DB1.xlsx", sheet = "taxa")
adm    <- readxl::read_xlsx("../DB1.xlsx", sheet = "adm")
users  <- readxl::read_xlsx("../DB1.xlsx", sheet = "users")
publ   <- readxl::read_xlsx("../DB1.xlsx", sheet = "publications")

lang <- "en"
publ_id <- "publ1"
TR <- read_json("www/translation1.json")
attach(.GlobalEnv, name = "lang")
tr <- function(b){pluck(b, lang)}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    # h1("Faunistica"),

# Publication info --------------------------------------------------------    
verticalLayout(
        verticalLayout(
            h3(tr(TR$publ$h_pub), align = "center", class = "row2"), 
            if(str_detect(publ[publ$publ_id == publ_id,]$auth, "_")){
                p(paste0(tr(TR$publ$aut2), 
                         gsub("_", ", ", publ[publ$publ_id == publ_id,]$auth)))
            } else {
                p(paste0(tr(TR$publ$aut1), 
                         publ[publ$publ_id == publ_id,]$auth))
            },
            p(paste0(tr(TR$publ$name_publ), publ[publ$publ_id == publ_id,]$name)),
            p(paste0(tr(TR$publ$ref), publ[publ$publ_id == publ_id,]$ref)),
            actionButton("Change", tr(TR$publ$chg_publ)),
            HTML("<br>")
        )
    ),

# place_adm ---------------------------------------------------------------
    verticalLayout(
        tags$hr(),
        h3(tr(TR$pl.adm$h_pl.adm), align = "center"), # , align = "center"
        HTML("<br>"),
        flowLayout(
            textInput("Country",   tr(TR$pl.adm$adm0)),
            textInput("Region",    tr(TR$pl.adm$adm1)),
            textInput("District",  tr(TR$pl.adm$adm2))
        ),
        HTML("<br>"),
        flowLayout(actionButton("hold_adm", tr(TR$serv$hold)), 
            actionButton("unhold_adm", tr(TR$serv$unhold))
         )
    ),
# place_geo ---------------------------------------------------------------
verticalLayout(
    tags$hr(),
    h3(tr(TR$pl.geo$h_pl.geo), align = "center"), # , align = "center"
    HTML("<br>"),
    flowLayout(
        textInput("loc1",      tr(TR$pl.geo$loc)),
        textInput("loc2", tr(TR$pl.geo$loc2)),
        textInput("rem", tr(TR$pl.geo$geo.rem)),
        verticalLayout(
            HTML("<b>Coordinates</b>"),
            splitLayout(
                textInput("N", label = NULL, width = 300, placeholder = "WGS'84 only"),
                HTML("<b>N</b>"),
                cellWidths = c("80%", "20%")
            )
        ), 
        verticalLayout(
            HTML("<br>"),
            splitLayout(
                textInput("E", label = NULL, width = 250, placeholder = "WGS'84 only"), 
                HTML("<b>E</b>"),
                cellWidths = c("80%", "20%")
            ),
        ),
    ),
    HTML("<br>"),
    flowLayout(actionButton("hold_geo", tr(TR$serv$hold)), 
               actionButton("unhold_geo", tr(TR$serv$unhold))
    )
),
# event -------------------------------------------------------------------
verticalLayout(
    tags$hr(),
    h3(tr(TR$event$h_event), align = "center"), # , align = "center"
    HTML("<br>"),
    flowLayout(
        textInput("Habitat", tr(TR$event$hab)),
        dateInput("dat", tr(TR$event$date), startview = "month",),
        textInput("Effort", tr(TR$event$hab),
                  placeholder = tr(TR$event$eff_fill)),
        textInput("event_rem", tr(TR$event$event_rem))
    ), 
    HTML("<br>"),
    flowLayout(actionButton("hold_event", tr(TR$serv$hold)), 
               actionButton("unhold_event", tr(TR$serv$unhold))
    )
),
# Taxa --------------------------------------------------------------------
verticalLayout(
    tags$hr(),
    h3(tr(TR$taxa$h_taxa), align = "center", textcolor = "red"), 
    HTML("<br>"),
    flowLayout(
        textInput("Fam", tr(TR$taxa$fam)),
        textInput("Gen", tr(TR$taxa$gen)),
        textInput("Sp", tr(TR$taxa$sp)),
        checkboxInput("sp_n.def", tr(TR$taxa$n_def)),
    ),
    HTML("<br>"),
    flowLayout(actionButton("hold_taxa", tr(TR$serv$hold)), 
               actionButton("unhold_taxa", tr(TR$serv$unhold))
    )
),
# Abu ---------------------------------------------------------------------
verticalLayout(
    tags$hr(),
    h3(tr(TR$taxa$abu), align = "center", textcolor = "red"),
    HTML("<br>"),
    flowLayout(
        numericInput("mmm", tr(TR$taxa$mmm), value = 0, min = 0, max = 999),
        numericInput("fff", tr(TR$taxa$fff), value = 0, min = 0, max = 999),
        numericInput("jjj", tr(TR$taxa$jjj), value = 0, min = 0, max = 999),
        textInput("ind_rem",  tr(TR$taxa$ind_rem))
    ),
    HTML("<br>"),
    actionButton("submit", tr(TR$serv$subm))
),
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
    column(div(a("Language", href="#contact")), class = "navup", width = 1)
)

)


# Server ------------------------------------------------------------------
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
