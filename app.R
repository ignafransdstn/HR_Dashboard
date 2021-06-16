library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(googledrive)
library(readxl)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(reshape2)

#Get Data
drive_auth(email = "balikpapanhr@gmail.com")
drive_find()
get_data <- drive_download("medical_record_hr_ibo", path = NULL, type = NULL, overwrite = TRUE, verbose = TRUE)
data_set <- read_xlsx("medical_record_hr_ibo.xlsx", 
                       sheet = "db_all")
data_raw <- select(data_set, "Vendor", "Kategori", "Invoice_No", "Karyawan", "Pasien", "Poli", "Nominal")

#Main Panel (Grafik) #olah data
data_grafik <- select(data_set, "Vendor", "Nominal", "Paid_Date")
data_grafik <- as_data_frame(data_grafik)
data_grafik <- data_grafik[, c(1, 3, 2)]
data_grafik$Paid_Date <- as.Date(c(data_grafik$Paid_Date), format = "%Y-%m-%d")
data_grafik$Nominal <- as.double(data_grafik$Nominal)
data_grafik <- data_grafik %>%
    replace_na(list(Nominal = 0))

#Grafik Bulanan
data_grafik_bulanan <- data_grafik %>%
    group_by(month = floor_date(Paid_Date, "month")) %>%
    summarize(Total = sum(Nominal))

#Main Panel Grafik #visualisasi
result_nominal <- ggplot(data_grafik_bulanan, aes(x = month, y = Total, fill = Total)) +
    geom_col() +
    scale_y_continuous(breaks = c(150000000, 125000000, 100000000, 75000000, 50000000, 25000000), labels = number_format(accuracy = 1.00)) +
    scale_x_date(breaks = date_breaks("months"),
                 labels = date_format("%b")) +
    scale_fill_continuous(breaks = c(150000000, 125000000, 100000000,75000000, 50000000, 25000000), labels = number_format(accuracy = 1.00), low = "yellow", high = "red") +
    labs(
        x = "Bulan",
        y = "Juta Rupiah",
        title = "Grafik Nominal",
        subtitle = "HR Data Info",
        caption = "Sumber data : HR Team Balikpapan"
    ) +
    theme_ipsum(
        base_size = 13,
        plot_title_size = 21
    ) +
    theme(plot.title.position = "plot",
          axis.title.x = element_text(color = "#DC143C", size=14, face="bold.italic", angle = 0, hjust = 0.5),
          axis.title.y = element_text(color = "#DC143C", size=14, face="bold.italic", angle = 90, vjust = 0.5)
    )

#Grafik out patient dan in patient
in_out_patient <- data_set[data_set$Kategori %in% c("OUT PATIENT BPN", "IN PATIENT BPN"), ]
in_out_patient <- select(in_out_patient, "Kategori", "Nominal", "Paid_Date" )
in_out_patient <- in_out_patient[, c(1, 3, 2)]
in_out_patient$Kategori <- as.factor(in_out_patient$Kategori)

in_patient <- filter(in_out_patient, Kategori == "IN PATIENT BPN")
in_patient$Nominal <- as.double(in_patient$Nominal)
in_patient_week <- in_patient %>%
    group_by(month = floor_date(Paid_Date, "month")) %>%
    summarize(Total = sum(Nominal))
in_patient_week <- in_patient_week %>%
    replace_na(list(Total = 0))

out_patient <- filter(in_out_patient, Kategori == "OUT PATIENT BPN")
out_patient$Nominal <- as.double(out_patient$Nominal)
out_patient_week <- out_patient %>%
    group_by(month = floor_date(Paid_Date, "month")) %>%
    summarize(Total = sum(Nominal))
out_patient_week <- out_patient_week %>%
    replace_na(list(Total = 0))

in_out_join <- merge(in_patient_week, out_patient_week, by = "month")
in_out_join <- reshape2::melt(in_out_join, id.var = 'month')
in_out_join$month <- as.Date(c(in_out_join$month), format = "%Y-%m-%d")

result_in_out <- ggplot(in_out_join, aes(x = month, y = value, col = variable)) +
    geom_line(size = 1) +
    scale_x_date(breaks = date_breaks("months"),
                 labels = date_format("%b")) +
    scale_y_continuous(labels = number_format(accuracy = 1.00)) +
    scale_colour_discrete(name = "Legend", labels =  c("In Patient", "Out Patient")) +
    labs(
        x = "Bulan",
        y = "Juta Rupiah",
        title = "Grafik In dan Out Patient",
        subtitle = "HR Data Info",
        caption = "Sumber data : HR Team Balikpapan"
    ) +
    theme_ipsum(
        base_size = 13,
        plot_title_size = 21
    ) +
    theme(plot.title.position = "plot",
          axis.title.x = element_text(color = "#DC143C", size=14, face="bold.italic", angle = 0, hjust = 0.5),
          axis.title.y = element_text(color = "#DC143C", size=14, face="bold.italic", angle = 90, vjust = 0.5)
    )

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Username/Password salah",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         tags$code("HR 4.0 2020")
                     ))
)

credentials = data.frame(
    username_id = c("myuser", "myuser1"),
    passod   = sapply(c("mypass", "mypass1"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
)

header <- dashboardHeader( title = "HR Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            sidebarMenu(
                menuItem("Data Base", tabName = "dashboard", icon = icon("address-card")),
                menuItem("Grafik", tabName = "grafik", icon = icon("chart-bar"))
            )
        }
    })

    output$body <- renderUI({
        if (USER$login == TRUE ) {
            tabItems(
            tabItem(tabName ="dashboard", class = "active",
                    fluidRow(
                        box(width = 12, dataTableOutput('results'))
                    )
                    ),
            tabItem(tabName = "grafik",
                    fluidRow(
                        column(6, plotOutput(outputId = "nominal", width = "750px", height = "500px"))
                    ),
                    fluidRow(
                        column(6, plotOutput(outputId = "nominal2", width = "750px", height = "500px"))
                    )
                    )
            )
        }
        else {
            loginpage
        }
    })
    
    output$results <-  DT::renderDataTable({
        DT::datatable(data_raw, options = list(autoWidth = TRUE,
                                       searching = TRUE))
    })
    
    output$nominal <- renderPlot({
        result_nominal
    })
    
    output$nominal2 <- renderPlot({
        result_in_out
    })
    
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
