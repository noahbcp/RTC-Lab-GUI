library(shiny)
library(shinyBS)
library(shinyjs)
library(readr)
library(openxlsx)
library(stringr)
library(tools)
options(readr.show_col_types = FALSE)

ui <- fluidPage(
useShinyjs(),
    verticalLayout(
        fileInput(inputId = "data_file",
                  label = "Upload data:",
                  multiple = TRUE,
                  accept = c(".csv", ".xlsx")),
        tableOutput(outputId = "uploaded_files"),
        numericInput(inputId = "n_cycles",
                     label = "Number of cycles:",
                     value = 60),
        bsButton(inputId = "go_button",
                 label = "Process",
                 disabled = TRUE),
        downloadButton(outputId = "dl_button",
                       label = "Download!"
        )
    )
)

server <- function(input, output, session) {
#-----Vars-----
    processed_data <- list()
#-----Closures-----
    batcher <- local({
        file_int <- 1
        list(
            file_int = function() {
                return(file_int)
            },
            increment_file_int = function() {
                file_int <<- file_int + 1
            }
        )
    })
    datahandler <- local({
        data <- list()
        pos_row <- c()
        pos_col <- c()
        n_cycles <- c()
        list(
            view_data = function(format) {
                if (format == "csv") {
                    data <<- read_csv(
                        input$data_file$datapath[batcher$file_int()],
                        col_names = c(as.character(1:13)))
                }
                if (format == "xlsx") {
                    data <- openxlsx::read.xlsx(
                        input$data_file$datapath[batcher$file_int()],
                        colNames = FALSE,
                        skipEmptyRows = FALSE,
                        rows = c(2:1048576) #max number of possible rows
                    )
                    colnames(data) <- c(as.character(1:13))
                    data <<- data
                }
            },
            find_data = function() {
                return((which(data == "A", arr.ind = TRUE))[1])
            },
            find_triplicate = function() {
                return(which(!is.na(data[datahandler$find_data(), 1:13]))[2])
            },
            fetch_exp = function() {
                pos_row <<- datahandler$find_data()
                pos_col <<- datahandler$find_triplicate()
                n_cycles <<- input$n_cycles
            },
            calculate_bret = function(batch = FALSE) {
                #Loop to fetch wavelength A
                datalist_wavelength_a <- list()
                pos_row_a <- pos_row
                pos_col_a <- pos_col
                for (i in 1:n_cycles) {
                    wavelength_a <- data[(pos_row_a:(pos_row_a + 7)),
                                         (pos_col_a:(pos_col_a + 2))]
                    wavelength_a <- as.vector(t(wavelength_a))
                    pos_row_a <- (pos_row_a + 23)
                    datalist_wavelength_a[[i]] <- as.numeric(wavelength_a)
                }
                #Loop to fetch wavelength B
                datalist_wavelength_b <- list()
                pos_row_b <- pos_row
                pos_col_b <- pos_col
                for (i in 1:n_cycles) {
                    wavelength_b <- data[((pos_row_b + 11):((pos_row_b + 7) + 11)),
                                         (pos_col_b:(pos_col_b + 2))]
                    wavelength_b <- as.vector(t(wavelength_b))
                    pos_row_b <- (pos_row_b + 23)
                    datalist_wavelength_b[[i]] <- as.numeric(wavelength_b)
                }
                #Calculate BRET ratio (wavelength A / wavelength B)
                datalist <- list(1:n_cycles)
                i <- 1
                for (i in 1:n_cycles) {
                    datalist[[i]] <- (datalist_wavelength_a[[i]] /
                                          datalist_wavelength_b[[i]])
                }
                datalist <- as.data.frame(do.call(rbind, datalist))
                return(datalist)
            }
        )
    })
#-----UI Control-----
    output$uploaded_files <- renderTable({
        files <- input$data_file
        files$name
    },
    striped = TRUE,
    rownames = TRUE,
    colnames = FALSE)
    observe({
        if (is.null(input$data_file)) {
            shinyjs::disable("go_button")
        } else {
            shinyjs::enable("go_button")
        }
    })
    observeEvent(input$go_button, {
        while (batcher$file_int() <= length(input$data_file$datapath)) {
            datahandler$view_data(
                format = file_ext(input$data_file$datapath)[batcher$file_int()])
            datahandler$fetch_exp()
            processed_data[[batcher$file_int()]] <<- datahandler$calculate_bret()
            batcher$increment_file_int()
        }
        names(processed_data) <- c(str_trunc(input$data_file$name, 31, "right"))
        output$dl_button <- downloadHandler(
            filename = function() {
                paste("camyen-", Sys.Date(), ".xlsx", sep = "")
            },
            content = function(file) {
                openxlsx::saveWorkbook(
                    wb = openxlsx::buildWorkbook(processed_data),
                    file = file
                )
            }
        )
    })
}

shinyApp(ui, server)