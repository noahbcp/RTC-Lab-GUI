library(shiny)
library(shinyjs)
shinyjs::useShinyjs()


ui <- fluidPage(
    inputPanel(
        textInput('drug', label = "Drug Name"),
        selectInput('direction', label = "Direction", choices = list("Horizontal", "Vertical")),
        selectInput('replicates', label = 'Replicates', choices = list("Singlets" = 1, "Doublets" = 2, "Triplicates" = 3), selected = 3),
        selectInput('dilution', label = "Dilution Type", choices = list("Ten Fold", "Half Log")),
        sliderInput('concrange', max = -3, min = -15, step = 1, value = c(-11, -6), label = 'Concentration Range (M)'),
    ),
    actionButton('add', label = "Add Variable"),
    tableOutput('plate')
)

server <- function(input, output, session) {
 
    # Render plate
    plate <- function(data = NA) {
        mtx <- matrix(data = data, nrow = 8, ncol = 12, dimnames = list(LETTERS[1:8], 1:12))
        return (mtx)
    }
    
    output$plate <- renderTable(plate(), rownames = TRUE, na = "-")
    
    # Update plate
    observeEvent(input$add, {
        createPlate <- function(replicates, direction) {
            data <- list()
            if (direction == "Horizontal") {
                for (i in 1:12) {
                    data[[i]] <- c(1:replicates)
                }
            } else {
                for (i in 1:replicates) {
                    data[[i]] <- c(1:8)
                }
            }
            plate <- plate(data)
            return(plate)
        }
        #output$plate <- renderTable(createPlate(replicates = input$replicates, direction = input$direction))
        output$plate <- renderTable(createPlate(replicates = 3, direction = "Vertical"), rownames = TRUE, na = "-")
    })
    
    # Update slider input
    observeEvent(input$dilution, {
        if (input$dilution == "Half Log") {
            updateSliderInput(inputId = 'concrange', step = 0.5)
        } else {
            updateSliderInput(inputId = 'concrange', step = 1)
        }
    })
}

shinyApp(ui, server)