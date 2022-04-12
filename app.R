library(shiny)
library(readxl)
library(data.table)
library(ggplot2)
library(cowplot)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Storm Chaser"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("data", 
                      "Upload STORM Data (.xlsx)",
                      accept = ".xlsx",
                      multiple = FALSE),
            sliderInput("x_width",
                        "Select Box Width (nm)",
                        min = 0,
                        max = 3000,
                        value = 500, 
                        step = 10),
            sliderInput("y_height",
                        "Select Box Height (nm)",
                        min = 0,
                        max = 3000,
                        value = 50,
                        step = 10),
            radioButtons("show_box", "Show Box on Plot", inline = TRUE, choices = c("Hide", "Show")),
            actionButton("analyze", "Calculate Average Heads per ROI")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
            column(8, 
             plotOutput("main_plot", height = 500,
                        brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)
                 )
             ), 
            column(4,
             plotOutput("zoom_plot", height = 500)
            )
        ),
        fluidRow(
            plotOutput("histogram", height = 600)
        )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    storm <- reactiveValues(data = data.table())
    observe({
        req(input$data)
        storm_data <- read_excel(input$data$datapath)
        setDT(storm_data)
        old_names <- c("CenterX[µm]", "CenterY[µm]")
        new_names <- c("x", "y")
        setnames(storm_data, old_names, new_names)
        storm$data <- storm_data
    })
    
    observe({
        req(input$x_width, input$y_height, input$data)
        req(nrow(storm$data) > 0)
        
        #convert from nm input to um
        x_width <- input$x_width/1000
        y_height <- input$y_height/1000

        x_interval <- seq(0, max(storm$data$x), by = x_width)
        y_interval <- seq(0, max(storm$data$y), by = y_height)

        x_interval_start <- head(x_interval, -1)
        x_interval_stop <- tail(x_interval, -1)

        y_interval_start <- head(y_interval, -1)
        y_interval_stop <- tail(y_interval, -1)

        x_coord <- data.frame(start = x_interval_start,
                              stop = x_interval_stop)

        y_coord <- data.frame(start = y_interval_start,
                              stop = y_interval_stop)
        
        storm$x_coord <- x_coord
        storm$y_coord <- y_coord
    })
    
    
    plot_ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$main_plot <- renderPlot({
        req(nrow(storm$data) > 0)
       g1 <-  ggplot() +
                geom_point(data = storm$data, aes(x=x, y=y), shape = 16, alpha = 0.5, size = 1)+
                ylab("Y (microns)")+
                xlab("X (microns)")+
                ggtitle("Main")+
                theme_bw(20)
       if(input$show_box == "Show"){
           g1+
             geom_vline(aes(xintercept=c(0, storm$x_coord$stop)))+
             geom_hline(aes(yintercept=c(0, storm$y_coord$stop)))
       } else {
           g1
       }
    })
    
    output$zoom_plot <- renderPlot({
        req(nrow(storm$data) > 0)
        g2 <- ggplot()+
                geom_point(data = storm$data, aes(x=x, y=y), shape = 16, alpha = 0.5, size = 1)+
                ylab("Y (microns)")+
                xlab("X (microns)")+
                ggtitle("Zoom")+
                coord_cartesian(xlim = plot_ranges$x, ylim = plot_ranges$y, expand = FALSE)+
                theme_bw(20)
        
        if(input$show_box == "Show"){
            g2+
                geom_vline(aes(xintercept=c(0, storm$x_coord$stop)))+
                geom_hline(aes(yintercept=c(0, storm$y_coord$stop)))
        } else {
            g2
        }
           
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
        brush <- input$plot_brush
        if (!is.null(brush)) {
            plot_ranges$x <- c(brush$xmin, brush$xmax)
            plot_ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            plot_ranges$x <- NULL
            plot_ranges$y <- NULL
        }
    })
    
    observeEvent(input$analyze, {
        req(nrow(storm$data) > 0)
        req(storm$x_coord, storm$y_coord)
        all_molecules <- vector("list")
        molecules <- vector()
        withProgress(message = 'Analyzing...',
                     detail = 'This may take a while...', value = 0, {
        total_time <- nrow(storm$x_coord)*nrow(storm$y_coord)
        for(x in 1:nrow(storm$x_coord)){
            x_1 <- storm$x_coord$start[[x]]
            x_2 <- storm$x_coord$stop[[x]]
            for(y in 1:nrow(storm$y_coord)){
                y_1 <- storm$y_coord$start[[y]]
                y_2 <- storm$y_coord$stop[[y]]

                roi <- storm$data[x >= x_1 & x<=x_2 & y>=y_1 & y<=y_2]
                molecules[y] <- nrow(roi)
              
                incProgress(1/total_time, detail = paste0("Analyzing x = ", x, "; y = ", y))
            }
            all_molecules[[x]] <- molecules
        }

        storm$all_molecules <- unlist(all_molecules)
        
     })
    })
    
    output$histogram <- renderPlot({
        req(storm$all_molecules)
         ggplot()+
           geom_histogram(aes(x = storm$all_molecules), binwidth = 1, color = "black")+
             ggtitle("Distribution of heads per ROI")+
             xlab("Number of Heads")+
             annotate("text", x = Inf, y = Inf, 
                      label = paste0("Average: ", round(mean(storm$all_molecules), 2)),
                      hjust = 1.1,
                      vjust = 1.5,
                      size = 10)+
           theme_bw(20)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
