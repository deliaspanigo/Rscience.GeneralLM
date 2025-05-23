
#' @export
GeneralLM_fix_slinreg_MM_output_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SUPER_OUTPUT"))
  
 
}

#' @export
GeneralLM_fix_slinreg_MM_output_server <- function(id, show_dev, 
                                                  mis_valores, active_TOOLS_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    OK_ALL_ACTIVE <- reactive({
      req(mis_valores())
      
      return(TRUE)
      
    })
    
    ############################################################################
    
     
    
    # En cuanto a renderUI, primero filtramos por grupo
    # Dentro de tu server, después de definir botones_info y el renderUI
    
   
    output$el_cartel <- renderUI({
      
      # internal_TOOLS_SELECTOR$ $selected_cartel
      mi_cartel <- active_TOOLS_SELECTOR$"pack_output"$selected_cartel
      fn_html_cartel(my_text = mi_cartel)
    })
    # style = "height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ajustes para evitar el scroll horizontal
    
    output$SUPER_OUTPUT <- renderUI({
      div(
      # style = "",  # Altura del contenedor (100% del contenedor padre)
        # card(
          # style = "height: 100%; display: flex; flex-direction: column;",  # Altura de la card (100% del contenedor padre)
          # card_header("Resultado"),
          # div(
            style = "flex-grow: 1; overflow-y: auto; height: 100%; width: 100%; max-width: 100%; box-sizing: border-box; overflow-x: hidden;",  # Ocupa el espacio restante y añade scroll si es necesario
            uiOutput(ns("el_cartel")),
            uiOutput(ns("mega_tabs"))#,
            # uiOutput(ns("mini_resumen")),
            # uiOutput(ns("botones_dinamicos")),
            # textOutput(ns("resultado")),
            # uiOutput(ns("resultado2"))
          # )
        )
      # )
      
    })
    ############################################################################
    
    list_vec01 <- list()
    list_vec01[[1]] <- list("title" = "1) References", "objects" = c("df_selected_vars"))
    list_vec01[[2]] <- list("title" = "2) Linear Regresion - Table", "objects" = c("df_table_reg"))
    list_vec01[[3]] <- list("title" = "3) R^2 and Ajusted R^2", "objects" = c("df_table_det_coef"))
    list_vec01[[4]] <- list("title" = "4) Position", "objects" = c("df_position"))
    list_vec01[[5]] <- list("title" = "5) Dispersion", "objects" = c("df_dispersion"))
    
    # Tab01 - Analysis
    output$dynamic_tab01_ui <- renderUI({
      req(mis_valores())
      fn_shiny_output_and_ui_1col(list_vec01, "render_tab01_", mis_valores, output, ns)
    })
    
    
    ############################################################################
    
    # Tab02 - Requeriments
    list_vec02 <- list()
    list_vec02[[1]] <- list("title" = "1) Requeriment - Normality test - Residuals", 
                            "objects" = c("test_residuals_normality"))
    
    list_vec02[[2]] <- list("title" = "2) Requeriment - Homogeneity visual evaluation - Residuals", 
                            "objects" = c("plot001"))

    
    
    output$dynamic_tab02_ui <- renderUI({
      req(mis_valores())
      fn_shiny_output_and_ui_1col(list_vec02, "render_tab02_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab03 - Plots - Raw Data
    list_vec03 <- list()
    list_vec03[[1]] <- list("title" = "", 
                            "table" = "df_table_plot001",
                            "plot"  = "plot002")
    
    list_vec03[[2]] <- list("title" = "", 
                            "table" = "df_table_plot002",
                            "plot"  = "plot003")
    
    
    
    output$dynamic_tab03_ui <- renderUI({
      req(mis_valores())
      
      fn_shiny_output_and_ui_2col(list_vec03, "render_tab03_", mis_valores, output, ns)
    })
    ##############################################################################
    

    

    output$dynamic_FULL_results_ui <- renderUI({
      req(mis_valores())
      
      # Tab05 - Summary Special
      the_names <- names(mis_valores())
      the_names <- na.omit(the_names)
      # the_names <- the_names[1:10]
      # Identifico plot y text
      # dt_plots <- grepl("^plot", the_names)
      # dt_text <- !dt_plots
      # the_names <- the_names[dt_text]
      # Ejemplo de vector con muchos nombres
      # Crear una variable lógica que diga TRUE cuando empieza con "plot"
      # the_names <- c("data1", "plotA", "data2", "plotB", "data3", "other", "plotC", "data4", "plotA", "plotA")
      
      # Indica si es plot
      # Indica si es plot
      
      list_separated <- fn_R_separate_vector2list_groups(vector_opt = the_names, 
                                             str_grepl = "^plot")
      # list_separated
      
      
      list_vec06 <- lapply(1:length(list_separated), function(x){
        the_title <- paste0("Part ", x, " of ", length(list_separated))
          
        list(title = the_title, 
           objects = list_separated[[x]])
          
      })
      
      # list_vec06 <- list()
      # list_vec06[[1]] <- list("title" = "1) Simple Linear Regresion", 
      #                         "objects" = the_names)
      
      
      fn_shiny_output_and_ui_1col(list_vec06, "render_tab06_", mis_valores, output, ns)
    })
    
    ##############################################################################
    # Tab05 - RCode
    
    output$shiny_ace_editor_MENU <- renderUI({
      req(mis_valores())
      
      #Rcode_script <- GeneralLM_fix_anova1_take_code(my_fn=GeneralLM_fix_anova1_RCode)
      # Calcular la altura adecuada para el editor basado en el número de líneas

      
      card(
        card_header("Editor Options"),
        card_body(
          
          fluidRow(
            column(3, 
                   selectInput(ns("theme"), "Editor Theme:", 
                               choices = c("xcode", "monokai", "github", "eclipse", "tomorrow", 
                                           "solarized_light", "solarized_dark", "textmate", "twilight"),
                               selected = "solarized_dark")),
            column(3,
                   sliderInput(ns("fontSize"), "Font Size:", min = 8, max = 40, value = 14, step = 1)
            ),
            column(3, downloadButton(ns("download_btn"), "Descargar como .R", icon = icon("download")))
            
          )
        )
      )
      
    })
    
    output$shiny_ace_CODE <- renderUI({
      req(mis_valores(), Rcode_script(), input$"theme", input$"fontSize")
      
      
      line_count <- length(strsplit(Rcode_script(), "\n")[[1]])
      line_count <- line_count + 5
      # Asignar aproximadamente 20px por línea para el alto del editor
      editor_height <- paste0(max(300, line_count * 20), "px")
      
            shinyAce::aceEditor(
              outputId = "script_part1",
              value = Rcode_script(),
              mode = "r",
              theme = input$"theme", #"chrome",
              height = editor_height,#"200px",
              fontSize = input$"fontSize", #14,
              showLineNumbers = TRUE,
              readOnly = TRUE,
              autoScrollEditorIntoView = TRUE,
              maxLines = 1000,  # Un número grande para evitar scroll
              minLines = line_count 
            )
      
    })
    
    output$shiny_ace_editor_OUTPUT <- renderUI({
      req(mis_valores())
      
     div(uiOutput(ns("shiny_ace_editor_MENU")),
         uiOutput(ns("shiny_ace_CODE")))
      
    })
    
    # Función para descargar el código como archivo .R
    output$download_btn <- downloadHandler(
      filename = function() {
        "code_generalLM_fixed_simple_lin_reg.R"
      },
      content = function(file) {
        writeLines(Rcode_script(), file)
      }
    )
    
    
   
    
    ############################################################################
    
    # Tab06 - Quarto
    the_quarto_file_path <- reactive({
      req(mis_valores())
      GeneralLM_fix_anova1_quarto_file_path()
    })
    
    module_quartoRenderer_server(id="quarto_doc", documento = the_quarto_file_path(), 
                                 Rcode_script = Rcode_script)
    
    output$dynamic_download_quarto <- renderUI({
      req(mis_valores())
      module_quartoRenderer_ui(id=ns("quarto_doc"))
    })
    
    ############################################################################
    output$mega_tabs <- renderUI({
      req(mis_valores())
      
      tabsetPanel(
        tabPanel(title = "Analysis",          uiOutput(ns("dynamic_tab01_ui"))),
        tabPanel(title = "Requeriments",      uiOutput(ns("dynamic_tab02_ui"))),
        tabPanel(title = "Plots",             uiOutput(ns("dynamic_tab03_ui"))),
        tabPanel(title = "Full Results",      uiOutput(ns("dynamic_FULL_results_ui")))
      )
      
    })
    ############################################################################

    
    

  })
}