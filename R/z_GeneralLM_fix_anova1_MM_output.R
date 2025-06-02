
#' @export
GeneralLM_fix_anova1_MM_output_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SUPER_OUTPUT"))
  
 
}

#' @export
GeneralLM_fix_anova1_MM_output_server <- function(id, mis_valores, active_TOOLS_SELECTOR) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    OK_ALL_ACTIVE <- reactive({
      req(mis_valores(), active_TOOLS_SELECTOR, active_TOOLS_SELECTOR$"check_output")
      
      return(TRUE)
      
    })
    
    ############################################################################
    
    
    crear_outputs_y_ui  <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
      # Crea los outputs en bucle, en ámbitos independientes para evitar sobrescrituras
      for (i in seq_along(list_objetos)) {
        id_output <- paste0(prefix, i)
        obj_name <- list_objetos[[i]]$"objects"
        
        # Crear un ámbito local para que cada output sea independiente
        local({
          n <- i
          id <- id_output
          obj <- obj_name
          output[[id]] <- renderPrint({
            req(mis_valores_reactive())
            mis_valores_reactive()[obj]
          })
        })
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(list_objetos), function(i) {
        id_output <- paste0(prefix, i)
        list(
          h4(list_objetos[[i]]$"title"),
          shinycssloaders::withSpinner(verbatimTextOutput(ns(id_output))),
          br()
        )
      })
      
      return(do.call(tagList, ui_list))
    }
    
    crear_outputs_y_ui2 <- function(list_objetos, prefix, mis_valores_reactive, output, ns) {
      # Crear los outputs en ámbitos independientes
      for (i in seq_along(list_objetos)) {
        id_output_table <- paste0(prefix, i, "_table")
        id_output_plot <- paste0(prefix, i, "_plot")
        
        obj_name_table <- list_objetos[[i]]$"table"
        obj_name_plot <- list_objetos[[i]]$"plot"
        
        local({
          n <- i
          id_table <- id_output_table
          id_plot <- id_output_plot
          obj_table <- obj_name_table
          obj_plot <- obj_name_plot
          
          output[[id_plot]] <- plotly::renderPlotly({
            req(mis_valores_reactive())
            mis_valores_reactive()[[obj_plot]]
          })
          
          output[[id_table]] <- renderPrint({
            req(mis_valores_reactive())
            mis_valores_reactive()[obj_table]
          })
        })
      }
      
      # Crear UI dinámicamente
      ui_list <- lapply(seq_along(list_objetos), function(i) {
        
        obj_name_plot <- list_objetos[[i]]$"plot"
        id_output_table <- paste0(prefix, i, "_table")
        id_output_plot <- paste0(prefix, i, "_plot")
        list(
          # h4(list_objetos[[i]]$"title"),
          # HTML(paste0("<b><u>R plot object:</u></b> ", obj_name_plot, collapse ="")),
          div(
            HTML(paste0("<b><u>R plot object:</u></b> ", obj_name_plot)),
            fluidRow(
              column(6, shinycssloaders::withSpinner(plotly::plotlyOutput(ns(id_output_plot)))),
              column(6, shinycssloaders::withSpinner(verbatimTextOutput(ns(id_output_table))))
            ),
            hr()
          )
        )
      })
      
      return(do.call(tagList, ui_list))
    }
    

    ############################################################################
    
    output$df_summary_anova <- DT::renderDataTable({
      req(mis_valores())
      tabla_p <- mis_valores()$"df_summary_anova"
      validate(need(!is.null(tabla_p), "No hay datos disponibles."))
      datatable(
        tabla_p,
        rownames = FALSE,
        editable = FALSE,
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          info = FALSE,
          autoWidth = TRUE
        ),
        class = 'stripe hover compact'
      ) %>% formatStyle(
        columns = names(tabla_p),
        target = 'cell',
        backgroundColor = 'white'
      )
    })
    
    output$the_plot <- plotly::renderPlotly({
      req(mis_valores())
      my_plot <- mis_valores()$"plot007_factor"
      my_plot
    })
    
    output$mini_resumen <- renderUI({
      req(mis_valores())
      
      div(
        class = "d-flex",  # Contenedor flexible
        style = "height: 90%",  # Altura del contenedor principal (80% de la ventana)
        div(
            style = "flex: 0 0 50%; max-width: 50%; padding: 10px; height: 50%;",
        card(
          card_header("Summary anova"),
          card_body(
            div(
              DTOutput(ns("df_summary_anova"))
            )
          )
        ),
        card(
          card_header("Interpretation"),
          card_body(
            div(
              div(
                tags$div(mis_valores()$"phrase_shapiro_selected"),
                tags$div(mis_valores()$"phrase_bartlett_selected"),
                tags$div(mis_valores()$"phrase_requeriments_selected"),
                tags$div(mis_valores()$"phrase_anova_selected")
              )
            )
          )
        )
        ),
        div(
          style = "flex: 0 0 50%; max-width: 50%; padding: 10px; height: 50%;",
        card(
          card_header("Plot"),
          card_body(
            div(
              plotly::plotlyOutput(ns("the_plot"))
            )
          )
        )
        )
      )
      
      
      
      
    })
    
    ############################################################################
    
    
    
    
    output$el_cartel <- renderUI({
      OK_ALL_ACTIVE()
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
    
    output$"mega_tabs" <- renderUI({
      
      bslib::navset_card_tab(
        title = "output",
        id = ns("mynav2"),
        height = "100%",  # Especificar altura explícitamente
          bslib::nav_panel(title = "Analysis",
                           uiOutput(ns("dynamic_tab01_ui"))
          ),
          bslib::nav_panel(title = "Requeriments",
                           uiOutput(ns("dynamic_tab02_ui"))
          ),
          bslib::nav_panel(title = "Plots - Raw Data",
                           uiOutput(ns("dynamic_tab03_ui"))
          ),
          bslib::nav_panel(title = "Plots - Residuals",
                           uiOutput(ns("dynamic_tab04_ui"))
          ),
          bslib::nav_panel(title = "Summary",
                           uiOutput(ns("dynamic_tab05_ui"))
          ),
          bslib::nav_panel(title = "Full Results",
                           uiOutput(ns("dynamic_tab06_ui"))
          )
      )
      
    })
    
    ############################################################################
    list_vec01 <- list()
    list_vec01[[1]] <- list("title" = "1) References", "objects" = c("df_selected_vars", "alpha_value"))
    list_vec01[[2]] <- list("title" = "2) Factor resumen", "objects" = c("df_factor_info", "check_unbalanced_reps", "phrase_selected_unbalanced"))
    list_vec01[[3]] <- list("title" = "3) Anova 1 way - Table", "objects" = c("df_table_anova"))
    list_vec01[[4]] <- list("title" = "4) Multiple comparation test (Tukey)", "objects" = c("df_tukey_table"))
    list_vec01[[5]] <- list("title" = "5) Model Error", "objects" = c("df_model_error"))
    
    # Tab01 - Analysis
    output$dynamic_tab01_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec01, "render_tab01_", mis_valores, output, ns)
    })
    
    
    ############################################################################
    
    # Tab02 - Requeriments
    list_vec02 <- list()
    list_vec02[[1]] <- list("title" = "1) Requeriment - Normaility test - Residuals", 
                            "objects" = c("test_residuals_normality"))
    
    list_vec02[[2]] <- list("title" = "2) Requeriment - Homogeneity test - Residuals", 
                            "objects" = c("test_residuals_homogeneity"))
    list_vec02[[3]] <- list("title" = "3) Estimated variances - Residuals", 
                            "objects" = c("df_residuals_variance_levels"))
    
    
    output$dynamic_tab02_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec02, "render_tab02_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab03 - Plots - Raw Data
    list_vec03 <- list()
    list_vec03[[1]] <- list("title" = "", 
                            "table" = "df_table_factor_plot001",
                            "plot"  = "plot001_factor")
    
    list_vec03[[2]] <- list("title" = "", 
                            "table" = "df_table_factor_plot002",
                            "plot"  = "plot002_factor")
    
    list_vec03[[3]] <- list("title" = "", 
                            "table" = "df_table_factor_plot003",
                            "plot"  = "plot003_factor")
    
    list_vec03[[4]] <- list("title" = "", 
                            "table" = "df_table_factor_plot004",
                            "plot"  = "plot004_factor")
    
    list_vec03[[5]] <- list("title" = "", 
                            "table" = "df_table_factor_plot005",
                            "plot"  = "plot005_factor")
    
    list_vec03[[6]] <- list("title" = "", 
                            "table" = "df_table_factor_plot006",
                            "plot"  = "plot006_factor")
    
    list_vec03[[7]] <- list("title" = "", 
                            "table" = "df_table_factor_plot007",
                            "plot"  = "plot007_factor")
    
    output$dynamic_tab03_ui <- renderUI({
      req(mis_valores())
      
      crear_outputs_y_ui2(list_vec03, "render_tab03_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab04 - Plots - Residuals
    list_vec04 <- list()
    list_vec04[[1]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot001",
                            "plot"  = "plot001_residuals")
    
    list_vec04[[2]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot002",
                            "plot"  = "plot002_residuals")
    
    list_vec04[[3]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot003",
                            "plot"  = "plot003_residuals")
    
    list_vec04[[4]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot004",
                            "plot"  = "plot004_residuals")
    
    list_vec04[[5]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot005",
                            "plot"  = "plot005_residuals")
    
    list_vec04[[6]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot006",
                            "plot"  = "plot006_residuals")
    
    list_vec04[[7]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot007",
                            "plot"  = "plot007_residuals")
    
    list_vec04[[8]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot008",
                            "plot"  = "plot008_residuals")
    
    list_vec04[[9]] <- list("title" = "", 
                            "table" = "df_table_residuals_plot009",
                            "plot"  = "plot009_residuals")
    
    list_vec04[[10]] <- list("title" = "", 
                             "table" = "df_table_residuals_plot010",
                             "plot"  = "plot010_residuals")
    
    output$dynamic_tab04_ui <- renderUI({
      req(mis_valores())
      
      crear_outputs_y_ui2(list_vec04, "render_tab04_", mis_valores, output, ns)
    })
    ##############################################################################
    
    # Tab05 - Summary Special
    list_vec05 <- list()
    list_vec05[[1]] <- list("title" = "1) Summary Anova", 
                            "objects" = c("df_summary_anova", "phrase_shapiro_selected",
                                          "phrase_bartlett_selected", "phrase_requeriments_selected",
                                          "phrase_anova_selected"))
    

    output$dynamic_tab05_ui <- renderUI({
      req(mis_valores())
      crear_outputs_y_ui(list_vec05, "render_tab05_", mis_valores, output, ns)
    })
    
    ##############################################################################
    
    

    output$dynamic_tab06_ui <- renderUI({
      req(mis_valores())
      
      # Tab05 - Summary Special
      the_names <- names(mis_valores())
      the_names <- na.omit(the_names)
      # the_names <- the_names[1:10]
      # Identifico plot y text
      dt_plots <- grepl("^plot", the_names)
      dt_text <- !dt_plots
      the_names <- the_names[dt_text]

      resultados <- mis_valores()
      
      # Asignar salidas dinámicas
      for (nombre in names(resultados)) {
        if (startsWith(nombre, "plot")) {
          local({
            nm <- nombre
            output[[nm]] <- plotly::renderPlotly({ resultados[[nm]]} )  # Corrección de cierre
          })
        } else {
          local({
            nm <- nombre
            output[[nm]] <- renderPrint({ resultados[nm]} )
          })
        }
      }
      
      ui_list <- list()
      for (nombre in names(resultados)) {
        new_ui <- c()
        if (startsWith(nombre, "plot")) {
          new_ui <- div(
            HTML(paste0("<b><u>R plot object:</u></b> ", nombre)),
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(nombre)))
          )
        } else {
          new_ui <- div(
            shinycssloaders::withSpinner(verbatimTextOutput(ns(nombre)))
          )
        }
        
        new_ui<- div(new_ui, hr())
        
        ui_list[[length(ui_list) + 1]] <- new_ui
      }
      
      div(
        h2("All results"),
        do.call(tagList, ui_list)
      )

    })
    
    ##############################################################################
    # Tab05 - RCode
 
     
    

  })
}