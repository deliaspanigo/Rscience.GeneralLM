
#' @export
GeneralLM_fix_slreg_RCode <- function(my_dataset, var_name_y, var_name_x, alpha_value){
  
  ### INIT CODE ###
  # # # # # Section 01 - Libraries ---------------------------------------------
  library("plotly")
  library("htmlwidgets")
  library("knitr")
  library("agricolae") # Tukey test
  library("dplyr")     # Developing with %>%
  library("openxlsx")  # Import files from xlsx
  library("plotly")    # Advanced graphical functions
  
  # # # # # Section 02 - Import dataset ----------------------------------------
  #---my_dataset <- _A_my_import_sentence_A_
  head(x = my_dataset, n = 5)
  
  # # # # # Section 03 - Settings ----------------------------------------------
  #---var_name_y     <- "_B_var_name_y_B_"
  #---var_name_x     <- "_B_var_name_x_B_"
  #---alpha_value    <- _B_alpha_value_B_
  

  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(my_dataset)
  vector_name_selected_vars <- c(var_name_y, var_name_x)
  vector_rol_vars <- c("VR", "X")
  
  
  
  
  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected vars. Only completed rows. Factor columns as factor object in R.
  minibase <- na.omit(my_dataset[vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars
  
  
  
  # # # # # Section 06 - Selected vars info as dataframe
  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars
  
  
  
  
  
  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minibase <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.numeric(minibase[,2]))
  )
  df_control_minibase
  
  
  
  # # # my_dataset and minibase reps
  # Our 'n' is from minibase
  df_show_n <- data.frame(
    "object" = c("my_dataset", "minibase"),
    "n_col" = c(ncol(my_dataset), ncol(minibase)),
    "n_row" = c(nrow(my_dataset), nrow(minibase))
  )
  df_show_n
  
  
  
  
  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_full <- lm(VR ~ X, data = minibase)
  summary_full <- summary(lm_full)
  df_table_reg <- as.data.frame(summary_full $coefficients) # Common anova table
  df_table_reg
  
  
  df_table_det_coef <- data.frame(
    "r.squared" =  summary_full$r.squared,
    "adj.r.squared" = summary_full$adj.r.squared,
    "f.obs" = summary_full$fstatistic[1],
    "df_num" = summary_full$fstatistic[2],
    "df_den" = summary_full$fstatistic[3]
  )
  df_table_det_coef$"p.value" <- pf(q = df_table_det_coef$"f.obs",
                                    df1 = df_table_det_coef$"df_num",
                                    df2 = df_table_det_coef$"df_den")
  rownames(df_table_det_coef) <- rep("", nrow(df_table_det_coef))
  
  
  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on my_dataset there are on minibase
  dt_rows_my_dataset_ok <- rowSums(!is.na(my_dataset[vector_name_selected_vars])) == length(vector_name_selected_vars)
  
  
  
  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_full$fitted.values
  minibase_mod$"residuals" <- lm_full$residuals
  minibase_mod$"studres" <- minibase_mod$"residuals"/sd(minibase_mod$"residuals")
  minibase_mod$"id_my_dataset" <- c(1:nrow(my_dataset))[dt_rows_my_dataset_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  
  
  
  
  
  
  # # # # # Section 08 - Requeriments for residuals-------------------------------
  # # # Normality test (Shapiro-Wilk)
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality
  
  
  
  
  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals
  
  
  
  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals
  
  detail_role <- c("VR", "X", "residuals")
  detail_name <- c(vector_name_selected_vars, "---")
  
  list_position <- lapply(1:length(detail_role), function(x){
    
    selected_role <- detail_role[x]
    
    data.frame(
      "rol_var" = selected_role,
      "var_name" = detail_name[x],
      "n" = length(minibase_mod[,selected_role]),
      "min" = min(minibase_mod[,selected_role]),
      "mean" = mean(minibase_mod[,selected_role]),
      "median" = median(minibase_mod[,selected_role]),
      "max" = max(minibase_mod[,selected_role])
    )
  })
  df_position <- do.call(rbind.data.frame, list_position)
  df_position
  
  
  
  
  list_dispersion <- lapply(1:length(detail_role), function(x){
    
    selected_role <- detail_role[x]
    
    data.frame(
      "rol_var" = detail_role[x],
      "var_name" = detail_name[x],
      "n" = length(minibase_mod[,selected_role]),
      "range" = max(minibase_mod[,selected_role]) - min(minibase_mod[,selected_role]),
      "variance" = var(minibase_mod[,selected_role]),
      "sd" = sd(minibase_mod[,selected_role])
    )
  })
  df_dispersion <- do.call(rbind.data.frame, list_dispersion)
  df_dispersion
  
  
  
  
  
  # Tabla plot 001
  df_table_plot001 <- df_position
  
  
  # Tabla plot 002
  df_table_plot002 <- df_position
  
  
  
  df_table_plot003 <- df_position
  
  
  df_table_plot004 <- df_position
  
  
  
  ###############
  
  plot001 <- plotly::plot_ly()
  
  plot001 <- add_trace(p = plot001,
                       x = minibase$X,
                       y = minibase$VR,
                       type = 'scatter',
                       mode = 'markers',
                       name = "data",
                       marker = list(size = 15, color = 'blue'))
  
  
  # Agregar la recta
  selected_slop <- df_table_reg[2,1]# Pendiente
  selected_constant <- df_table_reg[1,1]  # Ordenada al origen
  
  x_recta <- c(min(minibase$X), max(minibase$X))
  y_recta <- selected_slop * x_recta + selected_constant
  plot001 <- add_trace(p = plot001,
                       x = x_recta, y = y_recta,
                       type = 'scatter',
                       mode = 'lines',
                       name = 'slop',
                       line = list(width = 5, color = 'orange'))
  
  
  plot001 <- plotly::layout(p = plot001,
                            xaxis = list(title = "X"),
                            yaxis = list(title = "VR"),
                            title = "Plot 001 - Scatterplot XY",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  plot001 <- plotly::layout(p = plot001,
                            xaxis = list(zeroline = FALSE),
                            yaxis = list(zeroline = FALSE))
  
  # Mostrar el gráfico interactivo
  plot001
  
  
  
  
  
  # Crear el gráfico interactivo con Plotly
  
  plot002 <- plotly::plot_ly()
  
  plot002 <- add_trace(p = plot002,
                       x = minibase_mod$fitted.values,
                       y = minibase_mod$residuals,
                       type = 'scatter',
                       mode = 'markers',
                       name = "data",
                       marker = list(size = 15, color = 'blue'))
  
  
  # # Agregar la recta
  # selected_slop <- df_table_reg[2,1]# Pendiente
  # selected_constant <- df_table_reg[1,1]  # Ordenada al origen
  #
  # x_recta <- c(min(minibase$X), max(minibase$X))
  # y_recta <- selected_slop * x_recta + selected_constant
  # plot002 <- add_trace(p = plot002,
  #                      x = x_recta, y = y_recta,
  #                      type = 'scatter',
  #                      mode = 'lines',
  #                      name = 'slop',
  #                      line = list(width = 5, color = 'orange'))
  
  
  plot002 <- plotly::layout(p = plot002,
                            xaxis = list(title = "Fitted values"),
                            yaxis = list(title = "Residuals"),
                            title = "Plot 001 - Residuals vs. Fitted values",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  plot002 <- plotly::layout(p = plot002,
                            xaxis = list(zeroline = FALSE),
                            yaxis = list(zeroline = TRUE))
  
  # Mostrar el gráfico interactivo
  plot002
  
  
  
  
  plot003 <- plotly::plot_ly()
  
  plot003 <- add_trace(p = plot003,
                       x = minibase$X,
                       y = minibase$VR,
                       type = 'scatter',
                       mode = 'markers',
                       name = "data",
                       marker = list(size = 15, color = 'blue'))
  
  
  # Agregar la recta
  selected_slop <- df_table_reg[2,1]# Pendiente
  selected_constant <- df_table_reg[1,1]  # Ordenada al origen
  
  x_recta <- c(min(minibase$X), max(minibase$X))
  y_recta <- selected_slop * x_recta + selected_constant
  plot001 <- add_trace(p = plot003,
                       x = x_recta, y = y_recta,
                       type = 'scatter',
                       mode = 'lines',
                       name = 'slop',
                       line = list(width = 5, color = 'orange'))
  
  
  plot003 <- plotly::layout(p = plot003,
                            xaxis = list(title = "BLOCK"),
                            yaxis = list(title = "VR"),
                            title = "Plot 001 - Interacción Factor-Bloque",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  plot003 <- plotly::layout(p = plot003,
                            xaxis = list(zeroline = FALSE),
                            yaxis = list(zeroline = FALSE))
  
  # Mostrar el gráfico interactivo
  plot003
  
  
  
  
  plot004 <- plotly::plot_ly()
  
  plot004 <- add_trace(p = plot004,
                       x = minibase$X,
                       y = minibase$VR,
                       type = 'scatter',
                       mode = 'markers',
                       name = "data",
                       marker = list(size = 15, color = 'blue'))
  
  
  # Agregar la recta
  selected_slop <- df_table_reg[2,1]# Pendiente
  selected_constant <- df_table_reg[1,1]  # Ordenada al origen
  
  x_recta <- c(min(minibase$X), max(minibase$X))
  y_recta <- selected_slop * x_recta + selected_constant
  plot001 <- add_trace(p = plot004,
                       x = x_recta, y = y_recta,
                       type = 'scatter',
                       mode = 'lines',
                       name = 'slop',
                       line = list(width = 5, color = 'orange'))
  
  
  plot004 <- plotly::layout(p = plot004,
                            xaxis = list(title = "BLOCK"),
                            yaxis = list(title = "VR"),
                            title = "Plot 001 - Interacción Factor-Bloque",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  plot004 <- plotly::layout(p = plot004,
                            xaxis = list(zeroline = FALSE),
                            yaxis = list(zeroline = FALSE))
  
  # Mostrar el gráfico interactivo
  plot004
  
  ### END CODE ###
  
  #################################
  #####---
  
  #._ Capturar todos los objetos del entorno actual
  ._obj_names <- ls()
  
  #._ Filtrar para excluir los parámetros de la función
  ._obj_to_keep <- setdiff(._obj_names, names(formals(sys.function())))
  
  #._ Crear una lista con los objetos (excluyendo parámetros)
  ._result_list <- mget(._obj_to_keep)
  
  #._ Devolver la lista ordenada según su definición
  return(._result_list)
  
}

