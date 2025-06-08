
#' @export
Rs_LM_fix_anova_02_1way1block_RCode <- function(my_dataset, var_name_rv, var_name_factor, var_name_block, alpha_value){
  
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
  #---var_name_rv     <- "_B_var_name_rv_B_"
  #---var_name_factor <- "_B_var_name_factor_B_"
  #---var_name_block <- "_B_var_name_block_B_"
  #---alpha_value     <- _B_alpha_value_B_

  # # # # # Section 04 - Standard actions --------------------------------------
  # The factor must be factor data type on R.
  my_dataset[,var_name_factor] <- as.factor(as.character(my_dataset[,var_name_factor]))
  
  # # # # # Section 05 - Alpha and confidence value ----------------------------
  confidence_value <- 1 - alpha_value
  
  df_alpha_confidence <- data.frame(
    "order" = 1:2,
    "detail" = c("alpha value", "confidence value"),
    "probability" = c(alpha_value, confidence_value),
    "percentaje" =  paste0(c(alpha_value, confidence_value)*100, "%")
  )
  df_alpha_confidence

  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(my_dataset)
  vector_name_selected_vars <- c(var_name_rv, var_name_factor, var_name_block)
  vector_rol_vars <- c("VR", "FACTOR", "BLOCK")
  
  
  
  
  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected vars. Only completed rows. Factor columns as factor object in R.
  minibase <- na.omit(my_dataset[vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars
  minibase[,2] <- as.factor(as.character(minibase[,2]))
  minibase[,3] <- as.factor(as.character(minibase[,3]))
  minibase$"COMBINATION" <- paste0(minibase[,2], "_", minibase[,3])
  minibase$"COMBINATION" <- as.factor(as.character(minibase$COMBINATION))
  
  
  
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
    "control" = c("is.numeric()", "is.factor()", "is.factor()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]), is.factor(minibase[,3]))
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
  
  
  
  # # # Factor info
  # Default order for levels its alphabetic order.
  df_factor_info <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "color" = rainbow(nlevels(minibase[,2]))
  )
  df_factor_info
  
  
  df_block_info <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "n" = as.vector(table(minibase[,3])),
    "mean" = tapply(minibase[,1], minibase[,3], mean),
    "color" = rainbow(nlevels(minibase[,3]))
  )
  df_block_info
  
  
  
  df_combination_info <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = as.vector(table(minibase[,4])),
    "mean" = tapply(minibase[,1], minibase[,4], mean),
    "color" = rainbow(nlevels(minibase[,4]))
  )
  
  
  
  
  # # # Unbalanced reps for levels for FACTOR?
  # Important information for Tukey.
  # If reps its equal or not equal in all levels must be detailled
  # on Tukey.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps
  
  
  
  
  
  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_anova <- lm(VR ~ FACTOR + BLOCK, data = minibase)               # Linear model
  aov_anova <- aov(lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(aov_anova)[[1]])   # Common anova table
  df_table_anova
  
  
  
  # # # Standard error from model for each level
  model_error_var <- df_table_anova$`Mean Sq`[3]
  model_error_sd <- sqrt(model_error_var)
  
  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var" = model_error_var,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error
  
  
  
  
  
  # # # # # Section 09 - Tukey --------------------------------------------------
  # # # Tukey test - Tukey with groups - Full version
  tukey01_full_groups <- agricolae::HSD.test(y = lm_anova,
                                             trt = colnames(minibase)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)
  
  
  
  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = lm_anova,
                                            trt = colnames(minibase)[2],
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)
  
  
  
  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table
  
  
  
  # # # New table about Tukey
  df_tukey_table <- data.frame(
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
  df_tukey_table
  
  
  
  
  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on my_dataset there are on minibase
  dt_rows_my_dataset_ok <- rowSums(!is.na(my_dataset[vector_name_selected_vars])) == length(vector_name_selected_vars)
  
  
  
  # # # Object minibase_mod and new cols
  minibase_mod <- minibase
  minibase_mod$"order_number_lvl_factor" <- as.numeric(minibase_mod[,2])
  minibase_mod$"color_lvl_factor" <- df_factor_info$color[minibase_mod$"order_number_lvl_"]
  minibase_mod$"order_number_lvl_block" <- as.numeric(minibase_mod[,3])
  minibase_mod$"color_lvl_block" <- df_block_info$color[minibase_mod$"order_number_lvl_block"]
  minibase_mod$"order_number_lvl_combination" <- as.numeric(minibase_mod[,4])
  minibase_mod$"color_lvl_combination" <- df_combination_info$color[minibase_mod$"order_number_lvl_combination"]
  
  
  minibase_mod$"fitted.values" <- df_combination_info$"mean"[minibase_mod$"order_number_lvl_combination"]
  minibase_mod$"residuals" <- lm_anova$residuals
  minibase_mod$"id_my_dataset" <- c(1:nrow(my_dataset))[dt_rows_my_dataset_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"studres" <- minibase_mod$"residuals"/model_error_sd
  
  
  
  
  
  # # # # # Section 08 - Requeriments for residuals-------------------------------
  # # # Normality test (Shapiro-Wilk)
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality
  
  
  
  
  # # # Homogeinidy test (Bartlett)
  test_residuals_homogeneity <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
  test_residuals_homogeneity
  
  
  
  # # # Residuals variance from levels from original residuals
  df_residuals_variance_factor_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "factor" = levels(minibase_mod[,2]),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_variance_factor_levels
  
  
  
  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals
  
  
  
  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals
  
  
  # # # # # Section 10 - Partitioned Measures (VR) for FACTOR --------------------------------
  # # # Partitioned Measures Position (VR) for FACTOR
  df_vr_position_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = tapply(minibase[,1], minibase[,2], length),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "Q1" = tapply(minibase[,1], minibase[,2], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "Q3" = tapply(minibase[,1], minibase[,2], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,2], max)
  )
  
  
  
  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = tapply(minibase[,1], minibase[,2], length),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))})
  )
  df_vr_dispersion_levels_factor
  
  
  
  
  # # # # # Section 11 - Partitioned Measures (VR) for BLOCK --------------------------------
  # # # Partitioned Measures Position (VR) for BLOCK
  df_vr_position_levels_block <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "min" = tapply(minibase[,1], minibase[,3], min),
    "mean" = tapply(minibase[,1], minibase[,3], mean),
    "Q1" = tapply(minibase[,1], minibase[,3], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,3], median),
    "Q3" = tapply(minibase[,1], minibase[,3], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,3], max),
    "n" = tapply(minibase[,1], minibase[,3], length)
  )
  
  
  
  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_block <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "range" = tapply(minibase[,1], minibase[,3], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,3], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,3], sd),
    "standard_error" = tapply(minibase[,1], minibase[,3], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,3], length)
  )
  df_vr_dispersion_levels_block
  
  
  
  
  # # # # # Section 11 - Partitioned Measures (VR) for BLOCK --------------------------------
  # # # Partitioned Measures Position (VR) for BLOCK
  df_vr_position_levels_combination <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = tapply(minibase[,1], minibase[,4], length),
    "min" = tapply(minibase[,1], minibase[,4], min),
    "mean" = tapply(minibase[,1], minibase[,4], mean),
    "Q1" = tapply(minibase[,1], minibase[,4], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,4], median),
    "Q3" = tapply(minibase[,1], minibase[,4], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,4], max)
  )
  
  
  
  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_combiantion <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = tapply(minibase[,1], minibase[,4], length),
    "range" = tapply(minibase[,1], minibase[,4], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,4], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,4], sd),
    "standard_error" = tapply(minibase[,1], minibase[,4], function(x){sd(x)/sqrt(length(x))})
  )
  df_vr_dispersion_levels_combiantion
  
  
  
  # # # # # Section 12 - General Measures (VR) -----------------------------------
  # # # General Measures of Position (VR)
  df_vr_position_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )
  df_vr_position_general
  
  
  
  # # # General Measures of Dispersion (VR)
  df_vr_dispersion_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )
  df_vr_dispersion_general
  
  
  
  
  
  # # # # # Section 12 - Partitioned Measures (Residuals) for FACTOR -------------------------
  # # # Partitioned Measures of Position (residuals) for FACTOR
  df_residuals_position_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_position_levels_factor
  
  
  
  # # # Partitioned Measures of Dispersion (residuals) for FACTOR ----------------
  df_residual_dispersion_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residual_dispersion_levels_factor
  
  
  
  # # # General Measures of Position (residuals)
  df_residuals_position_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_position_general
  
  
  
  # # # General Measures of Dispersion (residuals)
  df_residuals_dispersion_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_dispersion_general
  
  
  # Tabla plot 001
  df_table_plot001 <- aggregate(VR ~ FACTOR + BLOCK, data = minibase, FUN = mean)
  colnames(df_table_plot001)[3] <- "mean"
  df_table_plot001$"color_factor" <- rep(df_factor_info$color, nlevels(minibase$BLOCK))
  
  
  # Tabla plot 002
  
  df_table_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  
  df_table_plot002["lower_limit"]  <- df_table_plot002$mean - df_table_plot002$model_error_se
  df_table_plot002["upper_limmit"] <- df_table_plot002$mean + df_table_plot002$model_error_se
  df_table_plot002["color"] <- df_factor_info$color
  
  correct_pos_letters <- order(df_tukey_table$level)
  vector_letters <- df_tukey_table$group[correct_pos_letters]
  df_table_plot002["group"] <- vector_letters
  
  # Crear el gráfico interactivo con Plotly
  plot001 <- df_table_plot001 %>%
    group_by(FACTOR)  %>%
    plot_ly(x = ~BLOCK,
            y = ~mean,
            type = 'scatter',
            mode = 'lines+markers',
            color = ~FACTOR,
            colors =~color_factor,
            line = list(width = 4),
            marker = list(size = 20, opacity = 0.7))
  
  plot001 <- plotly::layout(p = plot001,
                            xaxis = list(title = "BLOCK"),
                            yaxis = list(title = "VR"),
                            title = "Plot 001 - Interacción Factor-Bloque",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  plot001 <- plotly::layout(p = plot001,
                            xaxis = list(zeroline = FALSE),
                            yaxis = list(zeroline = FALSE))
  
  # Mostrar el gráfico interactivo
  plot001
  
  
  
  
  # # # Create a new plot...
  plot002 <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot002 <-   plotly::add_trace(p = plot002,
                                 type = "scatter",
                                 mode = "markers",
                                 x = df_table_plot002$level,
                                 y = df_table_plot002$mean,
                                 color = df_table_plot002$level,
                                 colors = df_table_plot002$color,
                                 marker = list(symbol = "line-ew-open",
                                               size = 50,
                                               opacity = 1,
                                               line = list(width = 5)),
                                 error_y = list(type = "data", array = df_table_plot002$model_error_se)
  )
  
  
  
  plot002 <-  add_text(p = plot002,
                       x = df_table_plot002$level,
                       y = df_table_plot002$mean,
                       text = df_table_plot002$group, name = "Tukey Group",
                       size = 20)
  
  # # # Title and settings...
  plot002 <- plotly::layout(p = plot002,
                            xaxis = list(title = "FACTOR"),
                            yaxis = list(title = "VR"),
                            title = "Plot 002 - Mean y model standard error",
                            font = list(size = 20),
                            margin = list(t = 100))
  
  
  
  # # # Without zerolines
  plot002 <-plotly::layout(p = plot002,
                           xaxis = list(zeroline = FALSE),
                           yaxis = list(zeroline = FALSE))
  
  
  # # # Plot output
  plot002
  
  
  ### END CODE ###
  
  #._ Capturar todos los objetos del entorno actual
  ._obj_names <- ls()
  
  #._ Filtrar para excluir los parámetros de la función
  ._obj_to_keep <- setdiff(._obj_names, names(formals(sys.function())))
  ._result_list <- mget(._obj_to_keep)

  # #._ Intentar ordenamiento por aparicion
  ._vector_orden <- fn_R_obj_name_in_order_from_fn(sys.function())
  ._result_list <- ._result_list[._vector_orden]
  # check_all <- identical(sort(._vector_orden), sort(._obj_to_keep))
  # print(check_all)
  # 
  # print(table(c(._vector_orden, ._obj_to_keep)))
  # if(check_all) ._result_list <- ._result_list[._vector_orden]
  #._ Devolver la lista ordenada según su definición
  return(._result_list)
  
}

