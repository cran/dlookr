#' @rdname plot_bar_category.data.frame
#' @export
plot_bar_category <- function(.data, ...) {
  UseMethod("plot_bar_category", .data)
}

#' Plot bar chart of categorical variables 
#'
#' @description The plot_bar_category() to visualizes the distribution of 
#' categorical data by level or relationship to specific numerical data by level.
#'
#' @details The distribution of categorical variables can be understood by 
#' comparing the frequency of each level. The frequency table helps with this. 
#' As a visualization method, a bar graph can help you understand 
#' the distribution of categorical data more easily than a frequency table.
#'
#' @param .data a data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param \dots one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_bar_category() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#' @param top an integer. Specifies the upper top rank to extract.
#' Default is 10.
#' @param add_character logical. Decide whether to include text variables in the
#' diagnosis of categorical data. The default value is TRUE, which also includes character variables.
#' @param title character. a main title for the plot.
#' @param each logical. Specifies whether to draw multiple plots on one screen. 
#' The default is FALSE, which draws multiple plots on one screen.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' 
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#' set.seed(123)
#' carseats$Test <- sample(LETTERS[1:15], 400, replace = TRUE)
#' carseats$Test[1:30] <- NA
#'
#' # Visualization of all numerical variables
#' plot_bar_category(carseats)
#'
#' # Select the variable to diagnose
#' # plot_bar_category(carseats, "ShelveLoc", "Urban")
#' # plot_bar_category(carseats, -ShelveLoc, -Urban)
#' 
#' # Visualize the each plots
#' plot_bar_category(carseats, each = TRUE)
#' 
#' # Not allow typographic argument
#' # plot_bar_category(carseats, typographic = FALSE)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Plot of all categorical variables
#' #carseats %>%
#' #   plot_bar_category()
#'
#' # Visualize just 7 levels of top frequency
#' carseats %>%
#'   plot_bar_category(top = 7)
#'    
#' # Visualize only factor, not character
#' # carseats %>%
#' #   plot_bar_category(add_character = FALSE) 
#'   
#' # Using groupd_df  ------------------------------
#' carseats %>% 
#'   group_by(ShelveLoc) %>% 
#'   plot_bar_category(top = 5)
#'   
#' # carseats %>% 
#' #   group_by(ShelveLoc) %>% 
#' #   plot_bar_category(each = TRUE, top = 5)  
#'   
#' @method plot_bar_category data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @export
#' @rdname plot_bar_category.data.frame 
plot_bar_category.data.frame <- function(.data, ..., top = 10, add_character = TRUE,
                                         title = "Frequency by levels of category",
                                         each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_bar_category_impl(.data, vars, top, add_character, title, each, typographic)
}

#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @importFrom purrr map
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom grid textGrob gpar
#' @importFrom tibble is_tibble as_tibble
#' @importFrom tidyselect all_of
plot_bar_category_impl <- function(df, vars, top, add_character, title, each, typographic) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- tibble::as_tibble(df)
  
  if (add_character)
    nm_factor <- find_class(df[, vars], type = "categorical2", index = FALSE)
  else
    nm_factor <- find_class(df[, vars], type = "categorical", index = FALSE)
  
  if (length(nm_factor) == 0) {
    message("There is no categorical variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_tally <- function(data, var, top = 10) {
    raws <- data %>% 
      select(levels = all_of(var)) %>% 
      group_by(levels) %>% 
      tally() %>% 
      mutate(na_flag = is.na(levels)) %>% 
      mutate(menas = round(sum(n) / (length(n) - sum(na_flag)))) %>%     
      arrange(na_flag, desc(n)) %>%     
      mutate(levels = factor(levels, levels = levels)) %>% 
      mutate(rank = row_number())
    
    tops <- raws %>% 
      filter(rank <= top) %>% 
      filter(!na_flag) 
    
    others <- raws %>% 
      filter(rank > top) %>% 
      filter(!na_flag) %>% 
      summarise(levels = "<Other>",
                n = sum(n),
                na_flag = FALSE,
                menas = 0,
                rank = top + 1) %>% 
      filter(n > 0)
    
    missing <- raws %>% 
      filter(na_flag) %>% 
      mutate(rank = top + 2)
    
    rbind(tops, others, missing) %>% 
      mutate(flag = ifelse(na_flag, "Missing", 
                           ifelse(menas == 0, "OtherTop", "Tops")))
  }
  
  if (typographic) {
    def_colors <- c("Tops" = "#d18975", "OtherTop" = "#8fd175",
                    "Missing" = "grey50")
  } else {
    def_colors <- c("Tops" = "#ff7f0e", "OtherTop" = "#1f77b4",
                    "Missing" = "grey50")    
  }
  
  plist <- purrr::map(nm_factor, function(x) {
    return(data.frame(variables = x, 
                      get_tally(df, x, top)))
  }) %>% 
    lapply(function(data) {
      if (each) {
        ylab <- "Frequency"
      } else {
        ylab <- ""
        title <- ""
      }
      
      p <- data %>% 
        arrange(rank) %>% 
        ggplot(aes(x = levels, y = n)) +
        geom_bar(aes(fill = flag), stat = "identity") +
        scale_colour_manual(values = def_colors,
                            aesthetics = c("colour", "fill")) +
        geom_hline(yintercept = max(data$menas), linetype = "dashed",
                   col = "blue") +
        facet_wrap(~ variables) + 
        scale_x_discrete(limits = rev) + 
        coord_flip() +   
        labs(title = title, x = "", y = ylab) +     
        theme(legend.position = "none",
              axis.title.y = element_blank())
      
      if (typographic) {
        p <- p + 
          theme_typographic() +
          theme(legend.position = "none",
                axis.title.x = element_text(size = 12))
        
        if (!each) {
          p <- p +
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  plot.margin = margin(0, 10, 0, 10)
            )
        }
      }
      
      p
    })
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title))
  }  
}


#' @method plot_bar_category grouped_df
#' @rdname plot_bar_category.data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
plot_bar_category.grouped_df <- function(.data, ..., top = 10, add_character = TRUE,
                                         title = "Frequency by levels of category",
                                         each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_bar_category_group_impl(.data, vars, top, add_character, title, each, typographic)
}

plot_bar_category_group_impl <- function(df, vars, top, add_character, title, each, typographic) {
  group_key <- attr(df, "groups") %>% 
    select(!tidyselect::matches("\\.rows")) %>% 
    names()
  
  n_levels <- attr(df, "group") %>% nrow() 
  
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  if (add_character)
    nm_factor <- find_class(df[, vars], type = "categorical2", index = FALSE)
  else
    nm_factor <- find_class(df[, vars], type = "categorical", index = FALSE)
  
  nm_factor <- setdiff(nm_factor, group_key)
  
  if (length(nm_factor) == 0) {
    message("There is no categorical variable in the data or variable list.\n")
    return(NULL)
  }
  
  get_tally_group <- function(data, group_key, var, top = 10) {
    new_group <- c(group_key, var)
    
    top_levels <- data[, var] %>% 
      table %>%
      sort(decreasing = TRUE) %>% 
      "["(seq(top)) %>% 
      names()
    
    raws <- data %>% 
      select(group_key, all_of(var)) %>% 
      group_by_at(new_group) %>% 
      tally() %>% 
      rename(levels = var) %>% 
      mutate(na_flag = is.na(levels)) %>% 
      mutate(menas = round(sum(n) / (length(n) - sum(na_flag)))) %>%     
      arrange(na_flag, desc(n)) %>%     
      mutate(levels = factor(levels, levels = levels)) %>% 
      mutate(rank = match(levels, top_levels))
    
    tops <- raws %>% 
      filter(levels %in% top_levels) %>% 
      filter(!na_flag)
    
    others <- raws %>% 
      filter(!levels %in% top_levels) %>% 
      filter(!na_flag) %>% 
      summarise(levels = "<Other>",
                n = sum(n),
                na_flag = FALSE,
                menas = NA,
                rank = top + 1,
                .groups = "drop") %>% 
      filter(n > 0)
    
    missing <- raws %>% 
      filter(na_flag) %>% 
      mutate(rank = top + 2)
    
    rbind(tops, others,missing) %>% 
      mutate(flag = ifelse(na_flag, "Missing", 
                           ifelse(is.na(menas), "OtherTop", "Tops")))
  }
  
  if (typographic) {
    def_colors <- c("Tops" = "#d18975", "OtherTop" = "#8fd175",
                    "Missing" = "grey50")
  } else {
    def_colors <- c("Tops" = "#ff7f0e", "OtherTop" = "#1f77b4",
                    "Missing" = "grey50")    
  }
  
  plist <- purrr::map(nm_factor, function(x) {
    return(data.frame(variables = x, 
                      get_tally_group(df, group_key, x, top)))
  }) %>% 
    lapply(function(data) {
      n_col <- data %>% 
        filter(!is.na(levels)) %>% 
        select(levels) %>% 
        unique() %>% 
        nrow()
      
      if (each) {
        ylab <- "Frequency"
        title <- paste(title, "(", 
                       paste(group_key, unique(data$variables), sep = " by "), ")")
      } else {
        ylab <- ""
        title <- ""
      }
      
      center <- round(nrow(df) / (n_levels * n_col))
      
      p <- ggplot(data, aes(x = reorder(levels, rank), y = n)) +
        geom_bar(aes(fill = flag), stat = "identity") +
        scale_colour_manual(values = def_colors,
                            aesthetics = c("colour", "fill")) +
        geom_hline(yintercept = center, linetype = "dashed",
                   col = "blue") +
        facet_grid(reformulate("variables", group_key)) + 
        scale_x_discrete(limits = rev) + 
        coord_flip() +
        labs(title = title, x = "", y = ylab) +     
        theme(legend.position = "none",
              axis.title.y = element_blank())
      
      if (typographic) {
        p <- p + 
          theme_typographic() +
          theme(legend.position = "none",
                axis.title.x = element_text(size = 12))
        
        if (!each) {
          p <- p +
            theme(axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  plot.margin = margin(0, 10, 0, 10)
            )
        }
      }
      
      p
    })
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    #do.call("grid.arrange", c(plist, nrow = n_row, top = title, 
    #                          right = group_key))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title, right = group_key))
  }
}


##----------------------------------------------------------------------------------

#' @rdname plot_qq_numeric.data.frame
#' @export
plot_qq_numeric <- function(.data, ...) {
  UseMethod("plot_qq_numeric", .data)
}

#' Plot Q-Q plot of numerical variables 
#'
#' @description The plot_qq_numeric() to visualizes the Q-Q plot of numeric data or 
#' relationship to specific categorical data.
#' 
#' @details The The Q-Q plot helps determine whether the distribution of a numeric variable 
#' is normally distributed. plot_qq_numeric() shows Q-Q plots of several numeric variables 
#' on one screen. This function can also display a Q-Q plot for each level of a specific 
#' categorical variable.
#'
#' @param .data data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param \dots one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_qq_numeric() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#'
#' @param col_point character. a color of points in Q-Q plot.
#' @param col_line character. a color of line in Q-Q plot.
#' @param title character. a main title for the plot. 
#' @param each logical. Specifies whether to draw multiple plots on one screen. 
#' The default is FALSE, which draws multiple plots on one screen.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Visualization of all numerical variables
#' # plot_qq_numeric(carseats)
#'
#' # Select the variable to diagnose
#' # plot_qq_numeric(carseats, "Sales", "Income")
#' plot_qq_numeric(carseats, -Sales, -Income)
#'
#' # Not allow the typographic elements
#' # plot_qq_numeric(carseats, "Sales", typographic = FALSE)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Plot of all numerical variables
#' # carseats %>%
#' #   plot_qq_numeric()
#'   
#' # Using groupd_df  ------------------------------
#' carseats %>% 
#'   group_by(ShelveLoc) %>% 
#'   plot_qq_numeric()
#'   
#' #carseats %>% 
#' #   group_by(ShelveLoc) %>% 
#' #   plot_qq_numeric(each = TRUE)  
#'   
#' @method plot_qq_numeric data.frame
#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @importFrom purrr map
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
#' @export
#' @rdname plot_qq_numeric.data.frame 
#' 
plot_qq_numeric.data.frame <- function(.data, ..., col_point = "steelblue", col_line = "black",
                                         title = "Q-Q plot by numerical variables",
                                         each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_qq_numeric_impl(.data, vars, col_point, col_line, title, each, typographic)
}

plot_qq_numeric_impl <- function(df, vars, col_point, col_line, title, each, typographic) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  nm_numeric <- find_class(df[, vars], type = "numerical", index = FALSE)
  
  if (length(nm_numeric) == 0) {
    message("There is no numerical variable in the data or variable list.\n")
    return(NULL)
  }
  
  suppressWarnings({
  plist <- purrr::map(nm_numeric, function(var) {
    if (each) {
      xlab <- "Theoretical" 
      ylab <- "Sample"
    } else {
      xlab <- ""
      ylab <- ""
      title <- ""
    }
      
    p_qq <- df %>% 
      mutate(variables = var) %>% 
      ggplot(aes_string(sample = var)) +
      stat_qq(col = col_point) + 
      stat_qq_line(col = col_line) +
      facet_wrap(~ variables) + 
      labs(title = title, x = xlab, y = ylab) +        
      theme(legend.position = "none")
    
    if (typographic) {
      p_qq <- p_qq + 
        theme_typographic() +
        theme(legend.position = "none",
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
      
      if (!each) {
        p_qq <- p_qq +
          theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.margin = margin(0, 10, 0, 10)
          )
      }
    }
    
    p_qq
  })
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title))
  }
  }) # End of suppressWarnings()
}

#' @method plot_qq_numeric grouped_df
#' @rdname plot_qq_numeric.data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
#' 
plot_qq_numeric.grouped_df <- function(.data, ..., col_point = "steelblue", col_line = "black",
                                         title = "Q-Q plot by numerical variables",
                                         each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_qq_numeric_group_impl(.data, vars, col_point, col_line, title, each, typographic)
}

#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @importFrom purrr map
#' @importFrom gridExtra grid.arrange
#' @importFrom grid textGrob gpar
plot_qq_numeric_group_impl <- function(df, vars, col_point, col_line, title, each, typographic) {
  group_key <- attr(df, "groups") %>% 
    select(!tidyselect::matches("\\.rows")) %>% 
    names()
  
  #n_levels <- attr(df, "group") %>% nrow() 
  
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  nm_numeric <- find_class(df[, vars], type = "numerical", index = FALSE)
  
  if (length(nm_numeric) == 0) {
    message("There is no numerical variable in the data or variable list.\n")
    return(NULL)
  }
  
  suppressWarnings({
  plist <- purrr::map(nm_numeric, function(var) {
    if (each) {
      xlab <- "Theoretical" 
      ylab <- "Sample"
      title <- paste(title, "(", paste("by", group_key), ")")
    } else {
      xlab <- ""
      ylab <- ""
      title = ""
    }
    
    p_qq <- df %>% 
      mutate(variables = var) %>% 
      ggplot(aes_string(sample = var)) +
      stat_qq(col = col_point) + 
      stat_qq_line(col = col_line) +
      facet_grid(reformulate("variables", group_key)) + 
      labs(title = title, x = xlab, y = ylab) +        
      theme(legend.position = "none")
    
    if (typographic) {
      p_qq <- p_qq + 
        theme_typographic() +
        theme(legend.position = "none",
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
      
      if (!each) {
        p_qq <- p_qq +
          theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.margin = margin(0, 10, 0, 10)
          )
      }
    }
    
    p_qq
  })
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title, right = group_key))
  }
  }) # End of suppressWarnings()
}

##----------------------------------------------------------------------------------

#' @rdname plot_box_numeric.data.frame
#' @export
plot_box_numeric <- function(.data, ...) {
  UseMethod("plot_box_numeric", .data)
}

#' Plot Box-Plot of numerical variables 
#'
#' @description The plot_box_numeric() to visualizes the box plot of numeric data or 
#' relationship to specific categorical data.
#' 
#' @details The The box plot helps determine whether the distribution of a numeric variable. 
#' plot_box_numeric() shows box plots of several numeric variables 
#' on one screen. This function can also display a box plot for each level of a specific 
#' categorical variable.
#'
#' @param .data data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param \dots one or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions.
#' Positive values select variables; negative values to drop variables.
#' If the first expression is negative, plot_box_numeric() will automatically
#' start with all variables.
#' These arguments are automatically quoted and evaluated in a context where
#' column names represent column positions.
#' They support unquoting and splicing.
#'
#' @param title character. a main title for the plot. 
#' @param each logical. Specifies whether to draw multiple plots on one screen. 
#' The default is FALSE, which draws multiple plots on one screen.
#' @param typographic logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
#' The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.
#' @examples
#' # Generate data for the example
#' carseats <- ISLR::Carseats
#' carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
#' carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
#'
#' # Visualization of all numerical variables
#' # plot_box_numeric(carseats)
#'
#' # Select the variable to diagnose
#' # plot_box_numeric(carseats, "Sales", "Income")
#' plot_box_numeric(carseats, -Sales, -Income)
#'
#' # Visualize the each plots
#' # plot_box_numeric(carseats, "Sales", "Income", each = TRUE)
#' 
#' # Not allow the typographic elements
#' # plot_box_numeric(carseats, typographic = FALSE)
#' 
#' # Using pipes ---------------------------------
#' library(dplyr)
#'
#' # Plot of all numerical variables
#' # carseats %>%
#' #   plot_box_numeric()
#'   
#' # Using groupd_df  ------------------------------
#' carseats %>% 
#'   group_by(ShelveLoc) %>% 
#'   plot_box_numeric()
#'   
#' # carseats %>% 
#' #   group_by(ShelveLoc) %>% 
#' #   plot_box_numeric(each = TRUE)  
#'   
#' @method plot_box_numeric data.frame
#' @import ggplot2
#' @import hrbrthemes
#' @import dplyr
#' @importFrom purrr map
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom grid textGrob gpar
#' @export
#' @rdname plot_box_numeric.data.frame 
#' 
plot_box_numeric.data.frame <- function(.data, ..., 
                                        title = "Box plot by numerical variables",
                                        each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_box_numeric_impl(.data, vars, title, each, typographic)
}

plot_box_numeric_impl <- function(df, vars, title, each, typographic) {
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  nm_numeric <- find_class(df[, vars], type = "numerical", index = FALSE)
  
  if (length(nm_numeric) == 0) {
    message("There is no numerical variable in the data or variable list.\n")
    return(NULL)
  }
  
  suppressWarnings({
  plist <- purrr::map(nm_numeric, function(var) {
    if (each) {
      xlab <- "" 
    } else {
      xlab <- ""
      title <- ""
    }
    
    p_box <- df %>% 
      mutate(variables = var) %>% 
      ggplot(aes_string(var)) +
      geom_boxplot(fill = "steelblue", alpha = 0.8) + 
      ylim(-0.7, 0.7) +
      facet_wrap(~ variables) + 
      labs(title = title, x = xlab) +        
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    if (typographic) {
      p_box <- p_box + 
        theme_typographic() +
        theme(legend.position = "none",
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
      
      if (!each) {
        p_box <- p_box +
          theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.margin = margin(0, 10, 0, 10)
          )
      }
    }
    
    p_box
  }
  )
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title))
  }  
  }) # End of suppressWarnings()
}

#' @method plot_box_numeric grouped_df
#' @rdname plot_box_numeric.data.frame
#' @importFrom tidyselect vars_select
#' @importFrom rlang quos
#' @importFrom tibble is_tibble
#' @export
#' 
plot_box_numeric.grouped_df <- function(.data, ..., 
                                        title = "Box plot by numerical variables",
                                        each = FALSE, typographic = TRUE) {
  vars <- tidyselect::vars_select(names(.data), !!! rlang::quos(...))
  
  plot_box_numeric_group_impl(.data, vars, title, each, typographic)
}

plot_box_numeric_group_impl <- function(df, vars, title, each, typographic) {
  group_key <- attr(df, "groups") %>% 
    select(!tidyselect::matches("\\.rows")) %>% 
    names()
  
  n_levels <- attr(df, "group") %>% nrow() 
  
  if (length(vars) == 0) vars <- names(df)
  
  if (length(vars) == 1 & !tibble::is_tibble(df)) df <- as_tibble(df)
  
  nm_numeric <- find_class(df[, vars], type = "numerical", index = FALSE)
  
  if (length(nm_numeric) == 0) {
    message("There is no numerical variable in the data or variable list.\n")
    return(NULL)
  }
  
  suppressWarnings({
  plist <- purrr::map(nm_numeric, function(var) {
    if (each) {
      xlab <- "" 
      ylab <- ""
      title <- paste(title, "(", paste("by", group_key), ")")      
    } else {
      xlab <- ""
      ylab <- ""
      title <- ""
    }
    
    p_box <- df %>% 
      mutate(variables = var) %>% 
      ggplot(aes_string(var, fill = group_key)) +
      geom_boxplot(alpha = 0.7) + 
      ylim(-0.7, 0.7) +
      facet_grid(reformulate("variables", group_key)) + 
      labs(title = title, x = xlab, y = ylab) +        
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    if (typographic) {
      p_box <- p_box + 
        theme_typographic() +
        scale_fill_ipsum() + 
        theme(legend.position = "none",
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
      
      if (!each) {
        p_box <- p_box +
          theme(axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                plot.margin = margin(0, 10, 0, 10),
                panel.spacing = grid::unit(0, "lines")
          )
      }
    }
    
    p_box
  }
  )
  
  if (each) {
    for (i in seq(plist))
      do.call("print", plist[i])
  } else {
    n <- length(plist)
    n_row <- floor(sqrt(n))
    
    if (typographic) {
      fontfamily <- get_font_family()
      
      title <- grid::textGrob(title, gp = grid::gpar(fontfamily = fontfamily, 
                                                     fontsize = 18, font = 2),
                              x = unit(0.075, "npc"), just = "left")
    }
    
    grobs <- gridExtra::arrangeGrob(grobs = plist, nrow = n_row)
    suppressWarnings(gridExtra::grid.arrange(grobs, top = title, right = group_key))
  }
  }) # End of suppressWarnings()
}
