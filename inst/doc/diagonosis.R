## ----environment, echo = FALSE, message = FALSE--------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "")
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data---------------------------------------------------------
library(nycflights13)
dim(flights)
flights

## ----diagnose------------------------------------------------------------
diagnose(flights)

## ----diagnoses-----------------------------------------------------------
# Select columns by name
diagnose(flights, year, month, day)
# Select all columns between year and day (inclusive)
diagnose(flights, year:day)
# Select all columns except those from year to day (inclusive)
diagnose(flights, -(year:day))

## ----diagnose_pipe-------------------------------------------------------
flights %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

## ----diagnose_pipe_numeric-----------------------------------------------
diagnose_numeric(flights)

## ----diagnose_pipe_numeric_pipe------------------------------------------
diagnose_numeric(flights) %>% 
  filter(minus > 0 | zero > 0) 

## ----diagnose_category---------------------------------------------------
diagnose_category(flights)

## ----diagnose_category_pipe----------------------------------------------
diagnose_category(flights) %>% 
  filter(is.na(levels))

## ----diagnose_category_pipe2---------------------------------------------
flights %>%
  diagnose_category(top = 500)  %>%
  filter(ratio <= 0.01)

## ----diagnose_outlier----------------------------------------------------
diagnose_outlier(flights)

## ----diagnose_outlier_pipe-----------------------------------------------
diagnose_outlier(flights) %>% 
  filter(outliers_cnt > 0) 

## ----diagnose_outlier_pipe2----------------------------------------------
diagnose_outlier(flights) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)

## ----plot_outlier, fig.width = 7, fig.height = 4-------------------------
flights %>%
  plot_outlier(arr_delay) 

## ----plot_outlier_pipe, fig.width = 7, fig.height = 4--------------------
flights %>%
  plot_outlier(diagnose_outlier(flights) %>% 
                 filter(outliers_ratio >= 0.5) %>% 
                 select(variables) %>% 
                 unlist())

## ----diagnose_report, eval=FALSE-----------------------------------------
#  flights %>%
#    diagnose_report()

## ---- eval=FALSE---------------------------------------------------------
#  flights %>%
#    diagnose_report(output_format = "html")

## ---- eval=FALSE---------------------------------------------------------
#  flights %>%
#    diagnose_report(output_format = "html", output_file = "Diagn.html")

## ----diag_title_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data Diagnostic Report Cover"----
knitr::include_graphics('img/diagn_title_pdf.png')

## ----diag_agenda_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data Diagnostic Report Contents"----
knitr::include_graphics('img/diagn_agenda_pdf.png')

## ----diag_intro_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Sample data diagnostic report table"----
knitr::include_graphics('img/diag_intro_pdf.png')

## ----diag_outlier_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data diagnosis report outlier diagnosis contents"----
knitr::include_graphics('img/diag_outlier_pdf.png')

## ----diag_egenda_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data Diagnostic report titles and table of contents"----
knitr::include_graphics('img/diag_agenda_html.png')

## ----diag_table_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Sample data diagnostic report table (html)"----
knitr::include_graphics('img/diag_table_html.png')

## ----diag_outlier_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Data diagnosis report outlier diagnosis contents (html)"----
knitr::include_graphics('img/diag_outlier_html.png')

