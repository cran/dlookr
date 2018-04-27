## ----environment, echo = FALSE, message = FALSE--------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "")
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(dlookr)
library(dplyr)
library(ggplot2)

## ----import_data---------------------------------------------------------
library(ISLR)
str(Carseats)

## ----missing-------------------------------------------------------------
carseats <- ISLR::Carseats

set.seed(123)
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA

set.seed(456)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

## ----describe------------------------------------------------------------
describe(carseats)

## ----describes2----------------------------------------------------------
# Select columns by name
describe(carseats, Sales, CompPrice, Income)
# Select all columns between year and day (inclusive)
describe(carseats, Sales:Income)
# Select all columns except those from year to day (inclusive)
describe(carseats, -(Sales:Income))

## ----describe_pipe-------------------------------------------------------
carseats %>%
  describe() %>%
  select(variable, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

## ----diagnose_pipe2------------------------------------------------------
carseats %>%
  group_by(US) %>% 
  describe(Sales, Income) 

## ----diagnose_pipe3------------------------------------------------------
carseats %>%
  group_by(US, Urban) %>% 
  describe(Sales, Income) 

## ----normality-----------------------------------------------------------
normality(carseats)

## ----normality2----------------------------------------------------------
# Select columns by name
normality(carseats, Sales, CompPrice, Income)

# Select all columns between year and day (inclusive)
normality(carseats, Sales:Income)

# Select all columns except those from year to day (inclusive)
normality(carseats, -(Sales:Income))

## ----normality_pipe------------------------------------------------------
library(dplyr)

carseats %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

## ----normality_pipe2-----------------------------------------------------
carseats %>%
  group_by(ShelveLoc, US) %>%
  normality(Income) %>% 
  arrange(desc(p_value))

## ----normality_pipe3-----------------------------------------------------
carseats %>%
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  normality(log_income) %>%
  filter(p_value > 0.01)

## ----plot_normality, fig.width = 7, fig.height = 4-----------------------
# Select columns by name
plot_normality(carseats, Sales, CompPrice)

## ----plot_normality2, fig.width = 7, fig.height = 4----------------------
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  plot_normality(Income)

## ----correlate-----------------------------------------------------------
correlate(carseats)

## ----correlate2----------------------------------------------------------
# Select columns by name
correlate(carseats, Sales, CompPrice, Income)

# Select all columns between year and day (inclusive)
correlate(carseats, Sales:Income)

# Select all columns except those from year to day (inclusive)
correlate(carseats, -(Sales:Income))

## ----correlate3----------------------------------------------------------
carseats %>%
  correlate(Sales:Income) %>%
  filter(as.integer(var1) > as.integer(var2))

## ----correlate4----------------------------------------------------------
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>%
  filter(abs(coef_corr) > 0.5)

## ----plot_correlate, fig.width = 7, fig.height = 4-----------------------
plot_correlate(carseats)

## ----plot_correlate2, fig.width = 7, fig.height = 4----------------------
# Select columns by name
plot_correlate(carseats, Sales, Price)

## ----plot_correlate3, fig.width = 7, fig.height = 4, warning=FALSE-------
carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  plot_correlate(Sales)

## ----target_by-----------------------------------------------------------
categ <- target_by(carseats, US)

## ----target_by2----------------------------------------------------------
# If the variable of interest is a numarical variable
cat_num <- relate(categ, Sales)
cat_num
summary(cat_num)

## ----target_by3, fig.width = 7, fig.height = 4, warning=FALSE------------
plot(cat_num)

## ----target_by4----------------------------------------------------------
# If the variable of interest is a categorical variable
cat_cat <- relate(categ, ShelveLoc)
cat_cat
summary(cat_cat)

## ----target_by5, fig.width = 7, fig.height = 4, warning=FALSE------------
plot(cat_cat)

## ----target_by6----------------------------------------------------------
# If the variable of interest is a numarical variable
num <- target_by(carseats, Sales)

## ----target_by7----------------------------------------------------------
# If the variable of interest is a numarical variable
num_num <- relate(num, Price)
num_num
summary(num_num)

## ----target_by8, fig.width = 7, fig.height = 4, warning=FALSE------------
plot(num_num)

## ----target_by9----------------------------------------------------------
# If the variable of interest is a categorical variable
num_cat <- relate(num, ShelveLoc)
num_cat
summary(num_cat)

## ----target_by10, fig.width = 7, fig.height = 4, warning=FALSE-----------
plot(num_cat)

## ----eda_report, eval=FALSE----------------------------------------------
#  carseats %>%
#    eda_report(target = Sales)

## ---- eval=FALSE---------------------------------------------------------
#  carseats %>%
#    eda_report(target = Sales, output_format = "html", output_file = "EDA.html")

## ----eda_title_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report cover"----
knitr::include_graphics('img/eda_title_pdf.png')

## ----eda_agenda_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA Report Contents"----
knitr::include_graphics('img/eda_agenda_pdf.png')

## ----eda_intro_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Example EDA report table"----
knitr::include_graphics('img/eda_intro_pdf.png')

## ----eda_normality_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Normality test information in EDA reports"----
knitr::include_graphics('img/eda_normality_pdf.png')

## ----eda_correlation_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Correlation information in EDA reports"----
knitr::include_graphics('img/eda_correlation_pdf.png')

## ----eda_lm_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Linear relationship information in EDA reports"----
knitr::include_graphics('img/eda_lm_pdf.png')

## ----eda_anova_pdf, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="Information about ANOVA in EDA reports"----
knitr::include_graphics('img/eda_anova_pdf.png')

## ----eda_egenda_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report titles and table of contents"----
knitr::include_graphics('img/eda_agenda_html.png')

## ----eda_table_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA report table example (Web)"----
knitr::include_graphics('img/eda_table_html.png')

## ----eda_normality_html, echo=FALSE, out.width='70%', fig.align='center', fig.pos="!h", fig.cap="EDA Report Normality Test Information (Web)"----
knitr::include_graphics('img/eda_normality_html.png')

