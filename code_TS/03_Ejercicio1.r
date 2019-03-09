########### Exercise #########################
library(tidyverse)
library(purrr)
library(pdftools)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
tab <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
})


#1. Haz un gráfico del número de muertes por fecha. Hint: convierte la variable meses de caracteres a números usando `recode` para redefinir la variable `tab`.
tab$month
tab$monthnumber <- recode(tab$month, "JAN"=1, "FEB"=2, "MAR"=3, "APR"=4, "MAY"=5, "JUN"=6, "JUL"=7, "AGO"=8, "SEP"=9, "OCT"=10, "NOV"=11, "DEC"=12)



#2. Crea una nueva columna "date" con la fecha de cada entrada. Hint: use the `make_date` function.
tab$fecha <- make_date(tab$year, tab$monthnumber, tab$day)

#3. Deaths vs date.
ggplot(tab, aes(x=fecha, y=deaths)) +
  geom_line()
?ggplot

#4. Probablemente las observaciones después de mayo de 2018 no se tomaron. Rehaz el plot sin esas observaciones
subset(tab, fecha < make_date(2018, 5, 1) ) %>% 
ggplot( aes(x=fecha, y=deaths)) + geom_line( col = "pink")

