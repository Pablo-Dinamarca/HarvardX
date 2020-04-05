library(tidyverse)
library(pdftools)
library(stringr)
options(digits = 3)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
View(txt)

ninep <- txt[9]

x <- str_split(ninep, "\n")
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)

s <- s %>% str_trim()
View(s)

























































































































































