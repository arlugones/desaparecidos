library(tidyverse)

extract_date <- function(x){
  x <- str_squish(x)
  day <- str_extract(x, '^[:digit:]{1,2}')
  raw_month <- str_extract(x, '(?<=([:blank:]{1,2}de[:blank:]{1,2}))[:alpha:]+')
  raw_year <- str_extract(x, '(?<=(de)[:blank:]{1})[:digit:]+')
  return(str_c(day, raw_month, raw_year, sep = '/'))
}

## Age calculated as of 2019
df <- read_csv('Desaparecidos2.csv') %>% filter(!is.na(Nombre))

## Cleaning scraped data
df$Edad <- str_extract(df$Edad, '[:digit:]+')
df$Apariencia <- str_remove(df$Apariencia, '(Contextura:[:blank:]*)')
df$Sexo <- str_remove(df$Sexo, '(Sexo:[:blank:]*)')
df$Nacionalidad <- str_remove(df$Nacionalidad, 'De nacionalidad:[:blank:]*')
df$Ojos <- str_remove(df$Ojos, 'Tiene ojos color:[:blank:]*')
df$Cabello <- str_remove(df$Cabello, 'Tiene cabello color:[:blank:]*')
df$Estatura <- str_extract(df$Estatura, '[:digit:]+')
df$Peso <- str_extract(df$Peso, '[:digit:]+')
df$Caracteristicas <- str_remove(df$Caracteristicas, 'Caracteristicas Particulares:[:blank:]*')
df$Caracteristicas <- str_remove(df$Caracteristicas, '(?<=[:blank:]{1})null')
df$FNac <- str_remove(df$FNac, 'Fecha de nacimiento:[:blank:]{0,1}') %>%
  extract_date() %>% lubridate::parse_date_time(orders = c('dmy'), tz = 'America/Guayaquil')
df$Visto <- str_squish(df$Visto) %>% str_remove('Fue visto por última vez el[:blank:]{0,1}')
df %>% separate(Visto, into = c('FechaVisto', 'LugarVisto'), sep = ' en ') -> df
df$LugarVisto <- str_to_upper(df$LugarVisto)
df$FechaVisto <- extract_date(df$FechaVisto) %>% lubridate::parse_date_time(orders = c('dmy'), tz = 'America/Guayaquil')

## Turning numeric columns on
df[, c(4, 11, 12)] <- sapply(df[, c(4, 11, 12)], function(x) as.numeric(as.character(x)))
