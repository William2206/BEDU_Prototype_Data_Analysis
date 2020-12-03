library(tidyverse)
library(data.table)

nombre_base <- c('otor_credito_importe_cheque', 'otor_credito_monto_credito_infonavit',
                 'otor_credito_numero_creditos_formalizados', 'subsidios_y_apoyos_vivienda_monto_subsidios',
                 'subsidios_y_apoyos_vivienda_numero_subsidios')

catalogo <- fread("C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/catalogo_categorias.csv", 
                  header = TRUE)

for(i in nombre_base){
  datos <- fread(paste0("C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/", 
                        i, ".csv"))
  datos <- fread(paste0("C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/", 
                        i, ".csv"), 
                 header = TRUE, colClasses = c('character', rep('numeric', ncol(datos)-1)))

  datos <- datos %>% gather(tipo, valor, -Nombre_de_la_Serie)
  datos <- as.data.table(datos)
  setkey(datos, 'tipo')
  setkey(catalogo, 'tipo')

  datos <- merge(datos, catalogo, all.x = T)

fwrite(datos, paste0('C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/R_', 
                     i,'.csv'))
}

