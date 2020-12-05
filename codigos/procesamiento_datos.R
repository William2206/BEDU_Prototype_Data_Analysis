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
  datos <- datos %>% mutate(anio = str_sub(Nombre_de_la_Serie, 1, 4))
  
  datos[, tipo2:= tipo]
  
  datos[tipo %in% c('Baja_California', 'Sonora', 'Chihuahua', 'Coahuila', 
                    'Nuevo_Leon', 'Tamaulipas'), tipo2:= 'norte']
  
  datos[tipo %in% c('Aguascalientes', 'Baja_California_Sur', 'Colima', 'Durango', 
                    'Jalisco', 'Michoacan', 'Nayarit', 'San_Luis_Potosi', 'Sinaloa', 'Zacatecas'), 
        tipo2:= 'centro norte']
  
  datos[tipo %in% c('CDMX', 'Mexico', 'Guanajuato', 'Hidalgo', 'Morelos', 'Puebla', 
                    'Queretaro', 'Tlaxcala'), 
        tipo2:= 'centro']

  datos[tipo %in% c('Campeche', 'Chiapas', 'Guerrero', 'Oaxaca', 'Quintana_Roo', 
                    'Tabasco', 'Veracruz', 'Yucatan'), tipo2:= 'sur']


  
  datos[tipo %in% c('Hasta_01_UMA', 'Mas_de_01_y_hasta_02_UMA'), tipo2:= 'Hasta_02_UMA']
  
  datos[tipo %in% c('Mas_de_02_y_hasta_03_UMA', 'Mas_de_03_y_hasta_04_UMA'), 
        tipo2:= 'Mas_de_02_y_hasta_04_UMA']
  
  datos[tipo %in% c('Mas_de_04_y_hasta_05_UMA', 'Mas_de_05_y_hasta_06_UMA'), 
        tipo2:= 'Mas_de_04_y_hasta_06_UMA']
  
  datos[tipo %in% c('Mas_de_06_y_hasta_07_UMA', 'Mas_de_07_y_hasta_08_UMA'), 
        tipo2:= 'Mas_de_06_y_hasta_08_UMA']
  
  datos[tipo %in% c('Mas_de_08_y_hasta_09_UMA', 'Mas_de_09_y_hasta_10_UMA'), 
        tipo2:= 'Mas_de_08_y_hasta_10_UMA']
  
  datos[tipo %in% c('Mas_de_10_y_hasta_11_UMA', 'Mas_de_11_y_hasta_12_UMA',
        'Mas_de_12_y_hasta_13_UMA', 'Mas_de_13_y_hasta_14_UMA',
        'Mas_de_14_y_hasta_15_UMA'), 
        tipo2:= 'Mas_de_10_y_hasta_15_UMA']
  
  datos[tipo %in% c('Mas_de_15_y_hasta_16_UMA', 'Mas_de_16_y_hasta_17_UMA',
        'Mas_de_17_y_hasta_18_UMA', 'Mas_de_18_y_hasta_19_UMA',
        'Mas_de_19_y_hasta_20_UMA'), 
        tipo2:= 'Mas_de_15_y_hasta_20_UMA'] 
  
  datos[tipo %in% c('Mas_de_20_y_hasta_21_UMA', 'Mas_de_21_y_hasta_22_UMA',
        'Mas_de_22_y_hasta_23_UMA', 'Mas_de_23_y_hasta_24_UMA',
        'Mas_de_24_y_hasta_25_UMA'), 
        tipo2:= 'Mas_de_20_y_hasta_25_UMA'] 
  
  datos[tipo == 'Mas_de_25_UMA', tipo2:= 'Mas_de_25_UMA'] 
  
  fwrite(datos, paste0('C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/R_', 
                     i,'.csv'))
}

