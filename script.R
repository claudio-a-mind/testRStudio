library(googledrive)
library(tidyverse)
library(readxl)


options(gargle_quiet = FALSE) # Configutación con errores 

drive_auth( email = "claudio.a@somosmind.com",
            token = "4/1AX4XfWg3sr2b6eRCJBp25Sdq2rZiJt0lfQ048iIbZLf690TB6EvY0Z3eOLI")

drive_download(as_id("1XrcFhzAfzf9K4Z2MJyd-BLAqZGbyzMcPRSNtOIXVaO4"),
               overwrite = TRUE, path = "FB_ads_all_account_utm_status.xlsx")
df01= read_xlsx("FB_ads_all_account_utm_status.xlsx")

# Cambio de formato en nombre de columna ----------------------------------
cambiar_nombre_variables <- function(df){
  variables=colnames(df)
  nuevas_variables = str_replace_all(str_squish((str_replace_all(variables , regex("\\W+"), " ")))," ","_")
  nuevas_variables=stringi::stri_trans_general(nuevas_variables,"Latin-ASCII")
  colnames(df)=nuevas_variables
  return(df)
}


df_3 = cambiar_nombre_variables(df01)

df_31= df_3 %>% 
  select(Account, Ad_name , Date, Link_asset_website_URL) %>% 
  group_by(Ad_name) %>% 
  mutate(aux_alerta_utm= grepl(pattern = "utm_", x = Link_asset_website_URL,ignore.case = TRUE), 
         alerta_utm = if_else(aux_alerta_utm==TRUE,0,1))


df_31$Date_chr = as.character(df_31$Date)

DF_AUX = df_31 %>% filter(Date_chr == "2021-07-13")

## Creación Vector: Nombres de clientes
UTMClientes = sort(unique(DF_AUX$Account))


TablaAux = DF_AUX %>% 
  group_by(Account) %>%
  summarise(Suma = sum(alerta_utm))


# Funciones Dashboard -----------------------------------------------------

Icon_Account = function(i) {
if(with(TablaAux, Suma[Account == UTMClientes[i]] > 0)){
  tags$i(class = "fa fa-exclamation-triangle", style = "color: red")
} else {
  tags$i(class = "fa fa-exclamation-triangle", style = "color: green")
}
}

UTM_Account = function(i){
DF_AUX %>% 
  filter(Account == UTMClientes[i]) %>% 
  group_by(Ad_name) %>% 
  summarise("Número Errores UTM" = sum(alerta_utm)) %>% 
  rename(Anuncio = Ad_name) 
}

UTM_Account(10)


