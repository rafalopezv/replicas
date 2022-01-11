#-------------------------
# limpieza datos globales
#-------------------------
library(tidyverse)
library(magrittr)

# serie de tiempo
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_m <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# descarga y limpieza
df <- read_csv(url) %>% mutate(base = "confirmados")
df_m <- read_csv(url_m) %>% mutate(base = "fallecidos")

df <- bind_rows(df, df_m) 
rm(df_m, url, url_m)

df %<>% 
  rename(pais_region = `Country/Region`) %>% 
  dplyr::select(-matches("Lat|Long")) %>% 
  mutate(pais_region = case_when(
    str_detect(string = `Province/State`, "Hong Kong") ~ "Hong Kong SAR, China",
    str_detect(string = `Province/State`, "Macau") ~ "Macao SAR, China",
    T ~ pais_region
  )) %>% 
  dplyr::select(-`Province/State`) %>% 
  gather(fecha, casos_acumulados, -pais_region, -base) 

df$fecha %<>% as.Date(., format = "%m/%d/%y")

# estandarización desde pacientes 0 e individualización de países en formato lista
df %>% 
  filter(casos_acumulados != 0) %>% 
  group_by(pais_region, base, fecha) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  group_split(pais_region, base) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(base, pais_region, fecha) -> df

# añadir variable de número de semanas: extensión de un año 
tibble(
  semana = rep((1:200), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp

df %<>% merge(., temp, all.x = T)
rm(temp)

# dividir entre semanas
df %>% 
  filter(pais_region == "Bolivia") %>% 
  filter(fecha <= "2022-01-09") %>% 
  group_split(base) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  filter(!is.na(incidencia)) -> bolivia

# arreglo fallecidos con reporte acumulado
bolivia %>% 
  filter(
    base == "fallecidos"
  ) %>% 
  arrange(desc(incidencia)) %>% 
  slice(1) %>% 
  pull(fecha) -> temporal

bolivia %>% 
  filter(
    base == "fallecidos",
    fecha %in% c(temporal, temporal + 1, temporal -1)
  ) %>% 
  arrange(fecha) %>% 
  select(fecha, incidencia) -> temporal_1

bolivia %>% 
  filter(
    base == "fallecidos"
  ) %>% 
  mutate(
    incidencia = case_when(
      fecha == temporal_1 %>% slice(2) %>% pull(fecha) ~ (temporal_1 %>% slice(1) %>% pull(incidencia) + temporal_1 %>% slice(3) %>% pull(incidencia)) /2,
      T ~ incidencia
    )
  ) -> bolivia_fallecidos

bolivia_fallecidos %>% 
  filter(fecha <= temporal_1 %>% slice(2) %>% pull(fecha)) %>% 
  select(fecha, dias, incidencia) %>% 
  arrange(desc(fecha)) -> temporal_2

bolivia_fallecidos %>% 
  mutate(
    numero = (temporal_1 %>% slice(2) %>% pull(incidencia) - temporal_2 %>% slice(1) %>% pull(incidencia)) / temporal_2 %>% slice(1) %>% pull(dias) - 1
  ) %>% 
  pull(numero) %>% 
  unique() -> temporal_3

bolivia_fallecidos %>% 
  mutate(
    incidencia = case_when(
      fecha < temporal_1 %>% slice(2) %>% pull(fecha) ~ incidencia + temporal_3,
      T ~ incidencia
    )
  ) -> bolivia_fallecidos

bolivia_fallecidos %>% 
  mutate(
    incidencia = case_when(
      dias == 2 ~ incidencia - temporal_3,
      T ~ incidencia
    )
  ) -> bolivia_fallecidos

# auitar días de mas en confimaados
bolivia %>% 
  group_split(base) %>% 
  map(
    ., ~arrange(., 
                fecha
    ) %>% 
      slice(1) %>% 
      select(base, fecha) 
  ) %>% 
  bind_rows() %>% 
  spread(base, fecha) %>% 
  mutate(diferencia = fallecidos - confirmados) %>% 
  pull(diferencia) %>% 
  as.numeric() -> diferencia

# quitar días de más de confirmados
bolivia %>% 
  filter(
    base == "confirmados", 
    fecha != "2021-12-22"
  ) %>% 
  arrange(fecha) %>% 
  slice(1: (nrow(.) - diferencia)) %>% 
  bind_rows(., bolivia %>% filter(base == "fallecidos")) -> bolivia

# juntar ambas bases por semana
bolivia %>% 
  filter(base == "confirmados") %>% 
  bind_rows(., bolivia_fallecidos) %>% 
  group_by(base, semana) %>%
  summarise(
    suma_incidencia = sum(incidencia),
    promedio_incidencia = mean(incidencia)
  ) %>% 
  ungroup() -> bolivia_semana

# sacar valores maximos
bolivia_semana %>% 
  filter(base == "fallecidos") %>% 
  pull(suma_incidencia) %>% 
  max() -> max_suma_fallecidos

bolivia_semana %>% 
  filter(base == "confirmados") %>% 
  pull(suma_incidencia) %>% 
  max() -> max_suma_confirmados

bolivia_semana %>% 
  filter(base == "fallecidos") %>% 
  pull(promedio_incidencia) %>% 
  max() -> max_promedio_fallecidos


bolivia_semana %>% 
  filter(base == "confirmados") %>% 
  pull(promedio_incidencia) %>% 
  max() -> max_promedio_confirmados


bolivia_semana %>% 
  group_split(base) %>% 
  map(., ~mutate(
    ., prop_suma_incidencia = prop.table(suma_incidencia),
    prop_promedio_incidencia = prop.table(promedio_incidencia)
  )) %>% 
  bind_rows() -> bolivia_semana


bolivia_semana %>% 
  filter(base ==  "fallecidos") -> fallecidos

bolivia_semana %>% 
  filter(base ==  "confirmados") -> confirmados

fallecidos %>% 
  mutate(
    indicador_promedio = .$promedio_incidencia / max_promedio_fallecidos,
    indicador_suma = .$suma_incidencia / max_suma_fallecidos,
  ) -> fallecidos

confirmados %>% 
  mutate(
    indicador_promedio = .$promedio_incidencia / max_promedio_confirmados,
    indicador_suma = .$suma_incidencia / max_suma_confirmados,
  ) %>% 
  bind_rows(., fallecidos) %>% 
  filter(semana != 94) -> bolivia_semana

# valores maximo, mininimo y promedio paara grid lines eje y

grids <- range(bolivia_semana$indicador_promedio)
grids <-grids %>% mean() %>% c(., grids)

# grafico
bolivia_semana %>% 
  arrange(desc(base)) %>% 
  ggplot() +
  scale_x_continuous(limits = c(0, 101)) +
  geom_line(aes(semana, indicador_promedio, color = base), size = 0.8) +
  geom_segment(
    aes(
      x = 0, y = grids[1], 
      xend = 94, yend = grids[1]
    ),
    size = 1/40, color = "darkgrey"
  ) +
  geom_segment(
    aes(
      x = 0, y = grids[2], 
      xend = 94, yend = grids[2]
    ),
    size = 1/40, color = "darkgrey"
  ) +
  geom_segment(
    aes(
      x = 0, y = grids[3], 
      xend = 94, yend = grids[3]
    ),
    size = 1/40,color = "darkgrey"
  ) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "SF Pro Display") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(2.5,2.5,2.5,.5), "cm")
  ) +
  scale_color_manual(values = c("#b30202", "#939393")) +
  geom_point(aes(x = 93, y = 0.68053593), size = 4.3,
             shape = 21, colour = "white", fill = "#b30202", stroke = 1) +
  geom_point(aes(x = 93, y = 0.45936982), size = 4.3,
             shape = 21, colour = "white", fill = "#939393", stroke = 1) +
  labs(
    x = "",
    y = ""
  )
   
ggsave("", height = 8, width = 7)

