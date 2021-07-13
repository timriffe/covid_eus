
# create rank plot of cumualtives by measure.
# don't include link w excess

# 1.       Eliminar el título “Tasas estandarizadas de…” o cualquiera de los títulos. Los vamos poner fuera del área del gráfico
# 2.       ¿Se puede poner en los gráficos de montañas la fecha de inicio de la serie? ¿Es 01/01/2020?
# 3.       Eliminar una de las líneas de etapas, la que corresponde a “fin movilidad (06/01/2021)”
# 4.       Nombres exacto de las etapas (puede ponerse un poco más pequeño el tamaño de la letra):
# –         1º Estado de Alarma (14/03/2020)
# –         Desescalada (05/05/2020) [ahora pone 04/05/2020]
# –         2º Estado de Alarma (25/10/2020)
# –         Apertura Navidad (23/12/2020)
# –         Fin 2º Estado de Alarma (09/05/2021)
# 5.       Grafico tasas acumuladas: en la leyenda quitar la palabra “variables” y poner los nombres completos de cada una:
# –         Casos
# –         Hospitalizaciones
# –         Ingresos UCI
# –         Defunciones
# –         Exceso mortalidad
# 6.       Formato de los archivos: en imagen, png o cualquier otro formato de imagen.
# 7.       Hemos quedado que la fecha final de datos sea el 30 de junio (en el caso de casos, hospi, UCI y def) y la última posible en el caso del exceso (creo que será 14 de junio según lo que me has mandado en las tasas acumuladas, pero confírmamelo).
# 8.        Confírmame que las fuentes de datos los gráficos de montañas, todos excepto el de exceso, serían: Fuente: elaboración propia a partir de datos del Panel COVID-19 del Centro Nacional de Epidemiología”. Y la del gráfico de montañas de exceso sería: Fuente: elaboración propia a partir de datos de la Estimación del número de defunciones semanales durante el brote de Covid-19 (EDeS) y de Cifras de Población del INE y de Eurostat.
# 9.       Grafico ranking CCAA: etiquetas de CCAA separadas para exceso de mortalidad y sin líneas que unan con los demás indicadores.
# 
# Sobre la mención a la accesibilidad del código que vamos a poner en la metodología, si redactas tú la frase con lo que quieres que aparezca concretamente, te lo agradezco. Y así no hay errores.

library(tidyverse)
library(ggridges)
library(lubridate)
library(ggrepel)
library(ggforce)
library(colorspace)
library(cowplot)
library(patchwork)
library(ggridges)

covid <- readRDS("Data/data_ccaa.rds") %>% 
  filter(!is.na(tasa), 
         # data cutoff declared for reporting
         fecha <= dmy("30-06-2021")) %>% 
  arrange(CCAA, variable, sexo, edad, year_iso, week_iso) %>% 
  group_by(CCAA, variable, sexo, edad) %>% 
  mutate(tasa_cumul = cumsum(tasa)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(value))

covid %>% 
  select(CCAA) %>% 
  distinct() %>% 
  mutate(CCAA_alpha = c("Andalucía","Aragón" ,"Asturias" ,"Madrid" ,"Valenciana" ,"Canarias"          ,"Cantabria" ,"Castilla y León"   ,"Castilla-La Mancha", "Catalunya" , "Extremadura" , "Galicia"           , "Illes Balears" , "Rioja" , "Murcia" , "Navarra" , "País Vasco"  )) %>% 
  arrange(CCAA_alpha) %>% 
  pull(CCAA) %>% 
  dput()

covid_st <-
  covid %>% 
  group_by(CCAA, CCAA_iso, year_iso, week_iso, fecha, variable) %>% 
  summarize(tasa_st = sum(tasa * stand_nac),
            tasa_st_cumul = sum(tasa_cumul * stand_nac),
            .groups = "drop") %>% 
  mutate(CCAA = factor(CCAA, levels =rev(c("Andalucía", "Aragón", "Asturias", "Canarias", "Cantabria", 
                                       "Castilla y León", "Castilla-La Mancha", "Catalunya ", "Extremadura", 
                                       "Galicia", "Illes Balears", "C. Madrid", "Murcia", "Navarra", 
                                       "País Vasco", "La Rioja", "C. Valenciana"))))

# covid_st_by_sex <-
#   covid %>% 
#   group_by(CCAA_iso,year_iso, week_iso, fecha, variable, sexo) %>% 
#   summarize(tasa_st_pv = sum(tasa * stand_pv) / sum(stand_pv), 
#             tasa_st_nac = sum(tasa * stand_nac) / sum(stand_nac),
#             .groups = "drop")

# plot objects
variables                            <- c("casos","hosp","uci","def","exceso")
variable_names                       <- c("Casos","Hospitalizaciones","Ingresos UCI",
                                          "Defunciones COVID-19","Exceso mortalidad")
names(variable_names)                <- variables
CCAA_isov                            <- covid_st %>% pull(CCAA) %>% unique() %>% sort()
CCAA_cols                            <- rep(gray(.5),length(CCAA_isov))
CCAA_cols[CCAA_isov == "Navarra"]    <- "#d4204d"
CCAA_cols[CCAA_isov == "País Vasco"] <- "#20ab3a"
heights                              <- c(casos = 250, hosp =3000 , uci = 40000, def = 8000, exceso = 8000  )
# heights <- c(casos = 250, hosp =3000 , uci = 50000, def = 10000, exceso = 10000  )

date_breaks <- as_date(
  c("2020-01-01","2020-04-01","2020-07-01","2020-10-01","2021-01-01","2021-04-01","2021-06-30"))

for (i in variables){
p <-
  covid_st %>% 
  dplyr::filter(variable == i,
                fecha <= dmy("30-06-2021"))  %>% 
  ggplot(aes(x = fecha, 
             #y = reorder(CCAA_iso, tasa_st_nac, na.rm=TRUE, FUN = var), 
             y = CCAA,
             height = tasa_st,
             fill = CCAA))  +
  # geom_text(data = label_df,
  #           mapping = aes(x = lx, y = ly, label = label)) + 
  geom_ridgeline(color  = gray(.2), alpha = .7,min_height = -2,scale = heights[i]) +
  # theme_bw() +
  theme(legend.pos = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=14),
        plot.margin = margin(t=2.5, r=3, b=1, l=2, "lines"),
        plot.title.position = "plot",
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#BBBBBB50"),
        axis.ticks = element_blank()) + 
  labs(y=element_blank(),
       x=element_blank()) + 
  scale_fill_manual(values = CCAA_cols, labels = CCAA_isov) + 
  scale_x_date(breaks = date_breaks,
               date_labels = "%d/%m/%Y") +
  coord_cartesian(clip = "off") +
  # annotations
   annotate("segment",
            x = dmy("14/03/2020"),
            xend = dmy("14/03/2020"),
            y = .5,
            yend = 19.5,
            color = "#4b8ead"
         ) +
  annotate("segment",
           x = dmy("05/05/2020"),
           xend = dmy("05/05/2020"),
           y = .5,
           yend =20.5,
           color = "#4b8ead"
  ) +
  annotate("segment",
           x = dmy("25/10/2020"),
           xend = dmy("25/10/2020"),
           y = .5,
           yend =19.5,
           color = "#4b8ead"
  ) +
  annotate("segment",
           x = dmy("23/12/2020"),
           xend = dmy("23/12/2020"),
           y = .5,
           yend =20.5,
           color = "#4b8ead"
  ) +
  annotate("segment",
           x = dmy("9/05/2021"),
           xend = dmy("9/05/2021"),
           y = .5,
           yend =20.5,
           color = "#4b8ead"
  )

pp <-
ggdraw(p) +
  draw_label(label="CCAA",x=.13,y=.84,size=16,fontface="bold") +
  draw_label(label="1º Estado de Alarma\n(14/03/2020)",x=.25,y=.88,size=10) +
  draw_label(label="Desescalada\n(05/05/2020)",x=.37,y=.95,size=10) +
  draw_label(label="2º Estado de Alarma\n(25/10/2020)",x=.535,y=.88,size=10)+
  draw_label(label="Apertura Navidad\n(23/12/2020)",x=.66,y=.95,size=10)+
  # draw_label(label="fin movilidad\n(6/01/2021)",x=.75,y=.80)+
  draw_label(label="Fin 2º Estado de Alarma\n(09/05/2021)",x=.84,y=.95,size=10)


ggsave(here::here("FigData",paste0(i,".png")),pp,width = 3000, height = 1700, units = "px")
}
# +
#   geom_vline(xintercept = as_date(c("2020-03-14",
#                                     "2020-05-05",
#                                     "2020-10-26",
#                                     "2021-05-10")),
#              color = "#0011FF50")
        

# cols <- c("casos" = "#274d52", 
#           "hosp" = "#c7a2a6", 
#           "uci" = "#818b70", 
#           "def" = "#604e3c",
#           "exceso" = "#8c9fb7")
# "#e41a1c" # red
# "#377eb8" # blue
# "#4daf4a" # green
# "#984ea3" # purple
# "#ff7f00" # orange
cols <- c("Casos" = "#4daf4a", 
          "Hospitalizaciones" = "#984ea3", 
          "Ingresos UCI" = "#377eb8", 
          "Defunciones COVID-19" = "#e41a1c",
          "Exceso mortalidad" = "#ff7f00")

DT <- 
  covid_st %>% 
  filter(fecha == ymd("2021-06-14")) %>% 
  mutate(tasa_st_cumul = tasa_st_cumul * 1000,
         lab_color = case_when(CCAA == "País Vasco" ~ "#20ab3a",
                               CCAA == "Navarra" ~ "#d4204d",
                               TRUE ~ "#000000")) %>% 
  group_by(CCAA) %>% 
  mutate(deaths = tasa_st_cumul[variable == "def"]) %>% 
  left_join(tibble(variable_names, variable = names(variable_names)), by = "variable")

pc <-
DT %>% 
  ggplot(aes(y = reorder(CCAA,deaths), 
             x = tasa_st_cumul, 
             color = variable_names))  +
  geom_point(size = 4) +
  scale_x_log10() + 
  # labs(title = "Tasas acumuladas estandarizadas (14 junio 2021)") +
  ylab("") +
  xlab("Tasa acumulada estandarizada per mil (log)") +
  theme(axis.text=element_text(size=10),
        title = element_text(size=14),
        plot.title.position = "plot",
        legend.title=element_blank(),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#BBBBBB50"),
        axis.ticks = element_blank()) + 
  scale_color_manual(values = cols) + 
  geom_hline(data = NULL, yintercept = 11, color = "#20ab3a", size = 5, alpha = .2) +
  geom_hline(data = NULL, yintercept = 10, color = "#d4204d", size = 5, alpha = .2) +
  geom_point(size = 4) 


ggsave(here::here("FigData","cumul.png"),pc,width = 3000, height = 1700, units = "px")



"2021-06-14"
"2021-06-28"

covid_ranks2 <-
  covid %>% 
  group_by(CCAA, variable,sexo,edad, stand_nac) %>%
  summarize(tasa = sum(tasa, na.rm = TRUE), .groups = "drop") %>% 
  group_by(CCAA, variable) %>% 
  summarize(tasa_st_nac = sum(tasa * stand_nac, na.rm=TRUE),
            .groups = "drop") %>% 
  group_by(variable) %>% 
  mutate(Rank = rank(tasa_st_nac),
         RelRank = tasa_st_nac / max(tasa_st_nac, na.rm=TRUE) * 17) %>% 
  ungroup() %>% 
  left_join(tibble(variable_name = variable_names, 
                   variable = names(variable_names)), 
            by = "variable") %>% 
mutate(variable_name = factor(variable_name, levels = c("Casos","Hospitalizaciones","Ingresos UCI","Defunciones COVID-19","Exceso mortalidad")))

ranks <-
  covid_ranks2 %>% 
  ggplot(aes(x = variable_name, y = Rank, color = CCAA, group = CCAA)) + 
  geom_point(size = 4) +
  geom_line(data = dplyr::filter(covid_ranks2, variable_name != "Exceso mortalidad")) +
  geom_text(data = dplyr::filter(covid_ranks2, 
                                       
                                       variable_name == "Exceso mortalidad"),
                  mapping = aes(x = 5.1, y = Rank, label = CCAA),
                  size = 4,  hjust = 0) +
  geom_text(data = dplyr::filter(covid_ranks2, 
                                 variable_name == "Casos"),
                  mapping = aes(x = .9, y = Rank, label = CCAA),
                  size = 4, hjust = 1) + 
  #annotate("text",x = rep(.6,17), y = 1:17, label = 17:1) +
  #annotate("text", x = .6, y= 17.9, label = "Rel. Rank", size = 6) +
  # annotate("text", x = 5.2, y= 19, label = "CCAA", size = 6) +
  labs(x = "") +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#BBBBBB50"),
        axis.ticks = element_blank(),
        plot.margin = margin(t=1, r=5, b=1, l=5, "lines"),
        axis.text=element_text(size=10))+
    coord_cartesian(clip = "off") 
ggsave(here::here("FigData","ranks.png"), ranks, width = 3000, height = 1700, units = "px")






################################

# library(tidyverse)
# library(readxl)
# dat <- read_excel("Data/Gráficos.xlsx", sheet = "table") 
#   
# 
# 
# p <-
#   dat %>% 
#   ggplot(aes(y = origen, 
#              x = Valor, 
#              group = interaction(origen, model), 
#              color = model)) +
#   geom_vline(data = NULL,aes(xintercept = 1), col = gray(.5))+
#   geom_point(position = position_dodge2(width = .4, reverse = TRUE), size = 2) +
#   scale_x_log10(breaks = c(.8,1.25,2,4)) +
#   geom_pointrange(aes(xmin = lower, xmax = upper),
#                   position = position_dodge2(width = .4, reverse = TRUE)) +
#   ylab("") +
#   xlab("log prevalence ratio") +
#   theme_minimal() +
#   theme(axis.text = element_text(size=12),
#         axis.title = element_text(size=14)) 
# 
# p
# ggsave("FigData/prevratio.svg",p)





