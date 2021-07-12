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
  filter(!is.na(tasa)) %>% 
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

covid_st_by_sex <-
  covid %>% 
  group_by(CCAA_iso,year_iso, week_iso, fecha, variable, sexo) %>% 
  summarize(tasa_st_pv = sum(tasa * stand_pv) / sum(stand_pv), 
            tasa_st_nac = sum(tasa * stand_nac) / sum(stand_nac),
            .groups = "drop")

# plot objects
variables <- c("casos","hosp","uci","def","exceso")
variable_names <- c("casos","hospitalizaciones","UCI","defunciones COVID-19","exceso mortalidad")
names(variable_names) <- variables
CCAA_isov <- covid_st %>% pull(CCAA) %>% unique() %>% sort()
CCAA_cols <- rep(gray(.5),length(CCAA_isov))
CCAA_cols[CCAA_isov == "Navarra"] <- "#d4204d"
CCAA_cols[CCAA_isov == "País Vasco"] <- "#20ab3a"
heights <- c(casos = 250, hosp =3000 , uci = 40000, def = 8000, exceso = 8000  )
# heights <- c(casos = 250, hosp =3000 , uci = 50000, def = 10000, exceso = 10000  )


i <- "def"
for (i in variables){
p <-
  covid_st %>% 
  dplyr::filter(variable == i)  %>% 
  ggplot(aes(x = fecha, 
             #y = reorder(CCAA_iso, tasa_st_nac, na.rm=TRUE, FUN = var), 
             y = CCAA,
             height = tasa_st,
             fill = CCAA))  +
  # geom_text(data = label_df,
  #           mapping = aes(x = lx, y = ly, label = label)) + 
  geom_ridgeline(color  = gray(.2), alpha = .7,min_height = -2,scale = heights[i]) +
  theme(legend.pos = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.margin = margin(t=2, r=3, b=1, l=2, "lines"),
        title = element_text(size=20,face="bold"),
        plot.title.position = "plot") + 
  labs(y=element_blank(),
       x=element_blank(),
       title = paste0("Tasas estandarizadas de ",variable_names[i],"\n\n")) + 
 
  scale_fill_manual(values = CCAA_cols, labels = CCAA_isov) + 
  scale_x_date(date_breaks = "3 months",date_labels = "%d/%m/%Y") +
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
           x = dmy("04/05/2020"),
           xend = dmy("04/05/2020"),
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
           x = dmy("6/01/2021"),
           xend = dmy("6/01/2021"),
           y = .5,
           yend =19.5,
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
  draw_label(label="CCAA",x=.14,y=.75,size=20,fontface="bold") +
  draw_label(label="estado de alarma\n(14/03/2020)",x=.25,y=.80) +
  draw_label(label="desescalada\n(04/05/2020)",x=.325,y=.86) +
  draw_label(label="estado de alarma\n(25/10/2020)",x=.545,y=.80)+
  draw_label(label="apertura Navidad\n(23/12/2020)",x=.62,y=.86)+
  draw_label(label="fin movilidad\n(6/01/2021)",x=.75,y=.80)+
  draw_label(label="fin del estado de alarma\n(9/05/2021)",x=.85,y=.87)


ggsave(here::here("FigData",paste0(i,".pdf")),pp)
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
cols <- c("casos" = "#4daf4a", 
          "hosp" = "#984ea3", 
          "uci" = "#377eb8", 
          "def" = "#e41a1c",
          "exceso" = "#ff7f00")
DT <- 
  covid_st %>% 
  filter(fecha == ymd("2021-06-14")) %>% 
  mutate(tasa_st_cumul = tasa_st_cumul * 1000,
         lab_color = case_when(CCAA == "País Vasco" ~ "#20ab3a",
                               CCAA == "Navarra" ~ "#d4204d",
                               TRUE ~ "#000000")) %>% 
  group_by(CCAA) %>% 
  mutate(deaths = tasa_st_cumul[variable == "def"]) 

pc <- 
DT %>% 
  ggplot(aes(y = reorder(CCAA,deaths), 
             x = tasa_st_cumul, 
             color = variable))  +
  geom_point(size = 4) +
  scale_x_log10() + 
  labs(title = "Tasas acumuladas estandarizadas (14 junio 2021)") +
  ylab("") +
  xlab("Tasa acumulada estandarizada per mil (log)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=20,face="bold"),
        plot.title.position = "plot") + 
  scale_color_manual(values = cols) + 
  geom_hline(data = NULL, yintercept = 11, color = "#20ab3a", size = 5, alpha = .2) +
  geom_hline(data = NULL, yintercept = 10, color = "#d4204d", size = 5, alpha = .2) +
  geom_point(size = 4) 


ggsave(here::here("FigData","cumul.pdf"),pc)




















