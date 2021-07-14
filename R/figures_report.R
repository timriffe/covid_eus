
# packages needed
library(tidyverse)
library(ggridges)
library(lubridate)
library(cowplot)

# someo data prep for the visualizations
covid <- readRDS("Data/data_ccaa.rds") %>% 
  filter(!is.na(tasa), 
         # data cutoff declared for reporting
         fecha <= dmy("30-06-2021")) %>% 
  arrange(CCAA, variable, sexo, edad, year_iso, week_iso) %>% 
  group_by(CCAA, variable, sexo, edad) %>% 
  mutate(tasa_cumul = cumsum(tasa)) %>% 
  ungroup() 

# the standarization
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

############################################################
# Ridge plots   (scale is relative, no need to annualize!) #                                  
############################################################
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


############################################################
# Dot plot of cumulative values                            #
# (discount to annualize, labels in per 1000, log scale)   #
############################################################
cols <- c("Casos" = "#4daf4a", 
          "Hospitalizaciones" = "#984ea3", 
          "Ingresos UCI" = "#377eb8", 
          "Defunciones COVID-19" = "#e41a1c",
          "Exceso mortalidad" = "#ff7f00")

DT <- 
  covid_st %>% 
  filter(fecha <= ymd("2021-06-14")) %>% 
  mutate(k = range(fecha) %>% diff() %>% as.numeric() * 1 / 365.25) %>% 
  filter(fecha == ymd("2021-06-14")) %>% 
  mutate(tasa_st_cumul = tasa_st_cumul * 1000 / k,
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

#############################
# Rank plot of cumulative   #
#############################
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





