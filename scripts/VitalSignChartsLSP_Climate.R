#
#Land Surface Phenology and Climate
#Gantt Charts for Data management timelines and workflows for 
# setting paths #### 
pathMain <- "Z:/DataManagement/VitalSign_DM tracking/2024/GanttCharts/"
pathCharts <- paste0(pathMain, "Charts/")



# Install packages --------------------------------------------------------


library(tidyverse)
library(here)
# install.packages("ggh4x")
library("ggh4x")
library(cowplot)


                     # Load excel file ---------------------------------------------------------

lsp <- readxl::read_xlsx(file.path(pathMain,"JRNVitalSigns2.xlsx"), 
                         sheet = "wr") %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End),
         Duration =difftime(End, Start),
         Days = str_remove(Duration, " days"),
         TaskManager = str_replace(TaskManager, "PM","PL")
  ) 

#Wrangle file
df_tidy <- lsp %>% 
  gather(key=date_type, value=date, -Task, -TaskManager,-Status, -VitalSign, -Days, -Duration, -TaskType, -TaskColor, -Notes) %>% 
  mutate(Task = ifelse(Task =="Validated/Certified data imported into Production SQL database", 
                       "Validated/Certified data imported into \nProduction SQL database", 
                       ifelse(Task=="Field data entered into SQL database (Development)",
                              "Field data entered into SQL database \n(Development)", Task)))

#Set themes for graphs
theme_set(theme_bw() + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
                  panel.grid.minor = element_line(colour="white", linewidth=0.5),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "gray90"),
                  legend.position="top",
                  plot.title = element_text(hjust = 0.5, size = 14),
                  legend.text = element_text(size =14),
                  text = element_text(size = 14)))

# LandSurfacePhenology -------------------------------------------------------
LSP_filter <- df_tidy %>% 
  filter(VitalSign=="LS Phenology") 


LSP_chart <- LSP_filter %>% 
  ggplot() +
  geom_hline(yintercept = as.numeric(ymd("2024-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_hline(yintercept = as.numeric(ymd("2025-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_hline(yintercept = as.numeric(ymd("2026-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_line(aes(x=fct_rev(fct_inorder(Task)), y=date, color=Status), linewidth=5) +
  #facet_grid(TaskType~., scales = "free")+
  #geom_hline(yintercept=as.Date("2019-10-27"), colour="black", linetype="dashed") +
  coord_flip() +
  geom_label(aes(label = TaskManager,x=as.character(fct_rev(fct_inorder(Task))), y=date - 0.5*Duration),
             data = LSP_filter %>% filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Data Life Cycle - Land Surface Phenology",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  theme(legend.justification=c(0.5,1))+
  theme(text = element_text(size = 14))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = c('goldenrod', 'dodgerblue4','brown4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =14))+
  scale_x_discrete(breaks = LSP_filter$Task, labels = LSP_filter$Task)+
  theme(axis.text.y = element_text(color = LSP_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2023-12-01"), as.Date("2024-12-31"))) 

LSP_chart

tiff(file.path(pathCharts,"lp_chart.tiff"), units="in", width=13, height = 7, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
LSP_chart
dev.off()


# LandSurfacePhenology -------------------------------------------------------
Climate_filter <- df_tidy %>% 
  filter(VitalSign=="Climate") 


Climate_chart <- Climate_filter %>% 
  ggplot() +
  geom_hline(yintercept = as.numeric(ymd("2024-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_hline(yintercept = as.numeric(ymd("2025-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_hline(yintercept = as.numeric(ymd("2026-01-01")), linetype="dashed", 
             color = "blue", size=1)+
  geom_line(aes(x=fct_rev(fct_inorder(Task)), y=date, color=Status), linewidth=5) +
  #facet_grid(TaskType~., scales = "free")+
  #geom_hline(yintercept=as.Date("2019-10-27"), colour="black", linetype="dashed") +
  coord_flip() +
  geom_label(aes(label = TaskManager,x=as.character(fct_rev(fct_inorder(Task))), y=date - 0.5*Duration),
             data = Climate_filter %>% filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Data Life Cycle - Climate",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  theme(legend.justification=c(0.5,1))+
  theme(text = element_text(size = 14))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = c('dodgerblue4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =14))+
  scale_x_discrete(breaks = Climate_filter$Task, labels = Climate_filter$Task)+
  theme(axis.text.y = element_text(color = Climate_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2023-09-01"), as.Date("2024-12-31"))) 

Climate_chart

tiff(file.path(pathCharts,"cl.tiff"), units="in", width=13, height = 5, res = 300)
Climate_chart
dev.off()
































