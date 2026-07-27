library(tidyverse)
library(here)

wr <- readxl::read_xlsx("water_resources.xlsx") %>% 
  mutate(Start = as.Date(Start),
         End = as.Date(End),
         Duration =difftime(End, Start),
         Days = str_remove(Duration, " days")
  ) 


df_tidy <- wr %>% 
  gather(key=date_type, value=date, -Task, -Status, -VitalSign, -Days, -Duration, -TaskType, -TaskColor, -Notes)


library("ggh4x")


wq_filter <- df_tidy %>% 
  filter(VitalSign=="Water quality") 

wq_chart <-wq_filter%>% 
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
  labs(title="Ideal Timeline - Water Quality",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  scale_color_manual(values = c("plum3",'darkorchid4','goldenrod', 'dodgerblue4','brown4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 12),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =12))+
  scale_x_discrete(breaks = wq_filter$Task, labels = wq_filter$Task)+
  theme(axis.text.y = element_text(color = wq_filter$TaskColor, size = 12))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-07-02"))) 

wq_chart


am_filter <- df_tidy %>% 
  filter(VitalSign=="Aquatic macroinvertebrates")


am_chart <- am_filter %>% 
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
  labs(title="Ideal Timeline - Aquatic Macroinvertebrates",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  scale_color_manual(values = c( 'dodgerblue4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 12),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =12)
  )+
  scale_x_discrete(breaks = am_filter$Task, labels = am_filter$Task)+
  theme(axis.text.y = element_text(color = am_filter$TaskColor, size = 10))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-07-02"))) 

am_chart

gw_filter <- df_tidy %>% 
  filter(VitalSign=="Groundwater")


gw_chart <- gw_filter %>% 
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
  labs(title="Ideal Timeline - Groundwater",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  scale_color_manual(values = c( 'dodgerblue4','brown4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 12),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =12)
        )+
  scale_x_discrete(breaks = gw_filter$Task, labels = gw_filter$Task)+
  theme(axis.text.y = element_text(color = gw_filter$TaskColor, size = 12))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-07-02"))) 

gw_chart

# wq_chart+
#     geom_rect(aes(ymin = as.Date("2024-02-01"), ymax = as.Date("2024-02-25"), xmin = "Field preparation", xmax = "Lab data imported into ?database"),
#           color = "black", fill = "#00668A")+
#     geom_rect(aes(ymin = as.Date("2024-02-01"), ymax = as.Date("2024-02-25"), xmin = "Lab data imported into ?database", xmax = "QA/QC"),
#             color = "black", fill = "#AC4F10")+
#     geom_rect(aes(ymin = as.Date("2024-02-01"), ymax = as.Date("2024-02-25"), xmin = "QA/QC", xmax = "Data publication"),
#             color = "black", fill = "#7A007A")+
#   scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-02-01"))) 
# 
# # 
#   geom_text(aes(x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2,
#                 label = text),
#             size = 20)
# library(here)
#   
#   #below removes color coding for text
# wq+
#   facet_grid(df_tidy$TaskType~., scales = "free_y", shrink = T)+
#   force_panelsizes(rows = c(1, .6,.3)) +
#   theme(axis.text.y = element_text(color = df_tidy$TaskColor, size = 10))

#Status options
#working, established, done, complete, operational
#incomplete, future steps, outstanding
#stuck, dependent
#working on it, needs improvement