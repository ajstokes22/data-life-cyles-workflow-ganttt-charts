#Gantt Charts for Data management timelines and workflows


# Install packages --------------------------------------------------------


library(tidyverse)
library(here)
# install.packages("ggh4x")
library("ggh4x")
library(cowplot)


# ~1 min to load R
# ~12 min to load packages
# ~14 min to load file 37 kb
# Load excel file ---------------------------------------------------------


wr <- readxl::read_xlsx("VitalSigns.xlsx", sheet = "vs") |> # use wr for the older version of status
  mutate(Start = as.Date(Start),
         End = as.Date(End),
         Duration =difftime(End, Start),
         Days = str_remove(Duration, " days"),
         TaskManager = str_replace(TaskManager, "PM","PL")
  ) 

#Wrangle file
df_tidy <- wr |> 
  gather(key=date_type, value=date, -Task, -TaskManager,-Status, -VitalSign, -Days, -Duration, -TaskType, -TaskColor, -Notes) |> 
  mutate(year = year(date))

#Set themes for graphdate()#Set themes for graphStart#Set themes for graphs
theme_set(theme_bw() + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
                  panel.grid.minor = element_line(colour="white", linewidth = 0.5),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "gray90"),
                  legend.position="top",
                  plot.title = element_text(hjust = 0.5, size = 14),
                  legend.text = element_text(size =14),
                  text = element_text(size = 14)))

# Water resources ---------------------------------------------------------

# Groundwater ====================
gw_filter <- df_tidy |>
  filter(VitalSign=="Groundwater")

#create vectors to be used in plot
gw_status_order <- gw_filter |> 
  select(Status) |> 
  distinct() |> 
  mutate(
    #order for status category
    status_color = case_when(Status =="Operational"~'dodgerblue4', 
                                  Status =="In progress"~"dodgerblue",
                                  Status =="Future development"~"plum",
                                  Status =="Outstanding"~'brown4',
                                  #Status =="Highly variable (Lab)"~2,
                                  ),
    status_order = case_when(Status =="Operational"~1,
                                  Status =="In progress"~2,
                                  Status =="Future development"~4,
                                  Status =="Outstanding"~3,
                                  #Status =="Highly variable (Lab)"~2,
         )) |> 
 
  arrange(status_order)


gw_chart <- gw_filter |> 
  ggplot() +
  geom_hline(yintercept = as.numeric(ymd("2024-01-01")), linetype="dashed", 
             color = "blue", linewidth=1)+
  geom_hline(yintercept = as.numeric(ymd("2025-01-01")), linetype="dashed", 
             color = "blue", linewidth=1)+
  geom_hline(yintercept = as.numeric(ymd("2026-01-01")), linetype="dashed", 
             color = "blue", linewidth=1)+
  geom_line(aes(x=fct_rev(fct_inorder(Task)), y=date, color=Status), linewidth=5) +
  #facet_grid(TaskType~., scales = "free")+
  #geom_hline(yintercept=as.Date("2019-10-27"), colour="black", linetype="dashed") +
  coord_flip() +
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = gw_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 35,
             size = 4) +
  labs(title="Data Life Cycle - Riparian (Alluvial Groundwater and Water Surface Flow)",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme(legend.justification=c(1,1))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = gw_status_order$status_color, breaks = gw_status_order$Status)+
  scale_x_discrete(breaks = gw_filter$Task, labels = gw_filter$Task)+
  theme(axis.text.y = element_text(color = gw_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-01-02"))) 

gw_chart

#save chart as tiff in charts folder
# tiff(here("charts","gw_chart.tiff"), units="in", width=12, height = 5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# gw_chart
# dev.off()

# Surface Flow ====================
sf_filter <- df_tidy |> 
  filter(VitalSign=="Groundwater")


sf_chart <- sf_filter |> 
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
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = sf_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 35,
             size = 4) +
  labs(title="Data Life Cycle - Surface Flow",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme(legend.justification=c(1,1))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = gw_status_order$status_color, breaks = gw_status_order$Status)+
  scale_x_discrete(breaks = sf_filter$Task, labels = sf_filter$Task)+
  theme(axis.text.y = element_text(color = sf_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-01-02"))) 

sf_chart

#save chart as tiff in charts folder
# tiff(here("charts","sf_chart.tiff"), units="in", width=12, height = 5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# sf_chart
# dev.off()

# Water quality ==========================
wq_filter <- df_tidy |> 
  filter(VitalSign=="Water quality") 

wq_end <- wq_filter |> 
  filter(date_type =="End")

wq_end2 <- rbind(wq_end, wq_end)


#create table for status order and vectors 

wq_status_order <- wq_filter |> 
  select(Status) |> 
  distinct() |> 
  mutate(
    #order for status category
    status_color = case_when(Status =="Operational"~'dodgerblue4',
                             Status =="Highly variable (Lab)"~"darkorchid4",
                             Status =="In progress"~"dodgerblue",
                            # Status =="Future development"~"plum",
                             Status =="Needs to be updated"~"goldenrod",
                             Status =="Outstanding"~'brown4'
                             ),
    status_order = case_when(Status =="Operational"~1,
                             Status =="Highly variable (Lab)"~2,
                             Status =="In progress"~3,
                            # Status =="Future development"~4,
                             Status =="Needs to be updated"~4,
                             Status =="Outstanding"~5
                             )) |> 
  arrange(status_order)


wq_chart <-wq_filter|> 
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
  # geom_label(aes(label = TaskManager, x=fct_rev(fct_inorder(Task)), y=date), nudge_x = 0.35, size = 4) +
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = wq_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  coord_flip() +
  labs(title="Data Life Cycle - Water Quality",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  scale_color_manual(values = wq_status_order$status_color, breaks = wq_status_order$Status)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =14))+
  theme(legend.justification=c(1,1))+
  theme(text = element_text(size = 14))+
  scale_x_discrete(breaks = wq_filter$Task, labels = wq_filter$Task)+
  theme(axis.text.y = element_text(color = wq_filter$TaskColor, size = 14))+
  theme(plot.title=element_text(hjust=.25))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-06-02"))) 

wq_chart

# tiff(here("charts","wq_chart.tiff"), units="in", width=14, height = 7.5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# wq_chart
# dev.off()

# Aquatic macroinvertebrates =====================
am_filter <- df_tidy |> 
  filter(VitalSign=="Aquatic macroinvertebrates")

am_status_order <- am_filter |> 
  select(Status) |> 
  distinct() |> 
  mutate(
    #order for status category
    status_color = case_when(Status =="Operational"~'dodgerblue4',
                             Status =="Highly variable (Lab)"~"darkorchid4",
                             #Status =="In progress"~"dodgerblue",
                             # Status =="Future development"~"plum",
                             #Status =="Needs to be updated"~"goldenrod",
                             #Status =="Outstanding"~'brown4'
    ),
    status_order = case_when(Status =="Operational"~1,
                             Status =="Highly variable (Lab)"~2
                             #Status =="In progress"~3,
                             # Status =="Future development"~4,
                             #Status =="Needs to be updated"~4,
                             #Status =="Outstanding"~5
    )) |> 
  arrange(status_order)

am_chart <- am_filter |> 
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
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = am_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Data Life Cycle - Aquatic Macroinvertebrates",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  theme(legend.justification=c(0.5,1))+
  theme(text = element_text(size = 14))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = am_status_order$status_color, breaks = am_status_order$Status)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =14))+
  scale_x_discrete(breaks = am_filter$Task, labels = am_filter$Task)+
  theme(axis.text.y = element_text(color = am_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-01-01"), as.Date("2026-01-02"))) 

am_chart

tiff(here("charts","am_chart.tiff"), units="in", width=11, height = 5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
am_chart
dev.off()


# Combined water resources graph ============
wr_chart =plot_grid(wq_chart+
            theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                  plot.title = element_blank())+
            labs(x ="Water Quality")+
              scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-01-02"))) 
            , 
          am_chart + 
            theme(legend.position = "none")+
            theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                  plot.title = element_blank())+
            labs(x = "Aquatic Macroinvertebrates"), 
          gw_chart+
            theme(legend.position = "none")+
            labs(x = "GroundWater")+
            theme(
                  plot.title = element_blank()), ncol = 1, align = "v",  rel_heights = c(.9,.70,.80))
# tiff(here("charts","wr_combined_chart.tiff"), units="in", width=12, height = 14, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# wr_chart
# dev.off()


df_tidy_wr <- df_tidy|> 
  filter(VitalSign %in% c("Groundwater", "Aquatic macroinvertebrates", "Water quality")) |> 
  mutate(Task_VitalSign = paste(Task, VitalSign, sep = "_"),
         VitalSignColor = ifelse(VitalSign=="Groundwater","forestgreen",
                                 ifelse(VitalSign=="Aquatic macroinvertebrates", "purple4",
                                        ifelse(VitalSign=="Water quality", "dodgerblue4", NA)))) 
  
GW_QAQC <- df_tidy_wr |> 
  filter(VitalSign =="Groundwater"& str_detect(Task, "QA/QC")& date_type=="Start")

GW_QAQC_field <- GW_QAQC |> 
  filter(str_detect(Task, "Field"))

GW_QAQC_continuous <- GW_QAQC |> 
  filter(str_detect(Task, "Aquarius"))

AM_QAQC <- df_tidy_wr |> 
  filter(VitalSign =="Aquatic macroinvertebrates"& str_detect(Task, "QA/QC")& date_type=="Start")

WQ_QAQC <- df_tidy_wr |> 
  filter(VitalSign =="Water quality"& str_detect(Task, "QA/QC")& date_type=="Start")

WQ_QAQC_field <- WQ_QAQC |> 
  filter(str_detect(Task, "Discrete"))

WQ_QAQC_lab <- WQ_QAQC |> 
  filter(str_detect(Task, "NWQL"))

#establish colors
names_vital_sign <- sort(unique(df_tidy_wr$VitalSign))


scale_color_vitalsign <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(c('purple4','forestgreen','dodgerblue4'), names_vital_sign), 
    ...
  )
}
wr_chart_timeorder <-df_tidy_wr|> 
  arrange(date) |> 
  ggplot() +
  geom_hline(yintercept = as.numeric(ymd("2024-01-01")), linetype="dotted", 
             color = "gray70", size=.5)+
  geom_hline(yintercept = as.numeric(ymd("2025-01-01")), linetype="dotted", 
             color = "gray70", size=.5)+
  geom_hline(yintercept = as.numeric(ymd("2026-01-01")), linetype="dotted", 
             color = "gray70", size=.5)+
  geom_hline(yintercept = as.numeric(ymd(GW_QAQC_field$date)), linetype="solid",
             color = GW_QAQC_field$VitalSignColor, size=.6)+
  geom_hline(yintercept = as.numeric(ymd(AM_QAQC$date)), linetype="solid",
             color = AM_QAQC$VitalSignColor, size=.6)+
  geom_hline(yintercept = as.numeric(ymd(WQ_QAQC_lab$date)), linetype="solid",
             color = WQ_QAQC_lab$VitalSignColor, size=.6)+
  geom_hline(yintercept = as.numeric(ymd(WQ_QAQC_field$date)), linetype="solid",
             color = WQ_QAQC_field$VitalSignColor, size=.6)+
  #geom_hline(yintercept = as.numeric(ymd(GW_QAQC_continuous$date)), linetype="dashed",
   #          color = "purple4", size=.6)+
  geom_line(aes(x=fct_rev(fct_inorder(Task_VitalSign)), y=date, color=VitalSign), linewidth=5) +
  #facet_grid(TaskType~., scales = "free")+
  #geom_hline(yintercept=as.Date("2019-10-27"), colour="black", linetype="dashed") +
  
  coord_flip() +
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task_VitalSign)), y=date),
             data = df_tidy_wr |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Timeline - Water Resources",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  scale_color_vitalsign()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 16),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =16))+
  
  theme(legend.justification=c(1,1))+
  theme(text = element_text(size = 14))+
  scale_x_discrete(breaks = df_tidy_wr$Task_VitalSign, labels = df_tidy_wr$Task_VitalSign)+
  theme(axis.text.y = element_text(color = df_tidy_wr$VitalSignColor, size = 10))+
  theme(plot.title=element_text(hjust=.25))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-02-01"), as.Date("2026-07-02"))) 

wr_chart_timeorder


# 
# tiff(here("charts","wr_chart_timeorder.tiff"), units="in", width=15, height = 15, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# wr_chart_timeorder
# dev.off()


# Upland Vegetation -------------------------------------------------------
uv_filter <- df_tidy |> 
  filter(VitalSign=="Upland vegetation")


uv_chart <- uv_filter |> 
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
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = uv_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Data Life Cycle - Upland Vegetation",
       x = "Task",
       y = "Date",
       colour = "Status") +
  theme_bw() +
  theme(legend.justification=c(0.5,1))+
  theme(text = element_text(size = 14))+
  theme(plot.title=element_text(hjust=.25))+
  scale_color_manual(values = c( 'goldenrod','dodgerblue4'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.45, size = 14),
        panel.grid.minor = element_line(colour="white", size=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90"),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.text = element_text(size =14))+
  scale_x_discrete(breaks = uv_filter$Task, labels = uv_filter$Task)+
  theme(axis.text.y = element_text(color = uv_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2023-12-31"), as.Date("2026-01-01"))) 

uv_chart

# 
# tiff(here("charts","uv_chart.tiff"), units="in", width=11, height = 5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# uv_chart
# dev.off()


# Birds -------------------------------------------------------
birds_filter <- df_tidy |> 
  filter(VitalSign=="Birds") 


birds_chart <- birds_filter |> 
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
  geom_label(aes(label = TaskManager,x=fct_rev(fct_inorder(Task)), y=date),
             data = birds_filter |> filter(date_type == "End"),
             nudge_x = 0.35,
             nudge_y = 10,
             size = 4) +
  labs(title="Ideal Data Life Cycle - Birds",
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
  scale_x_discrete(breaks = birds_filter$Task, labels = birds_filter$Task)+
  theme(axis.text.y = element_text(color = birds_filter$TaskColor, size = 14))+
  scale_y_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2024-11-01"), as.Date("2025-12-31"))) 

birds_chart

# tiff(here("charts","birds_chart.tiff"), units="in", width=13, height = 5, res = 300)# tiff(here("results","BODTAC_presentation_figs","fig_turnover.tiff"), units="in", width=11, height=7, res=300)
# birds_chart
# dev.off()





































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