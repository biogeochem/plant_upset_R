install.packages("pacman")
pacman::p_load(tidyverse, here, cowplot)


buoy_2021 <- read_csv(here("Data/data_cleaned/Buoy_2021.csv"))

events <- read_csv(here("Data/data_cleaned/Rising Floc Events 2021.csv"))

buoy_2021$DateTime <- as.POSIXct(buoy_2021$DateTime, format = "%m-%d-%Y %H:%M:%S")
buoy_2021$tempdiff <- buoy_2021$Temp00 - buoy_2021$Temp04

## 2021 MONITORING SEASON AND FLOC EVENTS PLOTTED BELOW

## 2021 ####

PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= PARAir), color="gold", size=0.2) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.2) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.2) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y=AirTemp), color="gold", shape=16, size=0.2) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.2) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.2)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y=BarometricPress), size=0.2) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y=WindSp), size=0.3) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.3) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.2) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.2) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.3) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.2) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.2) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.3) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.2) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.2) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.3) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-01 00:00:00"), as.POSIXct("2021-08-15 23:50:00")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

BUOY2021 <- plot_grid(PAR, TEMP, BP, WIND, TEMPdiff, pH, CHLA, DO, BGA, SpC, TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021.tiff", plot = BUOY2021, units="in", width=8, height=8, dpi = 300, compression = "lzw")

## June EVENT 1 ####

Jun14PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(8, 32), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(27.9, 28.4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(7.9, 8.7), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 3), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(52, 150), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 12), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(475, 488), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-14 18:00:00"), xmax = as.POSIXct("2021-06-14 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-13 00:00:00"), as.POSIXct("2021-06-15 23:50:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 23), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun14 <- plot_grid(Jun14PAR, Jun14TEMP, Jun14BP, Jun14WIND, Jun14TEMPdiff, Jun14pH, Jun14CHLA, Jun14DO, Jun14BGA, Jun14SpC, Jun14TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_June14.tiff", plot = Jun14, units="in", width=8, height=8, dpi = 300, compression = "lzw")



## June EVENTS 2 + 3 ####

Jun21_22PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(5, 29), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(27.6, 28.4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(8, 8.5), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(60, 140), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 12), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(461, 485), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun21_22TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-21 15:00:00"), xmax = as.POSIXct("2021-06-21 22:00:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-22 18:00:00"), xmax = as.POSIXct("2021-06-22 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-20 12:00:00"), as.POSIXct("2021-06-23 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 23), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))


Jun21_22 <- plot_grid(Jun21_22PAR, Jun21_22TEMP, Jun21_22BP, Jun21_22WIND, Jun21_22TEMPdiff, Jun21_22pH, Jun21_22CHLA, Jun21_22DO, Jun21_22BGA, Jun21_22SpC, Jun21_22TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_June21_22.tiff", plot=Jun21_22, units="in", width=8, height=8, dpi = 300, compression = "lzw")


## June EVENTS 4, 5 + 6 ####

Jun25_26_27PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(5, 29), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(28, 28.5), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(8, 8.5), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(80, 140), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 12), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(450, 469), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-06-25 14:30:00"), xmax = as.POSIXct("2021-06-25 22:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-26 14:30:00"), xmax = as.POSIXct("2021-06-26 21:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-06-27 15:00:00"), xmax = as.POSIXct("2021-06-27 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-24 18:00:00"), as.POSIXct("2021-06-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,23), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jun25_26_27 <- plot_grid(Jun25_26_27PAR, Jun25_26_27TEMP, Jun25_26_27BP, Jun25_26_27WIND, Jun25_26_27TEMPdiff, Jun25_26_27pH, Jun25_26_27CHLA, Jun25_26_27DO, Jun25_26_27BGA, Jun25_26_27SpC, Jun25_26_27TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_June25_26_27.tiff", plot=Jun25_26_27,  units="in", width=10, height=8, dpi = 300, compression = "lzw")




## July EVENTS 1 + 2 ####

Jul01_02PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(16, 34), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(27.9, 28.4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(7.9, 8.5), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 150), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 12), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(440, 470), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul01_02TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-01 19:30:00"), xmax = as.POSIXct("2021-07-01 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-06-30 12:00:00"), as.POSIXct("2021-07-03 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 23), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))


Jul01_02 <- plot_grid(Jul01_02PAR, Jul01_02TEMP, Jul01_02BP, Jul01_02WIND, Jul01_02TEMPdiff, Jul01_02pH, Jul01_02CHLA, Jul01_02DO, Jul01_02BGA, Jul01_02SpC, Jul01_02TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_July01_02.tiff", plot = Jul01_02, units="in", width=8, height=8, dpi = 300, compression = "lzw")





## July EVENTS 3 + 4 ####

Jul03_04PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(16, 34), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(27.9, 28.4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(8, 8.5), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 150), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 12), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(445, 460), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul03_04TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-02 18:00:00"), xmax = as.POSIXct("2021-07-02 21:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-03 14:20:00"), xmax = as.POSIXct("2021-07-03 20:15:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-04 13:45:00"), xmax = as.POSIXct("2021-07-04 22:40:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-02 12:00:00"), as.POSIXct("2021-07-05 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 23), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))


Jul03_04 <- plot_grid(Jul03_04PAR, Jul03_04TEMP, Jul03_04BP, Jul03_04WIND, Jul03_04TEMPdiff, Jul03_04pH, Jul03_04CHLA, Jul03_04DO, Jul03_04BGA, Jul03_04SpC, Jul03_04TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_July03_04.tiff", plot = Jul03_04, units="in", width=9, height=9, dpi = 300, compression = "lzw")


## July EVENTS 5, 6 + 7 ####

Jul25_27PAR <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= PARAir), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= PARW1), color="darkorange", size=0.7) +
  geom_point(aes(y= PARW2), color="royalblue1", size=0.7) +
  labs(x=NULL, y=expression("PAR (µmol s"^-1*" m "^-2*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27TEMP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=AirTemp), shape = 21, fill="gold2", colour="gold3", size=0.7) +
  geom_point(aes(y= Temp00), color="royalblue1", size=0.7) +
  geom_point(aes(y= Temp04), color="navajowhite4", size=0.5)  +
  labs(x=NULL, y="Temperature (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(12, 34.3), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27BP <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=BarometricPress), size=0.7) +
  labs(x=NULL, y="Barometric Pressure (inHg)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(27.95, 28.45), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27WIND <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=WindSp), size=0.7) +
  labs(x=NULL, y="Wind speed (mph)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27TEMPdiff <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=tempdiff), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Temp difference (°C)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(0,4), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27pH <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y=pHShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y=pHDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="pH") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(8.2, 9.1), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27CHLA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ChlRFUShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Chlorophyll (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27DO <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= ODOSatShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= ODOSatDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y="DO (% saturation)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(50, 200), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27BGA <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= BGA.PCShallow2), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Phycocyanin (RFU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(10, 30), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27SpC <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  geom_point(aes(y= SpCondShallow), color="royalblue1", size=0.7) +
  geom_point(aes(y= SpCondDeep), color="navajowhite4", size=0.5) +
  labs(x=NULL, y=expression("Sp. Conductivity (µS cm"^-1*")")) +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(423, 450), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))

Jul25_27TURB <- ggplot(buoy_2021, aes(x=DateTime)) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 13:50:00"), xmax = as.POSIXct("2021-07-25 16:20:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-25 20:20:00"), xmax = as.POSIXct("2021-07-25 23:30:00"), ymin = -Inf, ymax = Inf, fill = "red2", alpha = 0.15) +
  annotate("rect", xmin = as.POSIXct("2021-07-27 00:00"), xmax = as.POSIXct("2021-07-27 23:55:00"), ymin = -Inf, ymax = Inf, fill = "orange2", alpha = 0.15) +
  geom_point(aes(y= TurbShallow), color="royalblue1", size=0.7) +
  labs(x=NULL, y="Turbidity (NTU)") +
  scale_x_datetime(limits = c(as.POSIXct("2021-07-24 00:00:00"), as.POSIXct("2021-07-28 12:00:00")), date_labels = "%b %d \n %H:%M", expand = c(0.01,0.01), date_breaks = "1 day") +
  scale_y_continuous(limits = c(10, 30), expand = c(0.01, 0.01)) +
  theme_bw(base_size=10) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 9))


Jul25_27 <- plot_grid(Jul25_27PAR, Jul25_27TEMP, Jul25_27BP, Jul25_27WIND, Jul25_27TEMPdiff, Jul25_27pH, Jul25_27CHLA, Jul25_27DO, Jul25_27BGA, Jul25_27SpC, Jul25_27TURB, NULL, nrow = 4, align = "hv")

ggsave("Output/BUOY_2021_July25_27.tiff", plot = Jul25_27, units="in", width=9, height=8, dpi = 300, compression = "lzw")



