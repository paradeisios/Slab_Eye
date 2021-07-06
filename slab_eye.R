library(dplyr)
library(ggplot2)
library(gazer)
library(ggpubr)

options(scipen = 999)
my_options <- options(digits.secs = 6)   

filepath = "C://Users//mpoul//Desktop//slab_EYE_TRACKING.txt"
df = read.csv(filepath,sep=",",header = TRUE)
head(df)

###############################################################################

df$right.pupil_diameter_mm <- ifelse(df$right.pupil_diameter_mm == -1,NA,df$right.pupil_diameter_mm) 
df$left.pupil_diameter_mm <- ifelse(df$left.pupil_diameter_mm == -1,NA,df$left.pupil_diameter_mm) 
df$subject <- c(rep(1,2316))
df$trial <- c(rep(1,2316))
df %>%
  #slice(1001:1500)%>%
  select(mytimestamp,
         trial,
         subject,
         time,
         right.pupil_diameter_mm,
         left.pupil_diameter_mm,
         right.gaze_direction_normalized.x,
         right.gaze_direction_normalized.y,
         left.gaze_direction_normalized.x,
         left.gaze_direction_normalized.y) %>%
         mutate(pupil = (right.pupil_diameter_mm +left.pupil_diameter_mm)/2) -> df.eye


raw<- ggplot(data=df)+
  geom_path(aes(x=time,y=right.pupil_diameter_mm))


###############################################################################
pup_extend<- df.eye %>% 
  mutate(extendpupil=extend_blinks(right.pupil_diameter_mm, fillback=100, fillforward=100, hz=90))

deblinked <- ggplot(data=pup_extend)+
  geom_path(aes(x=time,y=extendpupil))

###############################################################################
smooth_interp <- smooth_interpolate_pupil(pup_extend, 
                                          pupil="pupil", 
                                          extendpupil="extendpupil", 
                                          extendblinks=TRUE, 
                                          step.first="interp", 
                                          filter="moving",
                                          maxgap=Inf, 
                                          type="linear", 
                                          hz=90, 
                                          n=5)
smoothed <- ggplot(data=smooth_interp)+
  geom_line(aes(x=time,y=pup_interp))
###############################################################################
baseline_pupil <- baseline_correction_pupil(smooth_interp, 
                                            pupil_colname='pup_interp', 
                                            baseline_window=c(70000,70500))

base_corrected <-ggplot(data=baseline_pupil)+
  geom_line(aes(x=time,y=baselinecorrectedp ))
###############################################################################

mad_removal <- baseline_pupil  %>% 
  group_by(subject, trial) %>% 
  mutate(speed=speed_pupil(pup_interp,time)) %>% 
  mutate(MAD=calc_mad(speed, n = 16)) %>% 
  filter(speed < MAD)

mad_corrected <-ggplot(data=mad_removal)+
  geom_line(aes(x=time,y=baselinecorrectedp ))
###############################################################################

ggarrange(raw, deblinked, smoothed,base_corrected,mad_corrected, 
          ncol = 1, 
          nrow = 5,
          labels=c("raw","deblinked","smooted","base_corrected","mad_corrected"))

