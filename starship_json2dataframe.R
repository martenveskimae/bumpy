#################################
## DATA MINING - MTAT.03.183
## 5. JAN 2018
## MÄRTEN VESKIMÄE
#################################

library(tidyverse)
library(data.table)
library(rjson)

setwd("~/starship_data")
jsons = paste0("starship sensors/",list.files("starship sensors"))
loc = fread("tallinn_location_july.csv")
bots = unique(loc$botid)

processFile = function(filepath) {
  new_row = list()
  i = 0
  j = 0
  con = file(filepath, "r")
  while(T){
    i = i+1
    if(i%%10000==0) print(i)
    line = readLines(con,n=1)
    if(length(line)==0) break
    line = fromJSON(line)
    meta = as.data.frame(line$meta)
    if(meta$botid %in% bots){
      j = j + 1
      orientation_delta = as.data.frame(line$data$orientation_delta)
      colnames(orientation_delta) = c("orientation_delta_x","orientation_delta_y","orientation_delta_z","orientation_delta_w")
      accel_vec = as.data.frame(line$data$accel_vec)
      colnames(accel_vec) = c("accel_vec_x","accel_vec_y","accel_vec_z")
      other = as.data.frame(line$data[4:11])
      new_row[[j]] = cbind(meta,orientation_delta,accel_vec,other)
    }
  }
  close(con)
  data.frame(rbindlist(new_row))
}

data = data.frame(rbindlist(lapply(jsons,function(x) processFile(x))))

local_sec = loc %>%
  group_by(botid,timestamp = round(timestamp,0)) %>%
  summarise(coordinates_long = mean(coordinates_long,na.rm=T),
            coordinates_lat = mean(coordinates_lat,na.rm=T),
            heading = mean(heading,na.rm=T),
            stdev = mean(stdev,na.rm=T))

starship_data = data %>%
  group_by(botid, timestamp = round(secs,0)) %>%
  summarise(orientation_delta_x = mean(orientation_delta_x,na.rm=T),
            orientation_delta_y = mean(orientation_delta_y,na.rm=T),
            orientation_delta_z = mean(orientation_delta_z,na.rm=T),
            orientation_delta_w = mean(orientation_delta_w,na.rm=T),
            accel_vec_x = mean(accel_vec_x,na.rm=T),
            accel_vec_y = mean(accel_vec_y,na.rm=T),
            accel_vec_z = mean(accel_vec_z,na.rm=T),
            magnetometer_azimuth = mean(magnetometer_azimuth,na.rm=T),
            magnetometer_azimuth_updated = mean(magnetometer_azimuth_updated,na.rm=T),
            standstill_detected = mean(standstill_detected,na.rm=T),
            gyro_model_name = gyro_model_name[1],
            nr_of_active_gyros = mean(nr_of_active_gyros,na.rm=T),
            estimated_gyro_stdev_per_sec = mean(estimated_gyro_stdev_per_sec,na.rm=T),
            estimated_gyro_systematic_error_stdev_per_sec = mean(estimated_gyro_systematic_error_stdev_per_sec,na.rm=T),
            estimated_gyro_sensitivity_error_stdev = mean(estimated_gyro_sensitivity_error_stdev,na.rm=T)) %>%
  inner_join(local_sec,.,c("botid","timestamp"))

save(starship_data,file="starship_data.Rda")
