# Pitch Sequencing with Baseball Savant

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)


# Downloading Data ----

files_2017 = list.files(path="~/Data Science Projects/MLB/Pitch Sequencing/Baseball Savant/2017 Season",pattern="*.csv",full.names=T)
season_2017 = lapply(files_2017, read.csv)

files_2018 = list.files(path="~/Data Science Projects/MLB/Pitch Sequencing/Baseball Savant/2018 Season",pattern="*.csv",full.names=T)
season_2018 = lapply(files_2018, read.csv)

files_2019 = list.files(path="~/Data Science Projects/MLB/Pitch Sequencing/Baseball Savant/2019 Season",pattern="*.csv",full.names=T)
season_2019 = lapply(files_2019, read.csv)

hitter_files = list.files(path="~/Data Science Projects/MLB/Pitch Sequencing/Baseball Savant/Hitters",pattern="*.csv",full.names=T)
hitter_seasons = lapply(hitter_files, read.csv)

season_2017_df = as.data.frame(rbindlist(season_2017))
season_2018_df = as.data.frame(rbindlist(season_2018))
season_2019_df = as.data.frame(rbindlist(season_2019))
hitter_seasons_df = as.data.frame(rbindlist(hitter_seasons))

# Condensing Data and Adding Factors ----
col_to_keep = c("game_year","player_name","p_throws","home_team","away_team","inning_topbot","inning","balls","strikes","outs_when_up","on_3b","on_2b","on_1b","pitch_number","pitch_type","pitch_name","release_speed","release_spin_rate","zone","batter","stand","description","hit_location","bb_type","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","launch_speed_angle")

season_2017_df_condensed = season_2017_df[,col_to_keep]
season_2018_df_condensed = season_2018_df[,col_to_keep]
season_2019_df_condensed = season_2019_df[,col_to_keep]

season_2017_df_condensed$game_year = factor(season_2017_df_condensed$game_year)
season_2017_df_condensed$inning = factor(season_2017_df_condensed$inning)
season_2017_df_condensed$balls = factor(season_2017_df_condensed$balls)
season_2017_df_condensed$strikes = factor(season_2017_df_condensed$strikes)
season_2017_df_condensed$outs_when_up = factor(season_2017_df_condensed$outs_when_up)
season_2017_df_condensed$pitch_number = factor(season_2017_df_condensed$pitch_number)
season_2017_df_condensed$release_speed = as.numeric(levels(season_2017_df_condensed$release_speed))[season_2017_df_condensed$release_speed]
season_2017_df_condensed$release_spin_rate = as.numeric(levels(season_2017_df_condensed$release_spin_rate))[season_2017_df_condensed$release_spin_rate]
season_2017_df_condensed$hit_distance_sc = as.numeric(levels(season_2017_df_condensed$hit_distance_sc))[season_2017_df_condensed$hit_distance_sc]
season_2017_df_condensed$launch_speed = as.numeric(levels(season_2017_df_condensed$launch_speed))[season_2017_df_condensed$launch_speed]
season_2017_df_condensed$launch_angle = as.numeric(levels(season_2017_df_condensed$launch_angle))[season_2017_df_condensed$launch_angle]
season_2017_df_condensed$estimated_ba_using_speedangle = as.numeric(levels(season_2017_df_condensed$estimated_ba_using_speedangle))[season_2017_df_condensed$estimated_ba_using_speedangle]
season_2017_df_condensed$estimated_woba_using_speedangle = as.numeric(levels(season_2017_df_condensed$estimated_woba_using_speedangle))[season_2017_df_condensed$estimated_woba_using_speedangle]
season_2017_df_condensed$woba_value = as.numeric(levels(season_2017_df_condensed$woba_value))[season_2017_df_condensed$woba_value]

season_2018_df_condensed$game_year = factor(season_2018_df_condensed$game_year)
season_2018_df_condensed$inning = factor(season_2018_df_condensed$inning)
season_2018_df_condensed$balls = factor(season_2018_df_condensed$balls)
season_2018_df_condensed$strikes = factor(season_2018_df_condensed$strikes)
season_2018_df_condensed$outs_when_up = factor(season_2018_df_condensed$outs_when_up)
season_2018_df_condensed$pitch_number = factor(season_2018_df_condensed$pitch_number)
season_2018_df_condensed$release_speed = as.numeric(levels(season_2018_df_condensed$release_speed))[season_2018_df_condensed$release_speed]
season_2018_df_condensed$release_spin_rate = as.numeric(levels(season_2018_df_condensed$release_spin_rate))[season_2018_df_condensed$release_spin_rate]
season_2018_df_condensed$hit_distance_sc = as.numeric(levels(season_2018_df_condensed$hit_distance_sc))[season_2018_df_condensed$hit_distance_sc]
season_2018_df_condensed$launch_speed = as.numeric(levels(season_2018_df_condensed$launch_speed))[season_2018_df_condensed$launch_speed]
season_2018_df_condensed$launch_angle = as.numeric(levels(season_2018_df_condensed$launch_angle))[season_2018_df_condensed$launch_angle]
season_2018_df_condensed$estimated_ba_using_speedangle = as.numeric(levels(season_2018_df_condensed$estimated_ba_using_speedangle))[season_2018_df_condensed$estimated_ba_using_speedangle]
season_2018_df_condensed$estimated_woba_using_speedangle = as.numeric(levels(season_2018_df_condensed$estimated_woba_using_speedangle))[season_2018_df_condensed$estimated_woba_using_speedangle]
season_2018_df_condensed$woba_value = as.numeric(levels(season_2018_df_condensed$woba_value))[season_2018_df_condensed$woba_value]

season_2019_df_condensed$game_year = factor(season_2019_df_condensed$game_year)
season_2019_df_condensed$inning = factor(season_2019_df_condensed$inning)
season_2019_df_condensed$balls = factor(season_2019_df_condensed$balls)
season_2019_df_condensed$strikes = factor(season_2019_df_condensed$strikes)
season_2019_df_condensed$outs_when_up = factor(season_2019_df_condensed$outs_when_up)
season_2019_df_condensed$pitch_number = factor(season_2019_df_condensed$pitch_number)
season_2019_df_condensed$release_speed = as.numeric(levels(season_2019_df_condensed$release_speed))[season_2019_df_condensed$release_speed]
season_2019_df_condensed$release_spin_rate = as.numeric(levels(season_2019_df_condensed$release_spin_rate))[season_2019_df_condensed$release_spin_rate]
season_2019_df_condensed$hit_distance_sc = as.numeric(levels(season_2019_df_condensed$hit_distance_sc))[season_2019_df_condensed$hit_distance_sc]
season_2019_df_condensed$launch_speed = as.numeric(levels(season_2019_df_condensed$launch_speed))[season_2019_df_condensed$launch_speed]
season_2019_df_condensed$launch_angle = as.numeric(levels(season_2019_df_condensed$launch_angle))[season_2019_df_condensed$launch_angle]
season_2019_df_condensed$estimated_ba_using_speedangle = as.numeric(levels(season_2019_df_condensed$estimated_ba_using_speedangle))[season_2019_df_condensed$estimated_ba_using_speedangle]
season_2019_df_condensed$estimated_woba_using_speedangle = as.numeric(levels(season_2019_df_condensed$estimated_woba_using_speedangle))[season_2019_df_condensed$estimated_woba_using_speedangle]
season_2019_df_condensed$woba_value = as.numeric(levels(season_2019_df_condensed$woba_value))[season_2019_df_condensed$woba_value]

# Adding Previous Pitch Data ----
season_2017_df_condensed$prev_pitch = lead(season_2017_df_condensed$pitch_type)
season_2018_df_condensed$prev_pitch = lead(season_2018_df_condensed$pitch_type)
season_2019_df_condensed$prev_pitch = lead(season_2019_df_condensed$pitch_type)

season_2017_df_condensed$prev_zone = lead(season_2017_df_condensed$zone)
season_2018_df_condensed$prev_zone = lead(season_2018_df_condensed$zone)
season_2019_df_condensed$prev_zone = lead(season_2019_df_condensed$zone)

season_2017_df_condensed$prev_desc = lead(season_2017_df_condensed$description)
season_2018_df_condensed$prev_desc = lead(season_2018_df_condensed$description)
season_2019_df_condensed$prev_desc = lead(season_2019_df_condensed$description)

# Filtering Swinging Strike Data ----
swing_strikes_one_strike_2017 = filter(season_2017_df_condensed, (description == "swinging_strike" | description== "swinging_strike_blocked") & strikes == 1)
swing_strikes_one_strike_2018 = filter(season_2018_df_condensed, (description == "swinging_strike" | description== "swinging_strike_blocked") & strikes == 1)
swing_strikes_one_strike_2019 = filter(season_2019_df_condensed, (description == "swinging_strike" | description== "swinging_strike_blocked") & strikes == 1)

pitches_after_swing_strikes_one_strike_2017 = filter(season_2017_df_condensed,(prev_desc == "swinging_strike" | prev_desc== "swinging_strike_blocked") & strikes == 2)
pitches_after_swing_strikes_one_strike_2018 = filter(season_2018_df_condensed,(prev_desc == "swinging_strike" | prev_desc== "swinging_strike_blocked") & strikes == 2)
pitches_after_swing_strikes_one_strike_2019 = filter(season_2019_df_condensed,(prev_desc == "swinging_strike" | prev_desc== "swinging_strike_blocked") & strikes == 2)

same_pitch_and_location_2017 = filter(pitches_after_swing_strikes_one_strike_2017, (pitch_type == prev_pitch & zone == prev_zone))
same_pitch_and_location_2018 = filter(pitches_after_swing_strikes_one_strike_2018, (pitch_type == prev_pitch & zone == prev_zone))
same_pitch_and_location_2019 = filter(pitches_after_swing_strikes_one_strike_2019, (pitch_type == prev_pitch & zone == prev_zone))

different_pitch_or_location_2017 = filter(pitches_after_swing_strikes_one_strike_2017, (pitch_type != prev_pitch | zone != prev_zone))
different_pitch_or_location_2018 = filter(pitches_after_swing_strikes_one_strike_2018, (pitch_type != prev_pitch | zone != prev_zone))
different_pitch_or_location_2019 = filter(pitches_after_swing_strikes_one_strike_2019, (pitch_type != prev_pitch | zone != prev_zone))

# Calculating Percent Strikeouts and Outs For Same Pitches ----
number_strikeout_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017,description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_same_p_l_2017 = number_strikeout_same_p_l_2017/nrow(same_pitch_and_location_2017)
number_strikeout_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018,description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_same_p_l_2018 = number_strikeout_same_p_l_2018/nrow(same_pitch_and_location_2018)
number_strikeout_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019,description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_same_p_l_2019 = number_strikeout_same_p_l_2019/nrow(same_pitch_and_location_2019)

number_out_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017,description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_same_p_l_2017 = number_out_same_p_l_2017/nrow(same_pitch_and_location_2017)
number_out_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018,description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_same_p_l_2018 = number_out_same_p_l_2018/nrow(same_pitch_and_location_2018)
number_out_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019,description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_same_p_l_2019 = number_out_same_p_l_2019/nrow(same_pitch_and_location_2019)

# Calculating Percent Strikeouts and Outs For Different Pitches ----
number_strikeout_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_dif_p_l_2017 = number_strikeout_diff_p_l_2017/nrow(different_pitch_or_location_2017)
number_strikeout_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_dif_p_l_2018 = number_strikeout_diff_p_l_2018/nrow(different_pitch_or_location_2018)
number_strikeout_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, description == "swinging_strike_blocked" | description == "swinging_strike" | description == "called_strike" | description == "foul_tip"))
percent_strikeout_dif_p_l_2019 = number_strikeout_diff_p_l_2019/nrow(different_pitch_or_location_2019)

number_out_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_diff_p_l_2017 = number_out_diff_p_l_2017/nrow(different_pitch_or_location_2017)
number_out_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_diff_p_l_2018 = number_out_diff_p_l_2018/nrow(different_pitch_or_location_2018)
number_out_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, description != "ball", description != "foul", description != "blocked_ball", description != "hit_into_play_no_out", description != "hit_by_pitch", description != "hit_into_play_score", description != "pitchout"))
percent_out_diff_p_l_2019 = number_out_diff_p_l_2019/nrow(different_pitch_or_location_2019)

# Creating Data Frames for Percent Strikeouts and Outs ----
  years = c(2017,2018,2019)
  
  percent_out_diff_p_l = c(percent_out_diff_p_l_2017,percent_out_diff_p_l_2018,percent_out_diff_p_l_2019)
  percent_out_same_p_l = c(percent_out_same_p_l_2017,percent_out_same_p_l_2018,percent_out_same_p_l_2019)
  
  percent_strikeout_diff_p_l = c(percent_strikeout_dif_p_l_2017,percent_strikeout_dif_p_l_2018,percent_strikeout_dif_p_l_2019)
  percent_strikeout_same_p_l = c(percent_strikeout_same_p_l_2017,percent_strikeout_same_p_l_2018,percent_strikeout_same_p_l_2019)
  
  percent_out_df = data.frame(years,percent_out_diff_p_l,percent_out_same_p_l)
  percent_strikeout_df = data.frame(years,percent_strikeout_diff_p_l,percent_strikeout_same_p_l)
  
# Calculating Average xwoba ----

xwoba_same_p_l_2017 = mean(same_pitch_and_location_2017$estimated_woba_using_speedangle,na.rm=TRUE)
xwoba_same_p_l_2018 = mean(same_pitch_and_location_2018$estimated_woba_using_speedangle,na.rm=TRUE)
xwoba_same_p_l_2019 = mean(same_pitch_and_location_2019$estimated_woba_using_speedangle,na.rm=TRUE)

xwoba_diff_p_l_2017 = mean(different_pitch_or_location_2017$estimated_woba_using_speedangle,na.rm=TRUE)
xwoba_diff_p_l_2018 = mean(different_pitch_or_location_2018$estimated_woba_using_speedangle,na.rm=TRUE)
xwoba_diff_p_l_2019 = mean(different_pitch_or_location_2019$estimated_woba_using_speedangle,na.rm=TRUE)

xwoba_same_p_l = c(xwoba_same_p_l_2017,xwoba_same_p_l_2018,xwoba_same_p_l_2019)
xwoba_diff_p_l = c(xwoba_diff_p_l_2017,xwoba_diff_p_l_2018,xwoba_diff_p_l_2019)

xwoba_df = data.frame(years,xwoba_diff_p_l,xwoba_same_p_l)

# Calculating Percent of Each Launch Angle/Speed Zone Same Pitch ----

percent_zone1_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 1))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))
percent_zone2_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 2))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))
percent_zone3_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 3))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))
percent_zone4_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 4))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))
percent_zone5_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 5))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))
percent_zone6_same_p_l_2017 = nrow(filter(same_pitch_and_location_2017, launch_speed_angle == 6))/nrow(filter(same_pitch_and_location_2017, grepl("hit_into_play",same_pitch_and_location_2017$description)))

percent_zone1_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 1))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))
percent_zone2_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 2))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))
percent_zone3_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 3))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))
percent_zone4_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 4))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))
percent_zone5_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 5))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))
percent_zone6_same_p_l_2018 = nrow(filter(same_pitch_and_location_2018, launch_speed_angle == 6))/nrow(filter(same_pitch_and_location_2018, grepl("hit_into_play",same_pitch_and_location_2018$description)))

percent_zone1_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 1))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))
percent_zone2_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 2))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))
percent_zone3_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 3))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))
percent_zone4_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 4))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))
percent_zone5_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 5))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))
percent_zone6_same_p_l_2019 = nrow(filter(same_pitch_and_location_2019, launch_speed_angle == 6))/nrow(filter(same_pitch_and_location_2019, grepl("hit_into_play",same_pitch_and_location_2019$description)))

# Calculating Percent of Each Launch Angle/Speed Zone Different Pitch ----

percent_zone1_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 1))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))
percent_zone2_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 2))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))
percent_zone3_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 3))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))
percent_zone4_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 4))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))
percent_zone5_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 5))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))
percent_zone6_diff_p_l_2017 = nrow(filter(different_pitch_or_location_2017, launch_speed_angle == 6))/nrow(filter(different_pitch_or_location_2017, grepl("hit_into_play",different_pitch_or_location_2017$description)))

percent_zone1_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 1))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))
percent_zone2_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 2))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))
percent_zone3_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 3))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))
percent_zone4_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 4))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))
percent_zone5_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 5))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))
percent_zone6_diff_p_l_2018 = nrow(filter(different_pitch_or_location_2018, launch_speed_angle == 6))/nrow(filter(different_pitch_or_location_2018, grepl("hit_into_play",different_pitch_or_location_2018$description)))

percent_zone1_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 1))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
percent_zone2_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 2))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
percent_zone3_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 3))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
percent_zone4_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 4))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
percent_zone5_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 5))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
percent_zone6_diff_p_l_2019 = nrow(filter(different_pitch_or_location_2019, launch_speed_angle == 6))/nrow(filter(different_pitch_or_location_2019, grepl("hit_into_play",different_pitch_or_location_2019$description)))
# Creating Data Frames for Launch Angle/Speed Zones ----

zone = c(1:6)

percent_zone_same_p_l_2017 = c(percent_zone1_same_p_l_2017,percent_zone2_same_p_l_2017,percent_zone3_same_p_l_2017,percent_zone4_same_p_l_2017,percent_zone5_same_p_l_2017,percent_zone6_same_p_l_2017)
percent_zone_diff_p_l_2017 = c(percent_zone1_diff_p_l_2017,percent_zone2_diff_p_l_2017,percent_zone3_diff_p_l_2017,percent_zone4_diff_p_l_2017,percent_zone5_diff_p_l_2017,percent_zone6_diff_p_l_2017)
percent_zone_2017_df = data.frame(zone,percent_zone_diff_p_l_2017,percent_zone_same_p_l_2017)

percent_zone_same_p_l_2018 = c(percent_zone1_same_p_l_2018,percent_zone2_same_p_l_2018,percent_zone3_same_p_l_2018,percent_zone4_same_p_l_2018,percent_zone5_same_p_l_2018,percent_zone6_same_p_l_2018)
percent_zone_diff_p_l_2018 = c(percent_zone1_diff_p_l_2018,percent_zone2_diff_p_l_2018,percent_zone3_diff_p_l_2018,percent_zone4_diff_p_l_2018,percent_zone5_diff_p_l_2018,percent_zone6_diff_p_l_2018)
percent_zone_2018_df = data.frame(zone,percent_zone_diff_p_l_2018,percent_zone_same_p_l_2018)

percent_zone_same_p_l_2019 = c(percent_zone1_same_p_l_2019,percent_zone2_same_p_l_2019,percent_zone3_same_p_l_2019,percent_zone4_same_p_l_2019,percent_zone5_same_p_l_2019,percent_zone6_same_p_l_2019)
percent_zone_diff_p_l_2019 = c(percent_zone1_diff_p_l_2019,percent_zone2_diff_p_l_2019,percent_zone3_diff_p_l_2019,percent_zone4_diff_p_l_2019,percent_zone5_diff_p_l_2019,percent_zone6_diff_p_l_2019)
percent_zone_2019_df = data.frame(zone,percent_zone_diff_p_l_2019,percent_zone_same_p_l_2019)

# Counting Pitchers Who Repeated Pitches Most ----

pitchers_repeat_most_2017 = same_pitch_and_location_2017 %>% group_by(player_name) %>% summarise(Number_of_Pitches = length(player_name)) %>% arrange(desc(Number_of_Pitches))
pitchers_repeat_most_2018 = same_pitch_and_location_2018 %>% group_by(player_name) %>% summarise(Number_of_Pitches = length(player_name)) %>% arrange(desc(Number_of_Pitches))
pitchers_repeat_most_2019 = same_pitch_and_location_2019 %>% group_by(player_name) %>% summarise(Number_of_Pitches = length(player_name)) %>% arrange(desc(Number_of_Pitches))

pitchers_repeat_most_with_pitch_2017 = same_pitch_and_location_2017 %>% group_by(player_name,pitch_type) %>% summarise(Number_of_Pitches = length(player_name), xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(Number_of_Pitches))
pitchers_repeat_most_with_pitch_2018 = same_pitch_and_location_2018 %>% group_by(player_name,pitch_type) %>% summarise(Number_of_Pitches = length(player_name), xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(Number_of_Pitches))
pitchers_repeat_most_with_pitch_2019 = same_pitch_and_location_2019 %>% group_by(player_name,pitch_type) %>% summarise(Number_of_Pitches = length(player_name), xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(Number_of_Pitches))

# Getting League Average Spin Rate and Velo for Each Pitch ----

grouped_by_pitch_type_2017 = season_2017_df_condensed %>% group_by(player_name,pitch_type) %>% summarise(Avg_Spin_Rate = mean(release_spin_rate,na.rm=TRUE),Avg_Velo = mean(release_speed,na.rm=TRUE))
grouped_by_pitch_type_2018 = season_2018_df_condensed %>% group_by(player_name,pitch_type) %>% summarise(Avg_Spin_Rate = mean(release_spin_rate,na.rm=TRUE),Avg_Velo = mean(release_speed,na.rm=TRUE))
grouped_by_pitch_type_2019 = season_2019_df_condensed %>% group_by(player_name,pitch_type) %>% summarise(Avg_Spin_Rate = mean(release_spin_rate,na.rm=TRUE),Avg_Velo = mean(release_speed,na.rm=TRUE))

grouped_by_pitch_type_2017$Spin_Rate_Percentile = ecdf(filter(grouped_by_pitch_type_2017,pitch_type==pitch_type)$Avg_Spin_Rate)(grouped_by_pitch_type_2017$Avg_Spin_Rate)
grouped_by_pitch_type_2018$Spin_Rate_Percentile = ecdf(filter(grouped_by_pitch_type_2018,pitch_type==pitch_type)$Avg_Spin_Rate)(grouped_by_pitch_type_2018$Avg_Spin_Rate)
grouped_by_pitch_type_2019$Spin_Rate_Percentile = ecdf(filter(grouped_by_pitch_type_2019,pitch_type==pitch_type)$Avg_Spin_Rate)(grouped_by_pitch_type_2019$Avg_Spin_Rate)

grouped_by_pitch_type_2017$Velo_Percentile = ecdf(filter(grouped_by_pitch_type_2017,pitch_type==pitch_type)$Avg_Velo)(grouped_by_pitch_type_2017$Avg_Velo)
grouped_by_pitch_type_2018$Velo_Percentile = ecdf(filter(grouped_by_pitch_type_2018,pitch_type==pitch_type)$Avg_Velo)(grouped_by_pitch_type_2018$Avg_Velo)
grouped_by_pitch_type_2019$Velo_Percentile = ecdf(filter(grouped_by_pitch_type_2019,pitch_type==pitch_type)$Avg_Velo)(grouped_by_pitch_type_2019$Avg_Velo)

# Adding Percentiles to Grouoped Repeated Pitches Data Frame ----

pitchers_repeat_most_with_pitch_2017 = arrange(merge(pitchers_repeat_most_with_pitch_2017,grouped_by_pitch_type_2017,by.x=c("player_name","pitch_type"),by.y=c("player_name","pitch_type")),desc(Number_of_Pitches))
pitchers_repeat_most_with_pitch_2018 = arrange(merge(pitchers_repeat_most_with_pitch_2018,grouped_by_pitch_type_2018,by.x=c("player_name","pitch_type"),by.y=c("player_name","pitch_type")),desc(Number_of_Pitches))
pitchers_repeat_most_with_pitch_2019 = arrange(merge(pitchers_repeat_most_with_pitch_2019,grouped_by_pitch_type_2019,by.x=c("player_name","pitch_type"),by.y=c("player_name","pitch_type")),desc(Number_of_Pitches))

# Creating Data Frame for Hitters and Same Pitches ----

hitters_same_p_l_2017 = same_pitch_and_location_2017[,c("game_year","batter","estimated_woba_using_speedangle")]
hitters_same_p_l_2018 = same_pitch_and_location_2018[,c("game_year","batter","estimated_woba_using_speedangle")]
hitters_same_p_l_2019 = same_pitch_and_location_2019[,c("game_year","batter","estimated_woba_using_speedangle")]

hitter_seasons_df_condensed = hitter_seasons_df[,c("year","first_name","last_name","player_id","est_woba")]
hitter_seasons_df_condensed = unite_(hitter_seasons_df_condensed,"batter_name",c("first_name","last_name"),sep=" ")

hitters_same_p_l_2017_with_xwoba = merge(hitters_same_p_l_2017,hitter_seasons_df_condensed,by.x=c("game_year","batter"),by.y=c("year","player_id"))
hitters_same_p_l_2018_with_xwoba = merge(hitters_same_p_l_2018,hitter_seasons_df_condensed,by.x=c("game_year","batter"),by.y=c("year","player_id"))
hitters_same_p_l_2019_with_xwoba = merge(hitters_same_p_l_2019,hitter_seasons_df_condensed,by.x=c("game_year","batter"),by.y=c("year","player_id"))

grouped_by_hitters_2017 = hitters_same_p_l_2017_with_xwoba %>% group_by(game_year,batter_name,batter,est_woba) %>% summarise(number_of_pitches = length(batter_name),same_pitch_xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(number_of_pitches))
grouped_by_hitters_2018 = hitters_same_p_l_2018_with_xwoba %>% group_by(game_year,batter_name,batter,est_woba) %>% summarise(number_of_pitches = length(batter_name),same_pitch_xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(number_of_pitches))
grouped_by_hitters_2019 = hitters_same_p_l_2019_with_xwoba %>% group_by(game_year,batter_name,batter,est_woba) %>% summarise(number_of_pitches = length(batter_name),same_pitch_xwoba = mean(estimated_woba_using_speedangle,na.rm=TRUE)) %>% arrange(desc(number_of_pitches))

# Creating Plots ----

percent_out_tidyr = gather(percent_out_df,Second_Pitch,Percent_Out,percent_out_diff_p_l:percent_out_same_p_l)
percent_out_graph = ggplot(percent_out_tidyr,aes(x=years,y=Percent_Out*100)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(2017:2019)) + scale_y_continuous(expand=c(0,0),limits=c(0,40)) + xlab("Year") + ylab("Percent") + ggtitle("Percent of Plays Ending in Outs After Swinging Strike") + theme_classic()
percent_out_graph = percent_out_graph + geom_text(label="*",x=2018,y=35)

percent_strikeout_tidyr = gather(percent_strikeout_df,Second_Pitch,Percent_Strikeout,percent_strikeout_diff_p_l:percent_strikeout_same_p_l)
percent_strikeout_graph = ggplot(percent_strikeout_tidyr,aes(x=years,y=Percent_Strikeout*100)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(2017:2019)) + scale_y_continuous(expand=c(0,0),limits=c(0,30)) + xlab("Year") + ylab("Percent") + ggtitle("Percent of Plays Ending in Strikeouts After Swinging Strike") + theme_classic()
percent_strikeout_graph = percent_strikeout_graph + geom_text(label="*",x=2017,y=26) + geom_text(label="*",x=2018,y=26) + geom_text(label="*",x=2019,y=26)

xwoba_tidyr = gather(xwoba_df,Second_Pitch,xwoba,xwoba_diff_p_l,xwoba_same_p_l)
xwoba_graph = ggplot(xwoba_tidyr,aes(x=years,y=xwoba)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(2017:2019)) + scale_y_continuous(expand=c(0,0),limits=c(0,0.40)) + xlab("Year") + ylab("xwoba") + ggtitle("xwOBA of Balls in Play After Swinging Strike") + theme_classic()
xwoba_graph = xwoba_graph + geom_text(label="*",x=2018,y=.35) + geom_text(label="*",x=2019,y=.35)

percent_zone_2017_tidyr = gather(percent_zone_2017_df,Second_Pitch,Percent_Zone,percent_zone_diff_p_l_2017:percent_zone_same_p_l_2017)
percent_zone_2017_graph = ggplot(percent_zone_2017_tidyr,aes(zone,Percent_Zone*100)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(1:6)) + scale_y_continuous(expand=c(0,0),limits=c(0,50)) + xlab("Zone") + ylab("Percent") + ggtitle("Distribution of Types of Balls Hit in Play in 2017") + theme_classic()

percent_zone_2018_tidyr = gather(percent_zone_2018_df,Second_Pitch,Percent_Zone,percent_zone_diff_p_l_2018:percent_zone_same_p_l_2018)
percent_zone_2018_graph = ggplot(percent_zone_2018_tidyr,aes(zone,Percent_Zone*100)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(1:6)) + scale_y_continuous(expand=c(0,0),limits=c(0,50)) + xlab("Zone") + ylab("Percent") + ggtitle("Distribution of Types of Balls Hit in Play in 2018") + theme_classic()

percent_zone_2019_tidyr = gather(percent_zone_2019_df,Second_Pitch,Percent_Zone,percent_zone_diff_p_l_2019:percent_zone_same_p_l_2019)
percent_zone_2019_graph = ggplot(percent_zone_2019_tidyr,aes(zone,Percent_Zone*100)) + geom_bar(aes(fill=Second_Pitch),stat='identity',position='dodge') + scale_fill_discrete(name="Second Pitch",labels=c("Different","Same")) + scale_x_discrete(expand=c(0,0),limits=c(1:6)) + scale_y_continuous(expand=c(0,0),limits=c(0,50)) + xlab("Zone") + ylab("Percent") + ggtitle("Distribution of Types of Balls Hit in Play in 2019") + theme_classic()

percent_zone_tidyr = rbind(percent_zone_2017_tidyr,percent_zone_2018_tidyr,percent_zone_2019_tidyr)
percent_zone_tidyr = percent_zone_tidyr %>% separate(Second_Pitch,c("Second_Pitch","Year"),sep="_p_l_")
percent_zone_tidyr_same_graph = ggplot(filter(percent_zone_tidyr,grepl("same",percent_zone_tidyr$Second_Pitch)),aes(zone,Percent_Zone*100)) + geom_bar(aes(fill=Year),stat="identity",position="dodge") + scale_fill_discrete(name="Year",labels=c("2017","2018","2019")) + scale_x_discrete(expand=c(0,0),limits=c(1:6)) + scale_y_continuous(expand=c(0,0),limits=c(0,50)) + xlab("Zone") + ylab("Percent") + ggtitle("Distribution of Luanch Angle/Speed by Year for Same Pitches") + theme_classic()
percent_zone_tidyr_diff_graph = ggplot(filter(percent_zone_tidyr,grepl("diff",percent_zone_tidyr$Second_Pitch)),aes(zone,Percent_Zone*100)) + geom_bar(aes(fill=Year),stat="identity",position="dodge") + scale_fill_discrete(name="Year",labels=c("2017","2018","2019")) + scale_x_discrete(expand=c(0,0),limits=c(1:6)) + scale_y_continuous(expand=c(0,0),limits=c(0,50)) + xlab("Zone") + ylab("Percent") + ggtitle("Distribution of Luanch Angle/Speed by Year for Different Pitches") + theme_classic()

pitchers_repeat_most_with_pitch_2017_tidyr = gather(pitchers_repeat_most_with_pitch_2017,Pitch_Attribute,Percentile,Spin_Rate_Percentile:Velo_Percentile)
pitchers_repeat_most_with_pitch_2017_tidyr = arrange(pitchers_repeat_most_with_pitch_2017_tidyr,desc(Number_of_Pitches),player_name)
pitchers_repeat_most_with_pitch_2017_tidyr = unite_(pitchers_repeat_most_with_pitch_2017_tidyr,"player_name_and_pitch",c("player_name","pitch_type"),sep=" - ")
pitchers_repeat_most_with_pitch_2017_graph = ggplot(pitchers_repeat_most_with_pitch_2017_tidyr[1:20,],aes(player_name_and_pitch,Percentile*100)) + geom_bar(aes(fill=Pitch_Attribute),stat='identity',position='dodge') + scale_fill_discrete(name=" ",labels=c("Spin Rate","Velocity")) + scale_x_discrete(expand=c(0,0)) + scale_y_continuous(expand=c(0,0),limits=c(0,100)) + xlab("Player Name and Pitch") + ylab("Percentile") + ggtitle("Pitch Attributes of Top Ten Pitch Repeaters in 2017") + theme_classic() + theme(axis.text.x = element_text(angle=45,hjust=1))

pitchers_repeat_most_with_pitch_2018_tidyr = gather(pitchers_repeat_most_with_pitch_2018,Pitch_Attribute,Percentile,Spin_Rate_Percentile:Velo_Percentile)
pitchers_repeat_most_with_pitch_2018_tidyr = arrange(pitchers_repeat_most_with_pitch_2018_tidyr,desc(Number_of_Pitches),player_name)
pitchers_repeat_most_with_pitch_2018_tidyr = unite_(pitchers_repeat_most_with_pitch_2018_tidyr,"player_name_and_pitch",c("player_name","pitch_type"),sep=" - ")
pitchers_repeat_most_with_pitch_2018_graph = ggplot(pitchers_repeat_most_with_pitch_2018_tidyr[1:20,],aes(player_name_and_pitch,Percentile*100)) + geom_bar(aes(fill=Pitch_Attribute),stat='identity',position='dodge') + scale_fill_discrete(name=" ",labels=c("Spin Rate","Velocity")) + scale_x_discrete(expand=c(0,0)) + scale_y_continuous(expand=c(0,0),limits=c(0,100)) + xlab("Player Name and Pitch") + ylab("Percentile") + ggtitle("Pitch Attributes of Top Ten Pitch Repeaters in 2018") + theme_classic() + theme(axis.text.x = element_text(angle=45,hjust=1))

pitchers_repeat_most_with_pitch_2019_tidyr = gather(pitchers_repeat_most_with_pitch_2019,Pitch_Attribute,Percentile,Spin_Rate_Percentile:Velo_Percentile)
pitchers_repeat_most_with_pitch_2019_tidyr = arrange(pitchers_repeat_most_with_pitch_2019_tidyr,desc(Number_of_Pitches),player_name)
pitchers_repeat_most_with_pitch_2019_tidyr = unite_(pitchers_repeat_most_with_pitch_2019_tidyr,"player_name_and_pitch",c("player_name","pitch_type"),sep=" - ")
pitchers_repeat_most_with_pitch_2019_graph = ggplot(pitchers_repeat_most_with_pitch_2019_tidyr[1:20,],aes(player_name_and_pitch,Percentile*100)) + geom_bar(aes(fill=Pitch_Attribute),stat='identity',position='dodge') + scale_fill_discrete(name=" ",labels=c("Spin Rate","Velocity")) + scale_x_discrete(expand=c(0,0)) + scale_y_continuous(expand=c(0,0),limits=c(0,100)) + xlab("Player Name and Pitch") + ylab("Percentile") + ggtitle("Pitch Attributes of Top Ten Pitch Repeaters in 2019") + theme_classic() + theme(axis.text.x = element_text(angle=45,hjust=1))

xwoba_vs_spin_rate_same_graph_2017 = ggplot(filter(pitchers_repeat_most_with_pitch_2017_tidyr,Pitch_Attribute=="Spin_Rate_Percentile" & Number_of_Pitches >=5),aes(Percentile,xwoba)) + geom_point() + geom_smooth(method="lm") + scale_y_continuous(limits=c(0,2)) + geom_text(x=0.50,y=1.75,label=expression("y = -0.02749x + 0.30845, R"^2*" = 0.0007942")) + ylab("xwOBA") + ggtitle("Spin Rate vs. xwOBA For Same Pitches in 2017") + theme_classic()
xwoba_vs_spin_rate_same_graph_2018 = ggplot(filter(pitchers_repeat_most_with_pitch_2018_tidyr,Pitch_Attribute=="Spin_Rate_Percentile" & Number_of_Pitches >=5),aes(Percentile,xwoba)) + geom_point() + geom_smooth(method="lm") + scale_y_continuous(limits=c(0,2)) + geom_text(x=0.50,y=1.75,label=expression("y = 0.1195x + 0.1817, R"^2*" = 0.02282")) + ylab("xwOBA") + ggtitle("Spin Rate vs. xwOBA For Same Pitches in 2018") + theme_classic()
xwoba_vs_spin_rate_same_graph_2019 = ggplot(filter(pitchers_repeat_most_with_pitch_2019_tidyr,Pitch_Attribute=="Spin_Rate_Percentile" & Number_of_Pitches >=5),aes(Percentile,xwoba)) + geom_point() + geom_smooth(method="lm") + scale_y_continuous(limits=c(0,2)) + geom_text(x=0.50,y=1.75,label=expression("y = -0.04697x + 0.28204, R"^2*" = 0.003342")) + ylab("xwOBA") + ggtitle("Spin Rate vs. xwOBA For Same Pitches in 2019") + theme_classic()

xwoba_vs_season_xwoba_graph_2017 = ggplot(filter(grouped_by_hitters_2017,number_of_pitches>=5),aes(est_woba,same_pitch_xwoba)) + geom_point() + geom_smooth(method="lm") + scale_x_continuous(limits=c(0.2,0.45)) + scale_y_continuous(limits=c(0,1.5)) + geom_text(x=0.35,y=1.5,label=expression("y = 0.02921x + 0.30796, R"^2*" = 0.02108")) + xlab("Season xwOBA") + ylab("Average xwOBA Against Same Pitch") + ggtitle("Season Long xwOBA vs. xwOBA for Same Pitches in 2017") + theme_classic()
xwoba_vs_season_xwoba_graph_2018 = ggplot(filter(grouped_by_hitters_2018,number_of_pitches>=5),aes(est_woba,same_pitch_xwoba)) + geom_point() + geom_smooth(method="lm") + scale_x_continuous(limits=c(0.2,0.45)) + scale_y_continuous(limits=c(0,1.5)) + geom_text(x=0.35,y=1.5,label=expression("y = 0.5141x + 0.1171, R"^2*" = 0.007")) + xlab("Season xwOBA") + ylab("Average xwOBA Against Same Pitch") + ggtitle("Season Long xwOBA vs. xwOBA for Same Pitches in 2018") + theme_classic()
xwoba_vs_season_xwoba_graph_2019 = ggplot(filter(grouped_by_hitters_2019,number_of_pitches>=5),aes(est_woba,same_pitch_xwoba)) + geom_point() + geom_smooth(method="lm") + scale_x_continuous(limits=c(0.2,0.45)) + scale_y_continuous(limits=c(0,1.5)) + geom_text(x=0.35,y=1.5,label=expression("y = 0.02099x + 0.30325, R"^2*" = 0.01115")) + xlab("Season xwOBA") + ylab("Average xwOBA Against Same Pitch") + ggtitle("Season Long xwOBA vs. xwOBA for Same Pitches in 2019") + theme_classic()

# Function to Calculate Significance ----

compare_percentages = function(p1,p2,p,n1,n2) {
  numerator = p1-p2
  denominator = sqrt(p*(1-p)*(1/n1+1/n2))
  z = numerator/denominator
  p_val = 2*pnorm(-abs(z))
  return(p_val)
}

# Manually Testing for Significance Between Percent Out Differences ----

total_percent_out_2017 = (number_out_diff_p_l_2017+number_out_same_p_l_2017)/(nrow(different_pitch_or_location_2017)+nrow(same_pitch_and_location_2017))
percent_out_2017_significance = compare_percentages(percent_out_df[1,2],percent_out_df[1,3],total_percent_out_2017,nrow(different_pitch_or_location_2017),nrow(same_pitch_and_location_2017))

total_percent_out_2018 = (number_out_diff_p_l_2018+number_out_same_p_l_2018)/(nrow(different_pitch_or_location_2018)+nrow(same_pitch_and_location_2018))
percent_out_2018_significance = compare_percentages(percent_out_df[2,2],percent_out_df[2,3],total_percent_out_2018,nrow(different_pitch_or_location_2018),nrow(same_pitch_and_location_2018))

total_percent_out_2019 = (number_out_diff_p_l_2019+number_out_same_p_l_2019)/(nrow(different_pitch_or_location_2019)+nrow(same_pitch_and_location_2019))
percent_out_2019_significance = compare_percentages(percent_out_df[3,2],percent_out_df[3,3],total_percent_out_2019,nrow(different_pitch_or_location_2019),nrow(same_pitch_and_location_2019))

total_percent_strikeout_2017 = (number_strikeout_diff_p_l_2017+number_strikeout_same_p_l_2017)/(nrow(different_pitch_or_location_2017)+nrow(same_pitch_and_location_2017))
percent_strikeout_2017_significance = compare_percentages(percent_strikeout_df[1,2],percent_strikeout_df[1,3],total_percent_strikeout_2017,nrow(different_pitch_or_location_2017),nrow(same_pitch_and_location_2017))

total_percent_strikeout_2018 = (number_strikeout_diff_p_l_2018+number_strikeout_same_p_l_2018)/(nrow(different_pitch_or_location_2018)+nrow(same_pitch_and_location_2018))
percent_strikeout_2018_significance = compare_percentages(percent_strikeout_df[2,2],percent_strikeout_df[2,3],total_percent_strikeout_2018,nrow(different_pitch_or_location_2018),nrow(same_pitch_and_location_2018))

total_percent_strikeout_2019 = (number_strikeout_diff_p_l_2019+number_strikeout_same_p_l_2019)/(nrow(different_pitch_or_location_2019)+nrow(same_pitch_and_location_2019))
percent_strikeout_2019_significance = compare_percentages(percent_strikeout_df[3,2],percent_strikeout_df[3,3],total_percent_strikeout_2019,nrow(different_pitch_or_location_2019),nrow(same_pitch_and_location_2019))
