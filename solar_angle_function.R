############# AMELIA HESKETH ##############
############ October 14, 2021 #############
## Calculating solar angle of substratum ##

# You will need four variables to run this formula:
 
# orientation_rad = compass angle the substratum faces relative to north = 0, in radians
# sigma = angle of substratum relative to horizontal (0 angle), in radians
# solar_azimuth_rad = solar azimuth for relevant site + time, in radians
# beta = solar elevation for relevant site + time, in radians

# Note that these variable names need to be column names (spelled exactly the same) in your dataframe.

solar_angle <- function(df){
  solar_angle_degrees = ((acos(sin(df$beta)*cos(df$sigma) + cos(abs(df$orientation_rad - df$solar_azimuth_rad))*cos(df$beta)*sin(df$sigma)))/(2*pi))*360
  solar_angle_degrees <- as.data.frame(solar_angle_degrees)
  df <- cbind(df, solar_angle_degrees)
}

# Example of function use:

# df_with_angles <- solar_angle(df)

# A new column containing the solar angle (in degrees, not radians) should now be 
# appended to the end of your original dataframe.