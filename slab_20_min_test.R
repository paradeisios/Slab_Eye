# find the start of the second block
# calculate average
# substract average from trials

correct.baseline <- function(smoothed_df)
  smoothed_df %>%
  group_by(subject,trial,NUMSQUARE) %>%
  summarise(baseline_per_trial = median(pup_interp)) %>%
  filter(NUMSQUARE == 1) -> baseline

df.baseline <- full_join(smoothed_df,baseline)

for (i in seq_along(df.baseline$baseline_per_trial)){
  
  if(!is.na(df.baseline$baseline_per_trial[i])){
      baseline=df.baseline$baseline_per_trial[i]
  }
  
  if(is.na(df.baseline$baseline_per_trial[i])){
      df.baseline$baseline_per_trial[i] = baseline
  }
  print(i)
}

df.baseline$baseline_corrected <- df.baseline$pup_interp - df.baseline$pup_interp
return(baseline_corrected)

