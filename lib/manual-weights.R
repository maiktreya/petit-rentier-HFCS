# Install and load the package
if(!"rif" %in% installed.packages()[,"Package"]) install.packages("rif")
library(rif)

# Convert dt_eff to a data.frame for compatibility with RIFreg
df_eff <- as.data.frame(dt_eff)

# Replicate each row of df_eff according to its weight
df_eff_weighted <- df_eff[rep(seq_len(nrow(df_eff)), dt_eff$facine3),]

# Now you can use df_eff_weighted with RIFreg:
rif_test1 <- RIFreg(actreales ~ worker + young + sex + renthog, data = df_eff_weighted, type = "q", tau = 0.5)
