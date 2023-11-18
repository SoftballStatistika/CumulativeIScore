# library(readxl)
library(stringr)
library(dplyr)

league <- "muski"

# directory with extracted zips
wr_dir <- file.path("2023", league)

aggregated_pitching <- data.frame(
  Name = character(0), W = numeric(0), L= numeric(0), SV= numeric(0), 
  IP = numeric(0), BF = numeric(0), Ball = numeric(0), Str = numeric(0),
  R = numeric(0), RA = numeric(0), ER = numeric(0), ERA = numeric(0),
  K = numeric(0), Kc = numeric(0), 
  Ks = numeric(0), H = numeric(0), BB = numeric(0), IBB= numeric(0),
  K.BB = numeric(0), BAA = numeric(0),
  HB = numeric(0), WP = numeric(0))

for( fl in list.files(wr_dir, pattern= "Pitching", recursive = TRUE)){
  
  wr_tb <- read.csv(file.path(wr_dir,fl), encoding = "UTF-8") %>%
    select(Name, W, L, SV, IP, BF, Ball, Str, R, ER, K, Kc, Ks, H, BB, IBB, HB, WP)
  
  # Group the data by 'Name' and summarize the other columns by sum
  aggregated_pitching <- aggregated_pitching %>%
    bind_rows(wr_tb) 
}




aggregated_data <- left_join(aggregated_batting, aggregated_fielding) %>%
  mutate(Name = case_when( 
    Name == "David SmokoviÄ‡" ~ "David Smokovic",
    # Name == "David Smokovi????" ~ "David Smokovic",
    Name == "Ivano Ä†oza" ~ "Ivano Coza",
    Name == "Jaka VeselinoviÄ" ~ "Jaka Veselinovic",
    Name == "Martin KovaÃ„Â" ~ "Martin Kovac",
    Name == "Martin Kovač" ~ "Martin Kovac",
    Name == "Christian Ivan Sket" ~ "Christian-Ivan Sket", 
    Name == "Christian Ivan" ~ "Christian-Ivan Sket",
    Name == "Oliver PavloviÄ‡" ~ "Oliver Pavlovic",
    Name == "MaÅ¡a Jadan" ~ "Masa Jadan",
    Name == "Gregor Novak Vukosavljevic" ~ "Gregor	Novak-Vukosavljevic",
    Name == "Yesminer Escobar Lopez" ~ "Yesminer	Escobar-Lopez",
    Name == "Maša Jadan" ~ "Masa Jadan",
    Name == "Erik Ziger" ~ "Erik Ziger",
    TRUE ~ Name  # Keep the original value for all other cases
  ))

aggregated_pitching <- aggregated_pitching %>%
  group_by(Name) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(RA = format(R/IP*(7), digits = 2),
         ERA = format(ER/IP*7, digits = 2),
         K.BB = format(K/BB,digits = 2),
         BAA = format(H/(BF - BB - HB - IBB), digits = 3)) %>%
  filter(Name != "TOTALS") 

write.csv(aggregated_pitching, paste0("pitching_", league,".txt"), quote = FALSE, row.names = FALSE)
