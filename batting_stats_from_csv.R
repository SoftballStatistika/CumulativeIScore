# library(readxl)
library(stringr)
library(dplyr)

league <- "zg_muski"

# Set the locale to UTF-8
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# directory with extracted zips
wr_dir <- file.path("2023", league)

aggregated_batting <- data.frame(
  Name = character(0),  PA = numeric(0),  AB = numeric(0),  R = numeric(0),  
  H = numeric(0),  B = numeric(0),  X1B = numeric(0),  X2B = numeric(0),
  X3B = numeric(0),  HR = numeric(0),  RBI = numeric(0), 
  AVG = numeric(0), SLG = numeric(0),  BB = numeric(0),
  SO = numeric(0),  HBP = numeric(0),  SB = numeric(0),  CS = numeric(0),
  SCB = numeric(0),  SF = numeric(0),  SAC = numeric(0),  ROE = numeric(0),
  FC = numeric(0),  CI = numeric(0),  GDP = numeric(0),  GTP = numeric(0),
  OBP = numeric(0)
)

for( fl in list.files(wr_dir, pattern= "Batting", recursive = TRUE)){
  
  wr_tb <- read.csv(file.path(wr_dir,fl),  encoding = "UTF-8") %>%
    select(Name, PA, AB, R, H, B, X1B, X2B, X3B, HR, RBI, BB, SO, HBP, SB, CS, SCB, SF, SAC, ROE, FC, CI, GDP, GTP)
  
  # Group the data by 'Name' and summarize the other columns by sum
  aggregated_batting <- aggregated_batting %>%
    bind_rows(wr_tb) 
}



  

aggregated_fielding <- data.frame(
  Name = character(0),  ERR = numeric(0),  PO = numeric(0),  A = numeric(0),  
  DP = numeric(0), FP = numeric(0))


for( fl in list.files(wr_dir, pattern= "Fielding", recursive = TRUE)){
  
  wr_tb <- read.csv(file.path(wr_dir,fl)) %>%
    select(Name, ERR, PO, A, DP)
  
  # Group the data by 'Name' and summarize the other columns by sum
  aggregated_fielding <- aggregated_fielding %>%
    bind_rows(wr_tb) %>%
    group_by(Name) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup()
}



aggregated_data <- left_join(aggregated_batting, aggregated_fielding) %>%
  mutate(Name = case_when( 
    Name == "David SmokoviÄ‡" ~ "David Smokovic",
    Name == "David Smokovi????" ~ "David Smokovic",
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

aggregated_data <- aggregated_data %>%
  group_by(Name) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(AVG = format(H/AB, digits = 2),
         SLG = format((X1B + 2*X2B + 3*X3B + 4*HR)/AB, digits =2),
         OBP = format((H + BB + HBP)/(AB + BB + HBP + SF),digits =2)) %>%
  filter(Name != "TOTALS") %>%
  mutate(FP = format((PO + A)/(PO + A + ERR), digits= 3))

write.table(aggregated_data, paste0("batting_all_", league,".txt"), quote = FALSE, row.names = FALSE,  fileEncoding = "UTF-8") 
