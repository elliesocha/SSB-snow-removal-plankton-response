# Chem table output 
chems = read_csv('SSBdata/SSB_Chems.csv')

a = chems |> 
  filter(month(sample_date) < 4 | month(sample_date) >= 12) |> 
  mutate_all(~replace(., .==-99, NA))

# Range of DOC in 0-3 m
a |> filter(Depth <=3) |> 
  group_by(year(sample_date)) |> 
  summarise(docmin = min(DOC, na.rm = T), docmax = max(DOC, na.rm = T))

# Setup table
b = a |> select(-lakeid, -Replicate) |> 
  group_by(sample_date, Depth) |> 
  summarise_all(mean, na.rm = T) |> 
  arrange(sample_date, Depth) |> 
  mutate(sample_date = as.character(sample_date))
  

xtable(b, type = "latex", file = "filename2.tex")
print(xtable(b, type = "latex", file = "filename2.tex"), include.rownames=FALSE)
