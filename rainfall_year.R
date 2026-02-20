###################################################
####           Max 24hrs / year                ####
###################################################
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(trend)

# Read files
files_names <- list.files(path = './daily_data', pattern = '*.csv', full.names = T)
aux <- readxl::read_xlsx("./daily_data/Celulas-FF.xlsx")
dir.create("./yearly_data", showWarnings = FALSE)
dir.create("./results_tables", showWarnings = FALSE)

results_max <- matrix(NA, ncol = 5, nrow = 159, dimnames = list(NULL, c("Posto", "Pettit", "Pettit-Change", "Mann.Kendall", "Sen.Slope")))
results_sum <- matrix(NA, ncol = 5, nrow = 159, dimnames = list(NULL, c("Posto", "Pettit", "Pettit-Change", "Mann.Kendall", "Sen.Slope")))
for (i in 1:length(files_names)) {
  # Extract max 24 hrs per year and total per year
  rainfall_year <- read_csv(files_names[i], col_types = c("D", "n"), col_names = c("date", "pr")) |> 
    drop_na() |> 
    mutate(year = year(date)) |> 
    mutate(pr = as.numeric(pr)) |> 
    group_by(year) |> 
    summarise(pr_max = max(pr, na.rm = TRUE),
              pr_sum = sum(pr, na.rm = TRUE)) |> 
    filter(year >= 1985)
  
  # extract latitude, longitude and identify "posto"
  parts <- strsplit(files_names[i], "/")[[1]]
  lat_lon <- str_extract_all(parts[3], "-?\\d+\\.\\d+")[[1]]
  write.csv(rainfall_year, paste0("./yearly_data/", parts[3]), row.names = FALSE)
  
  # Perform statistical tests
  pettit_max <- pettitt.test(rainfall_year$pr_max) 
  mk_max <- mk.test(rainfall_year$pr_max) 
  sens_max <- sens.slope(rainfall_year$pr_max) 
  
  pettit_sum <- pettitt.test(rainfall_year$pr_sum) 
  mk_sum <- mk.test(rainfall_year$pr_sum) 
  sens_sum <- sens.slope(rainfall_year$pr_sum) 
  
  # Save results
  results_max[i, ] <- c(aux |> filter(LAT == lat_lon[1], LONG == lat_lon[2]) |> select(GRADE) |> as.numeric(),
                        pettit_max$p.value,
                        as.numeric(rainfall_year[as.numeric(pettit_max$estimate), ]$year)[1],
                        mk_max$p.value, sens_max$estimates)
  
  results_sum[i, ] <- c(aux |> filter(LAT == lat_lon[1], LONG == lat_lon[2]) |> select(GRADE) |> as.numeric(),
    pettit_sum$p.value,
    as.numeric(rainfall_year[as.numeric(pettit_sum$estimate), ]$year)[1],
    mk_sum$p.value,  sens_sum$estimates)
}


write.csv(round(results_max, 4), "./results_tables/results_max_year.csv")
write.csv(round(results_sum, 4), "./results_tables/results_sum_year.csv")


# Save results statistically significant
results_max |> data.frame() |> filter(`Pettit` < 0.1 & Mann.Kendall < 0.1) |> round(4) |> write.csv( "./results_tables/results_max_year_significants.csv")
results_sum |> data.frame() |> filter(`Pettit` < 0.1 & Mann.Kendall < 0.1) |> round(4) |> write.csv( "./results_tables/results_sum_year_significants.csv")


## Additional Analisys: ESALQ 
esalq <- readxl::read_xlsx("./aux_data/ESALQ_data.xlsx", sheet = 1) |> 
  mutate(year = as.numeric(ANO), pr = as.numeric(pr)) |> 
  group_by(year) |> 
  summarise(pr_max = max(pr, na.rm = TRUE),
            pr_sum = sum(pr, na.rm = TRUE)) |> 
  filter(year >= 1917, year <= 2022)

pettit_max <- pettitt.test(esalq$pr_max) 
mk_max <- mk.test(esalq$pr_max) 
sens_max <- sens.slope(esalq$pr_max) 

pettit_sum <- pettitt.test(esalq$pr_sum) 
mk_sum <- mk.test(esalq$pr_sum) 
sens_sum <- sens.slope(esalq$pr_sum) 

esalq_results <- rbind(round(c(pettit_max$p.value, esalq$year[as.numeric(pettit_max$estimate)], mk_max$p.value, sens_max$estimates), 4),
                      round(c(pettit_sum$p.value, esalq$year[as.numeric(pettit_sum$estimate)], mk_sum$p.value, sens_sum$estimates), 4))

write.csv(esalq_results, "./results_tables/esalq_results.csv", row.names = c("esalq_max", "esalq_sum"))
## Additional Analisys: IAC/Campinas

iac <- readxl::read_xlsx("./aux_data/D4-044_Chuva_Diaria_Serie_Campinas.xlsx", sheet = 1) |>
  select(`Mês/Ano`, `Chuva máxima`, `Chuva total`) |> 
  separate(`Mês/Ano`, into = c("month", "year"), sep = "/") |> 
  mutate(chuva_max = as.numeric(`Chuva máxima`), chuva_total = as.numeric(`Chuva total`)) |> 
  group_by(year) |> 
  summarise(pr_max = max(chuva_max, na.rm = TRUE),
    pr_sum = sum(chuva_total, na.rm = TRUE)) |> 
  filter(year >= 1941, year <= 2022)

pettit_max <- pettitt.test(iac$pr_max) 
mk_max <- mk.test(iac$pr_max) 
sens_max <- sens.slope(iac$pr_max) 

pettit_sum <- pettitt.test(iac$pr_sum) 
mk_sum <- mk.test(iac$pr_sum) 
sens_sum <- sens.slope(iac$pr_sum) 

iac_results <- rbind(round(c(pettit_max$p.value, as.numeric(iac$year[as.numeric(pettit_max$estimate)]), mk_max$p.value, sens_max$estimates), 4),
                     round(c(pettit_sum$p.value, as.numeric(iac$year[as.numeric(pettit_sum$estimate)]), mk_sum$p.value, sens_sum$estimates), 4))


write.csv(iac_results, "./results_tables/iac_results.csv", row.names = c("iac_max", "iac_sum"))


