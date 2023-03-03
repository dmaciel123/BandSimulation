## code to prepare `DATASET` dataset goes here

require(data.table)
require(openxlsx)


l5_srf = read.xlsx('SRF/L5_RSR.xlsx', sheet = 1)
l7_srf = read.xlsx('SRF/L7_RSR_Ok.xlsx', sheet = 1)
l8_srf = read.xlsx('SRF/oli_SRF.xlsx', sheet = 1)
s2_srf = read.xlsx('SRF/Spectral Response - Sentinel 2.xlsx', sheet = 2)
s2b_srf = read.xlsx('SRF/Spectral Response - Sentinel 2.xlsx', sheet = 3)

s3_srf = read.xlsx('SRF/olci_FRE.xlsx', sheet = 1)
planet_srf = fread("SRF/Superdove.csv")

usethis::use_data(l5_srf, overwrite = TRUE)
usethis::use_data(l7_srf, overwrite = TRUE)
usethis::use_data(l8_srf, overwrite = TRUE)
usethis::use_data(s2_srf, overwrite = TRUE)
usethis::use_data(s2b_srf, overwrite = TRUE)

usethis::use_data(s3_srf, overwrite = TRUE)
usethis::use_data(planet_srf, overwrite = TRUE)
