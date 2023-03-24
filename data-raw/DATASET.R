## code to prepare `DATASET` dataset goes here

require(data.table)
require(openxlsx)


modis_srf = read.table("clipboard", header=F)
names(modis_srf) = c('wavelength','RSR_412', 'RSR_443','RSR_469','RSR_488','RSR_531','RSR_551','RSR_555',
                     'RSR_645','RSR_667','RSR_678','RSR_748',
                     'RSR_859','RSR_869','RSR_1240','RSR_1640','RSR_2130')


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
usethis::use_data(modis_srf, overwrite = TRUE)

