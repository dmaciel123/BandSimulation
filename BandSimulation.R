# Band simulation

require(dplyr)
require(data.table)


data = fread("Example/RemoteSensingReflectance.csv", header=T)


source('R/spectra_simulation.R')

## The entry for spectra simulation should be the spectra
#(each collumn is a station and each row is a wavelength)
#

point_names = names(data)[-1]

msi = msi_simulation(spectra = data[,-1], point_name = point_names)
oli = oli_simulation(spectra = data[,-1], point_name = point_names)
etm = etm_simulation(spectra = data[,-1], point_name = point_names)
tm = tm_simulation(spectra = data[,-1], point_name = point_names)
olci = olci_simulation(spectra = data[,-1], point_name = point_names)


