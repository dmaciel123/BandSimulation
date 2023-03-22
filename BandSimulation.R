# Band simulation

require(dplyr)
require(data.table)
require(bandSimulation)

data = fread("Example/GLORIA_Rrs.csv", header=T)


## The entry for spectra simulation should be the spectra
#(each collumn is a station and each row is a wavelength)
#

point_names = data$GLORIA_ID

## Select Rrs from 400 to 900 nm and transpose (Lines: wavelength, columns points)

rrs = select(data, paste("Rrs", 400:900, sep = "_")) %>% t()

# Performing simulation

msi = msi_simulation(spectra = rrs, point_name = point_names)
oli = oli_simulation(spectra = rrs, point_name = point_names)
etm = etm_simulation(spectra = rrs, point_name = point_names)
tm = tm_simulation(spectra = rrs, point_name = point_names)
olci = olci_simulation(spectra = rrs, point_name = point_names)
superDove = superdove_simulation(spectra = rrs, point_name = point_names)


