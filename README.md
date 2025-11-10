# 🌊 BandSimulation

**BandSimulation** is an R package designed to simulate the spectral bands of remote sensing instruments based on their **Spectral Response Functions (SRF)** or **Relative Spectral Responses (RSR)**.  
It enables users to generate realistic band-averaged reflectance values from hyperspectral or in situ spectral data, facilitating cross-sensor comparisons and model applications.

---

## 🛰️ Supported Sensors

The package currently includes SRF/RSR data for the following satellite sensors:

- **Landsat-5 / TM**  
- **Landsat-7 / ETM+**  
- **Landsat-8 / OLI**  
- **Sentinel-2 / MSI** (A and B)  
- **Sentinel-3 / OLCI**  
- **PlanetScope SuperDove**
- **CBERS-4 / MUX**
- **CBERS-4A / MUX**
- **CBERS-4A / WPM**
- **Amazônia-1 / WFI**


All spectral response functions were obtained from their official mission sources.

---

## 📘 Example

An example script is provided: **`BandSimulation.R`**  
It demonstrates how to apply the package using a subset of the freely available **Rrs** data from the **GLORIA dataset**  
(Lehmann et al., 2023, *Scientific Data*, https://doi.org/10.1038/s41597-023-01973-y).

---

## ⚙️ Installation and Usage

You can install and run **BandSimulation** directly from its GitHub repository.

### 🔹 Option 1: Instalation using devtools directly in R
```bash

devtools::install_github("dmaciel123/BandSimulation")
