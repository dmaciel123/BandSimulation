# Simulation of satellite data
#
# THis is a group of functions to simulate satellite bands
#
# This function focused on: OLCI, MSI, OLI and TM/ETM+ Spectral Response Functions
#
# Please, note that the input spectra should contain data between 400-900 nm and
# should be organized with the wavelengths in line, and stations in collumns



olci_simulation <- function(spectra, point_name) {
  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A

  S2_A_B1 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,2)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B2 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,3)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B3 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,4)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B4 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,5)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B5 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,6)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B6 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,7)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B7 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,8)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B8 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,9)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B9 <- read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,10)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B10 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,11)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B11 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,12)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B12 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,13)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B13 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,14)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B14 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,15)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B15 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,16)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B16 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,17)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B17 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,18)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B18 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,19)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B19 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,20)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B20 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,21)) %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S2_A_B21 <-read.xlsx("SRF/olci_FRE.xlsx", sheet = 1, cols = c(1,22)) %>% filter(Wavelength >= 400 & Wavelength <= 900)



  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  S2_A_B1 $FAC  <- S2_A_B1$B1  /sum(S2_A_B1$B1  )
  S2_A_B2 $FAC  <- S2_A_B2$B2  /sum(S2_A_B2$B2  )
  S2_A_B3 $FAC  <- S2_A_B3$B3  /sum(S2_A_B3$B3  )
  S2_A_B4 $FAC  <- S2_A_B4$B4  /sum(S2_A_B4$B4  )
  S2_A_B5 $FAC  <- S2_A_B5$B5 /sum(S2_A_B5 $B5 )
  S2_A_B6 $FAC  <- S2_A_B6$B6 /sum(S2_A_B6 $B6 )
  S2_A_B7 $FAC  <- S2_A_B7$B7 /sum(S2_A_B7 $B7 )
  S2_A_B8 $FAC  <- S2_A_B8$B8 /sum(S2_A_B8 $B8 )
  S2_A_B9 $FAC  <- S2_A_B9$B9 /sum(S2_A_B9 $B9 )
  S2_A_B10$FAC  <- S2_A_B10$B10/sum(S2_A_B10$B10)
  S2_A_B11$FAC  <- S2_A_B11$B11/sum(S2_A_B11$B11)
  S2_A_B12$FAC  <- S2_A_B12$B12/sum(S2_A_B12$B12)
  S2_A_B13$FAC  <- S2_A_B13$B13/sum(S2_A_B13$B13)
  S2_A_B14$FAC  <- S2_A_B14$B14/sum(S2_A_B14$B14)
  S2_A_B15$FAC  <- S2_A_B15$B15/sum(S2_A_B15$B15)
  S2_A_B16$FAC  <- S2_A_B16$B16/sum(S2_A_B16$B16)
  S2_A_B17$FAC  <- S2_A_B17$B17/sum(S2_A_B17$B17)
  S2_A_B18$FAC  <- S2_A_B18$B18/sum(S2_A_B18$B18)
  S2_A_B19$FAC  <- S2_A_B19$B19/sum(S2_A_B19$B19)


  espec <- data.frame(Wave = c(400:900), spectra)


  olci.df.sim <- data.frame(1:19)


  for(i in 2:ncol(espec)) {

    olci.df.sim[1,i] <- sum(na.rm = T, S2_A_B1$FAC * espec[,i])
    olci.df.sim[2,i] <- sum(na.rm = T, S2_A_B2$FAC * espec[,i])
    olci.df.sim[3,i] <- sum(na.rm = T, S2_A_B3$FAC * espec[,i])
    olci.df.sim[4,i] <- sum(na.rm = T, S2_A_B4$FAC * espec[,i])
    olci.df.sim[5,i] <- sum(na.rm = T, S2_A_B5$FAC * espec[,i])
    olci.df.sim[6,i] <- sum(na.rm = T, S2_A_B6$FAC * espec[,i])
    olci.df.sim[7,i] <- sum(na.rm = T, S2_A_B7$FAC * espec[,i])
    olci.df.sim[8,i] <- sum(na.rm = T, S2_A_B8$FAC * espec[,i])
    olci.df.sim[9,i] <- sum(na.rm = T, S2_A_B9$FAC * espec[,i])
    olci.df.sim[10,i] <-sum(na.rm = T, S2_A_B10$FAC * espec[,i])
    olci.df.sim[11,i]<- sum(na.rm = T, S2_A_B11$FAC * espec[,i])
    olci.df.sim[12,i]<- sum(na.rm = T, S2_A_B12$FAC * espec[,i])
    olci.df.sim[13,i]<- sum(na.rm = T, S2_A_B13$FAC * espec[,i])
    olci.df.sim[14,i]<- sum(na.rm = T, S2_A_B14$FAC * espec[,i])
    olci.df.sim[15,i]<- sum(na.rm = T, S2_A_B15$FAC * espec[,i])
    olci.df.sim[16,i]<- sum(na.rm = T, S2_A_B16$FAC * espec[,i])
    olci.df.sim[17,i]<- sum(na.rm = T, S2_A_B17$FAC * espec[,i])
    olci.df.sim[18,i]<- sum(na.rm = T, S2_A_B18$FAC * espec[,i])
    olci.df.sim[19,i]<- sum(na.rm = T, S2_A_B19$FAC * espec[,i])


  }

  olci.df.sim[,1] <-  c(400,
                        412,
                        442,
                        490,
                        510,
                        560,
                        620,
                        665,
                        673,
                        681,
                        708,
                        753,
                        761,
                        764,
                        767,
                        778,
                        865,
                        885,
                        900)

  names(olci.df.sim) <- c("Wave", point_name)

  return(olci.df.sim)

}

msi_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A
  S2_A_B1 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,2)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B2 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,3)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B3 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,4)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B4 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,5)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B5 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,6)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B6 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,7)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B7 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,8)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B8 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,9)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B8a <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 2, cols = c(1,10)) %>% filter(SR_WL >= 400 & SR_WL <= 900)

  #L? as fun??es de resposta - SENTINEL 2B

  S2_B_B1 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,2)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B2 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,3)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B3 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,4)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B4 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,5)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B5 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,6)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B6 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,7)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B7 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,8)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B8 <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,9)) %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B8a <- read.xlsx("SRF/Spectral Response - Sentinel 2.xlsx", sheet = 3, cols = c(1,10)) %>% filter(SR_WL >= 400 & SR_WL <= 900)

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  S2_A_B1$FAC <- S2_A_B1$S2A_SR_AV_B1/sum(S2_A_B1$S2A_SR_AV_B1)
  S2_A_B2$FAC <- S2_A_B2$S2A_SR_AV_B2/sum(S2_A_B2$S2A_SR_AV_B2)
  S2_A_B3$FAC <- S2_A_B3$S2A_SR_AV_B3/sum(S2_A_B3$S2A_SR_AV_B3)
  S2_A_B4$FAC <- S2_A_B4$S2A_SR_AV_B4/sum(S2_A_B4$S2A_SR_AV_B4)
  S2_A_B5$FAC <- S2_A_B5$S2A_SR_AV_B5/sum(S2_A_B5$S2A_SR_AV_B5)
  S2_A_B6$FAC <- S2_A_B6$S2A_SR_AV_B6/sum(S2_A_B6$S2A_SR_AV_B6)
  S2_A_B7$FAC <- S2_A_B7$S2A_SR_AV_B7/sum(S2_A_B7$S2A_SR_AV_B7)
  S2_A_B8$FAC <- S2_A_B8$S2A_SR_AV_B8/sum(S2_A_B8$S2A_SR_AV_B8)
  S2_A_B8a$FAC <- S2_A_B8a$S2A_SR_AV_B8A/sum(S2_A_B8a$S2A_SR_AV_B8A)

  S2_B_B1$FAC <- S2_B_B1$S2B_SR_AV_B1/sum(S2_B_B1$S2B_SR_AV_B1)
  S2_B_B2$FAC <- S2_B_B2$S2B_SR_AV_B2/sum(S2_B_B2$S2B_SR_AV_B2)
  S2_B_B3$FAC <- S2_B_B3$S2B_SR_AV_B3/sum(S2_B_B3$S2B_SR_AV_B3)
  S2_B_B4$FAC <- S2_B_B4$S2B_SR_AV_B4/sum(S2_B_B4$S2B_SR_AV_B4)
  S2_B_B5$FAC <- S2_B_B5$S2B_SR_AV_B5/sum(S2_B_B5$S2B_SR_AV_B5)
  S2_B_B6$FAC <- S2_B_B6$S2B_SR_AV_B6/sum(S2_B_B6$S2B_SR_AV_B6)
  S2_B_B7$FAC <- S2_B_B7$S2B_SR_AV_B7/sum(S2_B_B7$S2B_SR_AV_B7)
  S2_B_B8$FAC <- S2_B_B8$S2B_SR_AV_B8/sum(S2_B_B8$S2B_SR_AV_B8)
  S2_B_B8a$FAC <- S2_B_B8a$S2B_SR_AV_B8A/sum(S2_B_B8a$S2B_SR_AV_B8A)


  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_S2A <- data.frame(1:9)
  df_SIM_S2B <- data.frame(1:9)



  for(i in 2:ncol(espec)) {

    df_SIM_S2A[1,i] <- sum(na.rm = T, S2_A_B1$FAC * espec[,i])
    df_SIM_S2A[2,i] <- sum(na.rm = T, S2_A_B3$FAC * espec[,i])
    df_SIM_S2A[3,i] <- sum(na.rm = T, S2_A_B4$FAC * espec[,i])
    df_SIM_S2A[4,i] <- sum(na.rm = T, S2_A_B5$FAC * espec[,i])
    df_SIM_S2A[5,i] <- sum(na.rm = T, S2_A_B6$FAC * espec[,i])
    df_SIM_S2A[6,i] <- sum(na.rm = T, S2_A_B7$FAC * espec[,i])
    df_SIM_S2A[7,i] <- sum(na.rm = T, S2_A_B8$FAC * espec[,i])
    df_SIM_S2A[8,i] <- sum(na.rm = T, S2_A_B8a$FAC * espec[,i])
    df_SIM_S2A[9,i] <- sum(na.rm = T, S2_A_B8a$FAC * espec[,i])

    df_SIM_S2B[1,i] <- sum(na.rm = T, S2_B_B1$FAC * espec[,i])
    df_SIM_S2B[2,i] <- sum(na.rm = T, S2_B_B3$FAC * espec[,i])
    df_SIM_S2B[3,i] <- sum(na.rm = T, S2_B_B4$FAC * espec[,i])
    df_SIM_S2B[4,i] <- sum(na.rm = T, S2_B_B5$FAC * espec[,i])
    df_SIM_S2B[5,i] <- sum(na.rm = T, S2_B_B6$FAC * espec[,i])
    df_SIM_S2B[6,i] <- sum(na.rm = T, S2_B_B7$FAC * espec[,i])
    df_SIM_S2B[7,i] <- sum(na.rm = T, S2_B_B8$FAC * espec[,i])
    df_SIM_S2B[8,i] <- sum(na.rm = T, S2_B_B8a$FAC * espec[,i])
    df_SIM_S2B[9,i] <- sum(na.rm = T, S2_B_B8a$FAC * espec[,i])

  }


  df_SIM_S2A[,1] <- as.numeric(c("440","490","560","665","705","740","783","842","865"))
  df_SIM_S2B[,1] <- as.numeric(c("440","490","560","665","705","740","783","842","865"))


  names(df_SIM_S2A) <- c("Wave", point_name)
  names(df_SIM_S2B) <- c("Wave", point_name)


  res = list(s2a = df_SIM_S2A, s2b = df_SIM_S2B)

}

oli_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A
  OLI_B1 <- read.xlsx("SRF/oli_SRF.xlsx", sheet = 1, cols = c(1,2))
  OLI_B2 <- read.xlsx("SRF/oli_SRF.xlsx", sheet = 1, cols = c(1,3))
  OLI_B3 <- read.xlsx("SRF/oli_SRF.xlsx", sheet = 1, cols = c(1,4))
  OLI_B4 <- read.xlsx("SRF/oli_SRF.xlsx", sheet = 1, cols = c(1,5))
  OLI_B5 <- read.xlsx("SRF/oli_SRF.xlsx", sheet = 1, cols = c(1,6))

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  OLI_B1$FAC <- OLI_B1$OB1/sum(OLI_B1$OB1)
  OLI_B2$FAC <- OLI_B2$OB2/sum(OLI_B2$OB2)
  OLI_B3$FAC <- OLI_B3$OB3/sum(OLI_B3$OB3)
  OLI_B4$FAC <- OLI_B4$OB4/sum(OLI_B4$OB4)
  OLI_B5$FAC <- OLI_B5$OB5/sum(OLI_B5$OB5)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_OLI <- data.frame(1:5)



  for(i in 2:ncol(espec)) {

    df_SIM_OLI[1,i] <- sum(na.rm = T, OLI_B1$FAC * espec[,i])
    df_SIM_OLI[2,i] <- sum(na.rm = T, OLI_B2$FAC * espec[,i])
    df_SIM_OLI[3,i] <- sum(na.rm = T, OLI_B3$FAC * espec[,i])
    df_SIM_OLI[4,i] <- sum(na.rm = T, OLI_B4$FAC * espec[,i])
    df_SIM_OLI[5,i] <- sum(na.rm = T, OLI_B5$FAC * espec[,i])



  }


  df_SIM_OLI[,1] <- as.numeric(c("440", "490","560","665","865"))


  names(df_SIM_OLI) <- c("Wave", point_name)

  return(df_SIM_OLI)


}

etm_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simulation to Landsat-7/ETM+

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  etm_B1 <- read.xlsx("SRF/L7_RSR_Ok.xlsx", sheet = 1, cols = c(1,2))
  etm_B2 <- read.xlsx("SRF/L7_RSR_Ok.xlsx", sheet = 1, cols = c(1,3))
  etm_B3 <- read.xlsx("SRF/L7_RSR_Ok.xlsx", sheet = 1, cols = c(1,4))
  etm_B4 <- read.xlsx("SRF/L7_RSR_Ok.xlsx", sheet = 1, cols = c(1,5))

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  etm_B1$FAC <- etm_B1$ETMB1/sum(etm_B1$ETMB1)
  etm_B2$FAC <- etm_B2$ETMB2/sum(etm_B2$ETMB2)
  etm_B3$FAC <- etm_B3$ETMB3/sum(etm_B3$ETMB3)
  etm_B4$FAC <- etm_B4$ETMB4/sum(etm_B4$ETMB4)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_ETM <- data.frame(1:4)




  for(i in 2:ncol(espec)) {

    df_SIM_ETM[1,i] <- sum(na.rm = T, etm_B1$FAC * espec[,i])
    df_SIM_ETM[2,i] <- sum(na.rm = T, etm_B2$FAC * espec[,i])
    df_SIM_ETM[3,i] <- sum(na.rm = T, etm_B3$FAC * espec[,i])
    df_SIM_ETM[4,i] <- sum(na.rm = T, etm_B4$FAC * espec[,i])



  }



  df_SIM_ETM[,1] <- as.numeric(c("490","560","665","865"))


  names(df_SIM_ETM) <- c("Wave", point_name)

  return(df_SIM_ETM)




}


tm_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A
  tm_B1 <- read.xlsx("SRF/L5_RSR.xlsx", sheet = 1, cols = c(1,2))
  tm_B2 <- read.xlsx("SRF/L5_RSR.xlsx", sheet = 1, cols = c(1,3))
  tm_B3 <- read.xlsx("SRF/L5_RSR.xlsx", sheet = 1, cols = c(1,4))
  tm_B4 <- read.xlsx("SRF/L5_RSR.xlsx", sheet = 1, cols = c(1,5))

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  tm_B1$FAC <- tm_B1$TMB1/sum(tm_B1$TMB1)
  tm_B2$FAC <- tm_B2$TMB2/sum(tm_B2$TMB2)
  tm_B3$FAC <- tm_B3$TMB3/sum(tm_B3$TMB3)
  tm_B4$FAC <- tm_B4$TMB4/sum(tm_B4$TMB4)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_tm <- data.frame(1:4)




  for(i in 2:ncol(espec)) {

    df_SIM_tm[1,i] <- sum(na.rm = T, tm_B1$FAC * espec[,i])
    df_SIM_tm[2,i] <- sum(na.rm = T, tm_B2$FAC * espec[,i])
    df_SIM_tm[3,i] <- sum(na.rm = T, tm_B3$FAC * espec[,i])
    df_SIM_tm[4,i] <- sum(na.rm = T, tm_B4$FAC * espec[,i])



  }



  df_SIM_tm[,1] <- as.numeric(c("490","560","665","865"))


  names(df_SIM_tm) <- c("Wave", point_name)

  return(df_SIM_tm)




}




superdove_simulation <- function(spectra, point_name) {

  #Simulation for SuperDove bands
  #Data from here: https://support.planet.com/hc/en-us/articles/360014290293-Do-you-provide-Relative-Spectral-Response-Curves-RSRs-for-your-satellites-

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #Read the SRF
  # Bands
  # coastal
  # blue
  # green
  # green_II
  # yellow
  # red
  # red_edge
  # NIR

  B1 =     read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,2)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B2 =        read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,3)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B3 =       read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,4)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B4 =    read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,5)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B5 =      read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,6)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B6 =         read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,7)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B7 =    read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,8)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B8 =         read.table('SRF/Superdove.csv', header= T, sep = ',')[,c(1,9)] %>% filter(Wavelength >= 400 & Wavelength <= 900)




  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  B1$FAC  <- B1[,2]  /sum(B1[,2])
  B2$FAC  <- B2[,2]  /sum(B2[,2])
  B3$FAC  <- B3[,2]  /sum(B3[,2])
  B4$FAC  <- B4[,2]  /sum(B4[,2])
  B5$FAC  <- B5[,2] /sum(B5[,2])
  B6$FAC  <- B6[,2] /sum(B6[,2])
  B7$FAC  <- B7[,2] /sum(B7[,2])
  B8$FAC  <- B8[,2] /sum(B8[,2])


  espec <- data.frame(Wave = c(400:900), spectra)


  superdove.df.sim <- data.frame(1:8)


  for(i in 2:ncol(espec)) {

    superdove.df.sim[1,i] <- sum(na.rm = T, B1$FAC * espec[,i])
    superdove.df.sim[2,i] <- sum(na.rm = T, B2$FAC * espec[,i])
    superdove.df.sim[3,i] <- sum(na.rm = T, B3$FAC * espec[,i])
    superdove.df.sim[4,i] <- sum(na.rm = T, B4$FAC * espec[,i])
    superdove.df.sim[5,i] <- sum(na.rm = T, B5$FAC * espec[,i])
    superdove.df.sim[6,i] <- sum(na.rm = T, B6$FAC * espec[,i])
    superdove.df.sim[7,i] <- sum(na.rm = T, B7$FAC * espec[,i])
    superdove.df.sim[8,i] <- sum(na.rm = T, B8$FAC * espec[,i])



  }

  superdove.df.sim[,1] <-  c(443, 490, 531, 565, 610, 665, 705, 865)

  names(superdove.df.sim) <- c("Wave", point_name)

  return(superdove.df.sim)

}

