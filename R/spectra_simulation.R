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

  S3_A_B1 <- s3_srf[,c(1,2)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B2 <- s3_srf[,c(1,3)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B3 <- s3_srf[,c(1,4)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B4 <- s3_srf[,c(1,5)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B5 <- s3_srf[,c(1,6)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B6 <- s3_srf[,c(1,7)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B7 <- s3_srf[,c(1,8)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B8 <- s3_srf[,c(1,9)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B9 <- s3_srf[,c(1,10)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B10 <-s3_srf[,c(1,11)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B11 <-s3_srf[,c(1,12)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B12 <-s3_srf[,c(1,13)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B13 <-s3_srf[,c(1,14)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B14 <-s3_srf[,c(1,15)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B15 <-s3_srf[,c(1,16)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B16 <-s3_srf[,c(1,17)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B17 <-s3_srf[,c(1,18)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B18 <-s3_srf[,c(1,19)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B19 <-s3_srf[,c(1,20)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B20 <-s3_srf[,c(1,21)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  S3_A_B21 <-s3_srf[,c(1,22)] %>% filter(Wavelength >= 400 & Wavelength <= 900)



  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  S3_A_B1 $FAC  <- S3_A_B1$B1  /sum(S3_A_B1$B1  )
  S3_A_B2 $FAC  <- S3_A_B2$B2  /sum(S3_A_B2$B2  )
  S3_A_B3 $FAC  <- S3_A_B3$B3  /sum(S3_A_B3$B3  )
  S3_A_B4 $FAC  <- S3_A_B4$B4  /sum(S3_A_B4$B4  )
  S3_A_B5 $FAC  <- S3_A_B5$B5 /sum(S3_A_B5 $B5 )
  S3_A_B6 $FAC  <- S3_A_B6$B6 /sum(S3_A_B6 $B6 )
  S3_A_B7 $FAC  <- S3_A_B7$B7 /sum(S3_A_B7 $B7 )
  S3_A_B8 $FAC  <- S3_A_B8$B8 /sum(S3_A_B8 $B8 )
  S3_A_B9 $FAC  <- S3_A_B9$B9 /sum(S3_A_B9 $B9 )
  S3_A_B10$FAC  <- S3_A_B10$B10/sum(S3_A_B10$B10)
  S3_A_B11$FAC  <- S3_A_B11$B11/sum(S3_A_B11$B11)
  S3_A_B12$FAC  <- S3_A_B12$B12/sum(S3_A_B12$B12)
  S3_A_B13$FAC  <- S3_A_B13$B13/sum(S3_A_B13$B13)
  S3_A_B14$FAC  <- S3_A_B14$B14/sum(S3_A_B14$B14)
  S3_A_B15$FAC  <- S3_A_B15$B15/sum(S3_A_B15$B15)
  S3_A_B16$FAC  <- S3_A_B16$B16/sum(S3_A_B16$B16)
  S3_A_B17$FAC  <- S3_A_B17$B17/sum(S3_A_B17$B17)
  S3_A_B18$FAC  <- S3_A_B18$B18/sum(S3_A_B18$B18)
  S3_A_B19$FAC  <- S3_A_B19$B19/sum(S3_A_B19$B19)

espec <- data.frame(Wave = c(400:900), spectra)


  olci.df.sim <- data.frame(1:19)


  for(i in 2:ncol(espec)) {

    olci.df.sim[1,i] <- sum(na.rm = T, S3_A_B1$FAC * espec[,i])
    olci.df.sim[2,i] <- sum(na.rm = T, S3_A_B2$FAC * espec[,i])
    olci.df.sim[3,i] <- sum(na.rm = T, S3_A_B3$FAC * espec[,i])
    olci.df.sim[4,i] <- sum(na.rm = T, S3_A_B4$FAC * espec[,i])
    olci.df.sim[5,i] <- sum(na.rm = T, S3_A_B5$FAC * espec[,i])
    olci.df.sim[6,i] <- sum(na.rm = T, S3_A_B6$FAC * espec[,i])
    olci.df.sim[7,i] <- sum(na.rm = T, S3_A_B7$FAC * espec[,i])
    olci.df.sim[8,i] <- sum(na.rm = T, S3_A_B8$FAC * espec[,i])
    olci.df.sim[9,i] <- sum(na.rm = T, S3_A_B9$FAC * espec[,i])
    olci.df.sim[10,i] <-sum(na.rm = T, S3_A_B10$FAC * espec[,i])
    olci.df.sim[11,i]<- sum(na.rm = T, S3_A_B11$FAC * espec[,i])
    olci.df.sim[12,i]<- sum(na.rm = T, S3_A_B12$FAC * espec[,i])
    olci.df.sim[13,i]<- sum(na.rm = T, S3_A_B13$FAC * espec[,i])
    olci.df.sim[14,i]<- sum(na.rm = T, S3_A_B14$FAC * espec[,i])
    olci.df.sim[15,i]<- sum(na.rm = T, S3_A_B15$FAC * espec[,i])
    olci.df.sim[16,i]<- sum(na.rm = T, S3_A_B16$FAC * espec[,i])
    olci.df.sim[17,i]<- sum(na.rm = T, S3_A_B17$FAC * espec[,i])
    olci.df.sim[18,i]<- sum(na.rm = T, S3_A_B18$FAC * espec[,i])
    olci.df.sim[19,i]<- sum(na.rm = T, S3_A_B19$FAC * espec[,i])


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
  S2_A_B1 <-  s2_srf[,c(1,2)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B2 <-  s2_srf[,c(1,3)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B3 <-  s2_srf[,c(1,4)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B4 <-  s2_srf[,c(1,5)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B5 <-  s2_srf[,c(1,6)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B6 <-  s2_srf[,c(1,7)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B7 <-  s2_srf[,c(1,8)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B8 <-  s2_srf[,c(1,9)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_A_B8a <- s2_srf[,c(1,10)] %>% filter(SR_WL >= 400 & SR_WL <= 900)

  #L? as fun??es de resposta - SENTINEL 2B
  S2_B_B1 <-  s2b_srf[,c(1,2)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B2 <-  s2b_srf[,c(1,3)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B3 <-  s2b_srf[,c(1,4)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B4 <-  s2b_srf[,c(1,5)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B5 <-  s2b_srf[,c(1,6)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B6 <-  s2b_srf[,c(1,7)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B7 <-  s2b_srf[,c(1,8)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B8 <-  s2b_srf[,c(1,9)] %>% filter(SR_WL >= 400 & SR_WL <= 900)
  S2_B_B8a <- s2b_srf[,c(1,10)] %>% filter(SR_WL >= 400 & SR_WL <= 900)

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
    df_SIM_S2A[2,i] <- sum(na.rm = T, S2_A_B2$FAC * espec[,i])
    df_SIM_S2A[3,i] <- sum(na.rm = T, S2_A_B3$FAC * espec[,i])
    df_SIM_S2A[4,i] <- sum(na.rm = T, S2_A_B4$FAC * espec[,i])
    df_SIM_S2A[5,i] <- sum(na.rm = T, S2_A_B5$FAC * espec[,i])
    df_SIM_S2A[6,i] <- sum(na.rm = T, S2_A_B6$FAC * espec[,i])
    df_SIM_S2A[7,i] <- sum(na.rm = T, S2_A_B7$FAC * espec[,i])
    df_SIM_S2A[8,i] <- sum(na.rm = T, S2_A_B8$FAC * espec[,i])
    df_SIM_S2A[9,i] <- sum(na.rm = T, S2_A_B8a$FAC * espec[,i])

    df_SIM_S2B[1,i] <- sum(na.rm = T, S2_B_B1$FAC * espec[,i])
    df_SIM_S2B[2,i] <- sum(na.rm = T, S2_B_B2$FAC * espec[,i])
    df_SIM_S2B[3,i] <- sum(na.rm = T, S2_B_B3$FAC * espec[,i])
    df_SIM_S2B[4,i] <- sum(na.rm = T, S2_B_B4$FAC * espec[,i])
    df_SIM_S2B[5,i] <- sum(na.rm = T, S2_B_B5$FAC * espec[,i])
    df_SIM_S2B[6,i] <- sum(na.rm = T, S2_B_B6$FAC * espec[,i])
    df_SIM_S2B[7,i] <- sum(na.rm = T, S2_B_B7$FAC * espec[,i])
    df_SIM_S2B[8,i] <- sum(na.rm = T, S2_B_B8$FAC * espec[,i])
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
  OLI_B1 <- l8_srf[,c(1,2)]
  OLI_B2 <- l8_srf[,c(1,3)]
  OLI_B3 <- l8_srf[,c(1,4)]
  OLI_B4 <- l8_srf[,c(1,5)]
  OLI_B5 <- l8_srf[,c(1,6)]

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

  #Simulation to Landsat-7/ETM+.

  require(openxlsx)
  require(dplyr)
  require(tidyr)



  etm_B1 <- l7_srf[,c(1,2)]
  etm_B2 <- l7_srf[,c(1,3)]
  etm_B3 <- l7_srf[,c(1,4)]
  etm_B4 <- l7_srf[,c(1,5)]

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
  tm_B1 <- l5_srf[,c(1,2)]
  tm_B2 <- l5_srf[,c(1,3)]
  tm_B3 <- l5_srf[,c(1,4)]
  tm_B4 <- l5_srf[,c(1,5)]

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

  B1 =        planet_srf[,c(1,2)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B2 =        planet_srf[,c(1,3)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B3 =        planet_srf[,c(1,4)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B4 =        planet_srf[,c(1,5)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B5 =        planet_srf[,c(1,6)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B6 =        planet_srf[,c(1,7)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B7 =        planet_srf[,c(1,8)] %>% filter(Wavelength >= 400 & Wavelength <= 900)
  B8 =        planet_srf[,c(1,9)] %>% filter(Wavelength >= 400 & Wavelength <= 900)




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


modis_simulation <- function(spectra, point_name) {
  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A

  MODIS_B1 <- modis_srf[,c(1,2)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B2 <- modis_srf[,c(1,3)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B3 <- modis_srf[,c(1,4)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B4 <- modis_srf[,c(1,5)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B5 <- modis_srf[,c(1,6)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B6 <- modis_srf[,c(1,7)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B7 <- modis_srf[,c(1,8)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B8 <- modis_srf[,c(1,9)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B9 <- modis_srf[,c(1,10)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B10 <-modis_srf[,c(1,11)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B11 <-modis_srf[,c(1,12)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B12 <-modis_srf[,c(1,13)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B13 <-modis_srf[,c(1,14)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B14 <-modis_srf[,c(1,15)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B15 <-modis_srf[,c(1,16)] %>% filter(wavelength >= 400 & wavelength <= 900)
  MODIS_B16 <-modis_srf[,c(1,17)] %>% filter(wavelength >= 400 & wavelength <= 900)

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  MODIS_B1$FAC  <-  MODIS_B1$RSR_412  /sum(MODIS_B1$RSR_412)
  MODIS_B2$FAC  <-  MODIS_B2$RSR_443  /sum(MODIS_B2$RSR_443)
  MODIS_B3$FAC  <-  MODIS_B3$RSR_469  /sum(MODIS_B3$RSR_469)
  MODIS_B4$FAC  <-  MODIS_B4$RSR_488  /sum(MODIS_B4$RSR_488)
  MODIS_B5$FAC  <-  MODIS_B5$RSR_531 /sum(MODIS_B5$RSR_531)
  MODIS_B6$FAC  <-  MODIS_B6$RSR_551 /sum(MODIS_B6$RSR_551)
  MODIS_B7$FAC  <-  MODIS_B7$RSR_555 /sum(MODIS_B7$RSR_555)
  MODIS_B8$FAC  <-  MODIS_B8$RSR_645 /sum(MODIS_B8$RSR_645)
  MODIS_B9$FAC  <-  MODIS_B9$RSR_667 /sum(MODIS_B9$RSR_667)
  MODIS_B10$FAC  <- MODIS_B10$RSR_678/sum(MODIS_B10$RSR_678)
  MODIS_B11$FAC  <- MODIS_B11$RSR_748/sum(MODIS_B11$RSR_748)
  MODIS_B12$FAC  <- MODIS_B12$RSR_859/sum(MODIS_B12$RSR_859)
  MODIS_B13$FAC  <- MODIS_B13$RSR_869/sum(MODIS_B13$RSR_869)
  MODIS_B14$FAC  <- MODIS_B14$RSR_1240/sum(MODIS_B14$RSR_1240)
  MODIS_B15$FAC  <- MODIS_B15$RSR_1640/sum(MODIS_B15$RSR_1640)
  MODIS_B16$FAC  <- MODIS_B16$RSR_2130/sum(MODIS_B16$RSR_2130)

  espec <- data.frame(Wave = c(400:900), spectra)


  modis.df.sim <- data.frame(1:16)


  for(i in 2:ncol(espec)) {

    modis.df.sim[1,i] <- sum(na.rm = T, MODIS_B1$FAC * espec[,i])
    modis.df.sim[2,i] <- sum(na.rm = T, MODIS_B2$FAC * espec[,i])
    modis.df.sim[3,i] <- sum(na.rm = T, MODIS_B3$FAC * espec[,i])
    modis.df.sim[4,i] <- sum(na.rm = T, MODIS_B4$FAC * espec[,i])
    modis.df.sim[5,i] <- sum(na.rm = T, MODIS_B5$FAC * espec[,i])
    modis.df.sim[6,i] <- sum(na.rm = T, MODIS_B6$FAC * espec[,i])
    modis.df.sim[7,i] <- sum(na.rm = T, MODIS_B7$FAC * espec[,i])
    modis.df.sim[8,i] <- sum(na.rm = T, MODIS_B8$FAC * espec[,i])
    modis.df.sim[9,i] <- sum(na.rm = T, MODIS_B9$FAC * espec[,i])
    modis.df.sim[10,i] <-sum(na.rm = T, MODIS_B10$FAC * espec[,i])
    modis.df.sim[11,i]<- sum(na.rm = T, MODIS_B11$FAC * espec[,i])
    modis.df.sim[12,i]<- sum(na.rm = T, MODIS_B12$FAC * espec[,i])
    modis.df.sim[13,i]<- sum(na.rm = T, MODIS_B13$FAC * espec[,i])
    modis.df.sim[14,i]<- sum(na.rm = T, MODIS_B14$FAC * espec[,i])
    modis.df.sim[15,i]<- sum(na.rm = T, MODIS_B15$FAC * espec[,i])
    modis.df.sim[16,i]<- sum(na.rm = T, MODIS_B16$FAC * espec[,i])

  }

  modis.df.sim[,1] <-  c(412,
                        443,
                        469,
                        488,
                        531,
                        551,
                        555,
                        645,
                        667,
                        678,
                        748,
                        859,
                        869,
                        1240,
                        1640,
                        2130)

  names(modis.df.sim) <- c("Wave", point_name)

  return(modis.df.sim)

}


cbers_04_MUX_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A
  mux_b5 <- CBERS_04_MUX[,c(1,2)]
  mux_b6 <- CBERS_04_MUX[,c(1,3)]
  mux_b7 <- CBERS_04_MUX[,c(1,4)]
  mux_b8 <- CBERS_04_MUX[,c(1,5)]

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  mux_b5$FAC <- mux_b5$CBERS4_MUXB5_SRF/sum(mux_b5$CBERS4_MUXB5_SRF)
  mux_b6$FAC <- mux_b6$CBERS4_MUXB6_SRF/sum(mux_b6$CBERS4_MUXB6_SRF)
  mux_b7$FAC <- mux_b7$CBERS4_MUXB7_SRF/sum(mux_b7$CBERS4_MUXB7_SRF)
  mux_b8$FAC <- mux_b8$CBERS4_MUXB8_SRF/sum(mux_b8$CBERS4_MUXB8_SRF)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_mux <- data.frame(1:4)




  for(i in 2:ncol(espec)) {

    df_SIM_mux[1,i] <- sum(na.rm = T, mux_b5$FAC * espec[,i])
    df_SIM_mux[2,i] <- sum(na.rm = T, mux_b6$FAC * espec[,i])
    df_SIM_mux[3,i] <- sum(na.rm = T, mux_b7$FAC * espec[,i])
    df_SIM_mux[4,i] <- sum(na.rm = T, mux_b8$FAC * espec[,i])



  }



  df_SIM_mux[,1] <- as.numeric(c("490","560","665","865"))


  names(df_SIM_mux) <- c("Wave", point_name)

  return(df_SIM_mux)




}


cbers_04A_MUX_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)

  #L? as fun??es de resposta - SENTINEL 2A
  mux_b5 <- cbers_04A_MUX[,c(1,2)]
  mux_b6 <- cbers_04A_MUX[,c(1,3)]
  mux_b7 <- cbers_04A_MUX[,c(1,4)]
  mux_b8 <- cbers_04A_MUX[,c(1,5)]

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  mux_b5$FAC <- mux_b5$CBERS04A_MUXB5_SRF/sum(mux_b5$CBERS04A_MUXB5_SRF)
  mux_b6$FAC <- mux_b6$CBERS04A_MUXB6_SRF/sum(mux_b6$CBERS04A_MUXB6_SRF)
  mux_b7$FAC <- mux_b7$CBERS04A_MUXB7_SRF/sum(mux_b7$CBERS04A_MUXB7_SRF)
  mux_b8$FAC <- mux_b8$CBERS04A_MUXB8_SRF/sum(mux_b8$CBERS04A_MUXB8_SRF)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_mux <- data.frame(1:4)




  for(i in 2:ncol(espec)) {

    df_SIM_mux[1,i] <- sum(na.rm = T, mux_b5$FAC * espec[,i])
    df_SIM_mux[2,i] <- sum(na.rm = T, mux_b6$FAC * espec[,i])
    df_SIM_mux[3,i] <- sum(na.rm = T, mux_b7$FAC * espec[,i])
    df_SIM_mux[4,i] <- sum(na.rm = T, mux_b8$FAC * espec[,i])



  }



  df_SIM_mux[,1] <- as.numeric(c("490","560","665","865"))


  names(df_SIM_mux) <- c("Wave", point_name)

  return(df_SIM_mux)




}



cbers_04A_WPM_simulation = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)


  CBERS_04A_WPM = filter(CBERS_04A_WPM, Wavelength < 901)

  #L? as fun??es de resposta - SENTINEL 2A
  WPM_PAN <- CBERS_04A_WPM[,c(1,2)]
  WPM_b1  <- CBERS_04A_WPM[,c(1,3)]
  WPM_b2  <- CBERS_04A_WPM[,c(1,4)]
  WPM_b3  <- CBERS_04A_WPM[,c(1,5)]
  WPM_b4  <- CBERS_04A_WPM[,c(1,6)]

  #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  WPM_PAN$FAC <- WPM_PAN$CBERS04A_WPMPAN_SRF/sum(WPM_PAN$CBERS04A_WPMPAN_SRF)
  WPM_b1$FAC <- WPM_b1$CBERS04A_WPMB1_SRF/sum(WPM_b1$CBERS04A_WPMB1_SRF)
  WPM_b2$FAC <- WPM_b2$CBERS04A_WPMB2_SRF/sum(WPM_b2$CBERS04A_WPMB2_SRF)
  WPM_b3$FAC <- WPM_b3$CBERS04A_WPMB3_SRF/sum(WPM_b3$CBERS04A_WPMB3_SRF)
  WPM_b4$FAC <- WPM_b4$CBERS04A_WPMB4_SRF/sum(WPM_b4$CBERS04A_WPMB4_SRF)



  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_WPM <- data.frame(1:5)




  for(i in 2:ncol(espec)) {

    df_SIM_WPM[1,i] <- sum(na.rm = T, WPM_PAN$FAC * espec[,i])
    df_SIM_WPM[2,i] <- sum(na.rm = T, WPM_b1$FAC * espec[,i])
    df_SIM_WPM[3,i] <- sum(na.rm = T, WPM_b2$FAC * espec[,i])
    df_SIM_WPM[4,i] <- sum(na.rm = T, WPM_b3$FAC * espec[,i])
    df_SIM_WPM[5,i] <- sum(na.rm = T, WPM_b4$FAC * espec[,i])



  }



  df_SIM_WPM[,1] <- c("PAN", "490","560","665","865")


  names(df_SIM_WPM) <- c("Wave", point_name)

  return(df_SIM_WPM)




}


Amazonia_WFI = function(spectra, point_name) {

  #Simulação e Rrs

  #Simula??o Sentinel 2A e 2B

  require(openxlsx)
  require(dplyr)
  require(tidyr)


  amazonia_1 = filter(amazonia_1, Wavelength < 901)

  #L? as fun??es de resposta - SENTINEL 2A
  WPM_PAN <- CBERS_04A_WPM[,c(1,2)]

  AMAZONIA1_WFI_RO_B1_SRF  <- amazonia_1[,c(1,2)]
  AMAZONIA1_WFI_RO_B2_SRF  <- amazonia_1[,c(1,3)]
  AMAZONIA1_WFI_RO_B3_SRF  <- amazonia_1[,c(1,4)]
  AMAZONIA1_WFI_RO_B4_SRF  <- amazonia_1[,c(1,5)]
  AMAZONIA1_WFI_LO_B1_SRF  <- amazonia_1[,c(1,6)]
  AMAZONIA1_WFI_LO_B2_SRF  <- amazonia_1[,c(1,7)]
  AMAZONIA1_WFI_LO_B3_SRF  <- amazonia_1[,c(1,8)]
  AMAZONIA1_WFI_LO_B4_SRF  <- amazonia_1[,c(1,9)]

    #Factor de Corre??o (FAC) = Valor da fun??o de resposta / soma da fun??o de resposta

  AMAZONIA1_WFI_RO_B1_SRF$FAC <- AMAZONIA1_WFI_RO_B1_SRF$AMAZONIA1_WFI_RO_B1_SRF/sum(AMAZONIA1_WFI_RO_B1_SRF$AMAZONIA1_WFI_RO_B1_SRF)
  AMAZONIA1_WFI_RO_B2_SRF$FAC <- AMAZONIA1_WFI_RO_B2_SRF$AMAZONIA1_WFI_RO_B2_SRF/sum(AMAZONIA1_WFI_RO_B2_SRF$AMAZONIA1_WFI_RO_B2_SRF)
  AMAZONIA1_WFI_RO_B3_SRF$FAC <- AMAZONIA1_WFI_RO_B3_SRF$AMAZONIA1_WFI_RO_B3_SRF/sum(AMAZONIA1_WFI_RO_B3_SRF$AMAZONIA1_WFI_RO_B3_SRF)
  AMAZONIA1_WFI_RO_B4_SRF$FAC <- AMAZONIA1_WFI_RO_B4_SRF$AMAZONIA1_WFI_RO_B4_SRF/sum(AMAZONIA1_WFI_RO_B4_SRF$AMAZONIA1_WFI_RO_B4_SRF)
  AMAZONIA1_WFI_LO_B1_SRF$FAC <- AMAZONIA1_WFI_LO_B1_SRF$AMAZONIA1_WFI_LO_B1_SRF/sum(AMAZONIA1_WFI_LO_B1_SRF$AMAZONIA1_WFI_LO_B1_SRF)
  AMAZONIA1_WFI_LO_B2_SRF$FAC <- AMAZONIA1_WFI_LO_B2_SRF$AMAZONIA1_WFI_LO_B2_SRF/sum(AMAZONIA1_WFI_LO_B2_SRF$AMAZONIA1_WFI_LO_B2_SRF)
  AMAZONIA1_WFI_LO_B3_SRF$FAC <- AMAZONIA1_WFI_LO_B3_SRF$AMAZONIA1_WFI_LO_B3_SRF/sum(AMAZONIA1_WFI_LO_B3_SRF$AMAZONIA1_WFI_LO_B3_SRF)
  AMAZONIA1_WFI_LO_B4_SRF$FAC <- AMAZONIA1_WFI_LO_B4_SRF$AMAZONIA1_WFI_LO_B4_SRF/sum(AMAZONIA1_WFI_LO_B4_SRF$AMAZONIA1_WFI_LO_B4_SRF)


  espec <- data.frame(Wave = c(400:900), spectra)


  df_SIM_WFI_AMAZONIA <- data.frame(1:8)




  for(i in 2:ncol(espec)) {

    df_SIM_WFI_AMAZONIA[1,i] <- sum(na.rm = T, AMAZONIA1_WFI_RO_B1_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[2,i] <- sum(na.rm = T, AMAZONIA1_WFI_RO_B2_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[3,i] <- sum(na.rm = T, AMAZONIA1_WFI_RO_B3_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[4,i] <- sum(na.rm = T, AMAZONIA1_WFI_RO_B4_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[5,i] <- sum(na.rm = T, AMAZONIA1_WFI_LO_B1_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[6,i] <- sum(na.rm = T, AMAZONIA1_WFI_LO_B2_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[7,i] <- sum(na.rm = T, AMAZONIA1_WFI_LO_B3_SRF$FAC * espec[,i])
    df_SIM_WFI_AMAZONIA[8,i] <- sum(na.rm = T, AMAZONIA1_WFI_LO_B4_SRF$FAC * espec[,i])



  }



  df_SIM_WFI_AMAZONIA[,1] <- c("RO_490","RO_560","RO_665","RO_865", "LO_490","LO_560","LO_665","LO_865")


  names(df_SIM_WFI_AMAZONIA) <- c("Wave", point_name)

  return(df_SIM_WFI_AMAZONIA)




}



