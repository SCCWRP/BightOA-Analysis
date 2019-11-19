# Begin OA Script with Functions
library( readxl )
library( dplyr )
library( tidyr )
library( seacarb )

# Consider renaming all columns in dataframes where they are NOT grouped on
#merge data frames

oa_analysis <- function(ctd_data, bb_data){
  merge_dfs <- function(df1, df2, vcol){
    df2 %>%
      dplyr::inner_join(
        df1 %>%
          dplyr::select(season, agency, sampledate, station, depth, fieldrep, labrep, latitude, longitude, ctd_temperature, ctd_salinity, ctd_density, ctd_pH),
        by = vcol
      )
  }

  #Define Global Constants
  flag <- 8
  k1k2 <- "l"
  kf <- "dg"
  ks <- "d"
  pHscale <- "T"
  b <- "l10"
  #averageTA is also a constant since only one value is used for the entire CTD dataframe but we will worry about this one later
  #ta_mean <- mean(bb_data$bb_ta)

  ## Functions will not be written for:
  # p_insi (pressure at in situ)
  # ta_mean (total alkalinity average)

  ## Written functions


  # delta_pH_actual
  #   Input:  pH_insitu         (from pHinsi() function)
  #           pH_ctd            (from ctd data)
  #
  #for the following fcn, we should pass the ctd_bb_merged dataframe

  # delta_pH_actual <- function(df){
  #   Pinsi_bar <- 0.1 * swPressure(
  #     depth = df$depth,
  #     latitude = df$latitude
  #   )
  #   bb_pH_insitu <- pHinsi(
  #     pH = df$bb_pH,
  #     ALK = df$bb_ta * 1e6,
  #     Tinsi = df$ctd_temperature,
  #     Tlab = df$bb_temperature,
  #     Pinsi = Pinsi_bar,
  #     S = df$bb_salinity,
  #     k1k2 = k1k2,
  #     ks = ks,
  #     pHscale = pHscale,
  #     b = b
  #   )
  #   delta_pH <- bb_pH_insitu - df$ctd_pH
  #   return(delta_pH)
  # }

  #Find the delta pH using actual values to feed into linear model to fill in missing delta pH for pH_correction in ctd data/analysis
  delta_pH_actual <- function(df){
    Pinsi_bar <- 0.1 * swPressure(
      depth = df$depth,
      latitude = df$latitude
    )
    df$bb_pH_insitu <- pHinsi(
      pH = df$bb_pH,
      ALK = df$bb_ta * 1e6,
      Tinsi = df$ctd_temperature,
      Tlab = df$bb_temperature,
      Pinsi = Pinsi_bar,
      S = df$bb_salinity,
      k1k2 = k1k2,
      ks = ks,
      pHscale = pHscale,
      b = b
    )
    df$delta_pH <- df$bb_pH_insitu - df$ctd_pH
    return(df)
  }

  # delta_pH_fitted
  #   Input:  slope, intercept  (from linear_fit fcn)
  #           depth             (from bottle data)
  #   Output: delta_pH_actual
  #   Analysis:
  # instead make this into a pH_corrector function
  # delta_pH_fitted <- function(df, lm_coeffs){
  #
  #   delta_pH <- lm_coeffs$slope * df$depth + lm_coeffs$intercept
  #   return(delta_pH)
  # }

  #Correct pH using delta pH and linear regression coefficients

  pH_corrector <- function(df_ctd, lm_coeffs){ #pass ctd data
    df_ctd_join <- df_ctd %>%
      dplyr::left_join(
        lm_coeffs, by = c('season','agency','station', 'sampledate', 'fieldrep','labrep')) %>%
      dplyr::mutate(
        delta_pH = lm_coeffs$slope * depth + lm_coeffs$intercept,
        pH_corrected = ctd_pH - delta_pH)
    return(df_ctd_join$pH_corrected)
  }

  # omega_aragonite
  #   Input:  flag, k1k2, kf, ks, b   (constants)
  #           pH                      (from bottle (pH) & ctd (pH_corrected) data)
  #           ta                      (from bottle (TA) & ctd (ta_mean) data)
  #   Output: omega_aragonite
  #   Analysis: carb() from seacarb pkg
  # omega_aragonite <- function(df, pH, ta){
  #   var1 <- df$pH
  #   var2 <- df$ta / 1000000
  #   omega <- carb(flag, var1, var2, k1k2 = k1k2, kf = kf, ks = ks, b = b)$OmegaAragonite
  #   return(omega)
  # }

  # linear_fit

  # linreg_coeffs <- function(df, delta_pH){
  #   df$delta_pH = delta_pH
  #   coeff <- df %>%
  #     dplyr::group_by(
  #       season, agency, sampledate, station, fieldrep, labrep
  #     ) %>%
  #     tidyr::nest() %>%
  #     dplyr::mutate(
  #       intercept = purrr::map(data, function(df){
  #         lm(formula = delta_pH ~ depth, data = df)$coefficients[1]
  #       }),
  #       slope = purrr::map(data, function(df){
  #         lm(formula = delta_pH ~ depth, data = df)$coefficients[2]
  #       }),
  #       rsquared = purrr::map(data, function(df){
  #         summary(lm(formula = delta_pH ~ depth, data = df))$r.squared
  #       })
  #     ) %>%
  #     dplyr::select(-data) %>%
  #     tidyr::unnest()
  #   return(coeff)
  # }

  linreg_coeffs <- function(df_merged){
    coeff <- df_merged %>%
      dplyr::group_by(
        season, agency, sampledate, station, fieldrep, labrep
      ) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        intercept = purrr::map(data, function(df){
          lm(formula = delta_pH ~ depth, data = df)$coefficients[1]
        }),
        slope = purrr::map(data, function(df){
          lm(formula = delta_pH ~ depth, data = df)$coefficients[2]
        }),
        rsquared = purrr::map(data, function(df){
          summary(lm(formula = delta_pH ~ depth, data = df))$r.squared
        })
      ) %>%
      dplyr::select(-data) %>%
      tidyr::unnest()
    return(coeff)
  }



  # DO THIS INSTEAD LATER
  # dplyr::inner_join(ctd_data, bb_data, by = c('Season','Agency','SampleDate','Station','FieldRep','LabRep','Depth'), suffix = c('.ctd','.bottle'))
  # inside the mergedfs function
  
  ctd_data$station <- as.character(ctd_data$station)
  bb_data$station <- as.character(bb_data$station)
  
  #Construct analysis dataframes
  ctd_analysis <- ctd_data
  bb_analysis <- bb_data

  # merge dataframes of ctd and bb
  ctd_bb_merged <- merge_dfs(df1 = ctd_data, df2 = bb_data, vcol = c("season", "agency", "sampledate", "station", "depth", "fieldrep", "labrep"))

  # obtain delta_pH_fitted values for the linear model
  ctd_bb_merged <- delta_pH_actual(ctd_bb_merged)

  #run linear regression for coeffs
  #linreg <- linreg_coeffs(ctd_bb_merged, delta_pH_lm)
  linreg <- linreg_coeffs(ctd_bb_merged)

  #pH_correction
  ctd_analysis$ctd_pH_corrected <- pH_corrector(ctd_data,linreg)
  ctd_analysis$ctd_ta <- mean(bb_data$bb_ta)

  #Compute omega vals
  #bb_omega <- omega_aragonite(bb_data, bb_pH, bb_ta)
  bb_analysis$T_insitu <- ctd_bb_merged$ctd_temperature

  bb_analysis$bb_omega <- carb(
    flag = flag,
    var1 = bb_analysis$bb_pH,
    var2 = bb_analysis$bb_ta / 1000000,
    k1k2 = k1k2,
    kf = kf,
    ks = ks,
    b = b
  )$OmegaAragonite

  ctd_analysis$ctd_omega <- carb(
    flag = flag,
    var1 = ctd_analysis$ctd_pH_corrected,
    var2 = ctd_analysis$ctd_ta / 1000000,
    k1k2 = k1k2,
    kf = kf,
    ks = ks,
    b = b
  )$OmegaAragonite

  # rename colnames for analysis tables
  ctd_analysis <- ctd_analysis %>%
    dplyr::rename(
      TA = ctd_ta,
      Omega = ctd_omega,
      Temperature = ctd_temperature,
      Salinity = ctd_salinity,
      Density = ctd_density,
      pH = ctd_pH,
      pH_Corrected = ctd_pH_corrected
    )
  bb_analysis <- bb_analysis %>%
    dplyr::rename(
      Omega = bb_omega,
      TA = bb_ta,
      pH = bb_pH,
      Salinity = bb_salinity,
      FieldRep2 = fieldrep2,
      Temperature = bb_temperature
    )

  #End of OA Script

  return(list(ctd = ctd_analysis, bb = bb_analysis))
}



# End of OA analysis------
# result <- oa_analysis(ctd_data, bb_data)


