#' Function to update population matrix from Danmarks Statistik
#'
#' This function provides the utility of Danmark Statistik's API and cleans up the results for Epinion consultants to use in the daily
#' workflow with survey data and weights. The bulk of the function was written by Isak Dyrløv Klindt. Thus, all praise and errors should
#' be contributed to him.
#'
#' @param variable Argument that defines which variable you need to distribution for. Currently, the argument takes values such as "køn",
#' "alder", "køn_alder", "region", "uddannelse2", "person", "husstand" and "partivalg_sidste_fv".
#' @return A dataframe with the variable specified and a share of the population
#' @export
epinion_dst <- function(variable) {

  arg <- match.call()

  # Get info on all available tables from DST
  DST_tables <- dst_get_tables()

  # Get id on relevant tables
  # Region, køn, alder
  FOLK <- DST_tables %>%
    filter(str_detect(DST_tables$variables,
                      "område,køn,alder,civilstand,tid") == TRUE)

  # Uddannelse
  UDD_ALL <- DST_tables %>%
    filter(str_detect(DST_tables$variables,
                      "bopælsområde,herkomst,højest fuldførte uddannelse,alder,køn,tid") == TRUE &
             str_detect(DST_tables$id,
                        "^HF"))

  # Uddannelse ungegruppen
  UDD_YOUNG <- DST_tables %>%
    filter(str_detect(DST_tables$variables,
                      "bopælsområde,herkomst,højest fuldførte uddannelse,igangværende uddannelse,alder,køn,tid") == TRUE)

  # Bopæl, husstandsstørrelse, antal børn
  BOL <- DST_tables %>%
    filter(str_detect(DST_tables$variables,
                      "område,anvendelse,udlejningsforhold,antal værelser,husstandsstørrelse,antal børn,alder,køn,tid") == TRUE)


  # Get info about the structure of the desired data to set up
  # the search queries
  FOLK_meta <- dst_meta(table = FOLK$id, lang = "da") # age, region, gender
  UDD_ALL_meta <- dst_meta(table = UDD_ALL$id, lang = "da") # education all
  UDD_YOUNG_meta <- dst_meta(table = UDD_YOUNG$id, lang = "da") # education young
  BOL_meta <- dst_meta(table = BOL$id, lang = "da") # households

  # Below are the search queries. A "*" means that all
  # categories in that variable will be downloaded. If
  # you reach the limit on how much data you are allowed
  # to download you will receive and error message

  FOLK_query <- list(OMRÅDE = c("Region Hovedstaden", "Region Sjælland", "Region Syddanmark",
                                "Region Midtjylland", "Region Nordjylland"),
                     ALDER = "*",
                     Tid = FOLK$latestPeriod,
                     KØN = "*")

  UDD_ALL_query <- list(BOPOMR = "Hele landet",
                        ALDER = "*",
                        Tid = UDD_ALL$latestPeriod,
                        KØN = "*",
                        HFUDD = c("H10 Grundskole", "H20 Gymnasiale uddannelser",
                                  "H30 Erhvervsfaglige uddannelser", "H35 Adgangsgivende uddannelsesforløb",
                                  "H40 Korte videregående uddannelser, KVU", "H50 Mellemlange videregående uddannelser, MVU",
                                  "H60 Bacheloruddannelser, BACH", "H70 Lange videregående uddannelser, LVU",
                                  "H80 Ph.d. og forskeruddannelser"))

  UDD_YOUNG_query <- list(BOPOMR = "Hele landet",
                          ALDER = "*",
                          Tid = UDD_YOUNG$latestPeriod,
                          KOEN = "*",
                          UDDANNELSEF = c("H10 Grundskole", "H20 Gymnasiale uddannelser",
                                          "H30 Erhvervsfaglige uddannelser", "H35 Adgangsgivende uddannelsesforløb",
                                          "H40 Korte videregående uddannelser, KVU", "H50 Mellemlange videregående uddannelser, MVU",
                                          "H60 Bacheloruddannelser, BACH", "H70 Lange videregående uddannelser, LVU",
                                          "H80 Ph.d. og forskeruddannelser"))

  BOL_query <- list(AMT = "Hele landet",
                    ALDER = "*",
                    Tid = BOL$latestPeriod,
                    KØN = "*",
                    HUSSTØR = "*",
                    ANTBØRN = "*",
                    ANVENDELSE = "*")

  # Get data
  FOLK_raw <- dst_get_data(table = FOLK$id, query = FOLK_query, lang = "da")
  UDD_ALL_raw <- dst_get_data(table = UDD_ALL$id, query = UDD_ALL_query, lang = "da")
  UDD_YOUNG_raw <- dst_get_data(table = UDD_YOUNG$id, query = UDD_YOUNG_query, lang = "da")
  BOL_raw <- dst_get_data(table = BOL$id, query = BOL_query, lang = "da")

  # Filter out specific age group from UDD_ALL
  UDD_ALL_1 <- UDD_ALL_raw %>%
    filter(!ALDER %in% c("Alder i alt", "15-19 år"))

  # Also filter out specific age groups from HFUDD20
  UDD_1 <- UDD_YOUNG_raw %>%
    filter(!ALDER %in% c("Alder i alt")) %>% # filter out "i alt" from age variable
    mutate(ALDER = as.numeric(str_remove_all(ALDER, " år")),   # transform age variable into numeric and remove "år" from values
           AGE_GROUP = ifelse(ALDER %in% c(18:19), "18-19 år", NA)) %>%  # add age group variable containing only relevant ages
    filter(!is.na(AGE_GROUP)) %>%  # filter out irrelevant age group
    select(BOPOMR, -ALDER, ALDER = AGE_GROUP, TID, KØN = KOEN, HFUDD = UDDANNELSEF, value) %>%  # unselect and rename variables to match for rbind
    rbind(UDD_ALL_1) %>%
    filter(!KØN == "I alt",
           !HFUDD %in% c("H80 Ph.d. og forskeruddannelser", "H90 Uoplyst mv."))


  # Clean FOLK
  FOLK_1 <- FOLK_raw %>%
    filter(!ALDER == "I alt") %>%
    mutate(ALDER = as.numeric(str_remove_all(ALDER, " år"))) %>%
    filter(!KØN == "I alt",
           !ALDER < 18)


  # Clean BOL
  BOL_1 <- BOL_raw %>%
    filter(!ALDER %in% c("0-5 år", "6-11 år", "12-17 år"))

  if(eval(arg$variable)=="køn") {

    kon <- FOLK_1 %>%
      group_by(KØN) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(køn = recode_factor(.x = KØN,
                                 "Mænd" = "Mand",
                                 "Kvinder" = "Kvinde"),
             share = value/sum(value)) %>%
      select(køn, share) %>%
      as.data.frame()

    return(kon)

  } else if(eval(arg$variable)=="alder") {

    aldersgruppe <- FOLK_1 %>%
      mutate(age = ifelse(ALDER %in% c(18:34), "18-34 år", NA),
             age = ifelse(ALDER %in% c(35:55), "35-55 år", age),
             age = ifelse(ALDER > 55, "56 år eller derover", age)) %>% # create age group variable based on age
      group_by(age) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value)) %>%
      select(alder = age, share) %>%
      as.data.frame()

    return(aldersgruppe)

  } else if(eval(arg$variable)=="køn_alder") {

    kon_alder <- FOLK_1 %>%
      mutate(køn_alder = ifelse(ALDER %in% c(18:34) & KØN == "Mænd", "Mand 18-34 år", NA),
             køn_alder = ifelse(ALDER %in% c(35:55) & KØN == "Mænd", "Mand 35-55 år", køn_alder),
             køn_alder = ifelse(ALDER > 55 & KØN == "Mænd", "Mand 56 år eller derover", køn_alder),
             køn_alder = ifelse(ALDER %in% c(18:34) & KØN == "Kvinder", "Kvinde 18-34 år", køn_alder),
             køn_alder = ifelse(ALDER %in% c(35:55) & KØN == "Kvinder", "Kvinde 35-55 år", køn_alder),
             køn_alder = ifelse(ALDER > 55 & KØN == "Kvinder", "Kvinde 56 år eller derover", køn_alder), # create age group variable based on age
             køn_alder = factor(køn_alder, levels = c("Mand 18-34 år", "Mand 35-55 år", "Mand 56 år eller derover",
                                                      "Kvinde 18-34 år", "Kvinde 35-55 år", "Kvinde 56 år eller derover"))) %>%
      group_by(køn_alder) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value)) %>%
      select(-value) %>%
      as.data.frame()

    return(kon_alder)

  } else if(eval(arg$variable)=="region") {

    reg <- FOLK_1 %>%
      group_by(OMRÅDE) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value),
             region = gsub("Region ", "", OMRÅDE)) %>%
      select(region, share) %>%
      as.data.frame()

    return(reg)

  } else if(eval(arg$variable)=="uddannelse2") {

    udd <- UDD_1 %>%
      mutate(uddannelse2 = case_when(HFUDD == "H10 Grundskole" |
                                     HFUDD == "H30 Erhvervsfaglige uddannelser" |
                                     HFUDD == "H35 Adgangsgivende uddannelsesforløb" |
                                     HFUDD == "Grundskole og erhvervsfaglig uddannelse" ~ "Grundskole og erhvervsfaglig uddannelse",
                                     HFUDD == "H20 Gymnasiale uddannelser" |
                                     HFUDD == "H40 Korte videregående uddannelser, KVU" |
                                     HFUDD == "H50 Mellemlange videregående uddannelser, MVU" |
                                     HFUDD == "H60 Bacheloruddannelser, BACH" |
                                     HFUDD == "H70 Lange videregående uddannelser, LVU" ~ "Gymnasie og videregående uddannelse")) %>%
      group_by(uddannelse2) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value)) %>%
      select(-value) %>%
      as.data.frame()

    return(udd)

  } else if(eval(arg$variable)=="person") {

    per <- BOL_1 %>%
      mutate(husstør_num = as.numeric(str_remove_all(HUSSTØR, " personer| person| og derover")),
             antbørn_num = as.numeric(str_remove_all(ANTBØRN, " børn| barn| og derover")),
             husstør_korrigeret = husstør_num - antbørn_num,
             person = case_when(husstør_korrigeret == 1 ~ "1 person",
                                husstør_korrigeret == 2 ~ "2 personer",
                                husstør_korrigeret > 2 ~ "3 personer eller flere")) %>%
      filter(!is.na(person)) %>%
      group_by(person) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value)) %>%
      select(-value) %>%
      as.data.frame()

    return(per)

  } else if(eval(arg$variable)=="husstand") {

    husstandstype <- BOL_1 %>%
      mutate(husstand = case_when(ANVENDELSE == "Parcel/Stuehuse" ~ "Stuehus og parcelhus",
                                  ANVENDELSE == "Række-, kæde- og dobbelthuse" ~ "Række-, kæde- eller dobbelthus",
                                  ANVENDELSE == "Etageboliger" |
                                  ANVENDELSE == "Kollegier" |
                                  ANVENDELSE == "Fritidshuse" |
                                  ANVENDELSE == "Døgninstitutioner" |
                                  ANVENDELSE == "Andet" ~ "Etageboligbebyggelse, kollegium, fritidshuse, døgninstitution samt andet"),
             husstand = factor(husstand, levels = c("Stuehus og parcelhus", "Række-, kæde- eller dobbelthus",
                                                    "Etageboligbebyggelse, kollegium, fritidshuse, døgninstitution samt andet"))) %>%
      group_by(husstand) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/sum(value)) %>%
      select(-value) %>%
      as.data.frame()

    return(husstandstype)

  } else if(eval(arg$variable)=="partivalg_sidste_fv") {

    return(partivalg_f19)

  } else {

    print("Please specify variable")

  }
}
