library(tidyverse)
library(lubridate)
library(readxl)
library(odbc)
library(janitor)

con <- dbConnect(odbc(), "CaboodleProd")

monthlabs <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

bmcpprov <- c(
  '15643', '3664', '5009553', '5010324', '15635', '5014355', '3391', '5020174',
  '2912', '13644', '5002955', '5004188',	'15882', '5005608', '5007237',
  '5014593', '5013828', '5017851', '425252', '774317', '5027536'
  )

ibhprov <- c("5017437", "5021489", "445821", "5016609",
             "3644", "5016177", "5002378", "5002965")

uteprovs <- c("15643", "3664", "5009553", "5010324", "15635", "5014355",
                "3391", "5020174", "2912", "13644", "5002955", "5004188",
                "15882", "5005608", "5007237", "5014593", "5013828", "5017851", 
                "425252")

ttgprov <- c("15643", "3664", "5009553", "5010324", "15635", "5014355", "3391", 
             "5020174", "2912", "13644", "5002955", "5004188", "15882", 
             "5005608", "5007237", "5014593", "5013828", "5017851", "425252")

patlevel <- c("Butler BH", "Community BH", "NewPath", "Transitions")

monthlist <- seq.Date(as.Date("2022-01-01"), as.Date(floor_date(today() - 30)), by = "month")

dateframe <- tibble(
  Month = rep(monthlist, 74),
  Measure = rep(
    c("Crisis Stabilization", "Initial Intake"),
    each = length(monthlist),
    37
    ),
  Practice = rep(
    c(
      "Anderson Hills Pediatrics", "Best Point", "BMCP", "Butler BH", 
      "Catalyst", "Centerpoint Health", "CHSN Anderson Primary Care", 
      "CHSN Batesville Primary Care", "CHSN Cold Spring Primary Care",
      "CHSN Greensburg Primary Care", "CHSN Kenwood Crossing Primary Care", 
      "CHSN Liberty Primary Care", "CHSN Loveland Primary Care", 
      "CHSN Mason Primary Care", "CHSN Rockdale Primary Care", 
      "CHSN Southgate Primary Care", "CHSN Union Primary Care",
      "CHSN Wilmington Primary Care", "Cincinnati Health Department", 
      "Community BH", "Community IBH", "Crossroad Health Center", 
      "ESD Pediatrics", "Liberty Sharonville Pediatrics", "MLS", 
      "Montgomery Pediatrics", "Muddy Creek Pediatrics", "NewPath", "NKY",
      "OneQuest", "Pediatrics of Florence", "Pediatrics Of Florence", "Poppys",
      "Psychiatry", "Springdale Mason Pediatrics", "Transitions", 
      "West Side Pediatrics"
      ),
    each = length(monthlist) * 2
    )
)

aeu <- read_excel(
  "old data.xlsx",
  sheet = "All except Utilization",
  col_names = c(
    "Practice", 
    "Measure",
    "Month", 
    "Numerator", 
    "Denominator",
    "Network"
    ),
  skip = 1
) |>
  mutate(Aggregation = ifelse(Practice == "Community IBH", "Network", "Practice"))

newrow <- function(workbook){
  reportmonth <- as.character(parse_number(workbook))
  practice <- str_remove(workbook, reportmonth)
  practice <- str_remove(practice, " .xlsx")
  reportyear <- str_trunc(reportmonth, 4, "right", ellipsis = "")
  reportmonth <- str_trunc(reportmonth, 2, "left", ellipsis = "")
  reportmonth <- ymd(paste(reportyear, reportmonth, "01", sep = "-"))
  numerators <- as.numeric()
  denominators <- as.numeric()
  if(practice == "BMCP"){
    numerators[1:3] <- rep(NA, 3)
    pt <- read_excel(workbook, sheet = "Provider Turnover Template")
    denominators[1] <-  as.numeric(pt[8, 4])
    denominators[2:3] <- c(NA, NA)
    numerators[4] <- as.numeric(pt[8, 2])
    denominators[4] <- (as.numeric(pt[8, 3]) + as.numeric(pt[8, 4])) / 2
  }else if(practice == "Psychiatry"){
    numerators[1:3] <- rep(NA, 3)
    pt <- read_excel(workbook, sheet = "Provider Turnover Template")
    denominators[1] <- as.numeric(pt[5, 3])
    denominators[2:3] <- rep(NA, 2)
    numerators[4] <- as.numeric(pt[3, 3])
    denominators[4] <- (as.numeric(pt[4, 3]) + as.numeric(pt[5, 3])) / 2
  }else if(practice %in% c("Catalyst", patlevel)){
    ii <- read_excel(workbook, sheet = "Initial Intake Template")
    numerators[1] <- as.numeric(ii[3, 3])
    denominators[1] <- as.numeric(ii[4, 3])
    uc <- read_excel(workbook, sheet = "Universal Crisis Template")
    numerators[2] <- as.numeric(uc[3, 3])
    denominators[2] <- as.numeric(uc[4, 3])
    numerators[3] <- NA
    denominators[3] <- NA
    pt <- read_excel(workbook, sheet = "Provider Turnover Template")
    numerators[4] <- as.numeric(pt[3, 3])
    denominators[4] <- (as.numeric(pt[4, 3]) + as.numeric(pt[5, 3])) / 2
  }else{
    ii <- read_excel(workbook, sheet = "Initial Intake Template")
    numerators[1] <- as.numeric(ii[3, 3])
    denominators[1] <- as.numeric(ii[4, 3])
    uc <- read_excel(workbook, sheet = "Universal Crisis Template")
    numerators[2] <- as.numeric(uc[3, 3])
    denominators[2] <- as.numeric(uc[4, 3])
    pro <- read_excel(workbook, sheet = "PRO Org-Level Template")
    numerators[3] <- as.numeric(pro[3, 3])
    denominators[3] <- as.numeric(pro[4, 3])
    pt <- read_excel(workbook, sheet = "Provider Turnover Template")
    numerators[4] <- as.numeric(pt[3, 3])
    denominators[4] <- (as.numeric(pt[4, 3]) + as.numeric(pt[5, 3])) / 2
  }
  tibble(
    Practice = practice,
    Measure = c(
      "Initial Intake",
      "Crisis Stabilization",
      "PRO BOS",
      "Provider Turnover"
    ),
    Month = reportmonth,
    Numerator = numerators,
    Denominator = denominators,
    Network = ifelse(practice == "Community IBH", "IBH", "PINQ BH"),
    Aggregation = ifelse(Network == "IBH", "Network", "Practice")
  )
}

newdata <- filter(aeu, Month > today())

setwd("C:/Users/FLI6SH/OneDrive - cchmc/Documents/Behavioral Health/PINQ/PINQ-Dashboard/Data downloads")

workbooks <- list.files()

for (i in 1:length(workbooks)){
  newdata <- rbind(newdata, newrow(workbooks[i]))
}

newdata <- newdata |>
  filter(
    !is.na(Denominator),
    Denominator > 0
    ) |>
  mutate(Practice = ifelse(Practice == "CHNK", "OneQuest", Practice))

# IHN Practices

setwd("C:/Users/FLI6SH/OneDrive - cchmc/Documents/Behavioral Health/PINQ/PINQ-Dashboard/IBH")

ibhfiles <- list.files()

for(p in ibhfiles){
  x <- read_excel(
    p, 
    col_names = c("Practice", "monthname", "year", "II", "Crisis", "Denominator"),
    skip = 1
  ) |>
    filter(!is.na(monthname)) |> 
    mutate(
      monthname = coalesce(monthname, lag(monthname)),
      year = coalesce(year, lag(year)),
      monthname = case_when(
        monthname == "January" ~ "01",
        monthname == "February" ~ "02",
        monthname == "March" ~ "03",
        monthname == "April" ~ "04",
        monthname == "May" ~ "05",
        monthname == "June" ~ "06",
        monthname == "July" ~ "07",
        monthname == "August" ~ "08",
        monthname == "September" ~ "09",
        monthname == "October" ~ "10",
        monthname == "November" ~ "11",
        monthname == "December" ~ "12",
      ),
      Month = as.Date(paste(year, monthname, "01", sep = "-"))
    ) |>
    pivot_longer(
      cols = c(II:Crisis), 
      names_to = "Measure", 
      values_to = "Numerator"
    ) |>
    mutate(
      Network = "IBH",
      Aggregation = "Practice",
      Measure = ifelse(Measure == "II", "Initial Intake", "Crisis Stabilization"),
      Practice = case_when(
        Practice == "Total" ~ "IBH Network",
        str_starts(Practice, "CHSN") ~ paste0("CHSN ", str_to_title(str_remove(Practice, "CHSN "))),
        Practice == "Cincinnati Health Dept" ~ "Cincinnati Health Department",
        Practice == "Liberty Sharonville Pediatrics, Inc." ~ "Liberty Sharonville Pediatrics",
        Practice == "MONTGOMERY PEDIATRICS, INC" ~ "Montgomery Pediatrics",
        Practice == "Crossroads" ~ "Crossroad Health Center",
        TRUE ~ Practice
      )
    ) |>
    select(Practice, Denominator:Aggregation)
  newdata <- rbind(newdata, x)
}

denoms <- anti_join(aeu, newdata, join_by(Practice, Measure, Month)) |>
  rbind(newdata) |>
  filter(Measure == "Initial Intake") |>
  select(Practice, Month, Denominator)

#BMCP & Psych crisis

patientbos <- dbGetQuery(con, "
  SELECT DISTINCT sv.PatientDurableKey
      						,sv.NumericResponse AS PatientBOS
      						,dd1.DateValue AS BOS1Date
      						,prd.ProviderEpicID AS BOS1Prov
      						,sv.ResponseTimeKey
      						,ac.SBFlag
    FROM caboodle.dbo.SurveyAnswerFact sv
				JOIN caboodle.dbo.DateDim dd1
					ON sv.EncounterDateKey = dd1.DateKey
        JOIN caboodle.dbo.EncounterFact ef
					ON sv.EncounterKey = ef.EncounterKey
				JOIN caboodle.dbo.ProviderDim prd
					ON ef.ProviderDurableKey = prd.DurableKey
				LEFT JOIN AndersonCenter.dbo.PINQProvPsychiatry ac 
				  ON  prd.Providerepicid=ac.providerepicid
		WHERE sv.SurveyQuestionKey = 24552
			AND sv.Valid = 1
			AND sv.count > 0
  ") |>
  mutate(
    Practice = case_when(
      BOS1Prov %in% bmcpprov ~ "BMCP",
      !is.na(SBFlag) ~ "Psychiatry"
    )
  ) |>
  filter(!is.na(Practice)) |>
  group_by(PatientDurableKey, Practice, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup() 

firstbos <- patientbos |>
  group_by(PatientDurableKey, Practice) |>
  filter(BOS1Date == min(BOS1Date)) |>
  ungroup()

crisis <- dbGetQuery(con, "
    SELECT DISTINCT ef.PatientDurableKey
            				,PrimaryMRN
            				,ef.EncounterEpicCSN
            				,ef.EncounterKey
            				,SUM(BillingProcedureQuantity) AS CPTQty
            				,ef.ProviderDurableKey
            				,btf.BillingProcedureCode AS CPTCode
            				,prvd.ProviderEpicID
            				,prvd.Name AS ProviderName
            				,dad.DateValue AS EncDate
            				,ef.VisitTypeKey
            				,ac.SBFlag
            				,ac.NPI
            				,ef.DepartmentKey
      FROM caboodle.dbo.EncounterFact ef
    		JOIN caboodle.dbo.BillingTransactionFact btf
    			ON btf.EncounterKey = ef.EncounterKey
    		JOIN caboodle.dbo.PatientDim pd
    			ON pd.DurableKey = ef.PatientDurableKey
    		JOIN caboodle.dbo.DateDim dad
    			ON ef.DateKey = dad.DateKey
    		JOIN caboodle.dbo.ProviderDim prvd
    			ON ef.ProviderDurableKey = prvd.DurableKey
    		LEFT JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
  			  ON prvd.ProviderEpicID = ac.ProviderEpicID
      WHERE prvd.StartDate <= dad.DateValue
			   AND prvd.EndDate >= dad.DateValue
			   AND btf.BillingProcedureCode IN ('90839', 'S9485')
         AND ReportingTransactionType = 'charge'
         AND IsInactive = 0
      	 AND ef.EncounterKey > 0
      	 AND pd.IsCurrent = 1
      	 AND dad.DateValue >= '10/1/22'
      GROUP BY ef.patientdurablekey
        			,PrimaryMRN
        			,ef.EncounterEpicCSN
        			,ef.EncounterKey
        			,ef.ProviderDurableKey
        			,btf.BillingProcedureCode
        			,prvd.ProviderEpicID
        			,prvd.Name
        			,dad.DateValue
        			,ef.VisitTypeKey
        			,ac.SBFlag
        			,ac.NPI
        			,ef.DepartmentKey
    ") |>
  mutate(
    Practice = case_when(
      ProviderEpicID %in% bmcpprov ~ "BMCP",
      !is.na(SBFlag) &
      !VisitTypeKey %in% c("1536", "3113", "3670", "4246", "6024", "6749", "7333") &
        ((SBFlag == 1 & DepartmentKey == 968) | SBFlag == 0) &
        ((NPI == "1518017102" & EncDate < "2024-03-09") | NPI != "1518017102") ~
        "Psychiatry"
    )
  ) |>
  filter(!is.na(Practice)) |>
  mutate(Month = floor_date(EncDate, "month")) |>
  inner_join(firstbos) |>
  filter(
    Month >= "2022-10-01",
    EncDate > BOS1Date
  ) |>
  group_by(Month, Practice) |>
  reframe(Numerator = length(unique(EncounterKey))) |>
  mutate(
    Measure = "Crisis Stabilization",
    Network = "PINQ BH",
    Aggregation = "Practice"
  ) 

#BMCP initial intake

intake2 <- dbGetQuery(con, "
  SELECT DISTINCT ef.patientdurablekey
          				,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,SUM(BillingProcedureQuantity) AS CPTQty
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS CPTCode
          				,prvd.ProviderEpicID AS ProviderEpicID
          				,prvd.Name AS ProviderName
          				,dad.DateValue AS EncDate
          				,ef.VisitTypeKey
          				,ac.NPI
          				,ac.SBFlag
          				,ef.DepartmentKey
    FROM caboodle.dbo.EncounterFact ef
  		JOIN caboodle.dbo.BillingTransactionFact btf
  			ON btf.EncounterKey = ef.EncounterKey
  		JOIN caboodle.dbo.PatientDim pd
  			ON pd.DurableKey = ef.PatientDurableKey
  		JOIN caboodle.dbo.DateDim dad
  			ON ef.DateKey = dad.DateKey
  		JOIN caboodle.dbo.ProviderDim prvd
  			ON ef.ProviderDurableKey = prvd.DurableKey
  		LEFT JOIN AndersonCenter.dbo.PinqProvPsychiatry ac
  		  ON prvd.ProviderEpicID = ac.ProviderEpicID
  	WHERE prvd.StartDate <= dad.DateValue
			AND prvd.EndDate >= dad.DateValue
			AND btf.BillingProcedureCode = '90791'
  		AND ReportingTransactionType = 'charge'
  		AND IsInactive = 0
  		AND ef.EncounterKey > 0
  		AND pd.IsCurrent=1
  		AND dad.DateValue >= '10/1/22'
  	GROUP BY ef.patientdurablekey
      			,PrimaryMRN
      			,ef.EncounterEpicCSN
      			,ef.EncounterKey
      			,ef.ProviderDurableKey
      			,btf.BillingProcedureCode
      			,prvd.ProviderEpicID
      			,prvd.Name
      			,dad.DateValue
      			,dateadd(month, datediff(month, 0, dad.datevalue), 0)
      			,ef.VisitTypeKey
      			,ac.NPI
      			,ac.SBFlag
      			,ef.DepartmentKey
    ") |>
  mutate(
    Practice = case_when(
      ProviderEpicID %in% bmcpprov ~ "BMCP",
      !is.na(SBFlag) &
        !VisitTypeKey %in% c("1536", "3113", "3670", "4246", "6024", "6749", "7333") &
        ((SBFlag == 1 & DepartmentKey == 968) | SBFlag == 0) &
        ((NPI == "1518017102" & EncDate < "2024-03-09") | NPI != "1518017102") ~
        "Psychiatry"
    ),
    Month = floor_date(EncDate, "month")
  ) |>
  filter(!is.na(Practice))

crisisii <- intake2 |>
  filter(
    Month >= "2022-10-01",
    CPTQty > 0
  ) |>
  group_by(Month, Practice) |>
  reframe(Numerator = length(unique(EncounterEpicCSN))) |>
  mutate(
    Measure = "Initial Intake",
    Network = "PINQ BH",
    Aggregation = "Practice"
  ) |>
  mutate(Month = as.Date(Month)) |>
  rbind(crisis) |>
  left_join(denoms)

#Patient-level BOS

bosbooks <- workbooks[
  str_starts(workbooks, "Butler BH") | 
    str_starts(workbooks, "Community BH") | 
    str_starts(workbooks, "NewPath") | 
    str_starts(workbooks, "Transitions")
  ]

bos <- function(workbook){
  reportmonth <- as.character(parse_number(workbook))
  practice <- str_remove(workbook, reportmonth)
  practice <- str_remove(practice, " .xlsx")
  reportyear <- str_trunc(reportmonth, 4, "right", ellipsis = "")
  reportmonth <- str_trunc(reportmonth, 2, "left", ellipsis = "")
  reportmonth <- ymd(paste(reportyear, reportmonth, "01", sep = "-"))
  x <- read_excel(
    workbook, 
    sheet = "Patient-Level Template",
    range = "A5:H1000",
    col_types = c(
      "text", "date", "numeric", "date", 
      "numeric", "date", "numeric", "text"
      ),
    col_names = c(
      "pat_id", "StartDate", "StartBOS", "EndDate",
      "EndBOS", "LastSeen", "Sessions", "LostToFollowUp"
      )
    ) 
  if(nrow(x) == 0){
    x <- tibble(
      Numerator = NA,
      Denominator = NA,
      Practice = practice,
      Measure = "PRO BOS",
      Month = reportmonth,
      Network = ifelse(practice == "Community IBH", "IBH", "PINQ BH"),
      Aggregation = ifelse(practice == "Community IBH", "Network", "Practice")
    )
  }else{
    x <- x |>
      filter(
        !is.na(pat_id),
        floor_date(EndDate, "month") == reportmonth | 
          (Sessions >= 6 & LastSeen < ceiling_date(reportmonth, "month") - 91)
        ) 
    if(nrow(x) == 0){
      x <- tibble(
        Numerator = NA,
        Denominator = NA,
        Practice = practice,
        Measure = "PRO BOS",
        Month = reportmonth,
        Network = ifelse(practice == "Community IBH", "IBH", "PINQ BH"),
        Aggregation = ifelse(practice == "Community IBH", "Network", "Practice")
      )
    }else{
      x <- x |>
        separate_wider_delim(
        cols = c(StartDate, EndDate),
        delim = " ",
        names_sep = "Date",
        too_many = "drop"
      ) |>
      mutate(
        StartDate = mdy(StartDateDate1),
        EndDate = mdy(EndDateDate1),
        Numerator = case_when(
          EndBOS <= 9 ~ 1,
          ((StartBOS - EndBOS) / StartBOS) >= .4 ~ 1,
          TRUE ~ 0
        )
      ) |>
      reframe(
        Denominator = as.numeric(n()),
        Numerator = sum(Numerator)
      ) |>
      mutate(
        Practice = practice,
        Measure = "PRO BOS",
        Month = reportmonth,
        Network = ifelse(practice == "Community IBH", "IBH", "PINQ BH"),
        Aggregation = ifelse(Network == "IBH", "Network", "Practice")
      )
    }
  }
  x
}

setwd("C:/Users/FLI6SH/OneDrive - cchmc/Documents/Behavioral Health/PINQ/PINQ-Dashboard/Data downloads")

for(j in 1:length(bosbooks)){
  newdata <- rbind(newdata, bos(bosbooks[j]))
}

#Catalyst BOS

catalyst <- dbGetQuery(con, "
  SELECT *
    FROM AndersonCenter.dbo.Catalyst_1125
   ")

catalystrange <- which(workbooks == "Catalyst 202512.xlsx"):max(which(str_starts(workbooks, "Catalyst")))

for(k in catalystrange){
  x <- read_excel(
    workbooks[k], 
    sheet = "Patient-Level Template", 
    col_types = c("text", "date", rep("numeric", 3), "text", "skip"),
    skip = 2)
  catalyst <- rbind(catalyst, x)
}

dischar_events <- filter(catalyst, TxCode == "DISCHAR") |>
  group_by(patientid) |>
  reframe(dischar_date = max(PINQDate))

post_dischar_patients <- inner_join(
  catalyst, 
  dischar_events, 
  join_by(patientid)
  ) |>
  filter(PINQDate > dischar_date) |>
  distinct(patientid) |>
  mutate(PostDischarFlag = 1)

dischargedreal <- left_join(catalyst, post_dischar_patients) |>
  mutate(PostDischarFlag = coalesce(PostDischarFlag, 0))

sessioncount <- dischargedreal |>
  filter(
    TxCode == "DISCHAR",
    PostDischarFlag == 0
  ) |>
  group_by(patientid) |>
  reframe(SessionCount = length(unique(PINQDate)))

finaldischarged <- catalyst |>
  mutate(
    FinalPINQScore = PINQScore1 + PINQScore2 + PINQScore31
  ) |>
  group_by(patientid) |>
  mutate(
    EarliestPINQDate = min(PINQDate),
    LatestPINQDate = max(PINQDate),
    EarliestPINQScore = max(FinalPINQScore[PINQDate == EarliestPINQDate]),
    LatestPINQScore = max(FinalPINQScore[PINQDate == LatestPINQDate])
  ) |>
  distinct(
    patientid, 
    EarliestPINQDate, 
    LatestPINQDate, 
    EarliestPINQScore, 
    LatestPINQScore
  ) |>
  mutate(FUDate = NA) |>
  inner_join(sessioncount) |>
  mutate(
    FUSessionCounting = NA,
    LTFU = "No",
    ReportMonth = floor_date(LatestPINQDate, "month")
  ) |>
  arrange(patientid)

sessioncount2 <- filter(catalyst, TxCode != "DISCHAR") |>
  group_by(patientid) |>
  reframe(SessionCount = length(unique(PINQDate)))

finalnotdischarged <- catalyst |>
  mutate(FinalPINQScore = PINQScore1 + PINQScore2 + PINQScore31) |>
  group_by(patientid) |>
  mutate(
    EarliestPINQDate = min(PINQDate),
    LatestPINQDate = max(PINQDate),
    EarliestPINQScore = max(FinalPINQScore[PINQDate == EarliestPINQDate]),
    LatestPINQScore = max(FinalPINQScore[PINQDate == LatestPINQDate])
  ) |>
  distinct(
    patientid, 
    EarliestPINQDate, 
    LatestPINQDate, 
    EarliestPINQScore, 
    LatestPINQScore
  ) |>
  mutate(FUDate = as.Date(LatestPINQDate) + 90) |>
  inner_join(sessioncount2) |>
  mutate(
    FUSessionCounting = ifelse(SessionCount < 6, "Less than 6", "6 or Greater"),
    LTFU = ifelse(
      SessionCount >= 6 & FUDate <= floor_date(today(), "month") - 1, 
      "Yes", 
      "Not Yet"
    ),
    Month = ceiling_date(FUDate, "month")
  ) |>
  arrange(patientid)

zeros <- catalyst |>
  group_by(patientid) |>
  reframe(
    PINQScore1 = max(PINQScore1),
    PINQScore2 = max(PINQScore2),
    PINQScore31 = max(PINQScore31)
  ) |>
  filter(PINQScore1 == 0, PINQScore2 == 0, PINQScore31 == 0) |>
  select(patientid) |>
  inner_join(catalyst)

finalpatientlevel <- rbind(finaldischarged, finalnotdischarged) |>
  anti_join(zeros, join_by(patientid)) |>
  mutate(
    Numerator = case_when(
      LatestPINQScore <= 9 |
        ((EarliestPINQScore - LatestPINQScore) / EarliestPINQScore) >= .4 ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(patientid) |>
  mutate(row_count = n()) |>
  filter(row_count == 1)

catalystbos <- finalpatientlevel |>
  filter(Month < floor_date(today(), "month")) |>
  group_by(Month) |>
  reframe(
    Numerator = sum(Numerator),
    Denominator = n()
  ) |>
  mutate(
    Practice = "Catalyst",
    Measure = "PRO BOS",
    Network = "PINQ BH",
    Aggregation = "Practice"
  ) 

#BMCP & IBH BOS

etx1a <- dbGetQuery(con, "
  SELECT DISTINCT a.PatientDurableKey
          				,a.EncounterKey
          				,a.FlowsheetValueKey
          				,a.Value
          				,a.DateKey
          				,a.TakenInstant
          				,dad.DateValue AS FlowDate
          				,prd.DurableKey AS ProviderDurableKey
          				,prd.ProviderEpicID
  	FROM caboodle.dbo.FlowsheetValueFact a
  		JOIN caboodle.dbo.DateDim dad
  			ON a.DateKey = dad.DateKey
  		JOIN caboodle.dbo.EncounterFact ef
  			ON a.EncounterKey = ef.EncounterKey
  				AND ef.Count > 0
  		JOIN caboodle.dbo.ProviderDim prd
  			ON ef.ProviderDurableKey = prd.DurableKey
  	WHERE a.FlowsheetRowKey = 40093
  		AND a.Value = 'End of Active Tx'
  		AND a.Count > 0
  ") |>
  filter(ProviderEpicID %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(ProviderEpicID %in% bmcpprov, "BMCP", "Community IBH")) |>
  group_by(PatientDurableKey, Practice, DateKey) |>
  filter(TakenInstant == max(TakenInstant)) |>
  ungroup()

etx1b1 <- dbGetQuery(con, "
  SELECT DISTINCT fv.PatientDurableKey
          				,fv.EncounterKey
          				,fv.NumericValue
          				,fv.DateKey
          				,FirstDocumentedInstant
          				,fv.TakenInstant
          				,prd.ProviderEpicID
  	FROM caboodle.dbo.FlowsheetValueFact fv
  		JOIN caboodle.dbo.EncounterFact ef
  			ON fv.EncounterKey = ef.EncounterKey
      JOIN Caboodle.dbo.ProviderDim prd
  			ON ef.ProviderDurableKey = prd.DurableKey
    WHERE fv.count > 0
  		AND fv.FlowsheetRowKey = 51011
    ") |>
  filter(ProviderEpicID %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(ProviderEpicID %in% bmcpprov, "BMCP", "Community IBH")) |>
  inner_join(etx1a |> distinct(PatientDurableKey, Practice)) |>
  group_by(PatientDurableKey, Practice, DateKey) |>
  filter(TakenInstant == max(TakenInstant)) |>
  filter(FirstDocumentedInstant == max(FirstDocumentedInstant)) |>
  ungroup()

etx1b2 <- dbGetQuery(con, "
  SELECT DISTINCT sv.PatientDurableKey
          				,sv.EncounterKey
          				,sv.NumericResponse
          				,sv.ResponseDateKey
          				,sv.ResponseTimeKey
          				,prd.ProviderEpicID
  	FROM caboodle.dbo.SurveyAnswerFact sv
        JOIN caboodle.dbo.EncounterFact ef
			ON sv.EncounterKey = ef.EncounterKey
        JOIN caboodle.dbo.ProviderDim prd
			ON ef.ProviderDurableKey = prd.durablekey
    WHERE sv.SurveyQuestionKey = 24552
  		AND sv.Valid = 1
  		AND sv.Count > 0
    ")  |>
  filter(ProviderEpicID %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(ProviderEpicID %in% bmcpprov, "BMCP", "Community IBH")) |>
  inner_join(etx1a |> distinct(PatientDurableKey, Practice)) |>
  group_by(PatientDurableKey, Practice, ResponseDateKey) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  filter(ProviderEpicID == min(ProviderEpicID)) |>
  ungroup()

etx1b <- etx1a |>
  left_join(select(etx1b1, PatientDurableKey, DateKey, NumericValue, Practice)) |>
  rename(ProviderBOS = NumericValue) |>
  left_join(
    select(etx1b2, PatientDurableKey, ResponseDateKey, NumericResponse, Practice),
    join_by(PatientDurableKey, Practice, DateKey == ResponseDateKey)
    ) |>
  rename(PatientBOS = NumericResponse) |>
  mutate(TrueBOS = coalesce(ProviderBOS, PatientBOS)
  )

visit1a <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS cptcode
          				,prvd.ProviderEpicID
          				,prvd.name AS ProviderName
          				,dad.DateValue as ProcDate
          				,BillingProcedureQuantity
  	FROM caboodle.dbo.EncounterFact ef
  		JOIN caboodle.dbo.BillingTransactionFact btf
  			ON btf.EncounterKey = ef.EncounterKey
  		JOIN caboodle.dbo.PatientDim pd
  			ON pd.DurableKey = ef.PatientDurableKey
  		JOIN caboodle.dbo.DateDim dad
  			ON ef.DateKey = dad.DateKey
  		JOIN caboodle.dbo.ProviderDim prvd
  			ON ef.ProviderDurableKey = prvd.DurableKey
  	WHERE btf.BillingProcedureCode in  (
      	'90791', '90832', '90834', '90837',
      	'90839', '90840', '90846', '90847'
      	)
  		AND ReportingTransactionType = 'Charge'
  		AND IsInactive = 0
  		AND ef.EncounterKey > 0
  		AND pd.IsCurrent = 1
  		AND dad.DateValue >= '10/1/22'
  ") |>
  filter(ProviderEpicID %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(ProviderEpicID %in% bmcpprov, "BMCP", "Community IBH")) |>
  group_by(
    PatientDurableKey,
    PrimaryMRN,
    EncounterEpicCSN,
    EncounterKey,
    ProviderDurableKey,
    cptcode,
    ProviderEpicID,
    ProviderName,
    ProcDate,
    Practice
    ) |>
  reframe(CPTQty = sum(BillingProcedureQuantity)) |>
  filter(CPTQty > 0) |>
  mutate(trtstfl = ifelse(cptcode == "90791", 1, 0))

visit1b1 <- dbGetQuery(con, "
  SELECT DISTINCT sv.PatientDurableKey
          				,sv.NumericResponse as PatientBOS
          				,dd1.DateValue AS BOS1Date
          				,prd.ProviderEpicID AS BOS1Prov
          				,sv.ResponseTimeKey
	FROM caboodle.dbo.SurveyAnswerFact sv
		JOIN caboodle.dbo.datedim dd1
			ON sv.EncounterDateKey = dd1.DateKey
    JOIN caboodle.dbo.EncounterFact ef
			ON sv.EncounterKey = ef.EncounterKey
		JOIN caboodle.dbo.ProviderDim prd
			ON ef.ProviderDurableKey = prd.DurableKey
  WHERE sv.SurveyQuestionKey = 24552
		AND sv.Valid = 1
		AND sv.Count > 0
  ") |>
  filter(BOS1Prov %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(BOS1Prov %in% bmcpprov, "BMCP", "Community IBH")) |>
  inner_join(visit1a |> distinct(PatientDurableKey, Practice)) |>
  group_by(PatientDurableKey, Practice, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup()

visit1b2 <- dbGetQuery(con, "
  SELECT DISTINCT fv.PatientDurableKey
          				,fv.NumericValue as ProviderBOS
          				,dd1.DateValue as BOS2Date
          				,prd.ProviderEpicId AS BOS2Prov
          				,fv.FirstDocumentedInstant as pbosinst
	FROM caboodle.dbo.FlowsheetValueFact fv
		JOIN caboodle.dbo.DateDim dd1
			ON fv.DateKey = dd1.DateKey
		JOIN caboodle.dbo.EncounterFact ef
			ON fv.EncounterKey = ef.EncounterKey
		JOIN caboodle.dbo.ProviderDim prd
			on ef.ProviderDurableKey = prd.DurableKey
	WHERE fv.count > 0
	  AND fv.FlowsheetRowKey = 51011
    ") |>
  filter(BOS2Prov %in% c(bmcpprov, ibhprov)) |>
  mutate(Practice = ifelse(BOS2Prov %in% bmcpprov, "BMCP", "Community IBH")) |>
  inner_join(visit1a |> distinct(PatientDurableKey, Practice)) |>
  group_by(PatientDurableKey, Practice, BOS2Date) |>
  filter(pbosinst == max(pbosinst)) |>
  ungroup()

visit1b <- visit1a |>
  left_join(visit1b1, join_by(PatientDurableKey, Practice, ProcDate == BOS1Date)) |>
  rename(PatientBOS_Provider = BOS1Prov) |>
  left_join(
    visit1b2, 
    join_by(PatientDurableKey, Practice, ProcDate == BOS2Date),
    relationship = "many-to-many"
    ) |>
  rename(ProviderBOS_Provider = BOS2Prov) |>
  arrange(
    PatientDurableKey,
    ProcDate,
    Practice,
    -EncounterKey,
    -ProviderBOS,
    -PatientBOS,
    -as.numeric(cptcode),
    -as.numeric(ProviderEpicID)
    ) |>
  group_by(PatientDurableKey, Practice, ProcDate) |>
  mutate(rn = row_number()) |>
  filter(rn == 1) |>
  ungroup() |>
  mutate(
    TrueBOS = coalesce(ProviderBOS, PatientBOS),
    TrueBOS_prov = case_when(
      !is.na(ProviderBOS) ~ ProviderBOS_Provider,
      !is.na(PatientBOS) ~ PatientBOS_Provider
    )
  )

visit1c1 <- etx1b |>
  select(
    PatientDurableKey,
    Value,
    adt = FlowDate,
    ProviderEpicID,
    TrueBOS,
    Practice
  ) |>
  mutate(
    cptcode = "",
    trtstfl = 0,
    trtedfl = 1,
    source = 2,
    TrueBOS_Provider = ProviderEpicID
  )

visit1c2 <- visit1b |>
  select(
    PatientDurableKey,
    cptcode,
    adt = ProcDate,
    ProviderEpicID,
    TrueBOS,
    trtstfl,
    Practice
  ) |>
  mutate(
    Value = "",
    trtedfl = 0,
    source = 1,
    TrueBOS_Provider = ProviderEpicID,
  )

visit1c <- rbind(visit1c1, visit1c2) |>
  arrange(PatientDurableKey, Practice, adt, source) |>
  group_by(PatientDurableKey, Practice, adt) |>
  mutate(prvrn = row_number()) |>
  ungroup()

visit2a <- visit1c |>
  group_by(PatientDurableKey, Practice, adt) |>
  reframe(
    TrueBOS = max(coalesce(TrueBOS, -1)),
    Value = max(Value),
    cptcode = max(cptcode),
    trtstfl = max(trtstfl),
    trtedfl = max(trtedfl)
  )

visit2b <- filter(visit1c, prvrn == 1) |>
  distinct(PatientDurableKey, adt, Practice, ProviderEpicID)

visit2 <- left_join(visit2a, visit2b) |>
  arrange(PatientDurableKey, Practice, adt) |>
  group_by(PatientDurableKey, Practice) |>
  mutate(
    CheckLastEnd = lag(trtedfl, 1),
    trtstfl = ifelse(
      trtstfl == 0 & trtedfl == 0 & CheckLastEnd == 1,
      1.1,
      trtstfl
      ),
    TrueBOS = ifelse(TrueBOS == -1, NA, TrueBOS)
    ) |>
  ungroup()

starts <- filter(visit2, trtstfl > 0) |>
  select(
    PatientDurableKey, 
    Practice,
    trtsdt = adt, 
    StartProv = ProviderEpicID, 
    trtstfl
    ) 

ends <- filter(visit2, trtedfl == 1) |>
  select(
    PatientDurableKey,
    Practice,
    trtedt = adt, 
    EndProv = ProviderEpicID, 
    trtedfl
    ) |>
  mutate(
    trtedt = case_when(
      is.na(trtedt) ~ as.Date("2099-01-01"),
      TRUE ~ trtedt
      )
    )

course0 <- left_join(
  starts,
  ends,
  join_by(PatientDurableKey, Practice, trtsdt < trtedt),
  relationship = "many-to-many"
  ) |>
  mutate(
    trtedt = case_when(
      trtedt == "2099-01-01" ~ NA,
      TRUE ~ trtedt
    )
  )

ern1 <- filter(course0, !is.na(trtedt)) |>
  group_by(PatientDurableKey, Practice, trtedt) |>
  filter(trtsdt == max(trtsdt)) |>
  ungroup()

course1 <- filter(course0, is.na(trtedt)) |>
  rbind(ern1) |>
  arrange(PatientDurableKey, Practice, trtsdt, trtedt) |>
  group_by(PatientDurableKey, Practice, trtsdt) |>
  mutate(
    NextEnd = lead(trtedt, 1),
    LastEnd = lag(trtedt, 1),
    srn = row_number(),
    FirstEnd = min(trtedt),
    NewStart = FirstEnd + 90,
    fetxGap = as.numeric(trtedt - FirstEnd),
    netxGap = as.numeric(NextEnd - trtedt),
    letxGap = as.numeric(trtedt - LastEnd)
  ) |>
  ungroup()

course2a <- course1 |>
  filter(
    !is.na(fetxGap),
    fetxGap < 90
    ) |>
  group_by(PatientDurableKey, Practice, trtsdt) |>
  filter(srn == max(srn)) |>
  ungroup() |>
  select(
    PatientDurableKey,
    Practice,
    trtsdt,
    StartProv,
    trtedt,
    EndProv,
    trtstfl,
    trtedfl
    )

course2b <- course1 |>
  filter(
    !is.na(fetxGap),
    fetxGap >= 90
  ) |>
  mutate(
    trtsdt = case_when(
      srn > 2 & letxGap >= 90 ~ LastEnd + 1,
      TRUE ~ NewStart
      ),
    trtedt = case_when(
      netxGap < 90 ~ NextEnd,
      TRUE ~ trtedt
      ),
    trtstfl = 1.2
  ) |>
  select(PatientDurableKey:trtedfl)

course2c <- course1 |>
  filter(is.na(FirstEnd)) |>
  arrange(PatientDurableKey, Practice, trtsdt) |>
  group_by(PatientDurableKey, Practice) |>
  mutate(NextStart = lead(trtsdt, 1)) |>
  ungroup() |>
  select(PatientDurableKey:trtedfl, NextStart) |>
  mutate(
    trtedt = case_when(
      !is.na(NextStart) ~ NextStart - 1,
      TRUE ~ trtedt
    ),
    trtedfl = 1.1
  ) |>
  select(-NextStart)

course2 <- rbind(course2a, course2b) |>
  rbind(course2c) |>
  filter(
    StartProv == EndProv | is.na(EndProv)
  )

out1 <- inner_join(
  course2,
  select(visit2, PatientDurableKey, Practice, adt, cptcode),
  relationship = "many-to-many"
  ) |>
  filter(
    adt >= trtsdt,
    adt <= trtedt | is.na(trtedt),
    !is.na(cptcode),
    cptcode != ""
  ) |>
  group_by(
    PatientDurableKey, 
    Practice, 
    trtsdt, 
    trtedt, 
    StartProv, 
    EndProv, 
    trtstfl, 
    trtedfl
    ) |>
  reframe(
    VisitCount = length(unique(adt)),
    LastSeen = max(adt)
  ) |>
  filter(
    (
      (!is.na(trtedt) & trtedt >= "2022-10-01") |
      (is.na(trtedt) & VisitCount >= 6 & LastSeen >= "2021-10-01")
      ),
    !(is.na(trtedt) & (LastSeen + 90) >= today())
  ) |>
  mutate(
    LTFUDate = case_when(
      is.na(trtedt) ~ LastSeen + 90,
      TRUE ~ NA
    ),
    ReportMonth = case_when(
      !is.na(trtedt) ~ floor_date(trtedt, "month"),
      LTFUDate < "2022-10-01" ~ as.Date("2022-10-01"),
      TRUE ~ ceiling_date(LTFUDate, "month")
    ),
    LostToFollowUp = case_when(
      is.na(trtedt) & !is.na(LTFUDate) ~ "Y",
      !is.na(trtedt) ~ "N"
    ),
    trtedt = case_when(
      is.na(trtedt) ~ LTFUDate,
      TRUE ~ trtedt
    )
  ) |>
  rename(
    DateLastSeen = LastSeen,
    NumberOfSessions = VisitCount
  ) |>
  select(
    PatientDurableKey:trtedt,
    trtstfl:DateLastSeen,
    ReportMonth,
    LostToFollowUp
    )

out2a <- inner_join(
  filter(out1, LostToFollowUp == "N"),
  select(visit2, PatientDurableKey, Practice, TrueBOS, adt) |>
    filter(!is.na(TrueBOS)),
  join_by(PatientDurableKey, Practice, trtedt == adt)
  ) |>
  rename(TreatmentEndBOS = TrueBOS)

out2b <- inner_join(
  filter(out1, LostToFollowUp == "Y"),
  filter(visit2, !is.na(TrueBOS)) |> select(PatientDurableKey, Practice, adt, TrueBOS),
  join_by(PatientDurableKey, Practice, DateLastSeen == adt)
  ) |>
  rename(TreatmentEndBOS = TrueBOS) |>
  rbind(out2a)

out2c <- inner_join(
  filter(out1), 
  filter(visit2, !is.na(TrueBOS)),
  join_by(
    PatientDurableKey,
    Practice,
    trtsdt <= adt,
    trtedt >= adt,
  )
) |>
  group_by(PatientDurableKey, Practice, trtsdt, trtedt) |>
  filter(adt == min(adt)) |>
  ungroup() |>
  select(
    PatientDurableKey, 
    Practice,
    trtsdt, 
    trtedt, 
    FirstBOS = TrueBOS, 
    FirstBOSDate = adt
    )

out2 <- left_join(out2b, out2c) |>
  filter(ReportMonth <= floor_date(today(), "month"))

probos <- out2 |>
  mutate(
    Ratio = (TreatmentEndBOS - FirstBOS) / FirstBOS,
    NumeratorFlag = ifelse(
      TreatmentEndBOS <= 9 |
        (FirstBOS != 0 & Ratio <= -.4),
      1,
      0
    )
  ) |>
  select(
    Month = ReportMonth,
    PatientDurableKey:trtedt,
    NumberOfSessions,
    LostToFollowUp:FirstBOSDate,
    NumeratorFlag
  ) |>
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

proboslines <- probos |>
  group_by(Month, Practice) |>
  reframe(
    Numerator = sum(NumeratorFlag),
    Denominator = n()
    ) |>
  mutate(
    Measure = "PRO BOS",
    Network = ifelse(Practice == "Community IBH", "IBH", "PINQ BH"),
    Aggregation = "Practice"
  )

#Psychiatry BOS
etx1a <- dbGetQuery(con, "
  SELECT DISTINCT a.PatientDurableKey
						,a.EncounterKey
						,a.FlowsheetValueKey
						,a.Value
						,a.DateKey
						,a.TakenInstant
						,dad.DateValue AS FlowDate
						,prd.DurableKey AS ProviderDurableKey
						,prd.ProviderEpicID
			FROM caboodle.dbo.FlowsheetValueFact a
				JOIN caboodle.dbo.DateDim dad
					ON a.DateKey = dad.DateKey
				JOIN caboodle.dbo.EncounterFact ef
					ON a.EncounterKey = ef.EncounterKey
						AND ef.count > 0
				JOIN caboodle.dbo.ProviderDim prd
					ON ef.ProviderDurableKey = prd.DurableKey
				JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
					ON prd.ProviderEpicID = ac.ProviderEpicID
			WHERE (
			    CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
			      WHEN SBFlag = 0 THEN 1 ELSE 0 END =1)
				AND (
				  CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1 
				    WHEN ac.npi <> '1518017102' THEN 1 ELSE 0 END = 1
				)
				AND a.FlowsheetRowKey = 40093
				AND a.Value = 'End of Active Tx'
				AND a.Count > 0
      ") |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == max(TakenInstant)) |>
  ungroup()

etx1b1 <- dbGetQuery(con, "
  SELECT DISTINCT fv.PatientDurableKey
    							,fv.EncounterKey
    							,fv.NumericValue
    							,fv.DateKey
    							,FirstDocumentedInstant
    							,ac.npi
    							,fv.TakenInstant
    	FROM caboodle.dbo.FlowsheetValueFact fv
    	  JOIN caboodle.dbo.EncounterFact ef
					ON fv.EncounterKey = ef.EncounterKey
				JOIN caboodle.dbo.ProviderDim prd
					ON ef.ProviderDurableKey = prd.DurableKey
     		JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
					ON prd.ProviderEpicID = ac.ProviderEpicID
			WHERE (
			  CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
			    WHEN SBFlag = 0 THEN 1
			    ELSE 0 END = 1
			    )
				AND fv.Count > 0
				AND fv.FlowsheetRowKey = 51011
  ") |>
  inner_join(select(etx1a, PatientDurableKey, FlowDate), join_by(PatientDurableKey), relationship = "many-to-many") |>
  mutate(
    flag = case_when(
      npi == "1518017102" & FlowDate < "2024-03-09" ~ 1,
      npi != "1518017102" ~ 1,
      TRUE ~ 0
    )
  ) |>
  filter(flag == 1) |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == max(TakenInstant)) |>
  ungroup() |>
  distinct(
    PatientDurableKey,
    EncounterKey,
    NumericValue,
    DateKey,
    FirstDocumentedInstant
    )

etx1b2 <- dbGetQuery(con, "
  SELECT DISTINCT sv.PatientDurableKey
							,sv.EncounterKey
							,sv.NumericResponse
							,sv.ResponseDateKey
							,sv.ResponseTimeKey
							,ac.npi
		FROM caboodle.dbo.SurveyAnswerFact sv
	    JOIN caboodle.dbo.EncounterFact ef
				ON sv.EncounterKey = ef.EncounterKey
			JOIN caboodle.dbo.ProviderDim prd
				ON ef.ProviderDurableKey = prd.DurableKey
			JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
				ON prd.ProviderEpicID = ac.ProviderEpicID
		WHERE (
		    CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
		      WHEN SBFlag = 0 THEN 1
		      ELSE 0 END = 1
		      )
	    AND sv.SurveyQuestionKey = 24552
				AND sv.Valid = 1
				AND sv.Count > 0
   ") |>
  inner_join(
    select(etx1a, PatientDurableKey, FlowDate),
    join_by(PatientDurableKey),
    relationship = "many-to-many"
    ) |>
  mutate(
    flag = case_when(
      npi == "1518017102" & FlowDate < "2024-03-09" ~ 1,
      npi != "1518017102" ~ 1,
      TRUE ~ 0
    )
  ) |>
  filter(flag == 1) |>
  group_by(PatientDurableKey, ResponseDateKey) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup() |>
  distinct(
    PatientDurableKey,
    EncounterKey,
    NumericResponse,
    ResponseDateKey,
    ResponseTimeKey
  )

etx1b <- left_join(
  etx1a,
  select(etx1b1, PatientDurableKey, DateKey, NumericValue)
  ) |>
  left_join(
    select(etx1b2, PatientDurableKey, ResponseDateKey, NumericResponse),
    join_by(PatientDurableKey, DateKey == ResponseDateKey)
    ) |>
  mutate(TrueBOS = coalesce(NumericValue, NumericResponse)) |>
  rename(
    ProviderBOS = NumericValue,
    PatientBOS = NumericResponse
  )

visit1a <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,SUM(BillingProcedureQuantity) AS CPTQty
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS CPTCode
          				,prvd.ProviderEpicID AS ProviderEpicID
          				,prvd.Name AS ProviderName
          				,dad.DateValue AS ProcDate
          				,CASE WHEN BillingProcedureCode = '90791' THEN 1
          					ELSE 0 END AS trtstfl
  	FROM caboodle.dbo.EncounterFact ef
  		JOIN caboodle.dbo.BillingTransactionFact btf
  			ON btf.EncounterKey = ef.EncounterKey
  		JOIN caboodle.dbo.PatientDim pd
  			ON pd.DurableKey = ef.PatientDurableKey
  		JOIN caboodle.dbo.DateDim dad
  			ON ef.DateKey = dad.DateKey
  		JOIN caboodle.dbo.ProviderDim prvd
  			ON ef.ProviderDurableKey = prvd.DurableKey
  		JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
  			ON prvd.ProviderEpicID = ac.ProviderEpicID
  	WHERE (
  		CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
  			WHEN SBFlag = 0 THEN 1 ELSE 0 END = 1
  			)
  		AND (
  			CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1
  				WHEN ac.npi <> '1518017102' THEN 1
  				ELSE 0 END = 1
  				)
         	AND prvd.StartDate <= dad.DateValue
  		AND prvd.EndDate >= dad.DateValue
  		AND btf.BillingProcedureCode IN (
  		'90791', '90832', '90834', '90837',
  		'90846', '90847', '90839', '90840'
  		  )
  		AND ReportingTransactionType = 'charge'
  		AND IsInactive = 0
  		AND ef.EncounterKey > 0
  		AND pd.IsCurrent = 1
  		AND dad.DateValue >= '10/1/22'
  		AND VisitTypeKey NOT IN ('1536','3113','3670','4246','6024','6749','7333')
  	GROUP BY ef.PatientDurableKey
      			,PrimaryMRN
      			,ef.EncounterEpicCSN
      			,ef.EncounterKey
      			,ef.ProviderDurableKey
      			,btf.BillingProcedureCode
      			,prvd.ProviderEpicID
      			,prvd.Name
      			,dad.DateValue
      			,CASE WHEN BillingProcedureCode = '90791' THEN 1 ELSE 0 END
 ") |>
  filter(CPTQty > 0)

visit1b1 <- dbGetQuery(con, "
  SELECT DISTINCT sv.PatientDurableKey
									,sv.NumericResponse AS PatientBOS
									,dd1.DateValue AS BOS1Date
									,prd.ProviderEpicID AS BOS1Prov
									,sv.ResponseTimeKey
		FROM caboodle.dbo.SurveyAnswerFact sv
			JOIN caboodle.dbo.DateDim dd1
				ON sv.EncounterDateKey = dd1.DateKey
		  JOIN caboodle.dbo.EncounterFact ef
				ON sv.EncounterKey = ef.EncounterKey
			JOIN caboodle.dbo.ProviderDim prd
				ON ef.ProviderDurableKey = prd.DurableKey
			JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
				ON prd.ProviderEpicID = ac.ProviderEpicID
		WHERE (
		  CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
		    WHEN SBFlag = 0 THEN 1
		    ELSE 0 END = 1
		    )
		  AND (
		    CASE WHEN ac.npi = '1518017102' AND dd1.DateValue < '3/9/24' THEN 1
					WHEN ac.npi <> '1518017102' THEN 1
					ELSE 0 END = 1
					)
			AND sv.SurveyQuestionKey = 24552
			AND sv.Valid = 1
			AND sv.Count > 0
      ") |>
  inner_join(select(visit1a, PatientDurableKey), relationship = "many-to-many") |>
  group_by(PatientDurableKey, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup()

visit1b2 <- dbGetQuery(con, "
  SELECT DISTINCT fv.PatientDurableKey
									,fv.NumericValue AS ProviderBOS
									,dd1.DateValue AS BOS2Date
									,prd.ProviderEpicID AS BOS2Prov
									,fv.FirstDocumentedInstant AS pbosinst
		FROM caboodle.dbo.FlowsheetValueFact fv
			JOIN caboodle.dbo.DateDim dd1
				ON fv.DateKey = dd1.DateKey
			JOIN caboodle.dbo.EncounterFact ef
				ON fv.EncounterKey = ef.EncounterKey
			JOIN caboodle.dbo.ProviderDim prd
				ON ef.ProviderDurableKey = prd.DurableKey
			JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
				ON  prd.ProviderEpicID = ac.ProviderEpicID
		WHERE (
		  CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
		    WHEN SBFlag = 0 THEN 1
		    ELSE 0 END = 1
		    )
		  AND (
		    CASE WHEN ac.npi = '1518017102' AND dd1.DateValue < '3/9/24' THEN 1
					WHEN ac.npi <> '1518017102' THEN 1
					ELSE 0 END = 1
					)
			AND fv.Count > 0
			AND fv.FlowsheetRowKey = 51011
    ") |>
  inner_join(select(visit1a, PatientDurableKey), relationship = "many-to-many") |>
  group_by(PatientDurableKey, BOS2Date) |>
  filter(pbosinst == max(pbosinst)) |>
  ungroup()

visit1b <- visit1a |>
  left_join(
    visit1b1,
    join_by(PatientDurableKey, ProcDate == BOS1Date),
    relationship = "many-to-many"
    ) |>
  left_join(
    visit1b2,
    join_by(PatientDurableKey, ProcDate == BOS2Date),
    relationship = "many-to-many"
  ) |>
  select(
    PatientDurableKey:BOS1Prov,
    ProviderBOS:pbosinst
  ) |>
  rename(
    PatientBOS_Provider = BOS1Prov,
    ProviderBOS_Provider = BOS2Prov
  ) |>
  arrange(
    PatientDurableKey,
    ProcDate,
    -EncounterKey,
    -ProviderBOS,
    -PatientBOS,
    -as.numeric(CPTCode),
    -as.numeric(ProviderEpicID)
    ) |>
  group_by(PatientDurableKey, ProcDate) |>
  mutate(zrn = row_number()) |>
  ungroup() |>
  filter(zrn == 1) |>
  mutate(
    TrueBOS = coalesce(ProviderBOS, PatientBOS),
    TrueBOS_Prov = case_when(
      !is.na(ProviderBOS) ~ ProviderBOS_Provider,
      !is.na(PatientBOS) ~ PatientBOS_Provider
    )
  ) 

visit1c <- etx1b |>
  mutate(
    CPTCode = "",
    trtstfl = 0,
    trtedfl = 1,
    Source = 2,
    TrueBOS_Prov = ProviderEpicID
    ) |>
  select(
    PatientDurableKey,
    Value,
    adt = FlowDate,
    ProviderEpicID,
    TrueBOS:TrueBOS_Prov
  ) |>
  rbind(
    visit1b |>
      mutate(
        Value = "",
        TrueBOS_Prov = ProviderEpicID,
        trtedfl = 0,
        Source = 1
      ) |>
      select(
        PatientDurableKey,
        CPTCode,
        ProviderEpicID,
        adt = ProcDate,
        trtstfl,
        TrueBOS,
        Value,
        TrueBOS_Prov,
        trtedfl,
        Source
      )
  ) |>
  arrange(PatientDurableKey, adt, Source) |>
  group_by(PatientDurableKey, adt) |>
  mutate(prvrn = row_number()) |>
  ungroup()

visit2a <- visit1c |>
  mutate(TrueBOS = coalesce(TrueBOS, -1)) |>
  group_by(PatientDurableKey, adt) |>
  reframe(
    TrueBOS = max(TrueBOS),
    Value = max(Value),
    CPTCode = max(CPTCode),
    trtstfl = max(trtstfl),
    trtedfl = max(trtedfl)
  )

visit2b <- filter(visit1c, prvrn == 1) |>
  distinct(PatientDurableKey, adt, ProviderEpicID)

visit2 <- left_join(visit2a, visit2b) |>
  arrange(PatientDurableKey, adt) |>
  group_by(PatientDurableKey) |>
  mutate(
    CheckLastEnd = lag(trtedfl, 1),
    TrueBOS = ifelse(TrueBOS == -1, NA, TrueBOS),
    trtstfl = ifelse(
      trtstfl == 0 & trtedfl == 0 & CheckLastEnd == 1,
      1.1,
      trtstfl
      )
    )|>
  ungroup()

starts <- filter(visit2, trtstfl > 0) |>
  distinct(PatientDurableKey, trtstfl, adt, ProviderEpicID) |>
  rename(
    trtsdt = adt,
    StartProv = ProviderEpicID
  )

ends <- filter(visit2, trtedfl == 1) |>
  distinct(PatientDurableKey, adt, ProviderEpicID, trtedfl) |>
  rename(
    trtedt = adt,
    EndProv = ProviderEpicID
  ) |>
  mutate(
    trtedt = case_when(
      is.na(trtedt) ~ as.Date("2099-01-01"),
      TRUE ~ trtedt
    )
  )

course0 <- left_join(
  starts,
  ends,
  join_by(PatientDurableKey, trtsdt < trtedt),
  relationship = "many-to-many"
  ) |>
  unique()

course1 <- filter(course0, !is.na(trtedt)) |>
  group_by(PatientDurableKey, trtedt) |>
  filter(trtsdt == max(trtsdt)) |>
  ungroup() |>
  rbind(filter(course0, is.na(trtedt))) |>
  arrange(PatientDurableKey, trtsdt, trtedt) |>
  group_by(PatientDurableKey, trtsdt) |>
  mutate(
    NextEnd = lead(trtedt, 1),
    LastEnd = lag(trtedt, 1),
    srn = row_number(),
    FirstEnd = min(trtedt),
    NewStart = FirstEnd + 90,
    fetxgap = as.numeric(trtedt - FirstEnd),
    netxgap = as.numeric(NextEnd - trtedt),
    letxgap = as.numeric(trtedt - LastEnd)
  ) |>
  ungroup()

course2a <- course1 |>
  filter(
    !is.na(fetxgap),
    fetxgap < 90
    ) |>
  group_by(PatientDurableKey, trtsdt) |>
  filter(srn == max(srn)) |>
  select(PatientDurableKey:trtedfl) |>
  ungroup()

course2b <- course1 |>
  filter(
    !is.na(fetxgap),
    fetxgap >= 90
    ) |>
  mutate(
    trtsdt = case_when(
      srn > 2 & letxgap >= 90 ~ LastEnd + 1,
      TRUE ~ NewStart
      ),
    trtedt = case_when(
      netxgap < 90~ NextEnd,
      TRUE ~ trtedt
      ),
    trtstfl = 1.2
  ) |>
  select(PatientDurableKey:trtedfl)

course2 <- filter(course1, is.na(FirstEnd)) |>
  arrange(PatientDurableKey, trtsdt) |>
  group_by(PatientDurableKey) |>
  mutate(NextStart = lead(trtsdt, 1)) |>
  ungroup() |>
  mutate(
    trtedt = case_when(
      !is.na(NextStart) ~ NextStart - 1,
      TRUE ~ trtedt
      ),
    trtedfl = 1.1
  ) |>
  select(PatientDurableKey:trtedfl) |>
  rbind(course2a) |>
  rbind(course2b) |>
  filter(StartProv == EndProv | is.na(EndProv))

out1 <- inner_join(
  select(visit2, PatientDurableKey, adt, CPTCode),
  course2,
  relationship = "many-to-many"
) |>
  filter(
    adt >= trtsdt,
    adt <= trtedt | is.na(trtedt),
    !is.na(CPTCode),
    CPTCode != ""
    ) |>
  group_by(
    PatientDurableKey,
    trtsdt,
    trtedt,
    StartProv,
    EndProv,
    trtstfl,
    trtedfl
    ) |>
  reframe(
    VisitCount = length(unique(adt)),
    LastSeen = max(adt)
  ) |>
  filter(
    (
    (!is.na(trtedt) & trtedt >= "2022-10-01") |
      (is.na(trtedt) & VisitCount >= 6 & LastSeen >= "2021-10-01")
    ),
    !(is.na(trtedt) & LastSeen + 90 >= today())
  ) |>
  mutate(
    LTFUDate = case_when(
      is.na(trtedt) ~ LastSeen + 90,
      TRUE ~ NA
    ),
    Month = case_when(
      !is.na(trtedt) ~ floor_date(trtedt, "month"),
      LTFUDate < "2022-10-01" ~ as.Date("2022-10-01"),
      LTFUDate >= "2022-10-01" ~ ceiling_date(LTFUDate, "month")
    ),
    LostToFollowUp = case_when(
      is.na(trtedt) & !is.na(LTFUDate) ~ "Y",
      !is.na(trtedt) ~ "N"
    ),
    trtedt = case_when(
      is.na(trtedt) ~ LTFUDate,
      TRUE ~ trtedt
    )
  ) |>
  rename(
    DateLastSeen = LastSeen,
    NumberOfSessions = VisitCount
  )

out2a <- filter(out1, LostToFollowUp == "N") |>
  inner_join(
    select(visit2, TrueBOS, PatientDurableKey, adt) |> filter(!is.na(TrueBOS)),
    join_by(PatientDurableKey, trtedt == adt)
  ) |>
  rename(TreatmentEndBOS = TrueBOS)

out2b <- inner_join(
  filter(out1, LostToFollowUp == "Y"),
  select(visit2, TrueBOS, PatientDurableKey, adt) |> filter(!is.na(TrueBOS)),
  join_by(PatientDurableKey, DateLastSeen == adt)
) |>
  rename(TreatmentEndBOS = TrueBOS)

out2c <- inner_join(
  out1,
  filter(visit2, !is.na(TrueBOS)),
  join_by(PatientDurableKey, trtsdt <= adt, trtedt >= adt)
  ) |>
  group_by(PatientDurableKey, trtsdt, trtedt) |>
  filter(adt == min(adt)) |>
  ungroup() |>
  select(PatientDurableKey:trtedt, FirstBOS = TrueBOS, FirstBOSDate = adt)

out2 <- rbind(out2a, out2b) |>
  left_join(out2c) |>
  filter(Month <= floor_date(today(), "month"))

psychbos <- out2 |>
  mutate(
    NumeratorFlag = case_when(
      TreatmentEndBOS <= 9 ~ 1,
      FirstBOS != 0 & (TreatmentEndBOS - FirstBOS) / FirstBOS <= -0.4 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(
    Month,
    PatientDurableKey,
    trtsdt,
    FirstBOS,
    FirstBOSDate,
    trtedt,
    TreatmentEndBOS,
    DateLastSeen,
    NumberOfSessions,
    LostToFollowUp,
    NumeratorFlag
  ) |>
  filter(Month < floor_date(today(), "month")) |>
  arrange(Month, PatientDurableKey, trtsdt, trtedt)

psychboslines <- psychbos |>
  group_by(Month) |>
  reframe(
    Denominator = n(),
    Numerator = sum(NumeratorFlag)
  ) |>
  mutate(
    Practice = "Psychiatry",
    Network = "PINQ BH",
    Measure = "PRO BOS",
    Aggregation = "Practice"
  )


# pinq <- filter(alldata2, Network == "PINQ BH") |>
#   group_by(Measure, Month, Network) |>
#   reframe(
#     Numerator = sum(Numerator),
#     Denominator = sum(Denominator)
#   ) |>
#   mutate(Site = "PINQ BH Network")
# 
# alldata3 <- rbind(alldata2, pinq) |>
#   arrange(Site, Measure, Month) |>
#   group_by(Site, Measure) |>
#   mutate(
#     rn = row_number(),
#     BaseNum = sum(Numerator[rn <= 12]),
#     BaseDenom = sum(Denominator[rn <= 12]),
#     Centerline = BaseNum / BaseDenom,
#     UCL = Centerline + (3 * sqrt(Centerline / Denominator)),
#     LCL = Centerline - (3 * sqrt(Centerline / Denominator)),
#     datlab = paste0(
#       str_trunc(year(Month), 2, "left", ellipsis = ""),
#       "-",
#       monthlabs[month(Month)],
#       " (n = ",
#       round(Denominator, 0),
#       ")"
#     )
#   ) 

setwd("~/Behavioral Health/PINQ/PINQ-Dashboard")
# write_csv(alldata2, "all except utilization.csv")

oldsite <- read_excel(
  "old data.xlsx",
  sheet = "Utilization",
  col_types = c("date", "text", "numeric", "numeric", "text", rep("skip", 7), "text"),
  col_names = c(
    "Month",
    "Practice",
    "Denominator",
    "Numerator",
    "Measure",
    "Network"
  ),
  skip = 1
) |>
  mutate(
    Aggregation = "Practice",
    Practice = case_when(
      Practice == "BEST POINT" ~ "Best Point",
      Practice == "Englender, Sper & Drasnin, MDs, Inc. - Provider" ~ "ESD Pediatrics",
      Practice == "NEWPATH" ~ "NewPath",
      Practice == "BUTLER BH" ~ "Butler BH",
      Practice == "CHSN Rockdale SBHC" ~ "CHSN Rockdale Primary Care",
      TRUE ~ Practice
      ),
    Measure = ifelse(Measure == "IP Admissions", "Inpatient Admissions", Measure)
    )

oldibh <- read_excel(
  "old data.xlsx",
  sheet = "IBH Network Utilization-New",
  col_types = c("date", "text", "numeric", "numeric","text", rep("skip", 7)),
  col_names = c(
    "Month",
    "Practice",
    "Denominator",
    "Numerator",
    "Measure"
  ),
  skip = 1
  ) |>
  mutate(
    Measure = ifelse(
      Measure == "ED", 
      "ED Visits (Discharged)", 
      "Inpatient Admissions"
      ),
    Network = "IBH",
    Aggregation = "Network"
    )

oldpinq <- read_excel(
  "old data.xlsx",
  sheet = "PINQ Network Utilization",
  col_types = c("date", "text", "numeric", "numeric","text", rep("skip", 7)),
  col_names = c(
    "Month",
    "Practice",
    "Denominator",
    "Numerator",
    "Measure"
  ),
  skip = 1
  ) |>
  mutate(
    Measure = ifelse(
      Measure == "ED", 
      "ED Visits (Discharged)", 
      "Inpatient Admissions"
    ),
    Network = "PINQ BH",
    Aggregation = "Network",
    Practice = "PINQ BH Network"
  )
# 
# newsite <- read_excel("utilization.xlsx", sheet =  "practice")
# 
# newibh <- read_excel("utilization.xlsx", sheet = "IBH network")
# 
# newpinq <- read_excel("utilization.xlsx", sheet = "PINQ network")
# 
# utesite <- anti_join(oldsite, newsite, join_by(ReportMonth, Practice)) |>
#   rbind(newsite)
# 
# uteibh <- anti_join(oldibh, newibh, join_by(ReportMonth, Type)) |>
#   rbind(newibh) |>
#   mutate(Network = "IBH")
# 
# utepinq <- anti_join(oldpinq, newpinq, join_by(ReportMonth, Type)) |>
#   rbind(newpinq) |>
#   mutate(
#     Practice = "PINQ BH Network",
#     Network = "PINQ BH"
#     )
# 
# utilization <- rbind(utesite, uteibh) |>
#   rbind(utepinq) |>
#   mutate(
#     Type = case_when(
#       Type == "ED" ~ "ED Visits (Discharged)",
#       Type == "IP" ~ "IP Admissions",
#       TRUE ~ Type
#     ),
#     Practice = case_when(
#       Practice == "BEST POINT" ~ "Best Point",
#       Practice == "BUTLER BH" ~ "Butler BH",
#       Practice == "NEWPATH" ~ "NewPath",
#       TRUE ~ Practice
#     )
#   )
# 
# write_csv(utilization, "Utilization.csv")

indexdates <- dbGetQuery(con, "
  SELECT DateKey
        ,DateValue AS Month
    FROM caboodle.dbo.DateDim
    WHERE DateValue > '2025-09-30'
      AND DateValue < DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE()), 0)
      AND DayOfMonth = 1
  ") |>
  mutate(
    MonthEnd = ceiling_date(Month, "month") - 1,
    IndexNo = 1
    )

dxlist <- dbGetQuery(con, "
  SELECT DISTINCT y.Value
                	,y.DiagnosisKey
                	,x.Name
		FROM caboodle.dbo.TerminologyConceptSetDim x
			JOIN caboodle.dbo.DiagnosisTerminologyDim y
				ON x.TerminologyConceptKey = y.TerminologyConceptkey
					AND x.Concept = y.Value
					AND y.Type = 'ICD-10-CM'
		WHERE x.StandardName = 'ICD-10-CM'
			AND x.Name IN (
				'HP HEDIS V1 2023 MENTAL HEALTH DIAGNOSIS 2023-03-31',
				'CCHMC HP HEDIS V2022 INTENTIONAL SELF-HARM 2022-03-31'
				)
") |>
  group_by(DiagnosisKey) |>
  mutate(Row = row_number()) |>
  ungroup() |>
  pivot_wider(
    id_cols = DiagnosisKey:Name,
    names_from = Row,
    names_prefix = "Row",
    values_from = Value
  ) |>
  mutate(
    ICD10List = case_when(
      !is.na(Row3) ~ paste(Row1, Row2, Row3, sep = "; "),
      !is.na(Row2) ~ paste(Row1, Row2, sep = "; "),
      TRUE ~ Row1
    )
  ) 

popa <- dbGetQuery(con, "
  SELECT DISTINCT a.DurableKey AS PatientDurableKey
          				,a.PrimaryMRN
          				,c.DurableKey AS ProviderDurableKey
          				,a.BirthDate
          				,c.Name
          				,b.Specialty
          				,b.TeamType
          				,b.Role
          				,b.StartDateKey
          				,b.EndDateKey
          				,b.StartInstant
          				,c.PrimaryDepartment AS Practice
          				,c.PrimaryDepartmentEpicID
          				,1 AS IndexNo
          				,d1.DateValue AS StartDate
    FROM caboodle.dbo.PatientDim a
		JOIN caboodle.dbo.CareTeamFact b
			ON a.DurableKey = b.PatientDurableKey
				AND a.IsCurrent = 1
				AND a.PatientKey > 0
				AND b.Count = 1
		JOIN caboodle.dbo.ProviderDim c
			ON b.ProviderDurableKey = c.DurableKey
				AND c.IsCurrent = 1
				AND c.Name LIKE '%hplink%'
				AND b.Role = 'Behavioral Health Service'
		JOIN caboodle.dbo.DateDim d1
			ON b.StartDateKey = d1.DateKey
		JOIN caboodle.dbo.DateDim d2
			ON b.EndDateKey = d2.DateKey
    WHERE (d1.DateValue < d2.DateValue OR EndDateKey = -1)
   ") |>
  inner_join(
    indexdates,
    join_by(IndexNo, StartDate <= MonthEnd),
    relationship = "many-to-many"
    ) |>
  arrange(Month, PatientDurableKey, -StartDateKey, Name) |>
  group_by(Month, PatientDurableKey) |>
  mutate(xrn = row_number()) |>
  filter(xrn == 1) |>
  ungroup() |>
  distinct(
    Month,
    MonthEnd,
    PatientDurableKey,
    PrimaryMRN,
    Practice,
    BirthDate
  )

popb <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,'BMCP' AS Practice
          				,BirthDate
          				,prvd.ProviderEpicID
          				,DepartmentSpecialtyEpicID
          				,1 AS IndexNo
          				,dad.DateValue AS EncounterDate
	FROM caboodle.dbo.EncounterFact ef
		JOIN caboodle.dbo.DateDim dad
			ON ef.DateKey = dad.DateKey
		JOIN caboodle.dbo.PatientDim p
			ON p.DurableKey = ef.PatientDurableKey
		JOIN caboodle.dbo.DepartmentDim dd
			ON dd.DepartmentKey = ef.DepartmentKey
		JOIN caboodle.dbo.ProviderDim prvd
			ON (
			  ef.ProviderDurableKey = prvd.DurableKey
			    OR ef.AttendingProviderDurableKey = prvd.DurableKey
			  )
	WHERE DepartmentSpecialtyEpicID = '75'
		AND p.IsCurrent = 1
		AND prvd.IsCurrent = 1
		AND DerivedEncounterStatus = 'Complete'
		AND dad.DateValue > '2021-12-01'
  ") |>
  filter(ProviderEpicID %in% uteprovs) |>
  inner_join(
    indexdates,
    join_by(IndexNo, EncounterDate <= MonthEnd)
    ) |>
  filter(EncounterDate > MonthEnd - 30) |>
  group_by(Month, PatientDurableKey) |>
  filter(EncounterDate == max(EncounterDate)) |>
  ungroup() |>
  distinct(
    Month,
    MonthEnd,
    PatientDurableKey,
    PrimaryMRN,
    Practice,
    BirthDate
  )

popc <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,'Psychiatry' AS Practice
          				,BirthDate
          				,prvd.ProviderEpicID
          				,prvd.DurableKey
          				,ef.DateKey
          				,DepartmentSpecialtyEpicID
          				,EncounterEpicCSN
          				,1 AS IndexNo
          				,dad.DateValue AS EncounterDate
		FROM caboodle.dbo.EncounterFact ef
  		JOIN caboodle.dbo.DateDim dad
  			ON ef.DateKey = dad.DateKey
  		JOIN caboodle.dbo.PatientDim p
  			ON p.DurableKey = ef.PatientDurableKey
  		JOIN caboodle.dbo.DepartmentDim dd
  			ON dd.DepartmentKey = ef.DepartmentKey
  		JOIN caboodle.dbo.ProviderDim prvd
  			ON (
  			  ef.ProviderDurableKey = prvd.DurableKey 
  			  OR ef.AttendingProviderdurableKey = prvd.DurableKey
  			  )
  		JOIN AndersonCenter.dbo.PINQProvPsychiatry ac
  			ON prvd.ProviderEpicID = ac.ProviderEpicID
  	WHERE (
  	  CASE WHEN SBFlag = 1 AND ef.DepartmentKey = 968 THEN 1
  	    WHEN SBFlag = 0 THEN 1
  	    ELSE 0 END = 1
  	    )
		AND (
		  CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1
		    WHEN ac.npi <> '1518017102' THEN 1
		    ELSE 0 END = 1
		    )
		AND DepartmentSpecialtyEpicID = '37'
		AND VisitTypeKey NOT IN ('1536','3113','3670','4246','6024','6749','7333')
		AND p.IsCurrent = 1
		AND prvd.IsCurrent = 1
		AND DerivedEncounterStatus = 'Complete'
		AND dad.DateValue > '2022-12-01'
  ") |>
  inner_join(
    indexdates,
    join_by(IndexNo, EncounterDate <= MonthEnd),
    relationship = "many-to-many"
    ) |>
  filter(EncounterDate > MonthEnd - 30) |>
  group_by(Month, PatientDurableKey) |>
  filter(EncounterDate == max(EncounterDate)) |>
  ungroup() |>
  distinct(
    Month,
    MonthEnd,
    PatientDurableKey,
    PrimaryMRN,
    Practice,
    BirthDate
    )

popd <- rbind(popa, popb) |>
  rbind(popc)

hvpop1 <- dbGetQuery(con, "
  SELECT DISTINCT pd.DurableKey AS PatientDurableKey
          				,1 AS HVFlag
          				,1 AS IndexNo
          				,pd.DeathDate
          				,dds.DateValue AS StartDate
          				,dde.DateValue AS EndDate
          				,eef.EndDateKey
          				,eef.StartDateKey
	FROM caboodle.dbo.EligibilityEventFact eef
		JOIN caboodle.dbo.PatientDim pd
			ON eef.PatientDurableKey = pd.DurableKey
				AND pd.IsCurrent = 1
		JOIN caboodle.dbo.DateDim dds
			ON eef.StartDateKey = dds.DateKey
		JOIN caboodle.dbo.DateDim dde
			ON eef.EndDateKey = dde.DateKey
	WHERE eef.PayorName IN (
			    	'Healthvine Caresource Medicaid',
				    'HEALTHVINE HUMANA OH MEDICAID',
				    'HEALTHVINE AMERIHEALTH OH MEDICAID',
				    'HEALTHVINE ANTHEM OH MEDICAID'
				)
		AND dds.DateValue >= '2021-01-01'
		AND (EndDateKey = -1 OR EndDateKey > 20220000)
  ") |>
  inner_join(
    indexdates,
    join_by(IndexNo, StartDate <= MonthEnd)
    ) |>
  filter(
    (EndDateKey > StartDateKey & EndDate > Month) |
      EndDateKey == -1,
    (is.na(DeathDate) | DeathDate > Month)
  ) |>
  distinct(Month, MonthEnd, PatientDurableKey, HVFlag)

pop2 <- left_join(popd, hvpop1) |>
  mutate(HVFlag = coalesce(HVFlag, 0))

# PINQ BH Utilization

enc1e <- dbGetQuery(con, "
  SELECT DISTINCT a.PatientDurableKey
                  ,a.EncounterKey
          				,a.EncounterEpicCSN
          				,a.Date AS EncounterDate
          				,b.DepartmentEpicID
          				,b.DepartmentAbbreviation
          				,b.DepartmentSpecialty
          				,a.AdmissionType
          				,a.DischargeDisposition
          				,a.IsHospitalAdmission
          				,a.IsHospitalOutpatientVisit
          				,a.SourceKey
          				,sd.SourceID
          				,a.SourceComboKey
          				,sd.SourceName
          				,sd.IsLocalEpic
          				,sd.IsDerivedFromClaims
          				,IsDerivedFromBilling
          				,sd.IsExcludedFromThirdPartyViews
          				,ExternalType
          				,ExternalTypeCategoryKey
          				,Hl7Type
          				,Hl7TypeCategoryKey
          				,a.Type AS EncounterType
          				,a.TypeCategoryKey
          				,PatientClass
          				,PatientClassCategoryKey
          				,ProviderDurableKey
          				,PlaceOfServiceKey
          				,a.SourceComboId
          				,a.SourceComboIdTypeID
          				,PrimaryProcedureDurableKey
          				,prd.Name AS ProcName
          				,PrimaryDiagnosisKey
          				,DateKey
          				,Date AS StartDate
          				,EndInstant AS EndDate
          				,CASE WHEN IsLocalEpic = 1
          				    AND IsDerivedFromClaims = 0
          				    AND IsExcludedFromThirdPartyViews = 0
          				    AND IsDerivedFromBilling = 0 THEN 1
          					WHEN IsLocalEpic = 0
          					  AND IsDerivedFromClaims = 1
          					  AND IsExcludedFromThirdPartyViews = 1
          					  AND IsDerivedFromBilling = 1 THEN 2
          					WHEN IsLocalEpic = 0
          					  AND IsDerivedFromClaims = 0
          					  AND IsExcludedFromThirdPartyViews = 1
          					  AND IsDerivedFromBilling = 0 THEN 3
          					WHEN IsLocalEpic = 0
          					  AND IsDerivedFromClaims = 1
          					  AND IsExcludedFromThirdPartyViews = 1
          					  AND IsDerivedFromBilling = 0 THEN 4
          					END AS SourceFlag
          				,IsEDVisit
          				,IsClaimsDatalink
          				,HasFollowingAdmission
          				,CASE WHEN Hl7Type IS NOT NULL AND Hl7Type <> '' then Hl7Type
          					WHEN (Hl7type IS NULL OR Hl7Type = '')
          						AND (
          						  PatientClass IS NOT NULL
          						    AND NOT(PatientClass IN ('', '*Not Applicable'))
          						  ) THEN PatientClass
          					END AS xstrat
          				,CASE WHEN PrimaryDiagnosisKey > 0 THEN PrimaryDiagnosisKey
          					WHEN PrimaryDiagnosisKey < 0
          					  AND df.DiagnosisKey > 0 THEN df.DiagnosisKey
          					END AS encdx
		FROM caboodle.dbo.EncounterFact a
  		JOIN caboodle.dbo.DepartmentDim b
	  		ON a.DepartmentKey = b.DepartmentKey
	  	JOIN caboodle.dbo.SourceDim sd
			  ON a.SourceKey = sd.SourceKey
				  AND a.SourceKey > 0
		  JOIN caboodle.dbo.ProcedureDim prd
			  ON a.PrimaryProcedureKey = prd.ProcedureKey
		  LEFT JOIN caboodle.dbo.DiagnosisEventFact df
			  ON a.EncounterKey = df.EncounterKey
				  AND df.IsPrimary = 1
		WHERE a.Count > 0
		  AND a.DerivedEncounterStatus IN ('Complete', 'In Progress')
		  AND (
		    a.TypeCategoryKey in (819587, 819599, 819469, 819619, 3459972, 3460052)
		      OR (a.TypeCategoryKey = -2 AND a.Type = 'Hospital Encounter')
		    )
		  AND DateKey > 20251000
   ") |>
  mutate(
    Measure = case_when(
      (
        TypeCategoryKey %in% c(819469, 819587, 819599, 3460052) |
          (TypeCategoryKey == -2 & EncounterType == "Hospital Encounter")
        ) &
        coalesce(IsHospitalOutpatientVisit, 0) != 1 &
        (IsHospitalAdmission == 1 | PatientClass == "Observation") &
        coalesce(DischargeDisposition, "") != "Expired" ~ "Inpatient Admissions",
      (
        TypeCategoryKey %in% c(819587, 819599, 819619, 3459972) |
          (TypeCategoryKey == -2 & EncounterType == "Hospital Encounter")
      ) &
        !PatientClassCategoryKey %in% c(772820, 772847, 772824, 772827, 772826, 772851) &
        !AdmissionType %in% c("Elective", "Routine Elective") &
        IsEDVisit == 1 &
        (is.na(IsHospitalAdmission) | IsHospitalAdmission == 0) &
        (is.na(HasFollowingAdmission) | HasFollowingAdmission == 0) &
        !Hl7Type %in% c("Observation", "Inpatient Encounter") &
        !PatientClass %in% c("Observation", "InpatientEncounter") ~ 
          "ED Visits (Discharged)"
    )
  ) |>
  filter(!is.na(Measure)) |>
  inner_join(
    pop2,
    join_by(
      PatientDurableKey,
      EncounterDate >= Month,
      EncounterDate <= MonthEnd
      )
    ) |>
  mutate(
    Age = case_when(
      month(EncounterDate) < month(BirthDate) ~ year(EncounterDate) - year(BirthDate) - 1,
      month(EncounterDate) > month(BirthDate) ~ year(EncounterDate) - year(BirthDate),
      mday(EncounterDate) >= mday(BirthDate) ~ year(EncounterDate) - year(BirthDate),
      TRUE ~ year(EncounterDate) - year(BirthDate) -1
    )
  ) |>
  filter(Age < 18)

chmc1 <- inner_join(enc1e, dxlist, join_by(encdx == DiagnosisKey)) |>
  filter(
    SourceFlag == 1,
    (
      Measure == "Inpatient" | PatientClass == "Emergency" | 
        !DepartmentEpicID %in% c(
          "30002047", "30001047", "30005047", "30011047",
          "20001500", "30030047", "30010047"
        )
      )
    ) |>
  select(-c(Row1, Row2, Row3)) 

claimed0 <- inner_join(enc1e, dxlist, join_by(encdx == DiagnosisKey)) |>
  filter(SourceFlag == 2) |>
  select(-c(Row1, Row2, Row3))

claimed1 <- dbGetQuery(con, "
  SELECT DISTINCT pf.ReceivedClaimKey
          				,sf.SentClaimKey
          				,pf.ReceivedClaimProcedureKey
          				,pf.PayerClaimID
          				,pf.BillingProviderDurableKey
          				,pf.RenderingProviderDurableKey
          				,sf.RenderingProviderDurableKey AS SCRPDurableKey
          				,sf.BillingProviderDurableKey AS SCBPDurableKey
          				,pf.PerformedStartDateKey
          				,pf.PerformedEndDateKey
          				,sf.PerformedStartDateKey AS SCPSDateKey
          				,sf.PerformedEndDateKey AS SCPEDateKey
          				,pf.RevenueCode
          				,sf.RevenueCode AS SCRevenueCode
          				,pf.TaxIDNumber
          				,sf.TaxIDNumber AS SCTaxID
          				,ecm.SentClaimServiceLineKey
          				,ecm.IsDefiningClaim
          				,ecm.IsEDVisit
          				,ecm.IsHospitalAdmission
          				,ecm.EncounterDateKey
          				,ecm.EncounterKey
          				,BillingProcedureDurableKey
          				,LineProcessingStatus
          				,CASE WHEN pf.TaxIDNumber = '310833936'
          				    OR sf.TaxIDNumber = '310833936' THEN 1
          					ELSE 0 END AS CCHMCClaim
	FROM caboodle.epic.EncounterClaimMappingFact ecm
		LEFT JOIN caboodle.dbo.ReceivedClaimProcedureFact pf
			ON ecm.ReceivedClaimProcedureKey = pf.ReceivedClaimProcedureKey
				AND ecm.ReceivedClaimProcedureKey > 0
				AND pf.IsCurrentInClaimProcedureSequence = 1
				AND pf._HasSourceCaresource = 1
		LEFT JOIN caboodle.dbo.SentClaimServiceLineFact sf
			ON ecm.SentClaimServiceLineKey = sf.SentClaimServiceLineKey
				AND ecm.SentClaimServiceLineKey > 0
		WHERE ecm.EncounterDateKey > 20251000
  ") |>
  right_join(
    select(
      claimed0,
      Month,
      MonthEnd,
      EncounterKey,
      PatientDurableKey,
      Practice,
      BirthDate,
      Age,
      ICD10List,
      HasFollowingAdmission,
      Measure
      ),
    join_by(EncounterKey),
    relationship = "many-to-many"
    )

claimed2 <- filter(claimed1, CCHMCClaim != 1)

claimout1 <- claimed2 |>
  mutate(
    ClaimTIN = coalesce(TaxIDNumber, SCTaxID),
    ClaimRC = coalesce(RevenueCode, SCRevenueCode),
    ClaimBP = coalesce(BillingProviderDurableKey, SCBPDurableKey),
    HasFollowingAdmission = coalesce(HasFollowingAdmission, 0)
    ) |>
  distinct(
    Month,
    MonthEnd,
    PayerClaimID,
    EncounterKey,
    EncounterDateKey,
    PatientDurableKey,
    Practice,
    ClaimTIN,
    ClaimRC,
    ClaimBP,
    IsEDVisit,
    IsHospitalAdmission,
    HasFollowingAdmission,
    ICD10List,
    BirthDate,
    Age
    ) |>
  mutate(
    UC = ifelse(ClaimRC == "0456", 1, 0),
    ED = ifelse(
      ClaimRC == "0981" | (str_starts(ClaimRC, "045") & ! str_starts(ClaimRC, "0456")),
      1,
      0
    ),
    Obs = ifelse(ClaimRC %in% c("0760", "0762", "0769"), 1, 0),
    IP = case_when(
      str_starts(ClaimRC, "01") & !str_starts(ClaimRC, "018") ~ 1,
      str_starts(ClaimRC, "020") | str_starts(ClaimRC, "021") ~ 1,
      ClaimRC %in% c("1000", "1001", "1002") ~ 1,
      TRUE ~ 0
    )
  ) |>
  group_by(
    Month,
    MonthEnd,
    PayerClaimID,
    EncounterKey,
    EncounterDateKey,
    PatientDurableKey,
    Practice,
    ClaimTIN,
    ClaimBP,
    ICD10List,
    BirthDate,
    Age
  ) |>
  reframe(
    EDFlag = max(ED),
    UCFlag = max(UC),
    ObsFlag = max(Obs),
    IPFlag = max(IP),
    IsEDVisitFlag = max(IsEDVisit),
    IsHospitalAdmissionFlag = max(IsHospitalAdmission),
    FollowingAdmissionFlag = max(HasFollowingAdmission)
  ) |>
  mutate(
    Measure = case_when(
     (ObsFlag == 1 | IPFlag == 1) & EDFlag == 0 & UCFlag == 0 ~ "Inpatient Admissions",
     EDFlag == 1 & 
       UCFlag == 0 & 
       ObsFlag == 0 & 
       IPFlag == 0 & 
       IsHospitalAdmissionFlag == 0 ~ "ED Visits (Discharged)"
    )
  ) |>
  filter(!is.na(Measure)) |>
  group_by(Month, PatientDurableKey, EncounterKey, PayerClaimID) |>
  mutate(xrn = row_number()) |>
  ungroup() |>
  filter(xrn == 1)

claimed4 <- dbGetQuery(con, "
  SELECT pf.ReceivedClaimKey
        ,pf.PatientDurableKey
    		,pf.ReceivedClaimProcedureKey
    		,pf.PayerClaimID
    		,pf.PerformedStartDateKey
    		,pf.PerformedEndDateKey
    		,pf.BillingProviderDurableKey
    		,pf.RevenueCode
    		,pf.BillingProcedureDurableKey
    		,pf.Quantity
    		,pf.PrimaryDiagnosisKey
    		,CASE WHEN pf.RevenueCode = '0456' THEN 1
    			ELSE 0 END AS UC
    		,CASE WHEN pf.RevenueCode LIKE '045[^6]' THEN 1
    			WHEN pf.RevenueCode = '0981' THEN 1
    			ELSE 0 END AS ED
    		,CASE WHEN pf.RevenueCode IN ('0760', '0762', '0769') THEN 1
    			ELSE 0 END AS Obs
    		,CASE WHEN pf.RevenueCode LIKE '01[^8]%' THEN  1
    			WHEN pf.RevenueCode LIKE '02[01]%' THEN  1
    			WHEN pf.RevenueCode IN ('1000', '1001', '1002') THEN 1
    			ELSE 0 END AS IP
    		,pf.TaxIDNumber AS ClaimTIN
    		,pf.LineProcessingStatus
    		,d1.DateValue AS PerformedStartDate
    		,d2.DateValue AS PerformedEndDate
	FROM caboodle.dbo.ReceivedClaimProcedureFact pf
		JOIN caboodle.dbo.DateDim d1
			ON d1.DateKey = pf.PerformedStartDateKey
		JOIN caboodle.dbo.DateDim d2
			ON d2.DateKey = pf.PerformedEndDateKey
		LEFT JOIN caboodle.epic.EncounterClaimMappingFact ecm
			ON ecm.ReceivedClaimProcedureKey = pf.ReceivedClaimProcedureKey
	WHERE ecm.EncounterKey IS NULL
	  AND PerformedStartDateKey > 20220000
		AND pf.IsCurrentInClaimProcedureSequence = 1
		AND pf._HasSourceCaresource = 1
		AND pf.TaxIDNumber <> '310833936'
		AND pf.RevenueCode IS NOT NULL
		AND (
			pf.RevenueCode IN ('0760', '0762', '0769', '0981', '1000', '1001', '1002')
				OR pf.RevenueCode LIKE '01[^8]%'
				OR pf.RevenueCode LIKE '02[01]%'
				OR pf.RevenueCode LIKE '045%'
				)
  ") |>
  inner_join(pop2, relationship = "many-to-many") |>
  inner_join(dxlist, join_by(PrimaryDiagnosisKey == DiagnosisKey)) |>
  left_join(
    select(
      claimed1,
      EncounterKey,
      enclpstatus = LineProcessingStatus,
      PatientDurableKey,
      PerformedStartDateKey,
      PerformedEndDateKey,
      BillingProviderDurableKey,
      BillingProcedureDurableKey,
      RevenueCode
    )
  ) |>
  mutate(
    Age = case_when(
      month(PerformedStartDate) < month(BirthDate) ~ year(PerformedStartDate) - year(BirthDate) - 1,
      month(PerformedStartDate) > month(BirthDate) ~ year(PerformedStartDate) - year(BirthDate),
      mday(PerformedStartDate) >= mday(BirthDate) ~ year(PerformedStartDate) - year(BirthDate),
      TRUE ~ year(PerformedStartDate) - year(BirthDate) -1
    )
  ) |>
  filter(
    PerformedStartDate >= Month,
    PerformedEndDate <= MonthEnd,
    is.na(EncounterKey),
    Age < 18
  )

claimout2 <- claimed4 |>
  arrange(
    Month,
    PatientDurableKey,
    PerformedStartDateKey,
    PerformedEndDateKey,
    RevenueCode,
    BillingProviderDurableKey,
    LineProcessingStatus,
    PayerClaimID
  ) |>
  group_by(
    Month,
    PatientDurableKey,
    PerformedStartDateKey,
    PerformedEndDateKey,
    RevenueCode,
    BillingProviderDurableKey
  ) |>
  mutate(cid = row_number()) |>
  filter(cid == 1) |>
  rename(EncounterDateKey = PerformedStartDateKey) |>
  group_by(
    Month,
    PayerClaimID,
    PatientDurableKey,
    Practice,
    ClaimTIN,
    BillingProviderDurableKey,
    ICD10List,
    BirthDate,
    Age,
    EncounterDateKey,
    EncounterKey
    ) |>
  reframe(
    EDFlag = max(ED),
    UCFlag = max(UC),
    ObsFlag = max(Obs),
    IPFlag = max(IP)
    ) |>
  mutate(
    Measure = case_when(
      (ObsFlag == 1 | IPFlag == 1) & EDFlag == 0 & UCFlag == 0 ~ "Inpatient Admissions",
      EDFlag == 1 & ObsFlag == 0 & IPFlag == 0 & UCFlag == 0 ~ "ED Visits (Discharged)"
    )
  ) |>
  filter(!is.na(Measure)) |>
  arrange(PayerClaimID, EncounterDateKey) |>
  group_by(PayerClaimID) |>
  mutate(xrn = row_number()) |>
  ungroup()

ext1 <- inner_join(enc1e, dxlist, join_by(encdx == DiagnosisKey)) |>
  filter(SourceFlag %in% 3:4)

enc2e1 <- chmc1 |>
  mutate(
    skey = paste0("X.", as.character(EncounterKey)),
    EDFlag = ifelse(DepartmentEpicID %in% c("30001001", "30010001"), 1, 0),
    UCFlag = ifelse(
      DepartmentEpicID %in% c(
        "30002047", "30001047", "30005047", "30011047",
        "20001500", "30030047", "30010047"
      ),
      1,
      0
    )
  ) |>
  select(
    Month,
    skey,
    EncounterKey,
    EncounterDate = StartDate,
    edk = DateKey,
    Practice,
    PatientDurableKey,
    ProviderDurableKey,
    EDFlag,
    UCFlag,
    FollowingAdmissionFlag = HasFollowingAdmission,
    SourceFlag,
    ICD10List,
    BirthDate,
    Age,
    Measure
  ) 

enc2e2 <- claimout1 |>
  mutate(
    EncounterYear = str_trunc(EncounterDateKey, 4, "right", ellipsis = ""),
    EncounterNotYear = str_trunc(EncounterDateKey, 4, "left", ellipsis = ""),
    EncounterMonth = str_trunc(EncounterNotYear, 2, "right", ellipsis = ""),
    EncounterDay = str_trunc(EncounterNotYear, 2, "left", ellipsis = ""),
    EncounterDate = as.Date(paste(EncounterYear, EncounterMonth, EncounterDay, sep = "-"))
    ) |>
  select(
    Month,
    EncounterKey,
    EncounterDate,
    edk = EncounterDateKey,
    Practice,
    PatientDurableKey,
    ProviderDurableKey = ClaimBP,
    ICD10List,
    BirthDate,
    Age,
    Measure,
    EDFlag,
    UCFlag,
    FollowingAdmissionFlag
  ) |>
  mutate(
    skey = paste0("X.", EncounterKey),
    SourceFlag = 2.0
    )

enc2e3 <- claimout2 |>
  mutate(
    skey = paste(as.character(PayerClaimID), as.character(xrn), sep = "."),
    EncounterYear = str_trunc(EncounterDateKey, 4, "right", ellipsis = ""),
    EncounterNotYear = str_trunc(EncounterDateKey, 4, "left", ellipsis = ""),
    EncounterMonth = str_trunc(EncounterNotYear, 2, "right", ellipsis = ""),
    EncounterDay = str_trunc(EncounterNotYear, 2, "left", ellipsis = ""),
    EncounterDate = as.Date(paste(EncounterYear, EncounterMonth, EncounterDay, sep = "-")),
    SourceFlag = 2.1,
    FollowingAdmissionFlag = NA
    ) |>
  select(
    Month,
    EncounterKey,
    skey,
    EncounterDate,
    edk = EncounterDateKey,
    Practice,
    PatientDurableKey,
    ProviderDurableKey = BillingProviderDurableKey,
    SourceFlag,
    ICD10List,
    BirthDate,
    Age,
    Measure,
    EDFlag,
    UCFlag,
    FollowingAdmissionFlag,
    SourceFlag
  )

enc2e4 <- ext1 |>
  select(
    Month,
    EncounterKey,
    EncounterDate = StartDate,
    edk = DateKey,
    Practice,
    PatientDurableKey,
    ProviderDurableKey,
    SourceFlag,
    ICD10List,
    BirthDate,
    Age,
    Measure,
    FollowingAdmissionFlag = HasFollowingAdmission
  ) |>
  mutate(
    skey = paste0("X.", EncounterKey),
    EDFlag = ifelse(Measure == "ED Visits (Discharged)", 1, 0),
    UCFlag = 0
    ) 

enc2e <- rbind(enc2e1, enc2e2) |>
  rbind(enc2e3, enc2e4) |>
  arrange(
    Month,
    Measure,
    Practice,
    PatientDurableKey,
    edk,
    SourceFlag
  ) |>
  group_by(Month, Measure, Practice, PatientDurableKey, edk) |>
  mutate(xcounter1 = row_number()) |>
  arrange(
    Month,
    Measure,
    Practice,
    PatientDurableKey,
    ICD10List,
    edk,
    SourceFlag
  ) |>
  group_by(Month, Measure, Practice, PatientDurableKey, edk, ICD10List) |>
  mutate(xcounter2 = row_number()) |>
  filter(xcounter2 == 1)

practicefinala <- pop2 |>
  mutate(
    Practice = case_when(
      Practice == "EXT BEST POINT" ~ "Best Point",
      Practice == "EXT BUTLER BH SERVICES" ~ "Butler BH",
      Practice == "EXT NEWPATH CFS" ~ "NewPath",
      TRUE ~ Practice
    ),
    Age = case_when(
      month(MonthEnd) < month(BirthDate) ~ year(MonthEnd) - year(BirthDate) - 1,
      month(MonthEnd) > month(BirthDate) ~ year(MonthEnd) - year(BirthDate),
      mday(MonthEnd) >= mday(BirthDate) ~ year(MonthEnd) - year(BirthDate),
      TRUE ~ year(MonthEnd) - year(BirthDate) - 1
    )
  ) |>
  filter(Age < 18) |>
  group_by(Month, Practice) |>
  reframe(Denominator = length(unique(PatientDurableKey)))

practicefinalb <- enc2e |>
  mutate(
    Practice = case_when(
      Practice == "EXT BEST POINT" ~ "Best Point",
      Practice == "EXT BUTLER BH SERVICES" ~ "Butler BH",
      Practice == "EXT NEWPATH CFS" ~ "NewPath",
      TRUE ~ Practice
    )
  ) |>
  group_by(Month, Measure, Practice) |>
  reframe(Numerator = length(unique(skey)))

pinqutepracticefinal <- left_join(practicefinala, practicefinalb) |>
  mutate(
    Numerator = coalesce(Numerator, 0),
    Network = "PINQ BH",
    Aggregation = "Practice"
    ) |>
  filter(
    Practice %in% c("Best Point", "Butler BH", "NewPath", "BMCP", "Psychiatry")
  ) 

networkfinala <- pop2 |>
  mutate(
    Practice = case_when(
      Practice == "EXT BEST POINT" ~ "Best Point",
      Practice == "EXT BUTLER BH SERVICES" ~ "Butler BH",
      Practice == "EXT NEWPATH CFS" ~ "NewPath",
      TRUE ~ Practice
    ),
    Age = case_when(
      month(MonthEnd) < month(BirthDate) ~ year(MonthEnd) - year(BirthDate) - 1,
      month(MonthEnd) > month(BirthDate) ~ year(MonthEnd) - year(BirthDate),
      mday(MonthEnd) >= mday(BirthDate) ~ year(MonthEnd) - year(BirthDate),
      TRUE ~ year(MonthEnd) - year(BirthDate) - 1
    )
  ) |>
  filter(
    Age < 18,
    Practice %in% c("Best Point", "Butler BH", "NewPath", "BMCP", "Psychiatry")
    ) |>
  group_by(Month) |>
  reframe(Denominator = length(unique(PatientDurableKey)))

networkfinalb <- enc2e |>
  mutate(
    Practice = case_when(
      Practice == "EXT BEST POINT" ~ "Best Point",
      Practice == "EXT BUTLER BH SERVICES" ~ "Butler BH",
      Practice == "EXT NEWPATH CFS" ~ "NewPath",
      TRUE ~ Practice
    )
  ) |>
  filter(
    Practice %in% c("Best Point", "Butler BH", "NewPath", "BMCP", "Psychiatry")
    ) |>
  group_by(Month, Measure) |>
  reframe(Numerator = length(unique(skey)))

pinqutenetworkfinal <- left_join(networkfinala, networkfinalb) |>
  mutate(
    Numerator = coalesce(Numerator, 0),
    Practice = "PINQ BH Network",
    Network = "PINQ BH",
    Aggregation = "Network"
    )

# means <- tibble(
#   Practice = rep(
#     c("Best Point", "BMCP", "Butler BH", "NewPath", "Psychiatry"), 
#     each = 2, 
#     2
#     ),
#   Season = rep(c("Break", "School"), 10),
#   Type = rep(c("Inpatient", "ED"), each = 10),
#   Centerline = c(
#     2.860915493,
#     4.051683672,
#     3.552729993,
#     3.239104829,
#     10.37195994,
#     11.56677182,
#     5.62969141,
#     7.869098176,
#     5.843140482,
#     7.162418369,
#     2.732062707,
#     5.196466403,
#     3.232323232,
#     3.687013519,
#     4.828585225,
#     9.202453988,
#     6.463719766,
#     8.118910817,
#     4.57225364,
#     7.157207433
#   )
# )

# withseason <- practicefinal |>
#   mutate(
#     Season = ifelse(month(ReportMonth) %in% c(2:5, 9:11), "School", "Break")
#   ) |>
#   inner_join(means) |>
#   mutate(
#     reportyear = as.character(year(ReportMonth)),
#     reportyear = str_trunc(reportyear, 2, "left", ellipsis = ""),
#     reportmonth = monthlabs[month(ReportMonth)],
#     datlab = paste0(
#       reportyear,
#       "-",
#       reportmonth,
#       " (n = ",
#       format(Denominator, big.mark = ","),
#       ")"
#       ),
#     Rate = (Numerator / Denominator) * 1000,
#     UCL = Centerline + (3 * sqrt((Centerline * 1000) / Denominator)),
#     LCL = Centerline - (3 * sqrt((Centerline * 1000) / Denominator)),
#     LCL = ifelse(LCL < 0, 0, LCL),
#     Network = "PINQ BH"
#   ) |>
#   arrange(Practice, ReportMonth, Type)
# 
# network_means <- tibble(
#   Type = rep(c("Inpatient", "ED"), each = 2),
#   Season = rep(c("Break", "School"), 2),
#   Centerline = c(3.815228093, 5.59798851, 3.699650878, 5.717594268)
# )
# 
# networkseason <- networkfinal |>
#   mutate(Season = ifelse(month(ReportMonth) %in% c(2:5, 9:11), "School", "Break")) |>
#   inner_join(network_means) |>
#   mutate(
#     Practice = "PINQ BH Network",
#     reportyear = as.character(year(ReportMonth)),
#     reportyear = str_trunc(reportyear, 2, "left", ellipsis = ""),
#     reportmonth = monthlabs[month(ReportMonth)],
#     datlab = paste0(
#       reportyear,
#       "-",
#       reportmonth,
#       " (n = ",
#       format(Denominator, big.mark = ","),
#       ")"
#     ),
#     Rate = (Numerator / Denominator) * 1000,
#     UCL = Centerline + (3 * sqrt((Centerline * 1000) / Denominator)),
#     LCL = Centerline - (3 * sqrt((Centerline * 1000) / Denominator)),
#     LCL = ifelse(LCL < 0, 0, LCL),
#     Network = "PINQ BH"
#   )
# 
# pinqutilization <- rbind(networkseason, withseason)

#IBH utilization

dxlist <- dbGetQuery(con, "
  SELECT DISTINCT x.TerminologyConceptKey
        					,x.StandardName
        					,x.Concept
        					,x.ExternalID
        					,x.Name 
        					,y.Value
        					,y.DiagnosisKey
		FROM caboodle.dbo.TerminologyConceptSetDim x 
			JOIN caboodle.dbo.DiagnosisTerminologyDim y 
				ON x.TerminologyConceptKey = y.TerminologyConceptKey 
					AND x.Concept = y.Value 
					AND y.Type = 'ICD-10-CM'
		WHERE x.StandardName = 'ICD-10-CM' 
			AND x.Name = 'HP HEDIS V1 2023 MENTAL HEALTH DIAGNOSIS 2023-03-31'
") |>
  group_by(DiagnosisKey) |>
  mutate(Row = row_number()) |>
  pivot_wider(
    id_cols = DiagnosisKey:Name,
    names_from = Row,
    names_prefix = "Row",
    values_from = Value
  ) |>
  mutate(
    ICD10List = case_when(
      !is.na(Row3) ~ paste(Row1, Row2, Row3, sep = "; "),
      !is.na(Row2) ~ paste(Row1, Row2, sep = "; "),
      TRUE ~ Row1
    )
  ) |>
  ungroup()

denom1 <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey
          				,PrimaryMRN
          				,c.ProviderDurableKey
          				,ppm.NPI
          				,ppm.[Practice Name] AS Practice
          				,ppm.[Practice TIN] AS TIN
          				,dd1.DateValue AS TeamStartDate
          				,dd2.DateValue AS TeamEndDate
          				,pt.StartDate
          				,pt.EndDate
          				,c.EndDateKey
          				,c.StartDateKey
	FROM caboodle.dbo.PatientDim pt 
		JOIN caboodle.dbo.CareTeamFact c 
			ON c.PatientDurableKey = pt.DurableKey
		JOIN AndersonCenter.dbo.PracticeProvMap_notupdated ppm 
			ON ppm.ProviderDurableKey = c.ProviderDurableKey
		JOIN caboodle.dbo.DateDim dd1 
			ON dd1.DateKey = c.StartDateKey
		JOIN caboodle.dbo.DateDim dd2 
			ON dd2.DateKey = c.EndDateKey
	WHERE c.Role = 'general'
		AND ppm.[Practice TIN] IN (
		  '310843308', '310840164', '316094740', '316000064', '311321054'
		      )
		AND NOT ppm.NPI IN (
		    '1295105369', '1265435788', '1376073460', '1669878419', 
		    '1447896568', '1497105886', '1073678504'
		    )                   
   ") |>
  mutate(IndexNo = 1) |>
  inner_join(indexdates, relationship = "many-to-many") |>
  filter(
    Month >= TeamStartDate,
    Month >= StartDate,
    Month <= EndDate,
    (EndDateKey > StartDateKey & Month <= TeamEndDate) | EndDateKey == -1
  )

denom2 <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey
				,pt.PrimaryMRN
				,c.ProviderDurableKey
				,pd.NPI
				,CASE WHEN pd.NPI IN (
					'1043406366', '1104965433', '1134193790', '1184697021',
					'1326364555', '1639143209', '1689648248', '1821025966',
					'1821061771', '1841371911', '1881918332', '1952939902', '1740570209'
					  ) THEN 'Anderson Hills Pediatrics'
					WHEN pd.NPI IN (
					  '1669595450', '1457415390', '1497039085', 
					  '1184840043', '1679153928'
					  ) THEN 'Muddy Creek Pediatrics'
					WHEN pd.NPI IN (
						'1215947130', '1275857146', '1184639916', '1083098305', 
						'1558376392', '1356305478',	'1386152379', '1679823611', 
						'1710947817', '1760845556', '1992957138'
						) THEN 'Pediatrics of Florence' 
					WHEN pd.NPI IN ('1548255839', '1073508362', '1033505292') 
					  THEN 'Liberty Sharonville Pediatrics'
					WHEN pd.NPI IN (
					  '1295827459', '1417978107', '1609197912', 
					  '1568666436', '1023339769'
					  ) THEN 'Montgomery Pediatrics' END AS Practice
				,TIN = 'NA'
				,1 AS IndexNo
				,dd1.DateValue AS TeamStartDate
				,c.EndDateKey
				,c.StartDateKey
				,dd2.DateValue AS TeamEndDate
				,pt.StartDate
				,pt.EndDate
				,pd.StartDate AS ProviderStart
				,pd.EndDate AS ProviderEnd
	FROM caboodle.dbo.PatientDim pt 
		JOIN caboodle.dbo.CareTeamFact c 
			ON c.PatientDurableKey = pt.DurableKey
		JOIN caboodle.dbo.ProviderDim pd 
			ON pd.DurableKey = c.providerdurablekey
		JOIN caboodle.dbo.DateDim dd1 
			ON dd1.DateKey = c.StartDateKey
		JOIN caboodle.dbo.DateDim dd2 
			ON dd2.DateKey = c.EndDateKey
	WHERE c.Role = 'general'
		AND pd.NPI IN (
		  '1043406366',	'1104965433', '1134193790',	'1184697021', 	'1326364555',
		  '1639143209',	'1689648248',	'1821025966',	'1821061771',	'1841371911',
		  '1881918332',	'1952939902',	'1740570209',	'1669595450',	'1457415390',
		  '1497039085',	'1184840043',	'1215947130',	'1275857146',	'1184639916',	
		  '1083098305',	'1558376392',	'1356305478',	'1386152379',	'1679823611',
		  '1710947817',	'1760845556',	'1992957138',	'1679153928',	'1548255839',
		  '1073508362',	'1033505292',	'1295827459',	'1417978107',	'1609197912',
		  '1568666436',	'1023339769'
		  )                   
    ") |>
  inner_join(indexdates, relationship = "many-to-many") |>
  filter(
    Month >= TeamStartDate,
    (EndDateKey > StartDateKey & Month <= TeamEndDate) | EndDateKey == -1,
    Month >= StartDate,
    Month <= EndDate,
    Month >= ProviderStart,
    Month <= ProviderEnd
  )

denom3 <- dbGetQuery(con, "
  SELECT DISTINCT x.DurableKey
          				,x.PrimaryMRN
          				,BirthDate AS DOB
          				,y.DurableKey AS ProviderDurableKey
          				,y.Name AS ProvName
          				,y.NPI AS ProvNPI
          				,x.StartDate AS PatientStart
          				,x.EndDate AS PatientEnd
          				,y.StartDate AS ProviderStart
          				,y.EndDate AS ProviderEnd
          				,rvf.StartDate AS RegistryStart
          				,rvf.EndDate AS RegistryEnd
          				,dd1.DateValue AS CareTeamStart
          				,dd2.DateValue AS CareTeamEnd
          				,CASE WHEN PrimaryDepartmentEpicID IN ('20057540','20074540','20073540') THEN 'NKY'
          					WHEN PrimaryDepartmentEpicID = '20061540' THEN 'CHSN Anderson Primary Care'
          					WHEN PrimaryDepartmentEpicID = '20020540' THEN 'CHSN Batesville Primary Care'
          					WHEN PrimaryDepartmentEpicID IN ('20060540','20059540','20058540') THEN 'MLS'
          					WHEN PrimaryDepartmentEpicID = '20080540' THEN 'CHSN Kenwood Crossing Primary Care'
          					WHEN PrimaryDepartmentEpicID IN ('20001410','20067540') THEN 'CHSN Rockdale Primary Care'
          					WHEN PrimaryDepartmentEpicID IN ('20096540') THEN 'CHSN Wilmington Primary Care'
          					END AS Practice
          				,c.EndDateKey
          				,c.StartDateKey
	FROM caboodle.dbo.PatientDim x 
		JOIN caboodle.dbo.CareTeamFact c 
			ON c.PatientDurableKey = x.DurableKey
		JOIN caboodle.dbo.ProviderDim y 
			ON y.DurableKey = c.ProviderDurableKey
				AND x.PatientKey > 0 
				AND y.ProviderKey > 0
				AND y.PrimaryDepartmentEpicID IN (
					'20057540','20061540','20020540','20060540',
					'20059540','20058540','20080540','20074540',
					'20073540','20001410','20067540','20096540'
					)
		join caboodle.dbo.PatientRegistryValueFact rvf 
			ON x.DurableKey = rvf.PatientDurableKey 
				AND RegistryMetricKey = '5179' 
				AND rvf.Value = 'Active'
		JOIN caboodle.dbo.DateDim dd1 
			ON dd1.DateKey = c.StartDateKey
		JOIN caboodle.dbo.DateDim dd2 
			ON dd2.DateKey = c.EndDateKey
	WHERE x.Status <> 'Deceased' 
		AND c.Role = 'general'
		AND rvf.EndDate > '2025-09-30'
		AND x.EndDate > '2025-09-30'
		AND y.EndDate > '2025-09-30'
     ") |>
  mutate(IndexNo = 1) |>
  inner_join(indexdates, relationship = "many-to-many") |>
  filter(
    Month >= PatientStart,
    Month <= PatientEnd,
    Month >= ProviderStart,
    Month <= ProviderEnd,
    Month >= RegistryStart,
    Month <= RegistryEnd,
    Month >= CareTeamStart,
    (EndDateKey > StartDateKey & Month <= CareTeamEnd) | EndDateKey == -1
  )

denomall <- select(
  denom1, 
  PrimaryMRN, 
  DurableKey, 
  Practice, 
  Month, 
  MonthDateKey = DateKey, 
  MonthEnd
  ) |>
  rbind(
    select(
      denom2, 
      PrimaryMRN, 
      DurableKey, 
      Practice, 
      Month, 
      MonthDateKey = DateKey, 
      MonthEnd
    ) 
  ) |>
  rbind(
    select(
      denom3, 
      PrimaryMRN, 
      DurableKey, 
      Practice, 
      Month, 
      MonthDateKey = DateKey, 
      MonthEnd
    ) 
  )

denomagg <- denomall |>
  group_by(Month, Practice) |>
  reframe(Denominator = length(unique(DurableKey))) 

ednum <- dbGetQuery(con, "
  SELECT DISTINCT enc.EncounterEpicCSN
          				,enc.PatientDurableKey
          				,enc.DateKey AS EncStartDate
          				,dd.DateValue AS ddStartDate
          				,enc.DischargeDateKey AS EncEndDate
          				,dtg.Value
          				,enc.DischargeDisposition
          				,dd.DateValue AS EncounterDate
	from caboodle.dbo.EncounterFact enc  
		JOIN caboodle.dbo.DiagnosisEventFact dev 
			ON dev.EncounterKey = enc.EncounterKey
		JOIN caboodle.dbo.DepartmentDim d 
			ON d.DepartmentKey = enc.DepartmentKey
		JOIN caboodle.dbo.DiagnosisTerminologyDim dtg 
			ON dtg.DiagnosisKey = enc.PrimaryDiagnosisKey
		JOIN caboodle.dbo.DateDim dd 
			ON dd.DateKey = enc.DateKey
	WHERE enc.IsHospitalAdmission = 0
		AND enc.AdmissionType IN ('Emergency', 'Urgent')
		AND enc.Count > 0
		AND d.DepartmentEpicID IN ('30001001','30010001')--emergency  departments only
		AND dev.Type = 'billing diagnosis'
		AND dev._IsDeleted = 0
		AND dev.IsPrimary = 1
		AND dd.DateValue > '2025-09-30'           
    ") |>
  mutate(IndexNo = 1) |>
  inner_join(
    denomall, 
    join_by(PatientDurableKey == DurableKey),
    relationship = "many-to-many"
    ) |>
  filter(
    EncounterDate >= Month,
    EncounterDate <= MonthEnd
  ) |>
  inner_join(
    dxlist, 
    join_by(Value == ICD10List),
    relationship = "many-to-many"
    ) |>
  distinct(
    Month, 
    MonthDateKey, 
    MonthEnd, 
    PatientDurableKey, 
    PrimaryMRN, 
    Practice, 
    EncounterEpicCSN, 
    EncStartDate, 
    ddStartDate, 
    EncEndDate, 
    Value, 
    DischargeDisposition
    )

edagg <- left_join(denomagg, ednum, join_by(Practice, Month)) |>
  group_by(Practice, Month, Denominator) |>
  reframe(Numerator = length(unique(EncounterEpicCSN[!is.na(EncounterEpicCSN)]))) |>
  mutate(Measure = "ED Visits (Discharged)") 

admits <- dbGetQuery(con, "
  SELECT DepartmentEpicID
          				,a.EncounterKey
          				,a.EncounterEpicCSN
          				,a.PatientDurableKey AS DurableKey
          				,DepartmentName
          				,BedName
          				,DepartmentAbbreviation
          				,DepartmentSpecialty
          				,a.AdmissionSource
          				,a.AdmissionType
          				,a.DischargeDisposition
          				,IsHospitalAdmission
          				,a.PatientClass
          				,a.AdmissionDateKey
          				,AdmissionInstant
          				,a.DischargeDateKey
          				,a.DischargeInstant
          				,IsEDVisit
          				,ddm.Type
          				,ddm.Value
          				,ddm.DisplayString
          				,ptm.Code AS ProcCode
          				,ptm.CodeSet AS ProcCodeSet
          				,dev.Type AS BillType
          				,dtd.DateValue
	FROM caboodle.dbo.EncounterFact a 
		JOIN caboodle.dbo.DepartmentDim b 
			ON a.DepartmentKey = b.DepartmentKey
		LEFT JOIN caboodle.dbo.HospitalAdmissionFact haf 
			ON a.Encounterkey = haf.EncounterKey 
		JOIN caboodle.dbo.DiagnosisEventFact dev 
			ON dev.EncounterKey = a.EncounterKey
		JOIN caboodle.dbo.DiagnosisTerminologyDim ddm 
			ON a.PrimaryDiagnosisKey = ddm.DiagnosisKey 
				AND ddm.Type = 'ICD-10-CM'
		LEFT JOIN caboodle.dbo.ProcedureTerminologyDim ptm 
			ON haf.PrimaryCodedProcedureKey = ptm.ProcedureTerminologyKey
		JOIN caboodle.dbo.DateDim dtd 
			ON dtd.DateKey = a.DischargeDateKey
		WHERE a.PatientClass IN (
			'Inpatient', 'Inpatient Pulmonary', 'Inpatient Cancer and Blood Diseases', 
			'Inpatient Mental Health', 'Inpatient Rehab', 'Inpatient Cardiology', 
			'Inpatient Hematology/Oncology', 'Inpatient Neonatology', 
			'Inpatient Convalescent', 'Surgery Admit', 'Observation'
			) 
			AND a.Count > 0 
			AND NOT(ISNULL(IsHospitalOutpatientVisit, 0) = 1) 
			AND DATE <> ISNULL(EndInstant, 0)
			AND NOT a.EncounterEpicCSN IS NULL
			AND dev.Type = 'billing diagnosis'
			AND dev._IsDeleted = 0
			AND dev.IsPrimary = 1
			AND NOT DepartmentName IN ('PB2-400','PB3-200','PB3-400')  
			AND (a.DischargeDateKey < 0 OR a.DischargeDateKey > 20251000)
    ") |>
  inner_join(
    denomall, 
    join_by(DurableKey, DateValue >= Month, DateValue <= MonthEnd),
    relationship = "many-to-many"
    ) |>
  inner_join(dxlist, join_by(Value == ICD10List), relationship = "many-to-many") |>
  distinct(
    DepartmentEpicID,
    Month,
    MonthEnd,
    DurableKey,
    PrimaryMRN,
    Practice,
    EncounterKey,
    EncounterEpicCSN,
    DepartmentName,
    BedName,
    DepartmentAbbreviation,
    DepartmentSpecialty,
    AdmissionSource,
    AdmissionType,
    DischargeDisposition,
    IsHospitalAdmission,
    PatientClass,
    AdmissionDateKey,
    AdmissionInstant,
    DischargeDateKey,
    DischargeInstant,
    IsEDVisit,
    BillType,
    Value,
    DisplayString
  ) 

ipagg <- inner_join(denomagg, admits) |>
  group_by(Practice, Month, Denominator) |>
  reframe(Numerator = length(unique(EncounterKey[!is.na(EncounterKey)]))) |>
  mutate(Measure = "Inpatient Admissions")

ibhpracticeute <- rbind(edagg, ipagg) |>
  mutate(
    Network = "IBH",
    Aggregation = "Practice",
    Practice = ifelse(
      Practice == "Englender, Sper & Drasnin, MDs, Inc. - Provider", 
      "ESD Pediatrics",
      Practice
      )
  ) 

ibhutefinal <- edagg |>
  group_by(Month, Measure) |>
  reframe(
    Denominator = sum(Denominator),
    Numerator = sum(Numerator)
  ) |>
  rbind(
    ipagg |>
      group_by(Month, Measure) |>
      reframe(
        Denominator = sum(Denominator),
        Numerator = sum(Numerator)
      )
    ) |>
  mutate(
    Practice = "IBH Network",
    Network = "IBH",
    Aggregation = "Network"
  )

# mean_lookup <- tibble(
#   Mean = c(
#     0.425785574, 0.900448938, 2.732062707, 5.196466403, 3.232323232,
#     3.687013519, 4.828585225, 9.202453988, 2.531004809, 1.995808802, 0,
#     2.565089137, 2.072753653, 3.184375332, 9.49066751, 0, 0, 0,
#     0.693048721, 4.173274351, 2.468831009, 5.481955231, 1.516875237,
#     0.809301573, 0, 1.889823302, 2.338360809, 3.556007112, 0, 1.992031873,
#     0.738661545, 2.457002457, 6.463719766, 8.118910817, 3.653902368,
#     2.75065328, 2.187130922, 2.353187261, 4.57225364, 7.157207433, 2.111430758,
#     2.298146162, 1.589319771, 4.635889511, 2.678875846, 2.620735844,
#     2.860915493, 4.051683672, 3.552729993, 3.239104829, 10.37195994,
#     11.56677182, 4.693293284, 5.246839971, 1.69865806, 2.540937324,
#     2.098195552,  4.413306118, 9.49066751, 0, 18.81467545, 4.395604396,
#     0.583703012, 5.018820577, 6.615069127, 5.151983514, 0.476667143, 
#     2.214419031, 1.916810427, 1.935234163, 2.488215535, 1.900469688, 0,
#     1.186450733, 0, 0, 5.62969141, 7.869098176, 3.307015006, 0.683153436,
#     2.48849073, 2.007578609, 5.843140482, 7.162418369, 0.936373426,
#     2.8078057, 3.386960203, 5.39083558
#     ),
#   Type = rep(c("ED Visits (Discharged)", "IP Admissions"), each = 44),
#   Season = rep(c("Break", "School"), 44),
#   Practice = rep(
#     c(
#       "Anderson Hills Pediatrics", "Best Point", "BMCP", "Butler BH", 
#       "CHSN Anderson Primary Care", "CHSN Batesville Primary Care",
#       "CHSN Kenwood Crossing Primary Care", "CHSN Rockdale SBHC", 
#       "CHSN Wilmington Primary Care", "Cincinnati Health Department",
#       "Crossroad Health Center", 
#       "Englender, Sper & Drasnin, MDs, Inc. - Provider", 
#       "Liberty Sharonville Pediatrics", "MLS", "Montgomery Pediatrics",
#       "Muddy Creek Pediatrics", "NewPath", "NKY", "Pediatrics of Florence",
#       "Psychiatry", "Springdale Mason Pediatrics", "West Side Pediatrics"
#       ),
#     2,
#     each = 2
#     )
# )
# 
# ibhpractice <- practicefinal |>
#   mutate(
#     Season = case_when(
#       month(MeasureDate) %in% c(2:5, 9:11) ~ "School",
#       TRUE ~ "Break"
#     ),
#     datlab = paste0(
#       str_trunc(year(MeasureDate), 2, "right", ellipsis = ""),
#       "-",
#       monthlabs[month(MeasureDate)],
#       " n = ",
#       format(TotalPatients, big.mark = ","),
#       ")"
#     ),
#     Rate = (10000 * TotalEncounters) / TotalPatients
#   ) |>
#   left_join(mean_lookup, join_by(Practice, Season, Measure == Type)) |>
#   mutate(
#     UCL = Mean + (3 * sqrt((10000 * Mean) / TotalPatients)),
#     LCL = Mean - (3 * sqrt((10000 * Mean) / TotalPatients)),
#     LCL = ifelse(LCL < 0, 0, LCL),
#     Network = "IBH"
#   ) |>
#   rename(
#     Type = Measure,
#     Denominator = TotalPatients,
#     Numerator = TotalEncounters
#     )
# 
# network_means <- tibble(
#   Measure = rep(c("ED Visits (Discharged)", "IP Admissions"), each = 2),
#   Season = rep(c("Break", "School"), 2),
#   Mean = c(1.66821024, 3.030554633,  2.09877971, 2.752514537)
# )
# 
# ibhnetwork <- networkfinal |>
#   mutate(
#     Season = case_when(
#       month(MeasureDate) %in% c(2:5, 9:11) ~ "School",
#       TRUE ~ "Break"
#     ),
#     datlab = paste0(
#       str_trunc(year(MeasureDate), 2, "right", ellipsis = ""),
#       "-",
#       monthlabs[month(MeasureDate)],
#       " n = ",
#       format(Denominator, big.mark = ","),
#       ")"
#     ),
#     Rate = (10000 * Numerator) / Denominator
#   ) |>
#   left_join(network_means, join_by(Season, Type == Measure)) |>
#   mutate(
#     UCL = Mean + (3 * sqrt((10000 * Mean) / Denominator)),
#     LCL = Mean - (3 * sqrt((10000 * Mean) / Denominator)),
#     LCL = ifelse(LCL < 0, 0, LCL),
#     Network = "IBH"
#   )
# 
# alldata4 <- rbind(ibhpractice, ibhnetwork) |>
#   rename(Month = MeasureDate) |>
#   rbind(
#     alldata3 |>
#       rename(
#         Practice = Site,
#         Type = Measure,
#         )
#   )

alldata <- anti_join(aeu, newdata, join_by(Practice, Measure, Month)) |>
  rbind(newdata) |>
  anti_join(crisisii, join_by(Practice, Measure, Month)) |>
  anti_join(proboslines, join_by(Practice, Measure, Month)) |>
  anti_join(psychboslines, join_by(Practice, Measure, Month)) |>
  anti_join(catalystbos, join_by(Practice, Measure, Month)) |>
  rbind(catalystbos) |>
  rbind(proboslines) |>
  rbind(psychboslines) |>
  rbind(crisisii) |>
  rbind(oldsite) |>
  rbind(oldibh) |>
  rbind(oldpinq) |>
  anti_join(pinqutepracticefinal, join_by(Practice, Measure, Month)) |>
  rbind(pinqutepracticefinal) |>
  anti_join(pinqutenetworkfinal, join_by(Practice, Measure, Month)) |>
  rbind(pinqutenetworkfinal) |>
  anti_join(ibhpracticeute, join_by(Practice, Measure, Month)) |>
  rbind(ibhpracticeute) |>
  anti_join(ibhutefinal, join_by(Practice, Measure, Month)) |>
  rbind(ibhutefinal) |>
  mutate(Numerator = coalesce(Numerator, 0)) |>
  filter(
    Month >= "2022-01-01",
    !is.na(Denominator),
    Denominator > .00001,
    Month < floor_date(today() - 15, "month")
    )

pinq <- alldata |>
  filter(
    Network == "PINQ BH", 
    Aggregation == "Practice",
    Practice != "PINQ BH Network",
    !Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions")
  ) |> 
  group_by(Measure, Month, Network) |>
  reframe(
    Numerator = sum(Numerator),
    Denominator = sum(Denominator)
  ) |>
  mutate(
    Practice = "PINQ BH Network",
    Aggregation = "Network"
  )

alldata2 <- filter(
  alldata, 
  Measure %in% c("Crisis Stabilization", "Initial Intake")
  ) |>
  group_by(Practice, Measure) |>
  reframe(
    FirstMonth = min(Month),
    LastMonth = max(Month)
  ) |>
  inner_join(
    dateframe, 
    join_by(Practice, Measure, FirstMonth <= Month, LastMonth >= Month)
  ) |>
  distinct(Practice, Measure, Month) |>
  left_join(alldata) |>
  filter(is.na(Denominator)) |>
  group_by(Practice, Measure) |>
  mutate(
    FirstMonth = as.Date(min(Month)),
    LastMonth = as.Date(max(Month)),
    FirstMonth = floor_date(FirstMonth - 1, "month"),
    LastMonth = floor_date(LastMonth + 35, "month")
  ) |>
  select(Practice, Measure, Month, FirstMonth, LastMonth) |>
  inner_join(
    select(alldata, Practice, Measure, Month, Denominator1 = Denominator),
    join_by(Practice, Measure, FirstMonth == Month)
  ) |>
  inner_join(
    select(alldata, Practice, Measure, Month, Denominator2 = Denominator),
    join_by(Practice, Measure, LastMonth == Month)
  ) |>
  mutate(Denominator = (Denominator1 + Denominator2) / 2) |>
  select(Practice, Measure, Month, Denominator) |>
  mutate(
    Numerator = 0,
    Aggregation = "Practice"
  ) |>
  inner_join(alldata |> distinct(Practice, Network)) |>
  rbind(alldata) |>
  rbind(pinq) |>
  mutate(
    Season = case_when(
      !Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions") ~ "None",
      month(Month) %in% c(2:5, 9:11) ~ "School",
      TRUE ~ "Break"
    )
  ) |>
  arrange(Practice, Measure, Month) |>
  group_by(Practice, Measure) |>
  mutate(rn = row_number()) |>
  group_by(Practice, Measure, Season) |>
  mutate(
    BaseDenom = sum(Denominator[rn <= 24]),
    BaseNumerator = sum(Numerator[rn <= 24]),
    Centerline = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      BaseNumerator * 10000 / BaseDenom,
      BaseNumerator / BaseDenom
    ),
    Rate = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      Numerator * 10000 / Denominator,
      Numerator / Denominator
    ),
    xlab = paste0(
      str_trunc(year(Month), 2, "left", ellipsis = ""),
      "-",
      monthlabs[month(Month)],
      " (n = ",
      format(round(Denominator, 0), big.mark = ","),
      ")"
    ),
    UCL = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      Centerline + (3 * sqrt((10000 * Centerline) / Denominator)),
      Centerline + (3 * sqrt(Centerline / Denominator))
    ) ,
    LCL = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      Centerline - (3 * sqrt((10000 * Centerline) / Denominator)),
      Centerline - (3 * sqrt(Centerline / Denominator))
    ),
    LCL = ifelse(LCL < 0, 0, LCL),
    UCL = ifelse(Measure == "PRO BOS" & UCL > 1, 1, UCL)
  ) 

shifts <- filter(alldata2, rn > 24) |>
  arrange(Practice, Measure, Month) |>
  group_by(Practice, Measure) |>
  mutate(maxrn = max(rn)) |>
  ungroup() |>
  filter(maxrn >= 32) |>
  mutate(
    Above = ifelse(Rate > Centerline, "Y", "N"),
    RLEVal = paste0(Practice, Measure, Above)
    ) 
  
shiftrle <- rle(shifts$RLEVal)
shiftlabs <- 1:shiftrle$lengths[1]
shiftruns <- rep(1, each = shiftrle$lengths[1])
for (m in 2:length(shiftrle$lengths)){
  x <- 1:shiftrle$lengths[m]
  y <- rep(m, shiftrle$lengths[m])
  shiftlabs <- c(shiftlabs, x)
  shiftruns <- c(shiftruns, y)
}

shifts1 <- shifts |>
  mutate(
    Run = shiftlabs,
    RunNo = shiftruns
    ) |>
  group_by(Practice, Measure, RunNo) |>
  mutate(MaxRun = max(Run)) |>
  filter(MaxRun >= 8) |>
  group_by(Practice, Measure) |>
  mutate(StartDate = min(Month[MaxRun >= 8])) |>
  distinct(Practice, Measure, StartDate) |>
  inner_join(alldata2) |>
  filter(Month >= StartDate) |>
  group_by(Practice, Measure) |>
  mutate(rn = row_number()) |>
  filter(rn <= 24) |>
  group_by(Practice, Measure, Season) |>
  mutate(
    BaseDenom = sum(Denominator[rn <= 24]),
    BaseNumerator = sum(Numerator[rn <= 24]),
    ShiftPeriod = ifelse(rn <= 8, Rate, NA),
    Centerline = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      BaseNumerator * 10000 / BaseDenom,
      BaseNumerator / BaseDenom
    ),
    UCL = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      Centerline + (3 * sqrt((10000 * Centerline) / Denominator)),
      Centerline + (3 * sqrt(Centerline / Denominator))
    ) ,
    LCL = ifelse(
      Measure %in% c("ED Visits (Discharged)", "Inpatient Admissions"),
      Centerline - (3 * sqrt((10000 * Centerline) / Denominator)),
      Centerline - (3 * sqrt(Centerline / Denominator))
    ),
    LCL = ifelse(LCL < 0, 0, LCL),
    UCL = ifelse(Measure == "PRO BOS" & UCL > 1, 1, UCL)
  ) |>
  ungroup() |>
  select(Practice:Month, Centerline, UCL, LCL, ShiftPeriod) |>
  group_by(Practice, Measure) |>
  mutate(EndDate = max(Month))

alldata3 <- left_join(alldata2, shifts1, join_by(Practice, Measure, Month)) |>
  mutate(
    Centerline = coalesce(Centerline.y, Centerline.x),
    UCL = coalesce(UCL.y, UCL.x),
    LCL = coalesce(LCL.y, LCL.x),
    Month = as.character(Month)
  ) |>
  select(Practice:Season, Rate, xlab, ShiftPeriod, Centerline:LCL)

write_csv(alldata3, "for powerbi.csv")

#Time to goal

# etx1a <- dbGetQuery(con, "
#   SELECT DISTINCT a.PatientDurableKey
# 					,a.EncounterKey
# 					,a.FlowsheetValueKey
# 					,a.Value
# 					,a.DateKey
# 					,a.TakenInstant
# 					,dad.DateValue AS FlowDate
# 					,prd.DurableKey AS ProviderDurableKey
# 					,prd.ProviderEpicID
# 		FROM caboodle.dbo.FlowsheetValueFact a  
# 			JOIN caboodle.dbo.DateDim dad 
# 				ON a.DateKey = dad.DateKey
# 			JOIN caboodle.dbo.EncounterFact ef 
# 				ON a.EncounterKey = ef.EncounterKey 
# 					AND ef.Count > 0
# 			JOIN caboodle.dbo.ProviderDim prd 
# 				ON ef.ProviderDurableKey = prd.Durablekey 
#     WHERE a.FlowsheetRowKey = 40093 
# 		AND a.Value = 'End of Active Tx' 
# 		AND a.Count > 0                   
#   ") |>
#   filter(ProviderEpicID %in% uteprovs) |>
#   group_by(PatientDurableKey, DateKey) |>
#   filter(TakenInstant == max(TakenInstant)) |>
#   ungroup()
# 
# etx1b2 <- dbGetQuery(con, "
#   SELECT DISTINCT fv.PatientDurableKey
#     							,fv.EncounterKey
#     							,fv.NumericValue
#     							,fv.DateKey
#     							,FirstDocumentedInstant
#     							,prd.ProviderEpicID
#     							,fv.TakenInstant
# 				FROM caboodle.dbo.FlowsheetValueFact fv 
# 					--join #etx1a a on a.patientdurablekey=fv.patientdurablekey 
# 					JOIN caboodle.dbo.EncounterFact ef 
# 						ON fv.EncounterKey = ef.EncounterKey
# 					JOIN caboodle.dbo.ProviderDim prd 
# 						ON ef.ProviderDurableKey = prd.DurableKey 
# 				WHERE fv.Count > 0 
# 					AND fv.FlowsheetRowKey = 51011                   
#   ") |>
#   filter(ProviderEpicID %in% uteprovs) |>
#   group_by(PatientDurableKey, DateKey) |>
#   filter(TakenInstant == max(TakenInstant)) |>
#   filter(FirstDocumentedInstant == max(FirstDocumentedInstant)) |>
#   ungroup() |>
#   inner_join(etx1a |> distinct(PatientDurableKey))
# 
# etx1b3 <- dbGetQuery(con, "
#   SELECT DISTINCT sv.PatientDurableKey
#     							,sv.EncounterKey
#     							,sv.NumericResponse
#     							,sv.ResponseDateKey
#     							,sv.ResponseTimeKey
#     							,prd.ProviderEpicID
# 				FROM caboodle.dbo.SurveyAnswerFact sv 
# 					JOIN caboodle.dbo.EncounterFact ef 
# 						ON sv.EncounterKey = ef.EncounterKey
# 					JOIN caboodle.dbo.ProviderDim prd 
# 						ON ef.ProviderDurableKey = prd.DurableKey 
# 				where sv.SurveyQuestionKey = 24552 
# 					AND sv.Valid = 1 
# 					AND sv.Count > 0                   
#  ") |>
#   filter(ProviderEpicID %in% uteprovs) |>
#   group_by(PatientDurableKey, ResponseDateKey) |>
#   filter(ResponseTimeKey == max(ResponseTimeKey)) |>
#   filter(EncounterKey == max(EncounterKey)) |>
#   ungroup() |>
#   inner_join(etx1a |> distinct(PatientDurableKey))
# 
# etx1b <- etx1a |>
#   left_join(etx1b2, join_by(PatientDurableKey, DateKey)) |>
#   left_join(etx1b3, join_by(PatientDurableKey, DateKey == ResponseDateKey)) |>
#   mutate(TrueBOS = coalesce(NumericValue, NumericResponse)) |>
#   rename(
#     ProviderBOS = NumericValue,
#     PatientBOS = NumericResponse
#   ) |>
#   select(PatientDurableKey:ProviderEpicID.x, ProviderBOS, PatientBOS, TrueBOS) |> 
#   rename(
#     EncounterKey = EncounterKey.x,
#     TakenInstant = TakenInstant.x,
#     ProviderEpicID = ProviderEpicID.x
#   )
# 
# visit1a <- dbGetQuery(con, "
#   SELECT DISTINCT ef.PatientDurableKey
# 				,PrimaryMRN
# 				,ef.EncounterEpicCSN
# 				,ef.EncounterKey
# 				,SUM(BillingProcedureQuantity) AS CPTQty
# 				,ef.ProviderDurableKey
# 				,btf.BillingProcedureCode AS CPTCode
# 				,prvd.ProviderEpicID AS ProviderEpicID
# 				,prvd.Name AS ProviderName
# 				,dad.DateValue AS ProcDate
# 				,CASE WHEN btf.BillingProcedureCode = '90791' THEN 1 ELSE 0 END AS trtstfl
# 	FROM  caboodle.dbo.EncounterFact ef
# 		JOIN caboodle.dbo.BillingTransactionFact btf 
# 			ON btf.EncounterKey = ef.EncounterKey 
# 		JOIN caboodle.dbo.PatientDim pd 
# 			ON pd.DurableKey = ef.PatientDurableKey
# 		JOIN caboodle.dbo.DateDim dad 
# 			ON ef.DateKey = dad.DateKey 
# 		JOIN caboodle.dbo.ProviderDim prvd 
# 			ON ef.ProviderDurableKey = prvd.DurableKey 
# 	WHERE btf.BillingProcedureCode IN (
# 	    '90791', '90832', '90834', '90837', 
# 	    '90846', '90847', '90839', '90840'
# 	    )
# 		AND prvd.StartDate <= dad.DateValue 
# 		AND prvd.EndDate >= dad.DateValue
# 		AND IsInactive = 0
# 		AND ReportingTransactionType = 'charge'
# 		AND ef.EncounterKey > 0
# 		AND pd.IsCurrent = 1
# 		AND dad.DateValue >= '10/1/22'
# 	GROUP BY ef.PatientDurableKey
# 			,PrimaryMRN
# 			,ef.EncounterEpicCSN
# 			,ef.EncounterKey
# 			,ef.ProviderDurableKey
# 			,btf.BillingProcedureCode
# 			,prvd.ProviderEpicID 
# 			,prvd.Name 
# 			,dad.DateValue
# 			,CASE WHEN btf.BillingProcedureCode = '90791' THEN 1 ELSE 0 END                    
#    ") |>
#   filter(
#     ProviderEpicID %in% uteprovs,
#     CPTQty > 0
#     )
# 
# visit1b1 <- dbGetQuery(con, "
#   SELECT DISTINCT sv.PatientDurableKey
# 									,sv.NumericResponse AS PatientBOS
# 									,dd1.DateValue AS BOS1Date
# 									,sv.ResponseTimeKey
# 									,prd.ProviderEpicID AS BOS1Prov
# 		FROM caboodle.dbo.SurveyAnswerFact sv 
# 			JOIN caboodle.dbo.DateDim dd1 
# 				ON sv.EncounterDateKey = dd1.DateKey
# 			JOIN caboodle.dbo.EncounterFact ef 
# 				ON sv.EncounterKey = ef.EncounterKey
# 			JOIN caboodle.dbo.ProviderDim prd 
# 				ON ef.ProviderDurableKey = prd.durablekey 
# 		WHERE sv.SurveyQuestionKey = 24552 
# 			AND sv.Valid = 1 
# 			AND sv.Count > 0                     
#   ") |>
#   filter(
#     BOS1Prov %in% uteprovs,
#     PatientDurableKey %in% visit1a$PatientDurableKey
#     ) |>
#   group_by(PatientDurableKey, BOS1Date) |>
#   filter(ResponseTimeKey == max(ResponseTimeKey)) |>
#   ungroup()
# 
# visit1b2 <- dbGetQuery(con, "
#   SELECT DISTINCT fv.PatientDurableKey
# 										,fv.NumericValue AS ProviderBOS
# 										,dd1.DateValue AS BOS2Date
# 										,prd.ProviderEpicID AS BOS2Prov
# 										,fv.FirstDocumentedInstant AS pbosinst
# 							FROM caboodle.dbo.FlowsheetValueFact fv 
# 								JOIN caboodle.dbo.DateDim dd1 
# 									ON fv.DateKey = dd1.DateKey
# 								JOIN caboodle.dbo.EncounterFact ef 
# 									ON fv.EncounterKey = ef.EncounterKey
# 								JOIN caboodle.dbo.ProviderDim prd 
# 									ON ef.ProviderDurableKey = prd.DurableKey 
# 							WHERE fv.Count > 0 
# 								AND fv.FlowsheetRowKey = 51011                     
#   ") |>
#   filter(
#     BOS2Prov %in% uteprovs,
#     PatientDurableKey %in% visit1a$PatientDurableKey
#     ) |>
#   group_by(PatientDurableKey, BOS2Date) |>
#   filter(pbosinst == max(pbosinst)) |>
#   mutate(lines = n()) |>
#   arrange(PatientDurableKey, BOS2Date, ProviderBOS) |>
#   mutate(rn = row_number()) |>
#   filter(rn == 1) |>
#   ungroup() |>
#   select(-rn)
# 
# visit1b <- left_join(
#   visit1a, 
#   visit1b1, 
#   join_by(PatientDurableKey, ProcDate == BOS1Date)
#   ) |>
#   rename(PatientBOS_Provider = BOS1Prov) |>
#   left_join(visit1b2, join_by(PatientDurableKey, ProcDate == BOS2Date)) |>
#   rename(ProviderBOS_Provider = BOS2Prov) |>
#   group_by(PatientDurableKey, ProcDate) |>
#   filter(CPTCode == max(CPTCode)) |>
#   ungroup() |>
#   mutate(
#     TrueBOS = coalesce(ProviderBOS, PatientBOS),
#     TrueBOS_Prov = ifelse(!is.na(ProviderBOS), ProviderBOS_Provider, PatientBOS_Provider)
#   )
#   
# visit1c <- etx1b |>
#   select(PatientDurableKey, Value, adt = FlowDate, ProviderEpicID, TrueBOS) |>
#   mutate(
#     CPTCode = "",
#     TrueBOS_Prov = ProviderEpicID,
#     trtstfl = 0,
#     trtstfl = 0,
#     trtedfl = 1,
#     source = 2
#   ) |>
#   rbind(
#     select(
#       visit1b, 
#       PatientDurableKey, 
#       CPTCode, 
#       adt = ProcDate, 
#       ProviderEpicID, 
#       TrueBOS, 
#       trtstfl
#       ) |>
#       mutate(
#         Value = "",
#         TrueBOS_Prov = ProviderEpicID,
#         trtedfl = 0,
#         source = 1
#       )
#   ) |>
#   arrange(PatientDurableKey, adt, source) |>
#   group_by(PatientDurableKey, adt) |>
#   mutate(prvrn = row_number()) |>
#   ungroup()
# 
# visit2a <- visit1c |>
#   mutate(TrueBOS = coalesce(TrueBOS, -1)) |>
#   group_by(PatientDurableKey, adt) |>
#   reframe(
#     TrueBOS = max(TrueBOS),
#     Value = max(Value),
#     CPTCode = max(CPTCode),
#     trtstfl = max(trtstfl),
#     trtedfl = max(trtedfl)
#   )
# 
# visit2b <- filter(visit1c, prvrn == 1) |>
#   distinct(PatientDurableKey, adt, ProviderEpicID)
# 
# visit2 <- left_join(visit2a, visit2b) |>
#   arrange(PatientDurableKey, adt) |>
#   group_by(PatientDurableKey) |>
#   mutate(
#     CheckLastEnd = lag(trtedfl, 1),
#     TrueBOS = ifelse(TrueBOS == -1, NA, TrueBOS),
#     trtstfl = ifelse(trtstfl == 0 & trtedfl == 0 & CheckLastEnd == 1, 1.1, trtstfl)
#     ) |>
#   ungroup()
# 
# starts <- filter(visit2, trtstfl > 0) |>
#   select(PatientDurableKey, trtsdt = adt, StartProv = ProviderEpicID, trtstfl) |>
#   unique()
# 
# ends <- filter(visit2, trtedfl == 1) |>
#   select(PatientDurableKey, trtedt = adt, EndProv = ProviderEpicID, trtedfl)
# 
# course0 <- left_join(
#   starts, 
#   ends, 
#   join_by(PatientDurableKey, trtsdt < trtedt)
#   ) 
#   filter(trtsdt < trtedt | is.na(trtedt)) |>
#   arrange(PatientDurableKey, trtsdt)
#   
# course1 <- course0 |>
#   filter(!is.na(trtedt)) |>
#   group_by(PatientDurableKey, trtedt) |>
#   filter(trtsdt == max(trtsdt)) |>
#   rbind(filter(course0, is.na(trtedt))) |>
#   arrange(PatientDurableKey, trtsdt, trtedt) |>
#   group_by(PatientDurableKey, trtsdt) |>
#   mutate(
#     NextEnd = lead(trtedt, 1),
#     LastEnd = lag(trtedt, 1),
#     srn = row_number(),
#     FirstEnd = trtedt[srn == 1],
#     NewStart = FirstEnd + 90,
#     fetxgap = as.numeric(trtedt - FirstEnd),
#     netxgap = as.numeric(NextEnd - trtedt),
#     letxgap = as.numeric(trtedt - LastEnd)
#   ) |>
#   ungroup()
#   
# course2a <- course1 |>
#   filter(
#     !is.na(fetxgap),
#     fetxgap < 90
#   ) |>
#   group_by(PatientDurableKey, trtsdt) |>
#   filter(srn == max(srn)) |>
#   select(PatientDurableKey, trtsdt, StartProv, trtedt, EndProv, trtstfl, trtedfl)
# 
# course2b <- course1 |>
#   filter(
#     !is.na(fetxgap),
#     fetxgap >= 90
#     ) |>
#   mutate(
#     trtsdt = case_when(
#       srn > 2 & letxgap >= 90 ~ LastEnd + 1,
#       TRUE ~ NewStart
#       ),
#     trtedt = case_when(
#       netxgap < 90 ~ NextEnd,
#       TRUE ~ trtedt
#     ),
#     trtstfl = 1.2
#   ) |>
#   select(PatientDurableKey, trtsdt, trtedt, EndProv, trtstfl, trtedfl, StartProv)
# 
# course2c <- filter(course1, is.na(FirstEnd)) |>
#   arrange(PatientDurableKey, trtsdt) |>
#   group_by(PatientDurableKey) |>
#   mutate(NextStart = lead(trtsdt, 1)) |>
#   ungroup() |>
#   mutate(
#     trtedt = case_when(
#       !is.na(NextStart) ~ NextStart - 1,
#       TRUE ~ trtedt
#     ),
#     trtedfl = 1.2
#   ) |>
#   select(PatientDurableKey, trtsdt, StartProv, trtedt, EndProv, trtstfl, trtedfl)
# 
# course2 <- rbind(course2a, course2b) |>
#   rbind(course2c) |>
#   filter(is.na(EndProv) | EndProv == StartProv)
# 
# out1 <- select(visit2, PatientDurableKey, adt, CPTCode) |>
#   inner_join(course2, relationship = "many-to-many") |>
#   filter(
#     adt >= trtsdt,
#     (adt <= trtedt | is.na(trtedt)),
#     !is.na(CPTCode),
#     CPTCode != ""
#   ) |>
#   group_by(PatientDurableKey, trtsdt, trtedt, StartProv, EndProv, trtstfl, trtedfl) |>
#   reframe(
#     VisitCount = length(unique(adt)),
#     LastSeen = max(adt)
#   ) |>
#   filter(
#     (
#       (!is.na(trtedt) & trtedt >= "2022-10-01") |
#         (is.na(trtedt) & VisitCount >= 6 & LastSeen >= "2021-10-01")
#      ),
#     !(is.na(trtedt) & LastSeen + 90 >= today())
#   ) |>
#   mutate(
#     LTFUDate = case_when(
#       is.na(trtedt) ~ LastSeen + 90,
#       TRUE ~ NA
#     ),
#     Month = case_when(
#       !is.na(trtedt) ~ floor_date(trtedt, "month"),
#       LTFUDate <= "2022-10-01" ~ as.Date("2022-10-01"),
#       TRUE ~ ceiling_date(LTFUDate, "month") + 1
#     ),
#     LostToFollowUp = ifelse(is.na(trtedt) & !is.na(LTFUDate), "Y", "N"),
#     trtedt = case_when(
#       is.na(trtedt) ~ LTFUDate,
#       TRUE ~ trtedt
#     ),
#   ) |>
#   select(
#     Month, 
#     PatientDurableKey, 
#     trtsdt, 
#     trtedt, 
#     DateLastSeen = LastSeen, 
#     NumberOfSessions = VisitCount, 
#     LostToFollowUp, 
#     trtstfl, 
#     trtedfl
#     ) |>
#   arrange(PatientDurableKey, Month)
# 
# out2a <- inner_join(
#   out1,
#   select(visit2, PatientDurableKey, adt, TreatmentEndBOS = TrueBOS),
#   join_by(PatientDurableKey, trtedt == adt)
# ) |>
#   filter(
#     LostToFollowUp == "N", 
#     !is.na(TreatmentEndBOS)
#     )
# 
# out2b <- inner_join(
#   out1,
#   select(visit2, PatientDurableKey, adt, TreatmentEndBOS = TrueBOS),
#   join_by(PatientDurableKey, DateLastSeen == adt)
# ) |>
#   filter(
#     LostToFollowUp == "Y", 
#     !is.na(TreatmentEndBOS)
#   )
# 
# out2c <- inner_join(
#   filter(visit2, !is.na(TrueBOS)) |> select(PatientDurableKey, TrueBOS, adt),
#   out1,
#   join_by(PatientDurableKey, adt >= trtsdt, adt <= trtedt)
# ) |>
#   group_by(PatientDurableKey, trtsdt, trtedt) |>
#   filter(adt == min(adt)) |>
#   ungroup() |>
#   select(
#     PatientDurableKey, 
#     trtsdt, 
#     trtedt, 
#     FirstBOS = TrueBOS,
#     FirstBOSDate = adt
#     )
# 
# out2 <- rbind(out2a, out2b) |>
#   left_join(out2c) |>
#   filter(Month < floor_date(today(), "month")) |>
#   mutate(
#     Numerator = case_when(
#       TreatmentEndBOS <= 9 ~ 1,
#       FirstBOS != 0 & (TreatmentEndBOS - FirstBOS) / FirstBOS <= -0.4 ~ 1,
#       TRUE ~ 0
#     )
#   )
