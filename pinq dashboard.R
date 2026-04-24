library(tidyverse)
library(lubridate)
library(readxl)
library(odbc)

con <- dbConnect(odbc(), "CaboodleProd")

bmcpprov <- c(
  '15643', '3664', '5009553', '5010324', '15635', '5014355', '3391', '5020174', 
  '2912', '13644', '5002955', '5004188',	'15882', '5005608', '5007237', 
  '5014593', '5013828', '5017851', '425252', '774317', '5027536'
  )

ibhprov <- c("5017437", "5021489", "445821", "5016609", 
             "3644", "5016177", "5002378", "5002965")

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
  }else if(practice %in% c("Catalyst", "Community BH")){
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
    Site = practice,
    Measure = c(
      "Initial Intake",
      "Crisis Stabilization",
      "PRO BOS",
      "Provider Turnover"
    ),
    Month = reportmonth,
    Numerator = numerators,
    Denominator = denominators,
    Network = ifelse(practice == "Community IBH", "IBH", "PINQ BH")
  )
}

aeu <- read_excel(
  "old data.xlsx",
  sheet = "All except Utilization",
  col_names = c("Site", "Measure", "Month", "Numerator", "Denominator", "Network"),
  skip = 1
)

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
  mutate(Site = ifelse(Site == "CHNK", "OneQuest", Site))

alldata <- anti_join(aeu, newdata, join_by(Site, Measure, Month)) |>
  rbind(newdata)

denoms <- filter(alldata, Measure == "Initial Intake") |>
  select(Site, Month, Denominator)

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
  filter(ProviderEpicID %in% bmcpprov) |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == max(TakenInstant))

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
  filter(ProviderEpicID %in% bmcpprov) |>
  inner_join(etx1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == max(TakenInstant))

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
  filter(
    ProviderEpicID %in% bmcpprov,
    PatientDurableKey %in% etx1a$PatientDurableKey
    ) |>
  group_by(PatientDurableKey, ResponseDateKey) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup()

etx1b <- etx1a |>
  left_join(select(etx1b1, PatientDurableKey, DateKey, NumericValue)) |>
  rename(ProviderBOS = NumericValue) |>
  left_join(
    select(etx1b2, PatientDurableKey, ResponseDateKey, NumericResponse),
    join_by(PatientDurableKey, DateKey == ResponseDateKey)
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
  	'90846', '90847', '90839', '90840'
  	)
  		AND ReportingTransactionType = 'Charge'
  		AND IsInactive=0
  		AND ef.EncounterKey > 0
  		AND pd.IsCurrent=1
  		AND dad.DateValue >= '10/1/22'
  ") |>
  filter(ProviderEpicID %in% bmcpprov) |>
  group_by(
    PatientDurableKey,
    PrimaryMRN,
    EncounterEpicCSN,
    EncounterKey,
    ProviderDurableKey,
    cptcode,
    ProviderEpicID,
    ProviderName,
    ProcDate
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
  filter(BOS1Prov %in% bmcpprov) |>
  inner_join(visit1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, BOS1Date) |>
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
  filter(BOS2Prov %in% bmcpprov) |>
  inner_join(visit1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, BOS2Date) |>
  filter(pbosinst == max(pbosinst)) |>
  ungroup()

visit1b <- visit1a |>
  left_join(visit1b1, join_by(PatientDurableKey, ProcDate == BOS1Date)) |>
  rename(PatientBOS_Provider = BOS1Prov) |>
  left_join(visit1b2, join_by(PatientDurableKey, ProcDate == BOS2Date)) |>
  rename(ProviderBOS_Provider = BOS2Prov) |>
  arrange(
    PatientDurableKey,
    ProcDate,
    -EncounterKey,
    -ProviderBOS,
    -PatientBOS,
    -as.numeric(cptcode),
    -as.numeric(ProviderEpicID)
    ) |>
  group_by(PatientDurableKey, ProcDate) |>
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
  ungroup() |>
  select(
    PatientDurableKey,
    Value,
    adt = FlowDate,
    ProviderEpicID,
    TrueBOS
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
    trtstfl
  ) |>
  mutate(
    Value = "",
    trtedfl = 0,
    source = 1,
    TrueBOS_Provider = ProviderEpicID,
  )

visit1c <- rbind(visit1c1, visit1c2) |>
  arrange(PatientDurableKey, adt, source) |>
  group_by(PatientDurableKey, adt) |>
  mutate(prvrn = row_number()) |>
  ungroup()

visit2a <- visit1c |>
  group_by(PatientDurableKey, adt) |>
  reframe(
    TrueBOS = max(coalesce(TrueBOS, -1)),
    Value = max(Value),
    cptcode = max(cptcode),
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
    trtstfl = ifelse(
      trtstfl == 0 & trtedfl == 0 & CheckLastEnd == 1,
      1.1,
      trtstfl
      ),
    TrueBOS = ifelse(TrueBOS == -1, NA, TrueBOS)
    )

starts <- filter(visit2, trtstfl > 0) |>
  select(PatientDurableKey, trtsdt = adt, StartProv = ProviderEpicID, trtstfl) |>
  unique()

ends <- filter(visit2, trtedfl == 1) |>
  select(PatientDurableKey, trtedt = adt, EndProv = ProviderEpicID, trtedfl) |>
  unique() |>
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
  mutate(
    trtedt = case_when(
      trtedt == "2099-01-01" ~ NA,
      TRUE ~ trtedt
    )
  )

ern1 <- filter(course0, !is.na(trtedt)) |>
  group_by(PatientDurableKey, trtedt) |>
  filter(trtsdt == max(trtsdt))

course1 <- filter(course0, is.na(trtedt)) |>
  rbind(ern1) |>
  arrange(PatientDurableKey, trtsdt, trtedt) |>
  group_by(PatientDurableKey, trtsdt) |>
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
  group_by(PatientDurableKey, trtsdt) |>
  filter(srn == max(srn)) |>
  select(
    PatientDurableKey,
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
  arrange(PatientDurableKey, trtsdt) |>
  group_by(PatientDurableKey) |>
  mutate(NextStart = lead(trtsdt, 1)) |>
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
  select(visit2, PatientDurableKey, adt, cptcode),
  relationship = "many-to-many"
  ) |>
  filter(
    adt >= trtsdt,
    adt <= trtedt | is.na(trtedt),
    !is.na(cptcode),
    cptcode != ""
  ) |>
  group_by(PatientDurableKey, trtsdt, trtedt, StartProv, EndProv, trtstfl, trtedfl) |>
  reframe(
    VisitCount = length(unique(adt)),
    LastSeen = max(adt)
  ) |>
  mutate(
    LTFUDate = case_when(
      is.na(trtedt) ~ LastSeen + 90,
      TRUE ~ NA
    )
  ) |>
  filter(
    (!is.na(trtedt) & trtedt >= "2022-10-01") |
      (is.na(trtedt) & VisitCount >= 6 & LastSeen >= "2021-10-01"),
    !(is.na(trtedt) & (LastSeen + 90) >= today())
  ) |>
  mutate(
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
  select(visit2, PatientDurableKey, TrueBOS, adt) |>
    filter(!is.na(TrueBOS)),
  join_by(PatientDurableKey, trtedt == adt)
  ) |>
  rename(TreatmentEndBOS = TrueBOS)

out2b <- inner_join(
  filter(out1, LostToFollowUp == "Y"),
  filter(visit2, !is.na(TrueBOS)) |> select(PatientDurableKey, adt, TrueBOS),
  join_by(PatientDurableKey, DateLastSeen == adt)
) |>
  rename(TreatmentEndBOS = TrueBOS) |>
  rbind(out2a)

out2c <- out1 |>
  inner_join(
    select(visit2, PatientDurableKey, adt, TrueBOS) |> filter(!is.na(TrueBOS)),
    join_by(
      PatientDurableKey,
      trtsdt <= adt,
      trtedt >= adt
    ),
    relationship = "many-to-many") |>
  distinct(
    PatientDurableKey,
    trtsdt,
    trtedt,
    TrueBOS,
    adt
  ) |>
  arrange(PatientDurableKey, trtsdt, trtedt, adt) |>
  group_by(PatientDurableKey, trtsdt, trtedt) |>
  mutate(brn = row_number()) |>
  filter(brn == 1) |>
  rename(
    FirstBOS = TrueBOS,
    FirstBOSDate = adt
    )

bmcpprobos <- left_join(out2b, out2c, join_by(PatientDurableKey, trtsdt, trtedt)) |>
  filter(ReportMonth < floor_date(today(), "month")) |>
  mutate(
    Ratio = (TreatmentEndBOS - FirstBOS)/FirstBOS,
    NumeratorFlag = ifelse(
      TreatmentEndBOS <= 9 |
        (FirstBOS != 0 & Ratio <= -.4),
      1,
      0
    )
  ) |>
  select(
    ReportMonth,
    PatientDurableKey:trtedt,
    NumberOfSessions,
    LostToFollowUp:FirstBOSDate,
    NumeratorFlag
  ) |>
  mutate(PatientDurableKey = as.numeric(PatientDurableKey))

bmcpproboslines <- bmcpprobos |>
  group_by(ReportMonth) |>
  reframe(
    Numerator = sum(NumeratorFlag),
    Denominator = n()
    ) |>
  mutate(
    Site = "BMCP",
    Measure = "PRO BOS",
    Network = "PINQ BH"
  ) |>
  rename(Month = ReportMonth)

### IBH BOS

etx1a <- dbGetQuery(con, "
  SELECT DISTINCT a.PatientDurableKey
      						,a.EncounterKey
      						,a.FlowsheetValueKey
      						,a.Value
      						,a.DateKey
      						,a.TakenInstant
      						,dad.DateValue as FlowDate
      						,prd.DurableKey as ProviderDurableKey
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
  filter(ProviderEpicID %in% ibhprov) |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == max(TakenInstant)) |>
  ungroup()

etx1b1 <- dbGetQuery(con, "
  SELECT DISTINCT fv.PatientDurableKey
    							,fv.EncounterKey
    							,fv.NumericValue
    							,fv.DateKey
    							,FirstDocumentedInstant
    							,prd.ProviderEpicID
    							,fv.TakenInstant
		FROM caboodle.dbo.FlowsheetValueFact fv
		  JOIN caboodle.dbo.EncounterFact ef
			  ON fv.EncounterKey = ef.EncounterKey
			JOIN caboodle.dbo.ProviderDim prd
				ON ef.ProviderDurableKey = prd.DurableKey
		WHERE fv.Count > 0
		  AND fv.FlowsheetRowKey = 51011
  ") |>
  filter(ProviderEpicID %in% ibhprov) |>
  inner_join(etx1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, DateKey) |>
  filter(TakenInstant == TakenInstant) |>
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
				ON ef.ProviderDurableKey = prd.DurableKey
		WHERE sv.SurveyQuestionKey = 24552
			AND sv.Valid = 1
			AND sv.Count > 0
  ") |>
  filter(ProviderEpicID %in% ibhprov) |>
  inner_join(etx1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, ResponseDateKey) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup()

etx1b <- left_join(
  etx1a,
  select(etx1b1, NumericValue, PatientDurableKey, DateKey)
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

visit1aa <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,SUM(BillingProcedureQuantity) AS CPTQty
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS CPTCode
          				,prvd.ProviderEpicID
          				,prvd.Name AS ProviderName
          				,dad.DateValue AS ProcDate
          				,CASE WHEN BillingProcedureCode = '90791' THEN 1
          				  ELSE 0 END AS trtstfl
	FROM caboodle.dbo.EncounterFact ef
		JOIN caboodle.dbo.BillingTransactionFact btf
			ON btf.EncounterKey = ef.EncounterKey
		JOIN caboodle.dbo.PatientDim pd
			ON pd.DurableKey = ef.PatientDurableKey
		JOIN Caboodle.dbo.DateDim dad
			ON ef.DateKey = dad.DateKey
		JOIN caboodle.dbo.ProviderDim prvd
			ON ef.ProviderDurableKey = prvd.DurableKey
	WHERE prvd.StartDate <= dad.DateValue
		AND prvd.EndDate >= dad.DateValue
		AND btf.BillingProcedureCode IN  (
		                    '90791', '90832', '90834', '90837', 
		                    '90846', '90847', '90839', '90840'
		                      )
		AND ReportingTransactionType = 'charge'
		AND IsInactive = 0
		AND ef.EncounterKey > 0
		AND pd.IsCurrent = 1
		AND dad.DateValue >= '10/1/22'
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
  filter(ProviderEpicID %in% ibhprov)

visit1a <- filter(visit1aa, CPTQty > 0)

visit1b1 <- dbGetQuery(con, "
  SELECT sv.PatientDurableKey
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
		WHERE sv.SurveyQuestionKey = 24552
			AND sv.Valid = 1
			AND sv.Count > 0
  ") |>
  filter(BOS1Prov %in% ibhprov) |>
  inner_join(visit1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup() |>
  unique()

visit1b2 <- dbGetQuery(con, "
  SELECT fv.PatientDurableKey
				  ,fv.NumericValue AS ProviderBOS
					,dd1.DateValue as BOS2Date
					,prd.ProviderEpicID AS BOS2Prov
					,fv.FirstDocumentedInstant AS pbosinst
		FROM caboodle.dbo.FlowsheetValueFact fv
			JOIN caboodle.dbo.DateDim dd1
				ON fv.DateKey = dd1.DateKey
			JOIN caboodle.dbo.EncounterFact ef
				ON fv.EncounterKey = ef.EncounterKey
			JOIN caboodle.dbo.ProviderDim prd
				ON ef.ProviderDurableKey = prd.DurableKey
		WHERE fv.Count > 0
		  AND fv.FlowsheetRowKey = 51011
    ") |>
  filter(BOS2Prov %in% ibhprov) |>
  inner_join(visit1a |> distinct(PatientDurableKey)) |>
  group_by(PatientDurableKey, BOS2Date) |>
  filter(pbosinst == max(pbosinst)) |>
  ungroup() |>
  unique()

visit1b <- visit1a |>
  left_join(visit1b1, join_by(PatientDurableKey, ProcDate == BOS1Date)) |>
  left_join(visit1b2, join_by(PatientDurableKey, ProcDate == BOS2Date)) |>
  rename(
    PatientBOS_Provider = BOS1Prov,
    ProviderBOS_Provider  = BOS2Prov
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
  filter(zrn == 1) |>
  ungroup() |>
  mutate(
    TrueBOS = coalesce(ProviderBOS, PatientBOS),
    TrueBOS_Prov = case_when(
      !is.na(ProviderBOS) ~ ProviderBOS_Provider,
      !is.na(PatientBOS) ~ PatientBOS_Provider
    )
  )

visit1c <- visit1b |>
  mutate(
    Value = "",
    trtedfl = 0,
    Source = 1,
    TrueBOS_Prov = ProviderEpicID
  ) |>
  rename(adt = ProcDate) |>
  select(
    PatientDurableKey,
    CPTCode,
    ProviderEpicID,
    adt,
    trtstfl,
    TrueBOS:Source
  ) |>
  rbind(
    etx1b |>
      mutate(
        CPTCode = "",
        trtstfl = 0,
        trtedfl = 1,
        Source = 2,
        TrueBOS_Prov = ProviderEpicID
      ) |>
      rename(adt = FlowDate) |>
      select(
        PatientDurableKey,
        Value,
        adt,
        ProviderEpicID,
        TrueBOS:TrueBOS_Prov
        )
  ) |>
  arrange(PatientDurableKey, adt, Source) |>
  group_by(PatientDurableKey, adt) |>
  mutate(prvrn = row_number()) |>
  ungroup()

visit2 <- visit1c |>
  mutate(TrueBOS = coalesce(TrueBOS, -1)) |>
  group_by(PatientDurableKey, adt) |>
  reframe(
    TrueBOS = max(TrueBOS),
    Value = max(Value),
    CPTCode = max(CPTCode),
    trtstfl = max(trtstfl),
    trtedfl = max(trtedfl)
  ) |>
  left_join(
    filter(visit1c, prvrn == 1) |>
      select(PatientDurableKey, adt, ProviderEpicID)
    ) |>
  arrange(PatientDurableKey, adt) |>
  group_by(PatientDurableKey) |>
  mutate(CheckLastEnd = lag(trtedfl, 1)) |>
  ungroup() |>
  mutate(
    TrueBOS = ifelse(TrueBOS == -1, NA, TrueBOS),
    trtstfl = ifelse(
      trtstfl == 0 & trtedfl == 0 & CheckLastEnd == 1,
      1.1,
      trtstfl
      )
    )

starts <- filter(visit2, trtstfl > 0) |>
  distinct(PatientDurableKey, adt, ProviderEpicID, trtstfl) |>
  rename(trtsdt = adt, StartProv = ProviderEpicID)

ends <- filter(visit2, trtedfl == 1) |>
  distinct(PatientDurableKey, adt, ProviderEpicID, trtedfl) |>
  rename(
    trtedt = adt,
    EndProv = ProviderEpicID
  ) |>
  mutate(trtedt = coalesce(trtedt, as.Date("2099-01-01")))

course0 <- left_join(starts, ends, join_by(PatientDurableKey, trtsdt < trtedt))

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
  select(PatientDurableKey:trtedfl)

course2b <- course1 |>
  filter(
    !is.na(fetxgap),
    fetxgap > 90
  ) |>
  mutate(
    trtsdt = case_when(
      srn > 2 & letxgap >= 90 ~ LastEnd + 1,
      TRUE ~ NewStart
    ),
    trtedt = case_when(
      netxgap < 90 ~ NextEnd,
      TRUE ~ trtedt
    ),
    trtstfl = 1.2
  ) |>
  select(PatientDurableKey:trtedfl) |>
  unique()

course2 <- filter(course1, is.na(FirstEnd)) |>
  select(PatientDurableKey:trtedfl) |>
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
  select(-NextStart) |>
  rbind(course2a) |>
  rbind(course2b) |>
  filter(StartProv == EndProv | is.na(EndProv))

out1 <- course2 |>
  inner_join(
    select(visit2, PatientDurableKey, adt, CPTCode),
    relationship = "many-to-many"
    ) |>
  filter(
    adt >= trtsdt,
    (adt <= trtedt | is.na(trtedt)),
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
      is.na(trtedt) ~ LastSeen + 90
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
  )

out2a <- filter(out1, LostToFollowUp == "N") |>
  inner_join(
    select(visit2, TrueBOS, adt, PatientDurableKey) |>
      filter(!is.na(TrueBOS)),
    join_by(PatientDurableKey, trtedt == adt)
    ) |>
  rename(TreatmentEndBOS = TrueBOS)

out2b <- filter(out1, LostToFollowUp == "Y") |>
  inner_join(
    select(visit2, TrueBOS, adt, PatientDurableKey) |>
      filter(!is.na(TrueBOS)),
    join_by(PatientDurableKey, DateLastSeen == adt)
    ) |>
  rename(TreatmentEndBOS = TrueBOS)

out2c <- out1 |>
  inner_join(
    select(visit2, PatientDurableKey, adt, TrueBOS) |>
      filter(!is.na(TrueBOS)),
    join_by(PatientDurableKey, trtsdt <= adt, trtedt >= adt)
    ) |>
  group_by(PatientDurableKey, trtsdt, trtedt) |>
  filter(adt == min(adt)) |>
  select(
    PatientDurableKey,
    trtsdt,
    trtedt,
    FirstBOS = TrueBOS,
    FirstBOSDate = adt
    )

ibhbos <- rbind(out2a, out2b) |>
  left_join(out2c) |>
  filter(ReportMonth <= floor_date(today(), "month")) |>
  mutate(
    NumeratorFlag = case_when(
      TreatmentEndBOS <= 9 ~ 1,
      FirstBOS != 0 & ((TreatmentEndBOS - FirstBOS) / FirstBOS) <= -.4 ~ 1,
      TRUE ~ 0
    )
  ) |>
  distinct(
    ReportMonth,
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
  filter(ReportMonth < floor_date(today(), "month"))

ibhboslines <- ibhbos |>
  group_by(ReportMonth) |>
  reframe(
    Denominator = n(),
    Numerator = sum(NumeratorFlag)
  ) |>
  mutate(
    Site = "Community IBH",
    Network = "IBH",
    Measure = "PRO BOS"
    ) |>
  rename(Month = ReportMonth)

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
				AND (CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1 WHEN ac.npi <> '1518017102' THEN 1 ELSE 0 END =1)
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
  filter(zrn == 1) |>
  mutate(
    TrueBOS = coalesce(ProviderBOS, PatientBOS),
    TrueBOS_Prov = case_when(
      !is.na(ProviderBOS) ~ ProviderBOS_Provider,
      !is.na(PatientBOS) ~ PatientBOS_Provider
    )
  ) |>
  ungroup()

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
    )

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
  )

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
    ReportMonth = case_when(
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
  filter(ReportMonth <= floor_date(today(), "month"))

psychbos <- out2 |>
  mutate(
    NumeratorFlag = case_when(
      TreatmentEndBOS <= 9 ~ 1,
      FirstBOS != 0 & (TreatmentEndBOS - FirstBOS) / FirstBOS <= -0.4 ~ 1,
      TRUE ~ 0
    )
  ) |>
  select(
    ReportMonth,
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
  filter(ReportMonth < floor_date(today(), "month")) |>
  arrange(ReportMonth, PatientDurableKey, trtsdt, trtedt)

psychboslines <- psychbos |>
  group_by(ReportMonth) |>
  reframe(
    Denominator = n(),
    Numerator = sum(NumeratorFlag)
  ) |>
  mutate(
    Site = "Psychiatry",
    Network = "PINQ BH",
    Measure = "PRO BOS"
  ) |>
  rename(Month = ReportMonth)

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
    FROM caboodle.dbo.EncounterFact ef
  		JOIN caboodle.dbo.BillingTransactionFact btf
  			ON btf.EncounterKey = ef.EncounterKey
  		JOIN caboodle.dbo.PatientDim pd
  			ON pd.DurableKey = ef.PatientDurableKey
  		JOIN caboodle.dbo.DateDim dad
  			ON ef.DateKey = dad.DateKey
  		JOIN caboodle.dbo.ProviderDim prvd
  			ON ef.ProviderDurableKey = prvd.DurableKey
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
    ") |>
  filter(ProviderEpicID %in% bmcpprov) |>
  mutate(ReportMonth = floor_date(EncDate, "month"))

bmcpii <- intake2 |>
  filter(
    ReportMonth >= "2022-10-01",
    CPTQty > 0
  ) |>
  group_by(ReportMonth) |> 
  reframe(Intake = length(unique(EncounterEpicCSN))) |>
  mutate(
    Site = "BMCP",
    Measure = "Initial Intake",
    Network = "PINQ BH"
  ) |>
  rename(
    Month = ReportMonth,
    Numerator = Intake
    ) |>
  inner_join(denoms) |>
  unique()

intake2 <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          	  		,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,SUM(BillingProcedureQuantity) AS CPTQty
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS CPTCode
          				,prvd.ProviderEpicID AS ProviderEpicID
          				,prvd.Name AS ProviderName
          				,dad.DateValue AS EncDate
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
			WHEN SBFlag = 0 THEN 1
			ELSE 0 END = 1
				)
		AND (
		  CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1
				WHEN ac.npi <> '1518017102' THEN 1
				ELSE 0 END = 1
				)
		AND prvd.StartDate <= dad.DateValue
		AND prvd.EndDate >= dad.DateValue
		AND btf.BillingProcedureCode = '90791'
		AND ReportingTransactionType = 'charge'
		AND IsInactive = 0
		AND ef.EncounterKey > 0
		AND pd.IsCurrent = 1
		AND dad.DateValue >= '10/1/22'
		AND VisitTypeKey NOT IN ('1536','3113','3670','4246','6024','6749','7333')
	GROUP BY ef.patientdurablekey
    			,PrimaryMRN
    			,ef.EncounterEpicCSN
    			,ef.EncounterKey
    			,ef.ProviderDurableKey
    			,btf.BillingProcedureCode
    			,prvd.ProviderEpicID
    			,prvd.Name
    			,dad.DateValue
    ") |>
  mutate(ReportMonth = floor_date(EncDate, "month"))

psychii <- intake2 |>
  filter(
    ReportMonth >= "2022-10-01",
    CPTQty > 0
  ) |>
  group_by(ReportMonth) |>
  reframe(Numerator = length(unique(EncounterEpicCSN))) |>
  mutate(
    Site = "Psychiatry",
    Measure = "Initial Intake",
    Network = "PINQ BH"
    ) |>
  rename(Month = ReportMonth) |>
  inner_join(denoms) |>
  unique()

patientbos <- dbGetQuery(con, "
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
		WHERE sv.SurveyQuestionKey = 24552
			AND sv.Valid = 1
			AND sv.count > 0
  ") |>
  filter(BOS1Prov %in% bmcpprov) |>
  group_by(PatientDurableKey, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey))

firstbos <- patientbos |>
  group_by(PatientDurableKey) |>
  filter(BOS1Date == min(BOS1Date))

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
      FROM caboodle.dbo.EncounterFact ef
    		JOIN caboodle.dbo.BillingTransactionFact btf
    			ON btf.EncounterKey = ef.EncounterKey
    		JOIN caboodle.dbo.PatientDim pd
    			ON pd.DurableKey = ef.PatientDurableKey
    		JOIN caboodle.dbo.DateDim dad
    			ON ef.DateKey = dad.DateKey
    		JOIN caboodle.dbo.ProviderDim prvd
    			ON ef.ProviderDurableKey = prvd.DurableKey
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
    ") |>
  filter(ProviderEpicID %in% bmcpprov) |>
  mutate(ReportMonth = floor_date(EncDate, "month"))

bmcpcrisis <- inner_join(firstbos, crisis) |>
  filter(
    ReportMonth >= "2022-10-01",
    EncDate > BOS1Date
  ) |>
  group_by(ReportMonth) |>
  reframe(Crisis = length(unique(EncounterKey))) |>
  mutate(
    Site = "BMCP",
    Measure = "Crisis Stabilization",
    Network = "PINQ BH"
  ) |>
  rename(
    Month = ReportMonth,
    Numerator = Crisis
    ) |>
  inner_join(denoms) |>
  unique()

patientbos <- dbGetQuery(con, "
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
				ON  prd.ProviderEpicID = ac.ProviderEpicID
		WHERE sv.SurveyQuestionKey = 24552 
			AND sv.Valid = 1 
			AND sv.Count > 0
    ") |>
  group_by(PatientDurableKey, BOS1Date) |>
  filter(ResponseTimeKey == max(ResponseTimeKey)) |>
  ungroup()

firstbos <- patientbos |>
  group_by(PatientDurableKey) |>
  filter(BOS1Date == min(BOS1Date))

crisis <- dbGetQuery(con, "
  SELECT DISTINCT ef.PatientDurableKey
          				,PrimaryMRN
          				,ef.EncounterEpicCSN
          				,ef.EncounterKey
          				,SUM(BillingProcedureQuantity) AS CPTQty
          				,ef.ProviderDurableKey
          				,btf.BillingProcedureCode AS CPTCode
          				,prvd.ProviderEpicID AS ProviderEpicID
          				,prvd.Name AS ProviderName
          				,dad.DateValue AS EncDate
    FROM caboodle.dbo.encounterfact ef
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
  			WHEN SBFlag = 0 THEN 1 
  			ELSE 0 END = 1
  			)
		  AND (
			  CASE WHEN ac.npi = '1518017102' AND dad.DateValue < '3/9/24' THEN 1 
				  WHEN ac.npi <> '1518017102' THEN 1 ELSE 0 END = 1
				  )
    		AND prvd.StartDate <= dad.DateValue 
    		AND prvd.EndDate >= dad.DateValue
    		AND btf.BillingProcedureCode in ('90839','S9485') 
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
      ") |>
  mutate(ReportMonth = floor_date(EncDate, "month"))

psychcrisis <- inner_join(firstbos, crisis) |>
  filter(
    ReportMonth >= "2022-10-01",
    EncDate > BOS1Date
  ) |>
  group_by(ReportMonth) |>
  reframe(Crisis = length(unique(EncounterKey))) |>
  mutate(
    Site = "Psychiatry",
    Measure = "Crisis Stabilization",
    Network = "PINQ BH"
  ) |>
  rename(
    Month = ReportMonth,
    Numerator = Crisis
    ) |>
  inner_join(denoms) |>
  unique()

emrdata <- rbind(bmcpproboslines, ibhboslines) |>
  rbind(psychboslines) |>
  rbind(bmcpii) |>
  rbind(psychii) |>
  rbind(bmcpcrisis) |>
  rbind(psychcrisis)

alldata <- anti_join(alldata, emrdata, join_by(Site, Measure)) |>
  rbind(emrdata) |>
  mutate(
    MeasureLabel = case_when(
      Measure == "Crisis Stabilization" ~ "Rate of Crisis Stabilization Visits per FTE",
      Measure == "Initial Intake" ~ "Rate of New Visits per FTE",
      Measure == "PRO BOS" ~ "% of Patients Meeting Patient Reported Outcomes Goals",
      Measure == "Provider Turnover" ~ "Provider Turnover Rate per FTE"
    )
  )

setwd("~/Behavioral Health/PINQ/PINQ-Dashboard")

write_csv(alldata, "all except utilization.csv")

