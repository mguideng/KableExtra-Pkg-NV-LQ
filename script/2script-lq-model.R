#-----------------------------------------------------------------#
#                      kableExtra-Pkg-NV-LQ                       #
#-----------------------------------------------------------------#
# Script purpose:                                                 #
#   Models the Location Quotients into a table layout             #
#-----------------------------------------------------------------#


#### Import Raw Data ####
urlfile <- 'https://raw.githubusercontent.com/mguideng/KableExtra-Pkg-NV-LQ/master/data/dfraw.csv'
df <- read.csv(urlfile)


#### Clean Dataframe ####
colnames(df)

# Keep these 6 columns & rename
  # 1. area_title = `Area`
  # 2. agglvl_code =`Level`
  # 3. industry_code = `NAICS`
  # 4. industry_title = `Industry`
  # 5. own_title = `Ownership`
  # 6. annual_avg_emplvl = `Empl`

df <- df[c("area_title", "agglvl_code", "industry_code", "industry_title", "own_title", "annual_avg_emplvl")]
colnames(df) = c("Area", "Level", "NAICS", "Industry", "Ownership", "Empl")

# Variables list
  # Area = Either Las Vegas MSA, Nevada or US Total.
  # Level = Indicates how the comprehensive data is summarized according to NAICS levels.
    # Of interest here are levels:
      # 14 (National, by NAICS Sector level)
      # 74 (County, NAICS Sector level)
  # NAICS = numbering system to classify business industry based on economic activities.
    # Ranges from two to six-digits where a five or six-digit code is the most detailed. 
      # First two digits - Sector
      # The third digit - Subsector 
      # The fourth digit - Industry group
      # The fifth digit - NAICS industries
      # The sixth digit - national industries.
  # Industry - Title designated to a corresponding NAICS code (e.g., NAICS 22 is the
    # "Utilities" industry.
  # Ownership - whether owned by a government or private entity.
  # Employ - Count of employment (jobs)

# Change class modes
sapply(df, class)

df$Empl <- as.numeric(df$Empl)
df$Level <- as.integer(df$Level)
df$NAICS <- as.factor(df$NAICS)

cols.char <- c("Area", "Industry", "Ownership")
df[cols.char] <- sapply(df[cols.char],as.character)


# ---- Clean Variables in df ----

# Rename in `Area`
df$Area <- sub("LV County, Nevada", "LV", df$Area)
df$Area <- sub("U.S. TOTAL", "US", df$Area)

# Subset `Level` to Sector level (14 LV, 74 US) and `Ownership` to private
df <- subset(df, Level=="14" | Level=="74")
df <- subset(df, Ownership=="Private")
df$Level <- NULL
df$Ownership <- NULL

# Clean up `Industry` and rename
library(stringr)
df$Industry <- str_replace_all(df$Industry, "\\d |\\d", "")  # Remove digits
df$Industry <- gsub("NAICS ", "", df$Industry)               # Remove NAICS wording
df$Industry <- gsub("-", "", df$Industry)                    # Remove hypens

(head(df[,"Industry"],20))
# Note: There are 20 industries at the NAICS 2-digit level.

name.cng <- data.frame(orig = c("Agriculture, forestry, fishing and hunting", "Mining, quarrying, and oil and gas extraction", "Utilities", "Construction", "Manufacturing", "Wholesale trade", "Retail trade", "Transportation and warehousing", "Information", "Finance and insurance", "Real estate and rental and leasing", "Professional and technical services", "Management of companies and enterprises", "Administrative and waste services", "Educational services", "Health care and social assistance", "Arts, entertainment, and recreation", "Accommodation and food services", "Other services, except public administration", "Unclassified"),
                       changed = c("Forestry", "Mining", "Utilities", "Construction", "Manufacturing", "Wholesale", "Retail", "Trans/Ware", "Information", "Finance", "RE", "Professional", "Management", "Admin", "Educational", "Health", "Arts", "Accommodation", "Other", "Industries not classified") )

for (i in 1:NROW(name.cng)){ 
  df$Industry <- sub(name.cng[i,1], name.cng[i,2], df$Industry) }

# Remove row names
rownames(df) <- c()

# The df is ready


#### Create LQ Model ####

# Spread employment data by area and rename
library(tidyr)

lqmod <- df %>%
  spread(Area, Empl)

colnames(lqmod)[3] <- "LV.Empl"
colnames(lqmod)[4] <- "US.Empl"

# Add employment distrib percentages
lqmod$LV.Percent <- lqmod$LV.Empl/sum(lqmod$LV.Empl)
lqmod$US.Percent <- lqmod$US.Empl/sum(lqmod$US.Empl)

# Add local employment requirements columns, by year
lqmod$LV.EmplRqmts <- round(lqmod$US.Percent*sum(lqmod$LV.Empl), digits=0)

# Add local LQ columns
lqmod$LV.LQ <- as.numeric(as.character(round((lqmod$LV.Percent/lqmod$US.Percent), digits=2)))

# Add local LQ Classification
lqmod$LV.LQClass <- ifelse(lqmod$LV.LQ>=1.2,"Export", "")

# Reconvert local LQ class for formatting purposes
lqmod$LV.LQ <- as.factor(round((lqmod$LV.Percent/lqmod$US.Percent), digits=2))

# Add local Excess Empl (to export) or Deficit
lqmod$LV.ExcessEmpl <- lqmod$LV.Empl - lqmod$LV.EmplRqmts

# Rename and reorder columns
colnames(lqmod)
lqmod <- lqmod[,c(1,2,4,6,3,5,7,8,9,10)]


#### Export ####
getwd()
#setwd("C:/.../allfiles")
write.csv(lqmod,"lqmod.csv", row.names = F)

