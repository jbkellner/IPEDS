# IPEDS
R code for data transformation of IPEDS institutional characteristics and awards/degrees conferred for 2019-2023

Written by: Julie Kellner

Last modified: August 27, 2024

This R code will download the past 5 years of HD, IC and C_A datafiles from IPEDS
It will also download the frequency and varlist info from the dictionary for those years

The code then appends to the datafiles the plain language valuelabel
And combines all years together with a column for the year and original datafile

Output saved includes original downloaded files
And 5 new files for all 5 years, hd, ic, c_a, Frequencies, varlist

This code was tested and works on datafiles from 2019-2023

Original Data Source:
Institutional Characteristics All Years
https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&surveyNumber=1
Completions All Years
https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&surveyNumber=3

HD - Institutional characteristics directory info
IC - Institutional characteristics Educational offerings, organization, services and athletic associations
C_A - Awards/degrees conferred by program (6-digit CIP code), award level, race/ethnicity, and gender: July 1, prior year to June 30, current year
