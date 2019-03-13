# Numeric neighborhood numbers
#
# \code{nhood_num} returns the matching neighborhood name in string or factor format
#
# This is a function for converting a vector of City of St. Louis neighborhood numbers
# to either string or factor format.
#
# @usage nhood_num(var)
#
# @param var A string vector containing City of St. Louis neighborhood names
#
# @note Function is capable of handeling vectors that contain various spellings (i.e. "carondelet" or "Carondelet").
# It is also capable of handleing some common neighborhood abbreviations, such as "Bevo", "CWE", "FPSE", and "The Grove".
#
# @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#
# @export
gw_nhood_num <- function(var) {
  corrected <- stringr::str_to_title(var)

  nhood <- ifelse(corrected == "Carondelet", 1, NA)
  nhood <- ifelse(corrected == "Patch", 2, nhood)
  nhood <- ifelse(corrected == "Holly Hills", 3, nhood)
  nhood <- ifelse(corrected == "Boulevard Heights", 4, nhood)
  nhood <- ifelse(corrected == "Bevo Mill" |
                    corrected == "Bevo", 5, nhood)
  nhood <- ifelse(corrected == "Princeton Heights", 6, nhood)
  nhood <- ifelse(corrected == "South Hampton", 7, nhood)
  nhood <- ifelse(corrected == "St. Louis Hills", 8, nhood)
  nhood <- ifelse(corrected == "Lindenwood Park", 9, nhood)
  nhood <- ifelse(corrected == "Ellendale", 10, nhood)
  nhood <- ifelse(corrected == "Clifton Heights", 11, nhood)
  nhood <- ifelse(corrected == "The Hill", 12, nhood)
  nhood <- ifelse(corrected == "Southwest Garden", 13, nhood)
  nhood <- ifelse(corrected == "North Hampton", 14, nhood)
  nhood <- ifelse(corrected == "Tower Grove South" |
                    corrected == "TGS", 15, nhood)
  nhood <- ifelse(corrected == "Dutchtown", 16, nhood)
  nhood <- ifelse(corrected == "Mount Pleasant", 17, nhood)
  nhood <- ifelse(corrected == "Marine Villa", 18, nhood)
  nhood <- ifelse(corrected == "Gravois Park", 19, nhood)
  nhood <- ifelse(corrected == "Kosciusko", 20, nhood)
  nhood <- ifelse(corrected == "Soulard", 21, nhood)
  nhood <- ifelse(corrected == "Benton Park", 22, nhood)
  nhood <- ifelse(corrected == "McKinley Heights", 23, nhood)
  nhood <- ifelse(corrected == "Fox Park", 24, nhood)
  nhood <- ifelse(corrected == "Tower Grove East" |
                    corrected == "TGE", 25, nhood)
  nhood <- ifelse(corrected == "Compton Heights", 26, nhood)
  nhood <- ifelse(corrected == "Shaw", 27, nhood)
  nhood <- ifelse(corrected == "Botanical Heights", 28, nhood)
  nhood <- ifelse(corrected == "Tiffany", 29, nhood)
  nhood <- ifelse(corrected == "Benton Park West", 30, nhood)
  nhood <- ifelse(corrected == "The Gate District" |
                    corrected == "The Gate", 31, nhood)
  nhood <- ifelse(corrected == "Lafayette Square" |
                    corrected == "Lafayette Sq" |
                    corrected == "Lafayette Sq.", 32, nhood)
  nhood <- ifelse(corrected == "Peabody Darst Webbe" | corrected == "Peabody", 33, nhood)
  nhood <- ifelse(corrected == "LaSalle Park", 34, nhood)
  nhood <- ifelse(corrected == "Downtown", 35, nhood)
  nhood <- ifelse(corrected == "Downtown West", 36, nhood)
  nhood <- ifelse(corrected == "Midtown", 37, nhood)
  nhood <- ifelse(corrected == "Central West End" |
                    corrected == "CWE" |
                    corrected == "C.W.E.", 38, nhood)
  nhood <- ifelse(corrected == "Forest Park South East" |
                    corrected == "FPSE" |
                    corrected == "The Grove" |
                    corrected == "Grove" |
                    corrected == "Forest Park Southeast", 39, nhood)
  nhood <- ifelse(corrected == "Kings Oak", 40, nhood)
  nhood <- ifelse(corrected == "Cheltenham", 41, nhood)
  nhood <- ifelse(corrected == "Clayton-Tamm", 42, nhood)
  nhood <- ifelse(corrected == "Franz Park", 43, nhood)
  nhood <- ifelse(corrected == "Hi-Pointe" |
                    corrected == "Hi Pointe" |
                    corrected == "Hi Point" |
                    corrected == "Hi-Point", 44, nhood)
  nhood <- ifelse(corrected == "Wydown Skinker", 45, nhood)
  nhood <- ifelse(corrected == "Skinker DeBaliviere", 46, nhood)
  nhood <- ifelse(corrected == "DeBaliviere Place", 47, nhood)
  nhood <- ifelse(corrected == "West End", 48, nhood)
  nhood <- ifelse(corrected == "Visitation Park", 49, nhood)
  nhood <- ifelse(corrected == "Wells Goodfellow", 50, nhood)
  nhood <- ifelse(corrected == "Academy", 51, nhood)
  nhood <- ifelse(corrected == "Kingsway West", 52, nhood)
  nhood <- ifelse(corrected == "Fountain Park", 53, nhood)
  nhood <- ifelse(corrected == "Lewis Place", 54, nhood)
  nhood <- ifelse(corrected == "Kingsway East", 55, nhood)
  nhood <- ifelse(corrected == "Greater Ville", 56, nhood)
  nhood <- ifelse(corrected == "The Ville", 57, nhood)
  nhood <- ifelse(corrected == "Vandeventer", 58, nhood)
  nhood <- ifelse(corrected == "Jeff Vanderlou" |
                    corrected == "JVL", 59, nhood)
  nhood <- ifelse(corrected == "St. Louis Place" |
                    corrected == "St Louis Place" |
                    corrected == "Saint Louis Place", 60, nhood)
  nhood <- ifelse(corrected == "Carr Square", 61, nhood)
  nhood <- ifelse(corrected == "Columbus Square", 62, nhood)
  nhood <- ifelse(corrected == "Old North St. Louis" |
                    corrected == "Old North" |
                    corrected == "Old North St Louis" |
                    corrected == "Old North Saint Louis", 63, nhood)
  nhood <- ifelse(corrected == "Near North Riverfront" |
                    corrected == "Near North", 64, nhood)
  nhood <- ifelse(corrected == "Hyde Park", 65, nhood)
  nhood <- ifelse(corrected == "College Hill", 66, nhood)
  nhood <- ifelse(corrected == "Fairground Neighborhood" |
                    corrected == "Fairground", 67, nhood)
  nhood <- ifelse(corrected == "O'Fallon", 68, nhood)
  nhood <- ifelse(corrected == "Penrose", 69, nhood)
  nhood <- ifelse(corrected == "Mark Twain I-70 Industrial" |
                    corrected == "Mark Twain Industrial", 70, nhood)
  nhood <- ifelse(corrected == "Mark Twain", 71, nhood)
  nhood <- ifelse(corrected == "Walnut Park East", 72, nhood)
  nhood <- ifelse(corrected == "North Pointe" |
                    corrected == "North Point", 73, nhood)
  nhood <- ifelse(corrected == "Baden", 74, nhood)
  nhood <- ifelse(corrected == "Riverview", 75, nhood)
  nhood <- ifelse(corrected == "Walnut Park West", 76, nhood)
  nhood <- ifelse(corrected == "Covenant Blu-Grand Center" |
                    corrected == "Grand Center", 77, nhood)
  nhood <- ifelse(corrected == "Hamilton Heights", 78, nhood)
  nhood <- ifelse(corrected == "North Riverfront", 79, nhood)

  return(nhood)
}

# String or factor neighborhood names
#
# \code{nhood_str} is a function for converting a vector of City of St. Louis neighborhood numbers
# to either character or factor format.
#
# @usage nhood_str(var, asFactor = TRUE)
#
# @param var A numeric or integer vector containing City of St. Louis neighborhood numbers
#
# @param asFactor A logical scalar. Should the output be converted to a factor?
#
# @return A character or factor vector
#
# @source \href{https://www.stlouis-mo.gov/government/departments/planning/documents/citywide-neighborhood-map.cfm}{City of St. Louis Citywide Neighborhood Map}
#
#
gw_nhood_str <- function(var, asFactor = TRUE) {
  nhood <- ifelse(var == 1, "Carondelet", NA)
  nhood <- ifelse(var == 2, "Patch", nhood)
  nhood <- ifelse(var == 3, "Holly Hills", nhood)
  nhood <- ifelse(var == 4, "Boulevard Heights", nhood)
  nhood <- ifelse(var == 5, "Bevo Mill", nhood)
  nhood <- ifelse(var == 6, "Princeton Heights", nhood)
  nhood <- ifelse(var == 7, "South Hampton", nhood)
  nhood <- ifelse(var == 8, "St. Louis Hills", nhood)
  nhood <- ifelse(var == 9, "Lindenwood Park", nhood)
  nhood <- ifelse(var == 10, "Ellendale", nhood)
  nhood <- ifelse(var == 11, "Clifton Heights", nhood)
  nhood <- ifelse(var == 12, "The Hill", nhood)
  nhood <- ifelse(var == 13, "Southwest Garden", nhood)
  nhood <- ifelse(var == 14, "North Hampton", nhood)
  nhood <- ifelse(var == 15, "Tower Grove South", nhood)
  nhood <- ifelse(var == 16, "Dutchtown", nhood)
  nhood <- ifelse(var == 17, "Mount Pleasant", nhood)
  nhood <- ifelse(var == 18, "Marine Villa", nhood)
  nhood <- ifelse(var == 19, "Gravois Park", nhood)
  nhood <- ifelse(var == 20, "Kosciusko", nhood)
  nhood <- ifelse(var == 21, "Soulard", nhood)
  nhood <- ifelse(var == 22, "Benton Park", nhood)
  nhood <- ifelse(var == 23, "McKinley Heights", nhood)
  nhood <- ifelse(var == 24, "Fox Park", nhood)
  nhood <- ifelse(var == 25, "Tower Grove East", nhood)
  nhood <- ifelse(var == 26, "Compton Heights", nhood)
  nhood <- ifelse(var == 27, "Shaw", nhood)
  nhood <- ifelse(var == 28, "Botanical Heights", nhood)
  nhood <- ifelse(var == 29, "Tiffany", nhood)
  nhood <- ifelse(var == 30, "Benton Park West", nhood)
  nhood <- ifelse(var == 31, "The Gate District", nhood)
  nhood <- ifelse(var == 32, "Lafayette Square", nhood)
  nhood <- ifelse(var == 33, "Peabody Darst Webbe", nhood)
  nhood <- ifelse(var == 34, "LaSalle Park", nhood)
  nhood <- ifelse(var == 35, "Downtown", nhood)
  nhood <- ifelse(var == 36, "Downtown West", nhood)
  nhood <- ifelse(var == 37, "Midtown", nhood)
  nhood <- ifelse(var == 38, "Central West End", nhood)
  nhood <- ifelse(var == 39, "Forest Park South East", nhood)
  nhood <- ifelse(var == 40, "Kings Oak", nhood)
  nhood <- ifelse(var == 41, "Cheltenham", nhood)
  nhood <- ifelse(var == 42, "Clayton-Tamm", nhood)
  nhood <- ifelse(var == 43, "Franz Park", nhood)
  nhood <- ifelse(var == 44, "Hi-Pointe", nhood)
  nhood <- ifelse(var == 45, "Wydown Skinker", nhood)
  nhood <- ifelse(var == 46, "Skinker DeBaliviere", nhood)
  nhood <- ifelse(var == 47, "DeBaliviere Place", nhood)
  nhood <- ifelse(var == 48, "West End", nhood)
  nhood <- ifelse(var == 49, "Visitation Park", nhood)
  nhood <- ifelse(var == 50, "Wells Goodfellow", nhood)
  nhood <- ifelse(var == 51, "Academy", nhood)
  nhood <- ifelse(var == 52, "Kingsway West", nhood)
  nhood <- ifelse(var == 53, "Fountain Park", nhood)
  nhood <- ifelse(var == 54, "Lewis Place", nhood)
  nhood <- ifelse(var == 55, "Kingsway East", nhood)
  nhood <- ifelse(var == 56, "Greater Ville", nhood)
  nhood <- ifelse(var == 57, "The Ville", nhood)
  nhood <- ifelse(var == 58, "Vandeventer", nhood)
  nhood <- ifelse(var == 59, "Jeff Vanderlou", nhood)
  nhood <- ifelse(var == 60, "St. Louis Place", nhood)
  nhood <- ifelse(var == 61, "Carr Square", nhood)
  nhood <- ifelse(var == 62, "Columbus Square", nhood)
  nhood <- ifelse(var == 63, "Old North St. Louis", nhood)
  nhood <- ifelse(var == 64, "Near North Riverfront", nhood)
  nhood <- ifelse(var == 65, "Hyde Park", nhood)
  nhood <- ifelse(var == 66, "College Hill", nhood)
  nhood <- ifelse(var == 67, "Fairground Neighborhood", nhood)
  nhood <- ifelse(var == 68, "O'Fallon", nhood)
  nhood <- ifelse(var == 69, "Penrose", nhood)
  nhood <- ifelse(var == 70, "Mark Twain I-70 Industrial", nhood)
  nhood <- ifelse(var == 71, "Mark Twain", nhood)
  nhood <- ifelse(var == 72, "Walnut Park East", nhood)
  nhood <- ifelse(var == 73, "North Pointe", nhood)
  nhood <- ifelse(var == 74, "Baden", nhood)
  nhood <- ifelse(var == 75, "Riverview", nhood)
  nhood <- ifelse(var == 76, "Walnut Park West", nhood)
  nhood <- ifelse(var == 77, "Covenant Blu-Grand Center", nhood)
  nhood <- ifelse(var == 78, "Hamilton Heights", nhood)
  nhood <- ifelse(var == 79, "North Riverfront", nhood)

  if (asFactor == TRUE) {
    nhood <- as.factor(nhood)
  }

  return(nhood)
}
