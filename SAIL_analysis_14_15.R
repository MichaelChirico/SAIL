#Data Exploration / Analysis
#SAIL / Math 104 Data
#Michael Chirico

# Package Setup & Convenient Functions ####
rm(list=ls(all=T))
gc()
# Highly recommend reading the following to
#   get a jump start on using data.table:
#   * https://github.com/Rdatatable/data.table/wiki/Getting-started
#   * https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.pdf
#   * https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf
library(data.table)
library(xtable)
library(texreg)
library(readxl)
#this is my package of functions. I can't figure out
#  how to install from GitHub on an offline Windows machine...
#  see: stackoverflow.com/questions/33179156/ for possible updates
#  Until then, I'll simply source all the functions from that
#  package into a self-created namespace.
#    1) Add a fake "package" environment to the search() path
#         with assign
#    2) source the package functions (luckily all in the same
#         .R file) into that environment
#    3) carry on as if nothing's fishy about that.
attach(environment(), name = "package:funchir")
evalq(source(".\\code\\funchir_151018.R"), 
      envir = as.environment("package:funchir"))

setwd("C:\\Users\\Mike\\Desktop\\SAIL\\")
#store vector of common output / input directories
wds<-c(log=".\\logs\\",data=".\\data\\2014-15\\",
       maps=".\\data_mappings\\",out=".\\output\\2014-15\\")
#for replicability, it's good practice to make regular note of the
#  versions of packages that were used to produce the analysis
#  (in case some package goes defunct or later loses functionality)
write.packages(wds["log"] %+% "data_exploration_14_15.txt")

# Data Import ####
## I stored column names and types in a separate .csv file
##   for more efficient reading
col_info <- fread(".\\data\\2014-15\\sail_14c_15a_col_info.csv")
data <-
  #keeping data in original .xlsx format;
  #  read_excel is the fastest Excel reader I've seen in R,
  #  but it reads all data as a data.frame, so we wrap
  #  with setDT to convert it to a data.table
  setDT(read_excel(
    wds["data"] %+%
      "SAIL_Fall14_Spring15_MATH104_2012_14_cohorts_19Jun2015.xlsx",
    sheet = 2, col_names = col_info$column,
    col_types = col_info$class_hadley, skip = 1)); rm(col_info)

##Clean up classification
data[ , birth_date := as.Date(birth_date, format = "%d%b%Y")]
###From Registrar's Website (Academic Calendar),
###  first day of classes in Fall 2014 was Aug. 27
###We're actually calculating an approximate age by
###  dividing by 365.25 (leap years are a pain to deal with)
###We use unclass because the difftime class is annoying to print
data[ , age := unclass((as.Date("2014-08-27") - birth_date)/365.25)]

###SAT & ACT interpolation
###  Approach: Many students at Penn took both ACT & SAT;
###    fit a simple regression for those students to 
###    get an approximate correspondence between SAT & ACT,
###    and fill in the missing data
###  **TODO: Should probably use TLS instead of OLS...**
data[ , sat_total := sat_verbal + sat_math + sat_writing]

act_sat_map <- data[ , lm(sat_total ~ act)]
data[  ,sat_flag := FALSE]
data[is.na(sat_total),
     #round to a multiple of 10 since something like
     #  this is done for all reported SAT scores
     `:=`(sat_total = round(predict(
       #wrap ACT score in data.frame so that
       #  it fits the conventions of the predict function
       act_sat_map, data.frame(act)), -1),
       #also add a flag indicating we've done this
       sat_flag = TRUE)]

#Reverse the procedure for ACT;
#  **IIUC, with TLS we could use the same maping**
sat_act_map <- data[ , lm(act ~ sat_total)]
data[ , act_flag := FALSE]
data[is.na(act),
     `:=`(act = round(predict(
       sat_act_map, data.frame(sat_total))),
       act_flag = TRUE)]

#Assuming missing gross need implies
#  gross need is 0
data[is.na(gross_need_yr1), gross_need_yr1 := 0]

#which variables will we store as factors?
factors <- c("cohort", "gender", "ethnicity", "citizen",
             "pell_el_yr1", "school_1_1")
#to define columns via character vector, need to wrap in () --
#  otherwise, it will try to define a single column named 'factors'
data[ , (factors) := lapply(.SD, as.factor), .SDcols = factors]

#add a more useful version of the ethnicity factor -- 
#  collapse the least common categories into "other"
data[ , ethnicity2 := factor(ethnicity)]
levels(data$ethnicity2) <-
  list(White = "White", Black = "Black", Asian = "Asian",
       Hispanic = "Hispanic",
       #collapse the following 
       Other = c("American Indian or Alaskan Native",
                 "Non-resident Alien",
                 "Race and ethnicity unknown",
                 "Two or more races"))

#setting 2014 as the baseline cohort since that's the
#  cohort of the then-freshmen
data[ , cohort := factor(cohort, c("2014", "2012", "2013"))]

###Geographic grouping
#### set baseline citizenship as US Citizen
levels(data$citizen) <-
  c("US Citizen", "US Permanent Resident",
    "Non-Resident Alien")

#see state_us_region_mapping.csv --
#  this gives a named region to each state
#  to try and aggregate some of the geographic information
data[fread(wds["maps"] %+% "state_us_region_mapping.csv"),
     perm_res_us_region := i.us_region,
     #merging on:
     on = c(perm_res_state = "state")
     #anybody missed is outside the US
     ][is.na(perm_res_state), perm_res_us_region := "ROW"]

#re-order the regions so that the most common (Mid-Atlantic)
#  comes first
data[ , perm_res_us_region :=
       factor(perm_res_us_region,
              levels = c("Mid-Atlantic", "New England", "Midwest",
                         "South", "West", "ROW"))]

###Lots of foreign cities are surrounded by
###  noisy postal codes, e.g. Greater London SW1W 9HS
data[ , perm_res_city :=
       toupper(gsub("\\s\\w*\\d\\w*.*|[[:punct:]]","",
                    perm_res_city))]

#having cleaned city names, we can merge to find
#  the country associated with each city
data[fread(wds["maps"] %+% "city_country_mapping.csv"),
     perm_res_country := i.country,
     on = c(perm_res_city = "city")
     #Overwrite / declare as USA any city not matched
     ][!is.na(perm_res_state), perm_res_country := "USA"]

#aggregate again -- country to continent (& world region)
data[fread(wds["maps"] %+% "country_continent_mapping.csv"),
     "perm_res_" %+% c("continent", "world_region") :=
       .(i.continent, i.world_region),
     on = c(perm_res_country = "country")]

##Getting GPAs from letter grades, paring off
##  qualifiers (+ or -), and reordering
##  so that A comes first
lvls <- c("A", "B", "C", "D", "F", "W", "Other")
data[fread(wds["maps"] %+% "grade_gpa_mapping.csv"),
     `:=`(m104_letter_final=
            factor(i.letter_grade, levels = lvls),
          m104_gpa_final = i.gpa),
     on = c(m104_grade_final = "grade")]

#flag who completed the course
#  I: Incomplete; W: Withdrawal
data[ , m104_completed_final :=
       !m104_grade_final %in% c("I", "W")]

##rinse, wash, repeat for the semester-specific variables
##  (could be done all three times in a loop, but
##   that would just make the code more opaque)
data[fread(wds["maps"] %+% "grade_gpa_mapping.csv"),
     `:=`(m104_letter_14c =
            factor(i.letter_grade, levels = lvls),
          m104_gpa_14c = i.gpa),
     on = c(m104_grade_14c = "grade")]

data[ , m104_completed_14c :=
       !m104_grade_14c %in% c("I", "W")]

data[fread(wds["maps"] %+% "grade_gpa_mapping.csv"),
     `:=`(m104_letter_15a =
            factor(i.letter_grade, levels = lvls),
          m104_gpa_15a = i.gpa),
     on = c(m104_grade_15a = "grade")]

data[ , m104_completed_15a :=
       !m104_grade_15a %in% c("I","W")]

##Define SAIL sections & Lecture IDs
sec_lec_map <-
  fread(wds["maps"] %+% "section_lecture_mappings.csv",
        colClasses = abbr_to_colClass("cl", "43"), na.strings = "")
data[sec_lec_map, `:=`(m104_lecture_final = i.lecture,
                       #is it SAIL?
                       m104_sail_final = i.sail),
     on=c("m104_term_final" = "term",
          "m104_section_final" = "section")]

#all these students are in funky sections (engineering, LPS, etc.):
data[is.na(m104_sail_final) &
       !is.na(m104_lecture_final), m104_sail_final := FALSE]

#go again, but for 2014 Fall
data[sec_lec_map[term == "2014C"],
     `:=`(m104_lecture_14c = i.lecture,
          m104_sail_14c = i.sail),
     on = c("m104_section_14c" = "section")]
data[is.na(m104_sail_14c) &
       !is.na(m104_lecture_14c), m104_sail_14c := FALSE]

data[sec_lec_map[term == "2015A"],
     `:=`(m104_lecture_15a = i.lecture,
          m104_sail_15a = i.sail),
     on = c("m104_section_15a" = "section")]
data[is.na(m104_sail_15a) &
       !is.na(m104_lecture_15a), m104_sail_15a := FALSE]

##Define flags for various samples
###[F]lag for e[X]cluded sections from [14C]
data[ ,fx14c := m104_lecture_14c %in% c("007", "601")]
###[F]lag for [A]ll _in-sample_ obs. from [14C]
data[ ,fa14c := !fx14c & !is.na(m104_sail_14c)]
###[F]lag for [S]AIL obs. from [14C]
data[ ,fs14c := fa14c & m104_sail_14c]
###[F]lag for [N]on-SAIL obs. from [14C]
data[ ,fn14c := fa14c & !m104_sail_14c]
###[F]lag for e[X]cluded sections from [15A]
data[ ,fx15a := m104_lecture_15a == "601"]
###[F]lag for [A]ll _in-sample_ obs. from [15A]
data[ ,fa15a := !fx15a & !is.na(m104_sail_15a)]
###[F]lag for [S]AIL obs. from [15A]
data[ ,fs15a := fa15a & m104_sail_15a]
###[F]lag for [N]on-SAIL obs. from [15A]
data[ ,fn15a := fa15a & !m104_sail_15a]

# Background Data ####
##Table 1: Descriptive Statistics
### annoyingly, 'table' returns its own class of object
###   which doesn't play well with data.table.
###   use as.vector to fix this.
ptbl <- function(x) as.vector(table2(x, prop = TRUE, pct = TRUE))
### some convenience functions for filling in the
###   p-value column, depending on whether the 
###   associated rows are paired (T-Test)
###   or tabled (chi-squared test)
pchi <- function(x, y) chisq.test(x, y)$p.value
pt.t <- function(x, y) t.test(x ~ y)$p.value
pblk <- function(x, y) NA #for consistency, will need to return blanks sometimes
#approach: return each section of the output table as a "block"
#  (specifically, as a data.table), then use rbindlist
#  to stitch together these blocks as one big table to be
#  sent to xtable for TeX output
tbl1 <- rbindlist(lapply(
  #there are three elements necessary for each desired block of the
  #  output table: the variable [vr], the desired function used
  #  to summarize the variable [fn] (e.g., mean, median, table, etc.),
  #  and the associated p-test [pf]
  list(list(vr = "cohort", fn = ptbl, pf = pchi),
       list(vr = "gender",
            #convert to a percentage for prettier output
            fn = function(x) to.pct(mean(x == "Male")), pf = pchi),
       list(vr = "ethnicity2", fn = ptbl, pf = pchi),
       list(vr = "age", fn = mean, pf = pt.t),
       list(vr = "citizen", fn = ptbl, pf = pchi),
       list(vr = "hs_gpa", 
            fn = function(x) mean(x, na.rm = TRUE), pf = pt.t),
       list(vr = "hs_gpa",
            fn = function(x) to.pct(mean(is.na(x))), pf = pblk),
       list(vr = "perm_res_country",
            fn = function(x) to.pct(mean(x == "USA")), pf = pchi),
       list(vr = "perm_res_us_region", fn = ptbl, pf = pchi),
       list(vr = "sat_total", fn = mean, pf = pt.t),
       list(vr = "sat_flag", fn = function(x) to.pct(mean(x)), pf = pblk),
       list(vr = "act", fn = mean, pf = pt.t),
       list(vr = "act_flag", fn = function(x) to.pct(mean(x)), pf = pblk),
       #for gross need, express in $1000s (prettier)
       list(vr = "gross_need_yr1", fn = function(x) mean(x/1000), pf = pt.t),
       list(vr = "gross_need_yr1", fn = function(x) median(x/1000), pf = pblk),
       list(vr = "pell_el_yr1", fn = ptbl, pf = pchi),
       list(vr = "school_1_1", fn = ptbl, pf = pchi),
       #generally a lot easier to calculate sample size,
       #  but this approach is what fits in this framework:
       #  .I is the row index, 'length' counts how long it is
       list(vr = ".I", fn = length, pf = pblk)),
  function(lst){
    with(lst, data[ , {gvr <- get(vr) #retrieve our variable
                       #output has 8 columns:
                       # (Fall, Spring) x 
                       #   (All, SAIL, non-SAIL, 
                       #    p-value of difference)
                       #Recall the flags we defined above, e.g.,
                       #  [fa14c] allows us to extract anyone who
                       #  was in Math 104 in Fall '14
                    .("a14c" = fn(gvr[fa14c]), #[a]ll, fall (=[14c])
                      "s14c" = fn(gvr[fs14c]), #[s]ail, fall
                      "n14c" = fn(gvr[fn14c]), #[n]on-sail, fall
                      #[p]-value, fall -- basically testing
                      #  (as appropriate), whether [vr] is
                      #  independent of sail status
                      "p14c" = pf(gvr[fa14c], m104_sail_14c[fa14c]),
                      "a15a" = fn(gvr[fa15a]), #[all], spring (=[15a])
                      "s15a" = fn(gvr[fs15a]),
                      "n15a" = fn(gvr[fn15a]),
                      "p15a" = pf(gvr[fa15a], m104_sail_15a[fa15a]))}
                   #exclude the p-values on all but the first row
                   #  within each group
                  ][-1L, c("p14c", "p15a") := NA])}))

#name the columns appropriately now that we've got space
#  (would have been verbose to have done so in the loop above)
setnames(tbl1, rep(c("Overall", "SAIL", "Non-Sail", "$p$-value"), 2))

#row names of the table
#  (abbreviating to taste)
rn <- c(2012:2014, "% Male", "White", "Black", "Asian",
        "Hispanic", "Other", "Age", "US Citizen",
        "Perm. Res.", "Non-Res. Alien",
        "HS GPA", "% w/o HS GPA", "% from US",
        "Mid-Atlantic", "New England", "Midwest",
        "South", "West", "Rest of World", "SAT (of 2400)",
        "% Missing SAT", "ACT", "% Missing ACT",
        "Average ($1000)", "Median ($1000)",
        "Non-Eligible", "Eligible", "College", "Engineering",
        "Nursing", "Wharton", "# Obs.")

#for controlling the number of digits in output
#  (chosen manually row-by-row
dr <- rep(c(1, 2, 1, 2, 1, 0, 1, 2, 1, 0),
          c(9, 1, 3, 1, 8, 1, 3, 2, 6, 1))
#life is cruel, and we must specify the number
#  of digits for the column of row names
#  (even though it's a character column).
#  most concise way to do this is to simply treat
#  it like any other column, hence the 4
#  (instead of 3) in the first instance of rep:
dmat<-matrix(c(rep(dr, 4), rep(2, length(dr)),
               rep(dr, 3), rep(2, length(dr))),
             #have to add one column (for the row names)
             nrow = nrow(tbl1), ncol = ncol(tbl1) + 1)
#convert back to data.frame to take
#  advantage of row names (could also have
#  simply defined a column of rownames,
#  but whatever)
#see ?print.xtable for help on the arguments
#  sent to lyx.xtable
lyx.xtable(xtable(setDF(tbl1, rownames = rn),
                  caption = "Descriptive Statistics for SAIL," %+%
                    " 2014-15 Academic Year", label = "tbl:desc",
                  digits = dmat, align = "|r|rrrr|rrrr|"),
           #since we use math-scripting to make p-values
           #  in the column header, we have to tell
           #  xtable not to remove the $
           sanitize.colnames.function = identity,
           #this is for adding horizontal lines to
           #  separate out groups and to add
           #  group headers (for the tabled variables)
           add.to.row =
             list(pos = list(-1, 0, 4, 10, 16, 26, 28, 30),
                  command = 
                    c("\\hline \n& \\multicolumn{4}{|c|}{2014 Fall}" %+%
                        " & \\multicolumn{4}{|c|}{2015 Spring} \\\\ \n",
                      "\\hline \nCohort & & & & & & & &\\\\ \n",
                      "\\hline \nEthnicity & & & & & & & & \\\\ \n",
                      "\\hline \nU.S. Citizenship & & & & & & & & \\\\ \n",
                      "\\hline \nU.S. Region & & & & & & & & \\\\ \n",
                      "\\hline \nFirst-Year Need & & & & & & & & \\\\ \n",
                      "\\hline \nPell Eligibility & & & & & & & & \\\\ \n",
                      "\\hline \nProgram & & & & & & & & \\\\ \n")),
           hline.after = c(3, 9, 13, 22, 34, 35))

#write the raw output of the table as a .csv for simple
#  copying over to Excel (if necessary)
write.csv(tbl1, wds["out"] %+% "descriptive_stats_full_raw.csv")

##Table 2: Descriptive Statistics
##  (only students from main Cohort)
tbl2 <- rbindlist(lapply(
  list(list(vr = "gender",
            fn = function(x) to.pct(mean(x == "Male")), pf = pchi),
       list(vr = "ethnicity2", fn = ptbl, pf = pchi),
       list(vr = "age", fn = mean, pf = pt.t),
       list(vr = "citizen", fn = ptbl, pf = pchi),
       list(vr = "hs_gpa", 
            fn = function(x) mean(x, na.rm = TRUE), pf = pt.t),
       list(vr = "hs_gpa", fn = function(x) to.pct(mean(is.na(x))), pf = pblk),
       list(vr = "perm_res_country",
            fn = function(x) to.pct(mean(x == "USA")), pf = pchi),
       list(vr = "perm_res_us_region", fn = ptbl, pf = pchi),
       list(vr = "sat_total", fn = mean, pf = pt.t),
       list(vr = "sat_flag", fn = function(x) to.pct(mean(x)), pf = pblk),
       list(vr = "act", fn = mean, pf = pt.t),
       list(vr = "act_flag", fn = function(x) to.pct(mean(x)), pf = pblk),
       list(vr = "gross_need_yr1", fn = function(x) mean(x/1000), pf = pt.t),
       list(vr = "gross_need_yr1", fn = function(x) median(x/1000), pf = pblk),
       list(vr = "pell_el_yr1", fn = ptbl, pf = pchi),
       list(vr = "school_1_1", fn = ptbl, pf = pchi),
       list(vr = ".I", fn = length, pf = pblk)),
  function(lst){
    with(lst, data[
      cohort == "2014",{gvr <- get(vr)
                      .("a14c" = fn(gvr[fa14c]), 
                        "s14c" = fn(gvr[fs14c]),
                        "n14c" = fn(gvr[fn14c]),
                        "p14c" = pf(gvr[fa14c], m104_sail_14c[fa14c]),
                        "a15a" = fn(gvr[fa15a]),
                        "s15a" = fn(gvr[fs15a]),
                        "n15a" = fn(gvr[fn15a]),
                        "p15a" = pf(gvr[fa15a], m104_sail_15a[fa15a]))}
      ][-1L, c("p14c", "p15a") := NA])}))

setnames(tbl2, rep(c("Overall", "SAIL", "Non-Sail", "$p$-value"), 2))

rn <- c("% Male", "White", "Black", "Asian",
        "Hispanic", "Other", "Age", "US Citizen",
        "Perm. Res.", "Non-Res. Alien",
        "HS GPA", "% w/o HS GPA", "% from US",
        "Mid-Atlantic", "New England", "Midwest",
        "South", "West", "Rest of World", "SAT (of 2400)",
        "% Missing SAT", "ACT", "% Missing ACT",
        "Average ($1000)", "Median ($1000)",
        "Non-Eligible", "Eligible", "College", "Engineering",
        "Nursing", "Wharton", "# Obs.")

dr<-rep(c(1, 2, 1, 2, 1, 0, 1, 2, 1, 0),
        c(6, 1, 3, 1, 8, 1, 3, 2, 6, 1))
dmat<-matrix(c(rep(dr, 4), rep(2, length(dr)),
               rep(dr, 3), rep(2, length(dr))),
             nrow = nrow(tbl2), ncol = ncol(tbl2) + 1)
lyx.xtable(xtable(setDF(tbl2, rownames = rn),
                  caption = "Descriptive Statistics for SAIL," %+%
                    " 2014-15 Academic Year (2014 Cohort Only)",
                  label = "tbl:desc", digits = dmat, align = "|r|rrrr|rrrr|"),
           sanitize.colnames.function = identity,
           add.to.row =
             list(pos = list(-1, 1, 7, 13, 23, 25, 27),
                  command = 
                    c("\\hline \n& \\multicolumn{4}{|c|}{2014 Fall}" %+%
                        " & \\multicolumn{4}{|c|}{2015 Spring} \\\\ \n",
                      "\\hline \nEthnicity & & & & & & & & \\\\ \n",
                      "\\hline \nU.S. Citizenship & & & & & & & & \\\\ \n",
                      "\\hline \nU.S. Region & & & & & & & & \\\\ \n",
                      "\\hline \nFirst-Year Need & & & & & & & & \\\\ \n",
                      "\\hline \nPell Eligibility & & & & & & & & \\\\ \n",
                      "\\hline \nProgram & & & & & & & & \\\\ \n")),
           hline.after=c(0, 6, 10, 19, 31, 32))

write.csv(tbl2, wds["out"] %+% "descriptive_stats_2014_cohort_raw.csv")

# Outcomes Data
##Table 3: Grades
### we'll build this table column-wise instead of row-wise;
###   the key elements for a given column are the associated
###   flag [flg] used to identify relevant rows and 
###   the letter grade [grd] and GPA [gpa] assigned to those rows
tbl3 <- Reduce(cbind, lapply(
  list(list(flg = "fa14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fs14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fn14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fa15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a"),
       list(flg = "fs15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a"),
       list(flg = "fn15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a")),
  function(lst){
    with(lst, data[ , c(table2(get(grd)[get(flg)], 
                               prop = TRUE, pct = TRUE),
                     mean(get(gpa)[get(flg)], na.rm = TRUE))])}))
colnames(tbl3) <- rep(c("Overall", "SAIL", "Non-Sail"), 2)
rownames(tbl3)[8] <- c("Avg. GPA")

lyx.xtable(xtable(
  tbl3, caption = "Outcome Data for SAIL," %+%
    " 2014-15 Academic Year", label = "tbl:grds",
  digits = matrix(c(rep(1, 7), 2), nrow = nrow(tbl3),
                ncol = ncol(tbl3) + 1),
  align = "|r|rrr|rrr|"),
  add.to.row =
    list(pos = list(-1),
         command = c("\\hline \n& \\multicolumn{3}{|c|}{2014 Fall}" %+%
                       " & \\multicolumn{3}{|c|}{2015 Spring} \\\\ \n")),
  hline.after = c(0, 7, 8))

##Table 4: Grades (2014 Cohort Only)
tbl4< - Reduce(cbind,lapply(
  list(list(flg = "fa14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fs14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fn14c", grd = "m104_letter_14c", gpa = "m104_gpa_14c"),
       list(flg = "fa15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a"),
       list(flg = "fs15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a"),
       list(flg = "fn15a", grd = "m104_letter_15a", gpa = "m104_gpa_15a")),
  function(lst){
    with(lst, data[cohort == "2014",
                  c(table2(get(grd)[get(flg)],
                           prop = TRUE, pct = TRUE),
                    mean(get(gpa)[get(flg)], na.rm = TRUE))])}))
colnames(tbl4) <- rep(c("Overall", "SAIL", "Non-Sail"), 2)
rownames(tbl4)[8] <- c("Avg. GPA")

lyx.xtable(xtable(
  tbl4, caption = "Outcome Data for SAIL," %+%
    " 2014-15 Academic Year (2014 Cohort ONLY)", label = "tbl:grds",
  digits = matrix(c(rep(1, 7), 2), nrow = nrow(tbl4),
                ncol = ncol(tbl4) + 1),
  align = "|r|rrr|rrr|"),
  add.to.row =
    list(pos = list(-1),
         command = c("\\hline \n& \\multicolumn{3}{|c|}{2014 Fall}" %+%
                       " & \\multicolumn{3}{|c|}{2015 Spring} \\\\ \n")),
  hline.after = c(0, 7, 8))

##Regressions 1: GPA vs. SAIL (& covariates)
###To determine covariates,
###  find variables independently significantly
###  correlated with GPA via:
### data[,summary(lm(m104_gpa_14c~cohort+gender+
###                    ethnicity+age+citizen+hs_gpa+
###                    I(gross_need_yr1>0)+pell_el_yr1+
###                    school_1_1+sat_total))]
###  (and similarly for m104_gpa_15a)

#uncontrolled regression for '14 Fall
reg1_14c <- data[(fa14c), lm(m104_gpa_14c ~ m104_sail_14c)]
#now with controls
reg2_14c <- data[(fa14c), lm(m104_gpa_14c ~ m104_sail_14c + cohort +
                            ethnicity2 + hs_gpa  I(gross_need_yr1 > 0) +
                            act + gender)]
#uncontrolled regression for '15 Spring
reg1_15a <- data[(fa15a), lm(m104_gpa_15a ~ m104_sail_15a)]
reg2_15a <- data[(fa15a), lm(m104_gpa_15a ~ m104_sail_15a + cohort +
                            ethnicity2 + hs_gpa + I(gross_need_yr1 > 0) +
                            act + gender)]

#see ?texreg for help
lyx.texreg(list(reg1_14c, reg2_14c, reg1_15a, reg2_15a),
           custom.model.names = c("Fall '14", "Fall '14 (Controls)",
                                "Spring '15", "Spring '15 (Controls)"),
           custom.coef.names = c("Intercept", "SAIL",
                               "2012 Cohort", "2013 Cohort","Black",
                               "Asian", "Hispanic","Other Race",
                               "HS GPA", "Positive Gross Need",
                               "ACT", "Male", "SAIL"),
           caption = "Evaluating Salient Predictors of MATH 104 GPA")


#a glance at heterogeneity
data[(fa15a), summary(lm(m104_gpa_15a ~ cohort * m104_sail_15a +
                          ethnicity2 * m104_sail_15a + hs_gpa * m104_sail_15a +
                          I(gross_need_yr1 > 0) * m104_sail_15a +
                          act * m104_sail_15a + gender * m104_sail_15a +
                          perm_res_us_region * m104_sail_15a +
                          school_1_1 * m104_sail_15a))]

lyx.xtable(xtable(dcast(
  data[(fa15a), .(GPA = mean(m104_gpa_15a,na.rm = TRUE), .N),
       #this switches the SAIL indicator for a
       #  labeled character (pretty printing)
       keyby = .(sail = c("SAIL", "Non-SAIL")[2 - m104_sail_15a],
                 `Positive Gross Need` = gross_need_yr1 > 0)],
  `Positive Gross Need` ~ sail, value.var = c("GPA", "N"), sep = " "),
  caption = "Effect Heterogeneity with respect to Gross Need (Spring '15)"))

dcast(data[(fa15a), .(GPA = mean(m104_gpa_15a, na.rm = TRUE), .N),
           keyby = .(sail = c("SAIL", "Non-SAIL")[2 - m104_sail_15a],
                     `Positive Gross Need` = gross_need_yr1 > 0)],
      `Positive Gross Need` ~ sail, value.var = c("GPA", "N"), sep = " ")

lyx.xtable(xtable(
  data[(fa15a), .(sail = c("SAIL", "Non-SAIL")[2 - m104_sail_15a[1]],
                  GPA = mean(m104_gpa_15a, na.rm = TRUE)),
       keyby = .(section = m104_lecture_15a,
                 `Positive Gross Need` = gross_need_yr1 > 0)
       ][,.(SAIL = sail[1],
            `GPA Advantage for Pos. Gross Need` = diff(GPA)),
         #want to identify weak/strong sections, but not to
         #  call them out by name, so replace the actual section number
         #  with one that's randomly generated.
         by = section][ , section: = sample(LETTERS[1:4])
                       ][sample(.N)],
  caption = "Effect Heterogeneity by Anonymized Section"),
  include.rownames = FALSE)
