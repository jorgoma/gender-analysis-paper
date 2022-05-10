library(tidyverse)
library(tidytext)
library(tm)
library(forcats)
library(reshape)
library(wordcloud)
library(rmarkdown)
library(reactable)
library(ggrepel)
library(ggmap)
library(readr)
library(utils)
library(scales)
library(dplyr)
library(readxl)
library(stringi)


setwd("~/")
setwd("R/ARUK/Gender analysis")

#Load the data------------------------------

##Dimensions raw data

files <- list.files(path = "Dimensions", pattern = "*.csv", full.names = T)

df <- sapply(files, read_csv, simplify=FALSE, skip =1) %>% 
  bind_rows(.id = "id")

df <- df %>%
  select(
    `Publication ID`,
    DOI,
    PubYear,
    `Authors Affiliations`,
    `Research Organizations - standardized`,
    `GRID IDs`,
    `Country of Research organization`,
    `Times cited`,
    `Source title`)

#locations data downloaded from https://www.grid.ac/

grid_data <- read_csv("grid.csv")

##gender API data that gives each name a gender and a probability

files2 <- list.files(path = "Gender sources", pattern = "*.xlsx", full.names = T)

genders <- sapply(files2, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")%>%
  select(-id)%>%
  unique()

genders$First_Name <- genders$First_Name  %>%
  str_to_lower()


#Data cleaning to get Authorships--------------

#This gives a list with all authorships, including repeated for the same
#person who has multiple affiliations. We also only take into account 
#the first 25 authorships (i.e. 26th authorship and beyond are discarded)
#It only affects 0.8% of the papers

authorships_non_unique <- df %>%
  select(
    `Publication ID`,
    DOI,
    PubYear,
    `Authors Affiliations`,
    `Times cited`,
    `Source title`) %>%
  separate(
    `Authors Affiliations`,
    c("Author1", "Author2", "Author3", "Author4", "Author5", "Author6", "Author7", 
      "Author8", "Author9", "Author10", "Author11", "Author12", "Author13", 
      "Author14", "Author15", "Author16", "Author17", "Author18", "Author19",
      "Author20", "Author21", "Author22", "Author23", "Author24", "Author25"),
    sep = "\\); ") %>%
  pivot_longer(
    "Author1":"Author25",
    names_to = "AuthorOrder",
    values_to = "AuthorName_and_OrganisationName")%>%
  filter(
    AuthorName_and_OrganisationName != "NA")%>%
  separate(
    AuthorName_and_OrganisationName,
    c("Author_Name", "Organisation_Name"), #Author (Affiliation 1; Affiliation 2)
    sep = "\\(")%>%
  separate(
    Organisation_Name,
    c("Name1", "Name2","Name3", "Name4","Name5", "Name6",
      "Name7", "Name8","Name9", "Name10"),
    sep = "\\; ")%>%
  pivot_longer(
    "Name1":"Name10",
    names_to = "RO_Order",
    values_to = "Name")%>%
  filter(Name != "NA")%>%
  separate(Author_Name,
           c("Surname", "FirstName"),
           sep ="\\, ")%>%
  separate(FirstName,
           c("First_Name", "Initial"),#Lots of "Jorge G Magenti". Removing the "G"
           sep= "\\ ")%>%
  select(-"Initial")


authorships_non_unique$First_Name <- authorships_non_unique$First_Name %>%
  str_to_lower()%>%
  str_remove("\\.")

authorships_non_unique$First_Name <- stri_trans_general(authorships_non_unique$First_Name,
                                                  "Latin-ASCII")

authorships_non_unique$Surname <- authorships_non_unique$Surname %>%
  str_to_lower()%>%
  str_remove("\\.")

authorships_non_unique$Surname <- stri_trans_general(authorships_non_unique$Surname,
                                                        "Latin-ASCII")

authorships_non_unique$Name <- authorships_non_unique$Name %>%
  str_remove("\\)")



#merging the country data by Name of RO and the gender data by First_Name of author

authorships_non_unique_merged <- left_join(authorships_non_unique, grid_data, by= "Name")
authorships_non_unique_merged <- left_join(authorships_non_unique_merged, genders, by = "First_Name")%>%
  select(-ga_first_name, -State)





#removing the duplicates for people with multiple affiliations within the same country. 
#If those RO are in different countries, the author will count for both countries.


authorships_country <-  unique(authorships_non_unique_merged[c("Publication ID", 
                                                               "DOI", "AuthorOrder", 
                                                               "Surname", "First_Name", 
                                                               "Country", "ga_gender", 
                                                               "ga_accuracy", "ga_samples")])

#same thing, but with Publication Year added to calculate this ratio by year
authorships_country_trend <-  unique(authorships_non_unique_merged[c("Publication ID", 
                                                                     "DOI", "PubYear", "AuthorOrder", 
                                                                     "Surname", "First_Name", 
                                                                     "Country", "ga_gender", 
                                                                     "ga_accuracy", "ga_samples")])

#Calculations----------
##Authorships vs Authors by country====
#table with authorships ratio by country

authorships_country_table <- authorships_country %>%
  filter(str_length(authorships_country$First_Name)>1)%>% #removing authors with just initial
  filter(ga_accuracy>=95)%>% #filtering for gender prediction probbability >95%
  filter(Country == "United States"| Country == "United Kingdom" | #filtering for top 10 countries by papers
           Country == "Australia" | Country == "Canada" |   
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  group_by(Country, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authorship")




#table with authors ratio by country

authors_country_table <- authorships_country %>%
  filter(str_length(authorships_country$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

#removing authors with multiple affilitations in the same country

authors_country_table <- unique(authors_country_table[c("Surname", "First_Name", 
                                      "ga_gender", "Country",
                                      "ga_accuracy", "ga_samples")])

authors_country_table <- authors_country_table %>%
  group_by(Country, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authors")

#merging the authorships and authors table together, displaying only female ratio

author_vs_authorships_country <- rbind(authors_country_table, 
                                       authorships_country_table)%>%
  filter(ga_gender == "female")%>%
  select(- `n()`)


write.csv(author_vs_authorships_country, "Clean exports/Authors ratio by country.csv")


#repeat the same process as above, but adding Publication Year to the analysis to 
#see the trends in authorships and authors ratio

authorships_country_trend_table <- authorships_country_trend %>%
  filter(str_length(authorships_country_trend$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  group_by(PubYear, Country, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authorship")





authors_country_trend_table <- authorships_country_trend %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

authors_country_trend_table <- unique(authors_country_trend_table[c("Surname", "First_Name", 
                                      "ga_gender", "PubYear","Country",
                                      "ga_accuracy", "ga_samples")])


authors_country_trend_table <- authors_country_trend_table %>%
  filter(str_length(authors_country_trend_table$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(PubYear, Country, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authors")


authors_vs_authorships_country_trend <- rbind(authors_country_trend_table, authorships_country_trend_table)

write.csv(authors_vs_authorships_country_trend, "Clean exports/Authorship  and authors trend by country.csv")



##Authorships vs Authors overall ====

authorships_ratio <- authorships_country %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")


authorships_ratio <-  unique(authorships_ratio[c("Publication ID","DOI", "AuthorOrder", 
                                     "Surname", "First_Name", 
                                      "ga_gender", 
                                      "ga_accuracy", "ga_samples")])


authorships_ratio <- authorships_ratio %>%
  filter(str_length(authorships_ratio$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authorships")


author_ratio <- authorships_country %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

author_ratio <- unique(author_ratio[c("Surname", "First_Name", 
                                     "ga_gender", 
                                     "ga_accuracy", "ga_samples")])


author_ratio <- author_ratio %>%
  filter(str_length(author_ratio$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authors")




##Authorships and authors trends====


authorships_trend <- authorships_country_trend %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

authorships_trend <-  unique(authorships_trend[c("Publication ID","DOI", "AuthorOrder", 
                                           "Surname", "First_Name","PubYear", 
                                           "ga_gender", 
                                           "ga_accuracy", "ga_samples")])


authorships_trend_table <- authorships_trend %>%
  filter(str_length(authorships_trend$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(PubYear, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Authorship")



authors_trend <- authorships_country_trend %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

authors_trend <- unique(
  authorships_country_trend[c("PubYear", "Surname", "First_Name", "ga_gender", 
                              "ga_accuracy", "ga_samples")])

authors_trend_table <- authors_trend %>%
  filter(str_length(authors_trend$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(PubYear, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Author")

author_authorship_trend <- rbind(authors_trend_table, authorships_trend_table)


write.csv(author_authorship_trend, "Clean exports/Authors and authorship trend.csv")

















##Last authorship========

last_authorship <-  unique(authorships_country_trend[c("Publication ID", 
                                                 "DOI", "PubYear", "AuthorOrder", 
                                                 "Surname", "First_Name", 
                                                   "ga_gender", "Country",
                                                   "ga_accuracy", "ga_samples")])



#I did not figure out how to filter just the last author (i.e. removing
#all rows from authors who are not the highest numbered author in my list)
#Workaround: I download a csv with all unique authorships, 
#and in excel I manually arrange by Author Number and delete duplicates of DOI, 
#in order to keep only the highest Author Number for each DOI

#download list
write.csv(last_authorship, "Clean exports\\last authorship.csv", 
          row.names = FALSE)


#upload modified list
last_authorship_edited <- read_csv("Clean exports\\last authorship_edited.csv")

last_authorship_ratio <- last_authorship_edited %>%
  #filter(str_length(last_authorship_edited$First_Name)>1)%>%
  filter(ga_accuracy >= 95)%>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  group_by(ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Last authorships")

last_author_ratio <- last_authorship_edited %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  filter(PubYear == 2020)

last_author_ratio <- unique(last_author_ratio[c("Surname", "First_Name", 
                                      "ga_gender", 
                                      "ga_accuracy", "ga_samples")])


last_author_ratio <- last_author_ratio %>%
  #filter(str_length(last_author_ratio$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "Last author")

overall_ratios_table <- rbind(author_ratio, authorships_ratio, last_authorship_ratio)

write.csv(overall_ratios_table, "Clean exports\\overal ratios table.csv", 
          row.names = FALSE)

overall_ratios_table %>%
ggplot(aes( x = type, y = per, fill = ga_gender))+
  geom_col(position = position_dodge())+
  scale_fill_manual(values = c("#7900F1", "#1B909A"))+
  labs( x = " ", y = "Percentage", fill = "Gender")+
  geom_text(aes(label = paste0(per, "%")), size = 4.5, position = 
              position_dodge(0.9), vjust = -0.5)+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_blank(),
        panel.background = element_blank())














##First authorship, and trends=====


first_authorship_ratio_trend <- authorships_country_trend %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")

first_authorship_ratio_trend <-  unique(first_authorship_ratio_trend[c("Publication ID","DOI", "AuthorOrder", 
                                                 "Surname", "First_Name","PubYear", 
                                                 "ga_gender", 
                                                 "ga_accuracy", "ga_samples")])


first_authorship_ratio_trend <- first_authorship_ratio_trend %>%
  filter(str_length(first_authorship_ratio_trend$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  filter(AuthorOrder == "Author1")%>%
  group_by(PubYear, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "first authorship")




first_author_ratio <- authorships_country_trend %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  filter(PubYear == 2020)

first_author_ratio <- unique(first_author_ratio[c("Surname", "First_Name", 
                                                "ga_gender", 
                                                "ga_accuracy", "ga_samples")])


first_author_ratio <- first_author_ratio %>%
  #filter(str_length(last_author_ratio$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "First author")






last_authorships_ratio_year <- last_authorship_edited %>%
  filter(ga_accuracy >= 95)%>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")%>%
  group_by(PubYear, ga_gender)%>%
  summarise(n())%>%
  mutate(per =  round(`n()`/sum(`n()`)*100, 1),
         type = "last authorship")


authorships_trends <- rbind(first_authorship_ratio_trend, last_authorships_ratio_year)


write.csv(authorships_trends , "Clean exports\\first and last authorship ratio by year.csv", 
          row.names = FALSE)

for_excel2 <- authorships_trends%>%
  filter(ga_gender == "female")%>%
  filter(type == "first authorship")%>%
  select(PubYear, per)

write.csv(for_excel2 , "Clean exports\\for_excel2.csv", 
          row.names = FALSE)




##Authors X papers====

#how many authors of each gender are there that have published X number of papers?
Authors_X_papers <- authorships_country %>%
  filter(Country == "United States"| Country == "United Kingdom" | 
           Country == "Australia" | Country == "Canada" |
           Country == "Spain" | Country == "Italy" |
           Country == "France" | Country == "Germany" |
           Country == "Sweden" | Country == "Netherlands")


Authors_X_papers <-  unique(Authors_X_papers[c("Publication ID","DOI", "AuthorOrder", 
                                                 "Surname", "First_Name", 
                                                 "ga_gender", 
                                                 "ga_accuracy", "ga_samples")])



Authors_X_papers <-  Authors_X_papers %>%
  filter(str_length(Authors_X_papers$First_Name)>1)%>%
  filter(ga_accuracy>=95)%>%
  group_by(ga_gender, First_Name, Surname)%>%
  summarise(n())%>%
  dplyr::rename(Number_of_papers =`n()`)%>%
  group_by(ga_gender, Number_of_papers)%>%
  summarise(n())%>%
  dplyr::rename(Authors_with_X_papers =`n()`)

write.csv(Authors_X_papers, "Clean exports/Authors X papers.csv")






#some visuals as I was coding, before downloading and more easily visualising in Power BI


last_authorship%>%
  filter(PubYear > 2010)%>%
  filter(ga_gender == "male" | ga_gender == "female")%>%
  ggplot()+
  geom_boxplot(aes(group = PubYear, x = PubYear, y = `Times cited`))+
  facet_wrap(~ga_gender)


last_authorship%>%
  mutate(normalised_citation = `Times cited`/(2021-PubYear))%>%
  filter(ga_gender == "male" | ga_gender == "female")%>%
  group_by(PubYear,ga_gender) %>%
  summarise(n = n(),
            avg_citation = median(normalised_citation))%>%
  ggplot(aes(x = PubYear, y = avg_citation, fill = ga_gender))+
  geom_col(position = position_dodge())+
  scale_fill_manual(values = c("#7900F1", "#1B909A"))
  
  
last_authorship%>%
  mutate(normalised_citation = `Times cited`/(2021-PubYear))%>%
  filter(ga_gender == "male" | ga_gender == "female")%>%
  group_by(PubYear,ga_gender) %>%
  summarise(n = n(),
            avg_citation = mean(normalised_citation))%>%
  ggplot(aes(x = PubYear, y = avg_citation, fill = ga_gender))+
  geom_col(position = position_dodge())+
  scale_fill_manual(values = c("#7900F1", "#1B909A")) 
  

last_authorship%>%
  mutate(normalised_citation = `Times cited`/(2021-PubYear))%>%
  filter(ga_gender == "male" | ga_gender == "female")%>%
  ggplot(aes(x = ga_gender, y = normalised_citation, fill = ga_gender))+
  geom_boxplot()




