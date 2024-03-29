
# install required packages
#install.packages("tmap", repos = "https://r-tmap.github.io/tmap/", type = "source")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")


# load the tmap and pDataLarge packages
pacman::p_load(
  "tmap",
  "spDataLarge",
  "ggplot2",
  "spatstat",
  "spatialreg",
  "spgwr",
  "spdep",
  "adehabitatHR",
  "googleway",
  "gmapsdistance",
  "leaflet",
  "dismo",
  "raster",
  "sp",
  "leaflet",
  "tmap",
  "foreign",
  "sf",
  "terra",
  "RColorBrewer",
  "stargazer",
  "rmapshaper",
  "hrbrthemes"
)

# load the dataset
data("pol_pres15")



# runoff results of komorowski and duda on choropleth
tm_shape(pol_pres15) + 
  tm_fill("II_Duda_share", palette = "RdYlGn", style = "equal", n = 2, alpha = 1, breaks = c(0, 0.49, 1), 
          labels = c("Komoroski (48.45%)", "Duda(51.55%)")) + 
  tm_borders(alpha = 0.5, col = "black") +
  tm_compass() + 
  tm_layout(title = "Runoff Round Results: Komorowski and Duda", 
            legend.text.size = 1, legend.title.size = 0.01, 
            legend.position = c("left", "bottom"), legend.title.color = "white", frame = FALSE)



# percentage of vote share by each candidate
tm_shape(pol_pres15) + 
  tm_fill("II_Duda_share", palette = "RdYlGn", style = "equal", n = 10, breaks = c(0, 0.49, 1), 
          labels = c("Komoroski (91-100)","Komoroski (90-81)", "Komoroski (80-71)","Komoroski (70-61)","Komoroski (60-51)", "Duda (51-60)","Duda (61-70)","Duda (71-80)","Duda (81-90)","Duda (91-100)")) + 
  tm_borders(alpha = 0.5, col = "black") + 
  tm_compass() + 
  tm_layout(title = "Percentage of Vote Share in Runoff Round", 
            legend.text.size = 0.55, legend.title.size = 0.01, 
            legend.position = c("left", "bottom"), legend.title.color = "white", frame = FALSE)


tm_shape(pol_pres15) + 
  tm_fill("II_Duda_share", palette = "RdYlGn", style = "equal", n = 10, breaks = c(0, 0.49, 1), 
          labels = c("Komorowski (90-100]","Komorowski (80-90]", "Komorowski (70-80]","Komorowski (60-70]","Komorowski (50-60]", "Duda (50-60]","Duda (60-70]","Duda (70-80]","Duda (80-90]","Duda (90-100]")) + 
  tm_borders(alpha = 0.5, col = "black") + 
  tm_dots("II_of_which_voting_papers_taken_from_voting_envelopes", 
          col = "blue", 
          shape = 19) +
  tm_compass() + 
  tm_layout(title = "Correlation of PVE and High Concentrated Wins for Komorowski",
            legend.text.size = 0.40, legend.title.size = 0.01, 
            legend.position = c("left", "bottom"), legend.title.color = "white", frame = FALSE)

pol_pres15_enh <- pol_pres15 %>%
  mutate(II_percentage_postal_voting_successful = II_voting_envelopes_placed_in_ballot_box / II_postal_voting_envelopes_received,
         II_percentage_postal_packages_returned = II_postal_voting_envelopes_received / II_voters_sent_postal_voting_package)
  
pol_pres15_enh$II_percentage_postal_voting_successful[is.nan(pol_pres15_enh$II_percentage_postal_voting_successful)] <- NA
pol_pres15_enh$II_percentage_postal_packages_returned[is.nan(pol_pres15_enh$II_percentage_postal_packages_returned)] <- NA

pol_pres15_enh$II_percentage_postal_voting_successful[is.infinite(pol_pres15_enh$II_percentage_postal_voting_successful)] <- NA
pol_pres15_enh$II_percentage_postal_packages_returned[is.infinite(pol_pres15_enh$II_percentage_postal_packages_returned)] <- NA
  
tm_shape(pol_pres15_enh) + 
  tm_fill("II_percentage_postal_voting_successful", palette = "-Reds", style = "equal", n = 10) + 
  tm_borders(alpha = 0.5, col = "black") + 
  tm_compass() + 
  tm_layout(title = "Percentage of Returned Postal Votes that Were Used Correctly",
            legend.text.size = 0.40, legend.title.size = 0.01, 
            legend.position = c("left", "bottom"), legend.title.color = "white", frame = FALSE)

tm_shape(pol_pres15_enh) + 
  tm_fill("II_percentage_postal_packages_returned", palette = "-Purples", style = "equal", n = 10) + 
  tm_borders(alpha = 0.5, col = "black") + 
  tm_compass() + 
  tm_layout(title = "Percentage of Postal Votes that Were Returned Successfully",
            legend.text.size = 0.40, legend.title.size = 0.01, 
            legend.position = c("left", "bottom"), legend.title.color = "white", frame = FALSE)
  


  
