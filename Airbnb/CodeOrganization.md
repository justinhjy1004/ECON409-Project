Code Organization for Airbnb Analysis <h1>

Given that there are multiple differing kinds of data used to analyze the effects of Airbnb work, I would divide by 
functionality (wrangling, analysis and visualization) and further dividing them into smaller files for different files
(ZHVI, ZRI, Airbnb Listings and Reviews)

The main file main.R would be used as the driver file for all analysis as well as visualization needed for the final 
report. The functions in main.R might be dependent on certain functions written for analysis and visualization (and
would be noted if sourcing is required). 
