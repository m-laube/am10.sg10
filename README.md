# Celebrating Black Lives
## AM10 Data Visualisation | Study Group 10

In years gone by, **Black History Month** has been the only time of year when people talk about the achievements of Black People and recognise their contributions.

We want to change this by analysing **African-American Firsts**, that have historically marked footholds. These breakings of the colour barrier across a wide range of topics have often led to widespread cultural change. 

### The Data

The process of data extraction, enrichment, and cleaning is quite complex and time-consuming. We execute the following steps:

- First, we scrape a list of **African-American Firsts**, that can be found on [Wikipedia](https://en.wikipedia.org/wiki/List_of_African-American_firsts).
- After that, we scrape the individual [Wikipedia](https://en.wikipedia.org/wiki/Main_Page) webpages of each individual achiever to get their birthdays and birth locations. 
- Third, we use the scraped location information to get geospatial information via forward geocoding.
- Fourth, we infer the gender of each achiever and the category of the achievement. 
- Finally, we clean up loose ends, load shapefiles for America and enrich this geospatial data by adding [population information](https://conservancy.umn.edu/handle/11299/181605).

### The Story

After scraping and transforming the data, we are ready to celebrate **African-American Achievements**!  
In this work, we

- look at the number of **African-American Firsts** over time and draw connections to American history
- analyse where the achievers were born (geospatial analysis)
- take a critical look at the gender gap and stereotypical gender roles
- see how old the achievers were when breaking the colour barrier
- investigate which states have given rise to most achievers

Our complete work can be found under `code/final_group_project_sg10.Rmd`.

### Final Remarks

We celebrate Black Lives, their achievements, and many of their battles against racism. Even though these past achievements are outstanding, we must not stop fighting for equality!

Unfortunately, we are not yet in a perfect world, where each and everyone has the same chances and rights. **We must never forget that.** 

Additionally, we touch upon gender inequality and stereotypical gender roles. Here again the world has some way to go!

We hope that our work contributes to raising awareness about the crucial topic of racial and gender inequality.

"We are all equal and should be treated that way. Period." - Kamala Harris