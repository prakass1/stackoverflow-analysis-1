# eda_users.R
# eda of users



#################### sentiment #######################

# create a column that stores the sentiment score for a user in their AboutMe
users <- users %>%
    rowwise() %>% 
    mutate(Sentiment = GetSentiment(AboutMe))


# overall sentiment of users AboutMe
ggplot(users, aes(x = Sentiment)) +
    geom_density(fill = "steelblue") +
    scale_x_log10() +
    scale_alpha_discrete(range = c(0,1))
    

#################### AboutMe wordcloud #######################

# create a document feature matrix of users' bios
dfm_aboutme <- users$TidyAboutMe %>% 
    tokens() %>% 
    dfm() %>% 
    dfm_trim(min_termfreq = 10, verbose = FALSE)
# plot a wordcloud
set.seed(60)
textplot_wordcloud(dfm_aboutme, max_words = 200,
                   color = c('blue', 'green', 'purple'), adjust = 0.8)


###################### gender distribution #########################

# predict gender from name
genders <- users$DisplayName %>% 
    gender()

# plot gender distribution
ggplot(genders, aes(x = factor(1), fill = factor(gender))) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    labs(x = "", y = "", title = "Gender Distribution") +
    scale_fill_manual(name = "Gender", values=c("brown3", "slateblue")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())





#################################### locations #################################### 

# 1) create a lookup table of coordinates of locations
# 2) use the lookup table to get 


# get a sorted dataframe of user locations with frequency
locations <- users %>%
    filter(Location != "") %>% 
    count(Location, sort = TRUE) %>%
    ungroup()


# get longitude and latitude of all locations and store in coordinates
locations1 <- locations[1:2500,]
locations2 <- locations[2501:4130,]

coordinates1 <- geocode(locations1$Location, source="dsk")
coordinates2 <- geocode(locations2$Location, source="dsk")
coordinates <- bind_rows(coordinates1, coordinates2)

# create a tibble of locations with lon and lat info
locations <- tibble(Location = locations$Location, n = locations$n,
       lon = coordinates$lon, lat = coordinates$lat)




# select the questions posted by users with location information and create a 
# dataframe with Tags and Locations
tag_locations <- questions %>% 
    filter(OwnerUserId %in% users_with_locations$Id) %>% 
    select(OwnerUserId, Tags) %>% 
    rowwise() %>% 
    mutate(Location = GetUserLocation(OwnerUserId)) %>% 
    ungroup()

tag_locations <- tag_locations %>% 
    arrange(Tags, Location) %>% 
    select(-OwnerUserId)





# add coordinate info of all locations by looking up the locations tibble
tag_locations <- tag_locations %>% 
    rowwise() %>% 
    mutate(lon = {
        # look up the current location in the locations tibble and get the longitude
        locations[Location == locations$Location,]$lon
    }) %>% 
    mutate(lat = {
        # look up the current location in the locations tibble and get the latitude
        locations[Location == locations$Location,]$lat
    }) 



tag_locations %>% 
    ungroup %>% 
    group_by(Tags) %>% 
    summarize(n = count(Tags))


# get users with location and their coordinates
users_with_locations <- users %>%
    filter(Location != "") %>% 
    select(Location) %>% 
    arrange(Location)

users_with_locations <- users_with_locations %>% 
    rowwise() %>% 
    mutate(lon = {
        # look up the current location in the locations tibble and get the longitude
        locations[Location == locations$Location,]$lon
    }) %>% 
    mutate(lat = {
        # look up the current location in the locations tibble and get the latitude
        locations[Location == locations$Location,]$lat
    }) 




# plotting locations on the map
map <- locations %>%  
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas ) %>% 
    # add a cluster option to the markers
    addCircleMarkers(radius = 8,
        clusterOptions = markerClusterOptions()
        #radius = ~sqrt(n), color = c('green'), fill = TRUE, fillOpacity = 0.5
    )
map  # Print the map





# tag locations

# plotting locations on the map
map <- tag_locations %>%  
    leaflet() %>%
    addTiles() %>% 
    # add a cluster option to the markers
    addCircleMarkers(radius = 8,
                     #clusterOptions = markerClusterOptions()
                     color = ~Tags
    )
map  # Print the map




################# world map of user locations with plotly ###################

g <- list(
    scope = 'world',
    projection = list(type = 'world'),
    showland = TRUE,
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
)

a <- list(
    
)

# all user locations
user_map <- plot_geo(locations, sizes = c(1, 500)) %>%
    add_markers(
        x = ~lon, y = ~lat, size = ~n, color = ~n, hoverinfo = "text",
        text = ~paste(locations$Location)
    ) %>%
    layout(title = 'User Locations', geo = g)
user_map



# tag locations
tag_map <- plot_geo(tag_locations, sizes = c(1, 500)) %>%
    add_markers(
        x = ~lon, y = ~lat, color = ~Tags, hoverinfo = "text",
        text = ~paste(tag_locations$Tags)
    ) %>%
    layout(title = 'Tag Locations', geo = g)
tag_map







###############################################################

# for shiny
g <- list(
    scope = 'world',
    projection = list(type = 'world'),
    showland = TRUE,
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
)

tag_map <- plot_geo(tl, sizes = c(1, 500)) %>%
    add_markers(
        x = ~lon, y = ~lat, colors = "Greens"
    ) %>%
    layout(title = 'Tag Locations', geo = g)
tag_map


####################################################

tag_locations %>% 
    filter(Tags %in% input$tag) %>% 
    leaflet() %>%
    addProviderTiles(providers$CartoDB.DarkMatter ) %>% 
    # add a cluster option to the markers
    addCircleMarkers(
        #clusterOptions = markerClusterOptions(),
        radius = 15, color = c('purple'), fill = TRUE, fillOpacity = 0.7,
        popup = ~as.character(Location)
    )