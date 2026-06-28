# Convert dataframe to list

Convert a dataframe to a named list, useful when converting a dataframe
a to yaml.

## Usage

``` r
convert_df_to_list(df)
```

## Arguments

- df:

  A dataframe

## Value

A (yaml) list

## Examples

``` r
convert_df_to_list(dplyr::starwars)
#> [[1]]
#> [[1]]$name
#> [1] "Luke Skywalker"
#> 
#> [[1]]$height
#> [1] 172
#> 
#> [[1]]$mass
#> [1] 77
#> 
#> [[1]]$hair_color
#> [1] "blond"
#> 
#> [[1]]$skin_color
#> [1] "fair"
#> 
#> [[1]]$eye_color
#> [1] "blue"
#> 
#> [[1]]$birth_year
#> [1] 19
#> 
#> [[1]]$sex
#> [1] "male"
#> 
#> [[1]]$gender
#> [1] "masculine"
#> 
#> [[1]]$homeworld
#> [1] "Tatooine"
#> 
#> [[1]]$species
#> [1] "Human"
#> 
#> [[1]]$films
#> [[1]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "Revenge of the Sith"    
#> [5] "The Force Awakens"      
#> 
#> 
#> [[1]]$vehicles
#> [[1]]$vehicles[[1]]
#> [1] "Snowspeeder"           "Imperial Speeder Bike"
#> 
#> 
#> [[1]]$starships
#> [[1]]$starships[[1]]
#> [1] "X-wing"           "Imperial shuttle"
#> 
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "C-3PO"
#> 
#> [[2]]$height
#> [1] 167
#> 
#> [[2]]$mass
#> [1] 75
#> 
#> [[2]]$hair_color
#> [1] NA
#> 
#> [[2]]$skin_color
#> [1] "gold"
#> 
#> [[2]]$eye_color
#> [1] "yellow"
#> 
#> [[2]]$birth_year
#> [1] 112
#> 
#> [[2]]$sex
#> [1] "none"
#> 
#> [[2]]$gender
#> [1] "masculine"
#> 
#> [[2]]$homeworld
#> [1] "Tatooine"
#> 
#> [[2]]$species
#> [1] "Droid"
#> 
#> [[2]]$films
#> [[2]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "The Phantom Menace"     
#> [5] "Attack of the Clones"    "Revenge of the Sith"    
#> 
#> 
#> [[2]]$vehicles
#> [[2]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[2]]$starships
#> [[2]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[3]]
#> [[3]]$name
#> [1] "R2-D2"
#> 
#> [[3]]$height
#> [1] 96
#> 
#> [[3]]$mass
#> [1] 32
#> 
#> [[3]]$hair_color
#> [1] NA
#> 
#> [[3]]$skin_color
#> [1] "white, blue"
#> 
#> [[3]]$eye_color
#> [1] "red"
#> 
#> [[3]]$birth_year
#> [1] 33
#> 
#> [[3]]$sex
#> [1] "none"
#> 
#> [[3]]$gender
#> [1] "masculine"
#> 
#> [[3]]$homeworld
#> [1] "Naboo"
#> 
#> [[3]]$species
#> [1] "Droid"
#> 
#> [[3]]$films
#> [[3]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "The Phantom Menace"     
#> [5] "Attack of the Clones"    "Revenge of the Sith"    
#> [7] "The Force Awakens"      
#> 
#> 
#> [[3]]$vehicles
#> [[3]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[3]]$starships
#> [[3]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[4]]
#> [[4]]$name
#> [1] "Darth Vader"
#> 
#> [[4]]$height
#> [1] 202
#> 
#> [[4]]$mass
#> [1] 136
#> 
#> [[4]]$hair_color
#> [1] "none"
#> 
#> [[4]]$skin_color
#> [1] "white"
#> 
#> [[4]]$eye_color
#> [1] "yellow"
#> 
#> [[4]]$birth_year
#> [1] 41.9
#> 
#> [[4]]$sex
#> [1] "male"
#> 
#> [[4]]$gender
#> [1] "masculine"
#> 
#> [[4]]$homeworld
#> [1] "Tatooine"
#> 
#> [[4]]$species
#> [1] "Human"
#> 
#> [[4]]$films
#> [[4]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "Revenge of the Sith"    
#> 
#> 
#> [[4]]$vehicles
#> [[4]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[4]]$starships
#> [[4]]$starships[[1]]
#> [1] "TIE Advanced x1"
#> 
#> 
#> 
#> [[5]]
#> [[5]]$name
#> [1] "Leia Organa"
#> 
#> [[5]]$height
#> [1] 150
#> 
#> [[5]]$mass
#> [1] 49
#> 
#> [[5]]$hair_color
#> [1] "brown"
#> 
#> [[5]]$skin_color
#> [1] "light"
#> 
#> [[5]]$eye_color
#> [1] "brown"
#> 
#> [[5]]$birth_year
#> [1] 19
#> 
#> [[5]]$sex
#> [1] "female"
#> 
#> [[5]]$gender
#> [1] "feminine"
#> 
#> [[5]]$homeworld
#> [1] "Alderaan"
#> 
#> [[5]]$species
#> [1] "Human"
#> 
#> [[5]]$films
#> [[5]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "Revenge of the Sith"    
#> [5] "The Force Awakens"      
#> 
#> 
#> [[5]]$vehicles
#> [[5]]$vehicles[[1]]
#> [1] "Imperial Speeder Bike"
#> 
#> 
#> [[5]]$starships
#> [[5]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[6]]
#> [[6]]$name
#> [1] "Owen Lars"
#> 
#> [[6]]$height
#> [1] 178
#> 
#> [[6]]$mass
#> [1] 120
#> 
#> [[6]]$hair_color
#> [1] "brown, grey"
#> 
#> [[6]]$skin_color
#> [1] "light"
#> 
#> [[6]]$eye_color
#> [1] "blue"
#> 
#> [[6]]$birth_year
#> [1] 52
#> 
#> [[6]]$sex
#> [1] "male"
#> 
#> [[6]]$gender
#> [1] "masculine"
#> 
#> [[6]]$homeworld
#> [1] "Tatooine"
#> 
#> [[6]]$species
#> [1] "Human"
#> 
#> [[6]]$films
#> [[6]]$films[[1]]
#> [1] "A New Hope"           "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[6]]$vehicles
#> [[6]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[6]]$starships
#> [[6]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[7]]
#> [[7]]$name
#> [1] "Beru Whitesun Lars"
#> 
#> [[7]]$height
#> [1] 165
#> 
#> [[7]]$mass
#> [1] 75
#> 
#> [[7]]$hair_color
#> [1] "brown"
#> 
#> [[7]]$skin_color
#> [1] "light"
#> 
#> [[7]]$eye_color
#> [1] "blue"
#> 
#> [[7]]$birth_year
#> [1] 47
#> 
#> [[7]]$sex
#> [1] "female"
#> 
#> [[7]]$gender
#> [1] "feminine"
#> 
#> [[7]]$homeworld
#> [1] "Tatooine"
#> 
#> [[7]]$species
#> [1] "Human"
#> 
#> [[7]]$films
#> [[7]]$films[[1]]
#> [1] "A New Hope"           "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[7]]$vehicles
#> [[7]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[7]]$starships
#> [[7]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[8]]
#> [[8]]$name
#> [1] "R5-D4"
#> 
#> [[8]]$height
#> [1] 97
#> 
#> [[8]]$mass
#> [1] 32
#> 
#> [[8]]$hair_color
#> [1] NA
#> 
#> [[8]]$skin_color
#> [1] "white, red"
#> 
#> [[8]]$eye_color
#> [1] "red"
#> 
#> [[8]]$birth_year
#> [1] NA
#> 
#> [[8]]$sex
#> [1] "none"
#> 
#> [[8]]$gender
#> [1] "masculine"
#> 
#> [[8]]$homeworld
#> [1] "Tatooine"
#> 
#> [[8]]$species
#> [1] "Droid"
#> 
#> [[8]]$films
#> [[8]]$films[[1]]
#> [1] "A New Hope"
#> 
#> 
#> [[8]]$vehicles
#> [[8]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[8]]$starships
#> [[8]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[9]]
#> [[9]]$name
#> [1] "Biggs Darklighter"
#> 
#> [[9]]$height
#> [1] 183
#> 
#> [[9]]$mass
#> [1] 84
#> 
#> [[9]]$hair_color
#> [1] "black"
#> 
#> [[9]]$skin_color
#> [1] "light"
#> 
#> [[9]]$eye_color
#> [1] "brown"
#> 
#> [[9]]$birth_year
#> [1] 24
#> 
#> [[9]]$sex
#> [1] "male"
#> 
#> [[9]]$gender
#> [1] "masculine"
#> 
#> [[9]]$homeworld
#> [1] "Tatooine"
#> 
#> [[9]]$species
#> [1] "Human"
#> 
#> [[9]]$films
#> [[9]]$films[[1]]
#> [1] "A New Hope"
#> 
#> 
#> [[9]]$vehicles
#> [[9]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[9]]$starships
#> [[9]]$starships[[1]]
#> [1] "X-wing"
#> 
#> 
#> 
#> [[10]]
#> [[10]]$name
#> [1] "Obi-Wan Kenobi"
#> 
#> [[10]]$height
#> [1] 182
#> 
#> [[10]]$mass
#> [1] 77
#> 
#> [[10]]$hair_color
#> [1] "auburn, white"
#> 
#> [[10]]$skin_color
#> [1] "fair"
#> 
#> [[10]]$eye_color
#> [1] "blue-gray"
#> 
#> [[10]]$birth_year
#> [1] 57
#> 
#> [[10]]$sex
#> [1] "male"
#> 
#> [[10]]$gender
#> [1] "masculine"
#> 
#> [[10]]$homeworld
#> [1] "Stewjon"
#> 
#> [[10]]$species
#> [1] "Human"
#> 
#> [[10]]$films
#> [[10]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "The Phantom Menace"     
#> [5] "Attack of the Clones"    "Revenge of the Sith"    
#> 
#> 
#> [[10]]$vehicles
#> [[10]]$vehicles[[1]]
#> [1] "Tribubble bongo"
#> 
#> 
#> [[10]]$starships
#> [[10]]$starships[[1]]
#> [1] "Jedi starfighter"         "Trade Federation cruiser"
#> [3] "Naboo star skiff"         "Jedi Interceptor"        
#> [5] "Belbullab-22 starfighter"
#> 
#> 
#> 
#> [[11]]
#> [[11]]$name
#> [1] "Anakin Skywalker"
#> 
#> [[11]]$height
#> [1] 188
#> 
#> [[11]]$mass
#> [1] 84
#> 
#> [[11]]$hair_color
#> [1] "blond"
#> 
#> [[11]]$skin_color
#> [1] "fair"
#> 
#> [[11]]$eye_color
#> [1] "blue"
#> 
#> [[11]]$birth_year
#> [1] 41.9
#> 
#> [[11]]$sex
#> [1] "male"
#> 
#> [[11]]$gender
#> [1] "masculine"
#> 
#> [[11]]$homeworld
#> [1] "Tatooine"
#> 
#> [[11]]$species
#> [1] "Human"
#> 
#> [[11]]$films
#> [[11]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[11]]$vehicles
#> [[11]]$vehicles[[1]]
#> [1] "Zephyr-G swoop bike" "XJ-6 airspeeder"    
#> 
#> 
#> [[11]]$starships
#> [[11]]$starships[[1]]
#> [1] "Naboo fighter"            "Trade Federation cruiser"
#> [3] "Jedi Interceptor"        
#> 
#> 
#> 
#> [[12]]
#> [[12]]$name
#> [1] "Wilhuff Tarkin"
#> 
#> [[12]]$height
#> [1] 180
#> 
#> [[12]]$mass
#> [1] NA
#> 
#> [[12]]$hair_color
#> [1] "auburn, grey"
#> 
#> [[12]]$skin_color
#> [1] "fair"
#> 
#> [[12]]$eye_color
#> [1] "blue"
#> 
#> [[12]]$birth_year
#> [1] 64
#> 
#> [[12]]$sex
#> [1] "male"
#> 
#> [[12]]$gender
#> [1] "masculine"
#> 
#> [[12]]$homeworld
#> [1] "Eriadu"
#> 
#> [[12]]$species
#> [1] "Human"
#> 
#> [[12]]$films
#> [[12]]$films[[1]]
#> [1] "A New Hope"          "Revenge of the Sith"
#> 
#> 
#> [[12]]$vehicles
#> [[12]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[12]]$starships
#> [[12]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[13]]
#> [[13]]$name
#> [1] "Chewbacca"
#> 
#> [[13]]$height
#> [1] 228
#> 
#> [[13]]$mass
#> [1] 112
#> 
#> [[13]]$hair_color
#> [1] "brown"
#> 
#> [[13]]$skin_color
#> [1] "unknown"
#> 
#> [[13]]$eye_color
#> [1] "blue"
#> 
#> [[13]]$birth_year
#> [1] 200
#> 
#> [[13]]$sex
#> [1] "male"
#> 
#> [[13]]$gender
#> [1] "masculine"
#> 
#> [[13]]$homeworld
#> [1] "Kashyyyk"
#> 
#> [[13]]$species
#> [1] "Wookiee"
#> 
#> [[13]]$films
#> [[13]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "Revenge of the Sith"    
#> [5] "The Force Awakens"      
#> 
#> 
#> [[13]]$vehicles
#> [[13]]$vehicles[[1]]
#> [1] "AT-ST"
#> 
#> 
#> [[13]]$starships
#> [[13]]$starships[[1]]
#> [1] "Millennium Falcon" "Imperial shuttle" 
#> 
#> 
#> 
#> [[14]]
#> [[14]]$name
#> [1] "Han Solo"
#> 
#> [[14]]$height
#> [1] 180
#> 
#> [[14]]$mass
#> [1] 80
#> 
#> [[14]]$hair_color
#> [1] "brown"
#> 
#> [[14]]$skin_color
#> [1] "fair"
#> 
#> [[14]]$eye_color
#> [1] "brown"
#> 
#> [[14]]$birth_year
#> [1] 29
#> 
#> [[14]]$sex
#> [1] "male"
#> 
#> [[14]]$gender
#> [1] "masculine"
#> 
#> [[14]]$homeworld
#> [1] "Corellia"
#> 
#> [[14]]$species
#> [1] "Human"
#> 
#> [[14]]$films
#> [[14]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"      "The Force Awakens"      
#> 
#> 
#> [[14]]$vehicles
#> [[14]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[14]]$starships
#> [[14]]$starships[[1]]
#> [1] "Millennium Falcon" "Imperial shuttle" 
#> 
#> 
#> 
#> [[15]]
#> [[15]]$name
#> [1] "Greedo"
#> 
#> [[15]]$height
#> [1] 173
#> 
#> [[15]]$mass
#> [1] 74
#> 
#> [[15]]$hair_color
#> [1] NA
#> 
#> [[15]]$skin_color
#> [1] "green"
#> 
#> [[15]]$eye_color
#> [1] "black"
#> 
#> [[15]]$birth_year
#> [1] 44
#> 
#> [[15]]$sex
#> [1] "male"
#> 
#> [[15]]$gender
#> [1] "masculine"
#> 
#> [[15]]$homeworld
#> [1] "Rodia"
#> 
#> [[15]]$species
#> [1] "Rodian"
#> 
#> [[15]]$films
#> [[15]]$films[[1]]
#> [1] "A New Hope"
#> 
#> 
#> [[15]]$vehicles
#> [[15]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[15]]$starships
#> [[15]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[16]]
#> [[16]]$name
#> [1] "Jabba Desilijic Tiure"
#> 
#> [[16]]$height
#> [1] 175
#> 
#> [[16]]$mass
#> [1] 1358
#> 
#> [[16]]$hair_color
#> [1] NA
#> 
#> [[16]]$skin_color
#> [1] "green-tan, brown"
#> 
#> [[16]]$eye_color
#> [1] "orange"
#> 
#> [[16]]$birth_year
#> [1] 600
#> 
#> [[16]]$sex
#> [1] "hermaphroditic"
#> 
#> [[16]]$gender
#> [1] "masculine"
#> 
#> [[16]]$homeworld
#> [1] "Nal Hutta"
#> 
#> [[16]]$species
#> [1] "Hutt"
#> 
#> [[16]]$films
#> [[16]]$films[[1]]
#> [1] "A New Hope"         "Return of the Jedi" "The Phantom Menace"
#> 
#> 
#> [[16]]$vehicles
#> [[16]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[16]]$starships
#> [[16]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[17]]
#> [[17]]$name
#> [1] "Wedge Antilles"
#> 
#> [[17]]$height
#> [1] 170
#> 
#> [[17]]$mass
#> [1] 77
#> 
#> [[17]]$hair_color
#> [1] "brown"
#> 
#> [[17]]$skin_color
#> [1] "fair"
#> 
#> [[17]]$eye_color
#> [1] "hazel"
#> 
#> [[17]]$birth_year
#> [1] 21
#> 
#> [[17]]$sex
#> [1] "male"
#> 
#> [[17]]$gender
#> [1] "masculine"
#> 
#> [[17]]$homeworld
#> [1] "Corellia"
#> 
#> [[17]]$species
#> [1] "Human"
#> 
#> [[17]]$films
#> [[17]]$films[[1]]
#> [1] "A New Hope"              "The Empire Strikes Back"
#> [3] "Return of the Jedi"     
#> 
#> 
#> [[17]]$vehicles
#> [[17]]$vehicles[[1]]
#> [1] "Snowspeeder"
#> 
#> 
#> [[17]]$starships
#> [[17]]$starships[[1]]
#> [1] "X-wing"
#> 
#> 
#> 
#> [[18]]
#> [[18]]$name
#> [1] "Jek Tono Porkins"
#> 
#> [[18]]$height
#> [1] 180
#> 
#> [[18]]$mass
#> [1] 110
#> 
#> [[18]]$hair_color
#> [1] "brown"
#> 
#> [[18]]$skin_color
#> [1] "fair"
#> 
#> [[18]]$eye_color
#> [1] "blue"
#> 
#> [[18]]$birth_year
#> [1] NA
#> 
#> [[18]]$sex
#> [1] NA
#> 
#> [[18]]$gender
#> [1] NA
#> 
#> [[18]]$homeworld
#> [1] "Bestine IV"
#> 
#> [[18]]$species
#> [1] NA
#> 
#> [[18]]$films
#> [[18]]$films[[1]]
#> [1] "A New Hope"
#> 
#> 
#> [[18]]$vehicles
#> [[18]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[18]]$starships
#> [[18]]$starships[[1]]
#> [1] "X-wing"
#> 
#> 
#> 
#> [[19]]
#> [[19]]$name
#> [1] "Yoda"
#> 
#> [[19]]$height
#> [1] 66
#> 
#> [[19]]$mass
#> [1] 17
#> 
#> [[19]]$hair_color
#> [1] "white"
#> 
#> [[19]]$skin_color
#> [1] "green"
#> 
#> [[19]]$eye_color
#> [1] "brown"
#> 
#> [[19]]$birth_year
#> [1] 896
#> 
#> [[19]]$sex
#> [1] "male"
#> 
#> [[19]]$gender
#> [1] "masculine"
#> 
#> [[19]]$homeworld
#> [1] NA
#> 
#> [[19]]$species
#> [1] "Yoda's species"
#> 
#> [[19]]$films
#> [[19]]$films[[1]]
#> [1] "The Empire Strikes Back" "Return of the Jedi"     
#> [3] "The Phantom Menace"      "Attack of the Clones"   
#> [5] "Revenge of the Sith"    
#> 
#> 
#> [[19]]$vehicles
#> [[19]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[19]]$starships
#> [[19]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[20]]
#> [[20]]$name
#> [1] "Palpatine"
#> 
#> [[20]]$height
#> [1] 170
#> 
#> [[20]]$mass
#> [1] 75
#> 
#> [[20]]$hair_color
#> [1] "grey"
#> 
#> [[20]]$skin_color
#> [1] "pale"
#> 
#> [[20]]$eye_color
#> [1] "yellow"
#> 
#> [[20]]$birth_year
#> [1] 82
#> 
#> [[20]]$sex
#> [1] "male"
#> 
#> [[20]]$gender
#> [1] "masculine"
#> 
#> [[20]]$homeworld
#> [1] "Naboo"
#> 
#> [[20]]$species
#> [1] "Human"
#> 
#> [[20]]$films
#> [[20]]$films[[1]]
#> [1] "The Empire Strikes Back" "Return of the Jedi"     
#> [3] "The Phantom Menace"      "Attack of the Clones"   
#> [5] "Revenge of the Sith"    
#> 
#> 
#> [[20]]$vehicles
#> [[20]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[20]]$starships
#> [[20]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[21]]
#> [[21]]$name
#> [1] "Boba Fett"
#> 
#> [[21]]$height
#> [1] 183
#> 
#> [[21]]$mass
#> [1] 78.2
#> 
#> [[21]]$hair_color
#> [1] "black"
#> 
#> [[21]]$skin_color
#> [1] "fair"
#> 
#> [[21]]$eye_color
#> [1] "brown"
#> 
#> [[21]]$birth_year
#> [1] 31.5
#> 
#> [[21]]$sex
#> [1] "male"
#> 
#> [[21]]$gender
#> [1] "masculine"
#> 
#> [[21]]$homeworld
#> [1] "Kamino"
#> 
#> [[21]]$species
#> [1] "Human"
#> 
#> [[21]]$films
#> [[21]]$films[[1]]
#> [1] "The Empire Strikes Back" "Return of the Jedi"     
#> [3] "Attack of the Clones"   
#> 
#> 
#> [[21]]$vehicles
#> [[21]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[21]]$starships
#> [[21]]$starships[[1]]
#> [1] "Slave 1"
#> 
#> 
#> 
#> [[22]]
#> [[22]]$name
#> [1] "IG-88"
#> 
#> [[22]]$height
#> [1] 200
#> 
#> [[22]]$mass
#> [1] 140
#> 
#> [[22]]$hair_color
#> [1] "none"
#> 
#> [[22]]$skin_color
#> [1] "metal"
#> 
#> [[22]]$eye_color
#> [1] "red"
#> 
#> [[22]]$birth_year
#> [1] 15
#> 
#> [[22]]$sex
#> [1] "none"
#> 
#> [[22]]$gender
#> [1] "masculine"
#> 
#> [[22]]$homeworld
#> [1] NA
#> 
#> [[22]]$species
#> [1] "Droid"
#> 
#> [[22]]$films
#> [[22]]$films[[1]]
#> [1] "The Empire Strikes Back"
#> 
#> 
#> [[22]]$vehicles
#> [[22]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[22]]$starships
#> [[22]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[23]]
#> [[23]]$name
#> [1] "Bossk"
#> 
#> [[23]]$height
#> [1] 190
#> 
#> [[23]]$mass
#> [1] 113
#> 
#> [[23]]$hair_color
#> [1] "none"
#> 
#> [[23]]$skin_color
#> [1] "green"
#> 
#> [[23]]$eye_color
#> [1] "red"
#> 
#> [[23]]$birth_year
#> [1] 53
#> 
#> [[23]]$sex
#> [1] "male"
#> 
#> [[23]]$gender
#> [1] "masculine"
#> 
#> [[23]]$homeworld
#> [1] "Trandosha"
#> 
#> [[23]]$species
#> [1] "Trandoshan"
#> 
#> [[23]]$films
#> [[23]]$films[[1]]
#> [1] "The Empire Strikes Back"
#> 
#> 
#> [[23]]$vehicles
#> [[23]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[23]]$starships
#> [[23]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[24]]
#> [[24]]$name
#> [1] "Lando Calrissian"
#> 
#> [[24]]$height
#> [1] 177
#> 
#> [[24]]$mass
#> [1] 79
#> 
#> [[24]]$hair_color
#> [1] "black"
#> 
#> [[24]]$skin_color
#> [1] "dark"
#> 
#> [[24]]$eye_color
#> [1] "brown"
#> 
#> [[24]]$birth_year
#> [1] 31
#> 
#> [[24]]$sex
#> [1] "male"
#> 
#> [[24]]$gender
#> [1] "masculine"
#> 
#> [[24]]$homeworld
#> [1] "Socorro"
#> 
#> [[24]]$species
#> [1] "Human"
#> 
#> [[24]]$films
#> [[24]]$films[[1]]
#> [1] "The Empire Strikes Back" "Return of the Jedi"     
#> 
#> 
#> [[24]]$vehicles
#> [[24]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[24]]$starships
#> [[24]]$starships[[1]]
#> [1] "Millennium Falcon"
#> 
#> 
#> 
#> [[25]]
#> [[25]]$name
#> [1] "Lobot"
#> 
#> [[25]]$height
#> [1] 175
#> 
#> [[25]]$mass
#> [1] 79
#> 
#> [[25]]$hair_color
#> [1] "none"
#> 
#> [[25]]$skin_color
#> [1] "light"
#> 
#> [[25]]$eye_color
#> [1] "blue"
#> 
#> [[25]]$birth_year
#> [1] 37
#> 
#> [[25]]$sex
#> [1] "male"
#> 
#> [[25]]$gender
#> [1] "masculine"
#> 
#> [[25]]$homeworld
#> [1] "Bespin"
#> 
#> [[25]]$species
#> [1] "Human"
#> 
#> [[25]]$films
#> [[25]]$films[[1]]
#> [1] "The Empire Strikes Back"
#> 
#> 
#> [[25]]$vehicles
#> [[25]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[25]]$starships
#> [[25]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[26]]
#> [[26]]$name
#> [1] "Ackbar"
#> 
#> [[26]]$height
#> [1] 180
#> 
#> [[26]]$mass
#> [1] 83
#> 
#> [[26]]$hair_color
#> [1] "none"
#> 
#> [[26]]$skin_color
#> [1] "brown mottle"
#> 
#> [[26]]$eye_color
#> [1] "orange"
#> 
#> [[26]]$birth_year
#> [1] 41
#> 
#> [[26]]$sex
#> [1] "male"
#> 
#> [[26]]$gender
#> [1] "masculine"
#> 
#> [[26]]$homeworld
#> [1] "Mon Cala"
#> 
#> [[26]]$species
#> [1] "Mon Calamari"
#> 
#> [[26]]$films
#> [[26]]$films[[1]]
#> [1] "Return of the Jedi" "The Force Awakens" 
#> 
#> 
#> [[26]]$vehicles
#> [[26]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[26]]$starships
#> [[26]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[27]]
#> [[27]]$name
#> [1] "Mon Mothma"
#> 
#> [[27]]$height
#> [1] 150
#> 
#> [[27]]$mass
#> [1] NA
#> 
#> [[27]]$hair_color
#> [1] "auburn"
#> 
#> [[27]]$skin_color
#> [1] "fair"
#> 
#> [[27]]$eye_color
#> [1] "blue"
#> 
#> [[27]]$birth_year
#> [1] 48
#> 
#> [[27]]$sex
#> [1] "female"
#> 
#> [[27]]$gender
#> [1] "feminine"
#> 
#> [[27]]$homeworld
#> [1] "Chandrila"
#> 
#> [[27]]$species
#> [1] "Human"
#> 
#> [[27]]$films
#> [[27]]$films[[1]]
#> [1] "Return of the Jedi"
#> 
#> 
#> [[27]]$vehicles
#> [[27]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[27]]$starships
#> [[27]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[28]]
#> [[28]]$name
#> [1] "Arvel Crynyd"
#> 
#> [[28]]$height
#> [1] NA
#> 
#> [[28]]$mass
#> [1] NA
#> 
#> [[28]]$hair_color
#> [1] "brown"
#> 
#> [[28]]$skin_color
#> [1] "fair"
#> 
#> [[28]]$eye_color
#> [1] "brown"
#> 
#> [[28]]$birth_year
#> [1] NA
#> 
#> [[28]]$sex
#> [1] "male"
#> 
#> [[28]]$gender
#> [1] "masculine"
#> 
#> [[28]]$homeworld
#> [1] NA
#> 
#> [[28]]$species
#> [1] "Human"
#> 
#> [[28]]$films
#> [[28]]$films[[1]]
#> [1] "Return of the Jedi"
#> 
#> 
#> [[28]]$vehicles
#> [[28]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[28]]$starships
#> [[28]]$starships[[1]]
#> [1] "A-wing"
#> 
#> 
#> 
#> [[29]]
#> [[29]]$name
#> [1] "Wicket Systri Warrick"
#> 
#> [[29]]$height
#> [1] 88
#> 
#> [[29]]$mass
#> [1] 20
#> 
#> [[29]]$hair_color
#> [1] "brown"
#> 
#> [[29]]$skin_color
#> [1] "brown"
#> 
#> [[29]]$eye_color
#> [1] "brown"
#> 
#> [[29]]$birth_year
#> [1] 8
#> 
#> [[29]]$sex
#> [1] "male"
#> 
#> [[29]]$gender
#> [1] "masculine"
#> 
#> [[29]]$homeworld
#> [1] "Endor"
#> 
#> [[29]]$species
#> [1] "Ewok"
#> 
#> [[29]]$films
#> [[29]]$films[[1]]
#> [1] "Return of the Jedi"
#> 
#> 
#> [[29]]$vehicles
#> [[29]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[29]]$starships
#> [[29]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[30]]
#> [[30]]$name
#> [1] "Nien Nunb"
#> 
#> [[30]]$height
#> [1] 160
#> 
#> [[30]]$mass
#> [1] 68
#> 
#> [[30]]$hair_color
#> [1] "none"
#> 
#> [[30]]$skin_color
#> [1] "grey"
#> 
#> [[30]]$eye_color
#> [1] "black"
#> 
#> [[30]]$birth_year
#> [1] NA
#> 
#> [[30]]$sex
#> [1] "male"
#> 
#> [[30]]$gender
#> [1] "masculine"
#> 
#> [[30]]$homeworld
#> [1] "Sullust"
#> 
#> [[30]]$species
#> [1] "Sullustan"
#> 
#> [[30]]$films
#> [[30]]$films[[1]]
#> [1] "Return of the Jedi"
#> 
#> 
#> [[30]]$vehicles
#> [[30]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[30]]$starships
#> [[30]]$starships[[1]]
#> [1] "Millennium Falcon"
#> 
#> 
#> 
#> [[31]]
#> [[31]]$name
#> [1] "Qui-Gon Jinn"
#> 
#> [[31]]$height
#> [1] 193
#> 
#> [[31]]$mass
#> [1] 89
#> 
#> [[31]]$hair_color
#> [1] "brown"
#> 
#> [[31]]$skin_color
#> [1] "fair"
#> 
#> [[31]]$eye_color
#> [1] "blue"
#> 
#> [[31]]$birth_year
#> [1] 92
#> 
#> [[31]]$sex
#> [1] "male"
#> 
#> [[31]]$gender
#> [1] "masculine"
#> 
#> [[31]]$homeworld
#> [1] NA
#> 
#> [[31]]$species
#> [1] "Human"
#> 
#> [[31]]$films
#> [[31]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[31]]$vehicles
#> [[31]]$vehicles[[1]]
#> [1] "Tribubble bongo"
#> 
#> 
#> [[31]]$starships
#> [[31]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[32]]
#> [[32]]$name
#> [1] "Nute Gunray"
#> 
#> [[32]]$height
#> [1] 191
#> 
#> [[32]]$mass
#> [1] 90
#> 
#> [[32]]$hair_color
#> [1] "none"
#> 
#> [[32]]$skin_color
#> [1] "mottled green"
#> 
#> [[32]]$eye_color
#> [1] "red"
#> 
#> [[32]]$birth_year
#> [1] NA
#> 
#> [[32]]$sex
#> [1] "male"
#> 
#> [[32]]$gender
#> [1] "masculine"
#> 
#> [[32]]$homeworld
#> [1] "Cato Neimoidia"
#> 
#> [[32]]$species
#> [1] "Neimodian"
#> 
#> [[32]]$films
#> [[32]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[32]]$vehicles
#> [[32]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[32]]$starships
#> [[32]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[33]]
#> [[33]]$name
#> [1] "Finis Valorum"
#> 
#> [[33]]$height
#> [1] 170
#> 
#> [[33]]$mass
#> [1] NA
#> 
#> [[33]]$hair_color
#> [1] "blond"
#> 
#> [[33]]$skin_color
#> [1] "fair"
#> 
#> [[33]]$eye_color
#> [1] "blue"
#> 
#> [[33]]$birth_year
#> [1] 91
#> 
#> [[33]]$sex
#> [1] "male"
#> 
#> [[33]]$gender
#> [1] "masculine"
#> 
#> [[33]]$homeworld
#> [1] "Coruscant"
#> 
#> [[33]]$species
#> [1] "Human"
#> 
#> [[33]]$films
#> [[33]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[33]]$vehicles
#> [[33]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[33]]$starships
#> [[33]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[34]]
#> [[34]]$name
#> [1] "Padmé Amidala"
#> 
#> [[34]]$height
#> [1] 185
#> 
#> [[34]]$mass
#> [1] 45
#> 
#> [[34]]$hair_color
#> [1] "brown"
#> 
#> [[34]]$skin_color
#> [1] "light"
#> 
#> [[34]]$eye_color
#> [1] "brown"
#> 
#> [[34]]$birth_year
#> [1] 46
#> 
#> [[34]]$sex
#> [1] "female"
#> 
#> [[34]]$gender
#> [1] "feminine"
#> 
#> [[34]]$homeworld
#> [1] "Naboo"
#> 
#> [[34]]$species
#> [1] "Human"
#> 
#> [[34]]$films
#> [[34]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[34]]$vehicles
#> [[34]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[34]]$starships
#> [[34]]$starships[[1]]
#> [1] "Naboo fighter"       "H-type Nubian yacht" "Naboo star skiff"   
#> 
#> 
#> 
#> [[35]]
#> [[35]]$name
#> [1] "Jar Jar Binks"
#> 
#> [[35]]$height
#> [1] 196
#> 
#> [[35]]$mass
#> [1] 66
#> 
#> [[35]]$hair_color
#> [1] "none"
#> 
#> [[35]]$skin_color
#> [1] "orange"
#> 
#> [[35]]$eye_color
#> [1] "orange"
#> 
#> [[35]]$birth_year
#> [1] 52
#> 
#> [[35]]$sex
#> [1] "male"
#> 
#> [[35]]$gender
#> [1] "masculine"
#> 
#> [[35]]$homeworld
#> [1] "Naboo"
#> 
#> [[35]]$species
#> [1] "Gungan"
#> 
#> [[35]]$films
#> [[35]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones"
#> 
#> 
#> [[35]]$vehicles
#> [[35]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[35]]$starships
#> [[35]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[36]]
#> [[36]]$name
#> [1] "Roos Tarpals"
#> 
#> [[36]]$height
#> [1] 224
#> 
#> [[36]]$mass
#> [1] 82
#> 
#> [[36]]$hair_color
#> [1] "none"
#> 
#> [[36]]$skin_color
#> [1] "grey"
#> 
#> [[36]]$eye_color
#> [1] "orange"
#> 
#> [[36]]$birth_year
#> [1] NA
#> 
#> [[36]]$sex
#> [1] "male"
#> 
#> [[36]]$gender
#> [1] "masculine"
#> 
#> [[36]]$homeworld
#> [1] "Naboo"
#> 
#> [[36]]$species
#> [1] "Gungan"
#> 
#> [[36]]$films
#> [[36]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[36]]$vehicles
#> [[36]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[36]]$starships
#> [[36]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[37]]
#> [[37]]$name
#> [1] "Rugor Nass"
#> 
#> [[37]]$height
#> [1] 206
#> 
#> [[37]]$mass
#> [1] NA
#> 
#> [[37]]$hair_color
#> [1] "none"
#> 
#> [[37]]$skin_color
#> [1] "green"
#> 
#> [[37]]$eye_color
#> [1] "orange"
#> 
#> [[37]]$birth_year
#> [1] NA
#> 
#> [[37]]$sex
#> [1] "male"
#> 
#> [[37]]$gender
#> [1] "masculine"
#> 
#> [[37]]$homeworld
#> [1] "Naboo"
#> 
#> [[37]]$species
#> [1] "Gungan"
#> 
#> [[37]]$films
#> [[37]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[37]]$vehicles
#> [[37]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[37]]$starships
#> [[37]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[38]]
#> [[38]]$name
#> [1] "Ric Olié"
#> 
#> [[38]]$height
#> [1] 183
#> 
#> [[38]]$mass
#> [1] NA
#> 
#> [[38]]$hair_color
#> [1] "brown"
#> 
#> [[38]]$skin_color
#> [1] "fair"
#> 
#> [[38]]$eye_color
#> [1] "blue"
#> 
#> [[38]]$birth_year
#> [1] NA
#> 
#> [[38]]$sex
#> [1] "male"
#> 
#> [[38]]$gender
#> [1] "masculine"
#> 
#> [[38]]$homeworld
#> [1] "Naboo"
#> 
#> [[38]]$species
#> [1] "Human"
#> 
#> [[38]]$films
#> [[38]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[38]]$vehicles
#> [[38]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[38]]$starships
#> [[38]]$starships[[1]]
#> [1] "Naboo Royal Starship"
#> 
#> 
#> 
#> [[39]]
#> [[39]]$name
#> [1] "Watto"
#> 
#> [[39]]$height
#> [1] 137
#> 
#> [[39]]$mass
#> [1] NA
#> 
#> [[39]]$hair_color
#> [1] "black"
#> 
#> [[39]]$skin_color
#> [1] "blue, grey"
#> 
#> [[39]]$eye_color
#> [1] "yellow"
#> 
#> [[39]]$birth_year
#> [1] NA
#> 
#> [[39]]$sex
#> [1] "male"
#> 
#> [[39]]$gender
#> [1] "masculine"
#> 
#> [[39]]$homeworld
#> [1] "Toydaria"
#> 
#> [[39]]$species
#> [1] "Toydarian"
#> 
#> [[39]]$films
#> [[39]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones"
#> 
#> 
#> [[39]]$vehicles
#> [[39]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[39]]$starships
#> [[39]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[40]]
#> [[40]]$name
#> [1] "Sebulba"
#> 
#> [[40]]$height
#> [1] 112
#> 
#> [[40]]$mass
#> [1] 40
#> 
#> [[40]]$hair_color
#> [1] "none"
#> 
#> [[40]]$skin_color
#> [1] "grey, red"
#> 
#> [[40]]$eye_color
#> [1] "orange"
#> 
#> [[40]]$birth_year
#> [1] NA
#> 
#> [[40]]$sex
#> [1] "male"
#> 
#> [[40]]$gender
#> [1] "masculine"
#> 
#> [[40]]$homeworld
#> [1] "Malastare"
#> 
#> [[40]]$species
#> [1] "Dug"
#> 
#> [[40]]$films
#> [[40]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[40]]$vehicles
#> [[40]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[40]]$starships
#> [[40]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[41]]
#> [[41]]$name
#> [1] "Quarsh Panaka"
#> 
#> [[41]]$height
#> [1] 183
#> 
#> [[41]]$mass
#> [1] NA
#> 
#> [[41]]$hair_color
#> [1] "black"
#> 
#> [[41]]$skin_color
#> [1] "dark"
#> 
#> [[41]]$eye_color
#> [1] "brown"
#> 
#> [[41]]$birth_year
#> [1] 62
#> 
#> [[41]]$sex
#> [1] "male"
#> 
#> [[41]]$gender
#> [1] "masculine"
#> 
#> [[41]]$homeworld
#> [1] "Naboo"
#> 
#> [[41]]$species
#> [1] "Human"
#> 
#> [[41]]$films
#> [[41]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[41]]$vehicles
#> [[41]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[41]]$starships
#> [[41]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[42]]
#> [[42]]$name
#> [1] "Shmi Skywalker"
#> 
#> [[42]]$height
#> [1] 163
#> 
#> [[42]]$mass
#> [1] NA
#> 
#> [[42]]$hair_color
#> [1] "black"
#> 
#> [[42]]$skin_color
#> [1] "fair"
#> 
#> [[42]]$eye_color
#> [1] "brown"
#> 
#> [[42]]$birth_year
#> [1] 72
#> 
#> [[42]]$sex
#> [1] "female"
#> 
#> [[42]]$gender
#> [1] "feminine"
#> 
#> [[42]]$homeworld
#> [1] "Tatooine"
#> 
#> [[42]]$species
#> [1] "Human"
#> 
#> [[42]]$films
#> [[42]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones"
#> 
#> 
#> [[42]]$vehicles
#> [[42]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[42]]$starships
#> [[42]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[43]]
#> [[43]]$name
#> [1] "Darth Maul"
#> 
#> [[43]]$height
#> [1] 175
#> 
#> [[43]]$mass
#> [1] 80
#> 
#> [[43]]$hair_color
#> [1] "none"
#> 
#> [[43]]$skin_color
#> [1] "red"
#> 
#> [[43]]$eye_color
#> [1] "yellow"
#> 
#> [[43]]$birth_year
#> [1] 54
#> 
#> [[43]]$sex
#> [1] "male"
#> 
#> [[43]]$gender
#> [1] "masculine"
#> 
#> [[43]]$homeworld
#> [1] "Dathomir"
#> 
#> [[43]]$species
#> [1] "Zabrak"
#> 
#> [[43]]$films
#> [[43]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[43]]$vehicles
#> [[43]]$vehicles[[1]]
#> [1] "Sith speeder"
#> 
#> 
#> [[43]]$starships
#> [[43]]$starships[[1]]
#> [1] "Scimitar"
#> 
#> 
#> 
#> [[44]]
#> [[44]]$name
#> [1] "Bib Fortuna"
#> 
#> [[44]]$height
#> [1] 180
#> 
#> [[44]]$mass
#> [1] NA
#> 
#> [[44]]$hair_color
#> [1] "none"
#> 
#> [[44]]$skin_color
#> [1] "pale"
#> 
#> [[44]]$eye_color
#> [1] "pink"
#> 
#> [[44]]$birth_year
#> [1] NA
#> 
#> [[44]]$sex
#> [1] "male"
#> 
#> [[44]]$gender
#> [1] "masculine"
#> 
#> [[44]]$homeworld
#> [1] "Ryloth"
#> 
#> [[44]]$species
#> [1] "Twi'lek"
#> 
#> [[44]]$films
#> [[44]]$films[[1]]
#> [1] "Return of the Jedi"
#> 
#> 
#> [[44]]$vehicles
#> [[44]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[44]]$starships
#> [[44]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[45]]
#> [[45]]$name
#> [1] "Ayla Secura"
#> 
#> [[45]]$height
#> [1] 178
#> 
#> [[45]]$mass
#> [1] 55
#> 
#> [[45]]$hair_color
#> [1] "none"
#> 
#> [[45]]$skin_color
#> [1] "blue"
#> 
#> [[45]]$eye_color
#> [1] "hazel"
#> 
#> [[45]]$birth_year
#> [1] 48
#> 
#> [[45]]$sex
#> [1] "female"
#> 
#> [[45]]$gender
#> [1] "feminine"
#> 
#> [[45]]$homeworld
#> [1] "Ryloth"
#> 
#> [[45]]$species
#> [1] "Twi'lek"
#> 
#> [[45]]$films
#> [[45]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[45]]$vehicles
#> [[45]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[45]]$starships
#> [[45]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[46]]
#> [[46]]$name
#> [1] "Ratts Tyerel"
#> 
#> [[46]]$height
#> [1] 79
#> 
#> [[46]]$mass
#> [1] 15
#> 
#> [[46]]$hair_color
#> [1] "none"
#> 
#> [[46]]$skin_color
#> [1] "grey, blue"
#> 
#> [[46]]$eye_color
#> [1] "unknown"
#> 
#> [[46]]$birth_year
#> [1] NA
#> 
#> [[46]]$sex
#> [1] "male"
#> 
#> [[46]]$gender
#> [1] "masculine"
#> 
#> [[46]]$homeworld
#> [1] "Aleen Minor"
#> 
#> [[46]]$species
#> [1] "Aleena"
#> 
#> [[46]]$films
#> [[46]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[46]]$vehicles
#> [[46]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[46]]$starships
#> [[46]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[47]]
#> [[47]]$name
#> [1] "Dud Bolt"
#> 
#> [[47]]$height
#> [1] 94
#> 
#> [[47]]$mass
#> [1] 45
#> 
#> [[47]]$hair_color
#> [1] "none"
#> 
#> [[47]]$skin_color
#> [1] "blue, grey"
#> 
#> [[47]]$eye_color
#> [1] "yellow"
#> 
#> [[47]]$birth_year
#> [1] NA
#> 
#> [[47]]$sex
#> [1] "male"
#> 
#> [[47]]$gender
#> [1] "masculine"
#> 
#> [[47]]$homeworld
#> [1] "Vulpter"
#> 
#> [[47]]$species
#> [1] "Vulptereen"
#> 
#> [[47]]$films
#> [[47]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[47]]$vehicles
#> [[47]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[47]]$starships
#> [[47]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[48]]
#> [[48]]$name
#> [1] "Gasgano"
#> 
#> [[48]]$height
#> [1] 122
#> 
#> [[48]]$mass
#> [1] NA
#> 
#> [[48]]$hair_color
#> [1] "none"
#> 
#> [[48]]$skin_color
#> [1] "white, blue"
#> 
#> [[48]]$eye_color
#> [1] "black"
#> 
#> [[48]]$birth_year
#> [1] NA
#> 
#> [[48]]$sex
#> [1] "male"
#> 
#> [[48]]$gender
#> [1] "masculine"
#> 
#> [[48]]$homeworld
#> [1] "Troiken"
#> 
#> [[48]]$species
#> [1] "Xexto"
#> 
#> [[48]]$films
#> [[48]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[48]]$vehicles
#> [[48]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[48]]$starships
#> [[48]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[49]]
#> [[49]]$name
#> [1] "Ben Quadinaros"
#> 
#> [[49]]$height
#> [1] 163
#> 
#> [[49]]$mass
#> [1] 65
#> 
#> [[49]]$hair_color
#> [1] "none"
#> 
#> [[49]]$skin_color
#> [1] "grey, green, yellow"
#> 
#> [[49]]$eye_color
#> [1] "orange"
#> 
#> [[49]]$birth_year
#> [1] NA
#> 
#> [[49]]$sex
#> [1] "male"
#> 
#> [[49]]$gender
#> [1] "masculine"
#> 
#> [[49]]$homeworld
#> [1] "Tund"
#> 
#> [[49]]$species
#> [1] "Toong"
#> 
#> [[49]]$films
#> [[49]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[49]]$vehicles
#> [[49]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[49]]$starships
#> [[49]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[50]]
#> [[50]]$name
#> [1] "Mace Windu"
#> 
#> [[50]]$height
#> [1] 188
#> 
#> [[50]]$mass
#> [1] 84
#> 
#> [[50]]$hair_color
#> [1] "none"
#> 
#> [[50]]$skin_color
#> [1] "dark"
#> 
#> [[50]]$eye_color
#> [1] "brown"
#> 
#> [[50]]$birth_year
#> [1] 72
#> 
#> [[50]]$sex
#> [1] "male"
#> 
#> [[50]]$gender
#> [1] "masculine"
#> 
#> [[50]]$homeworld
#> [1] "Haruun Kal"
#> 
#> [[50]]$species
#> [1] "Human"
#> 
#> [[50]]$films
#> [[50]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[50]]$vehicles
#> [[50]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[50]]$starships
#> [[50]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[51]]
#> [[51]]$name
#> [1] "Ki-Adi-Mundi"
#> 
#> [[51]]$height
#> [1] 198
#> 
#> [[51]]$mass
#> [1] 82
#> 
#> [[51]]$hair_color
#> [1] "white"
#> 
#> [[51]]$skin_color
#> [1] "pale"
#> 
#> [[51]]$eye_color
#> [1] "yellow"
#> 
#> [[51]]$birth_year
#> [1] 92
#> 
#> [[51]]$sex
#> [1] "male"
#> 
#> [[51]]$gender
#> [1] "masculine"
#> 
#> [[51]]$homeworld
#> [1] "Cerea"
#> 
#> [[51]]$species
#> [1] "Cerean"
#> 
#> [[51]]$films
#> [[51]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[51]]$vehicles
#> [[51]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[51]]$starships
#> [[51]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[52]]
#> [[52]]$name
#> [1] "Kit Fisto"
#> 
#> [[52]]$height
#> [1] 196
#> 
#> [[52]]$mass
#> [1] 87
#> 
#> [[52]]$hair_color
#> [1] "none"
#> 
#> [[52]]$skin_color
#> [1] "green"
#> 
#> [[52]]$eye_color
#> [1] "black"
#> 
#> [[52]]$birth_year
#> [1] NA
#> 
#> [[52]]$sex
#> [1] "male"
#> 
#> [[52]]$gender
#> [1] "masculine"
#> 
#> [[52]]$homeworld
#> [1] "Glee Anselm"
#> 
#> [[52]]$species
#> [1] "Nautolan"
#> 
#> [[52]]$films
#> [[52]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[52]]$vehicles
#> [[52]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[52]]$starships
#> [[52]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[53]]
#> [[53]]$name
#> [1] "Eeth Koth"
#> 
#> [[53]]$height
#> [1] 171
#> 
#> [[53]]$mass
#> [1] NA
#> 
#> [[53]]$hair_color
#> [1] "black"
#> 
#> [[53]]$skin_color
#> [1] "brown"
#> 
#> [[53]]$eye_color
#> [1] "brown"
#> 
#> [[53]]$birth_year
#> [1] NA
#> 
#> [[53]]$sex
#> [1] "male"
#> 
#> [[53]]$gender
#> [1] "masculine"
#> 
#> [[53]]$homeworld
#> [1] "Iridonia"
#> 
#> [[53]]$species
#> [1] "Zabrak"
#> 
#> [[53]]$films
#> [[53]]$films[[1]]
#> [1] "The Phantom Menace"  "Revenge of the Sith"
#> 
#> 
#> [[53]]$vehicles
#> [[53]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[53]]$starships
#> [[53]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[54]]
#> [[54]]$name
#> [1] "Adi Gallia"
#> 
#> [[54]]$height
#> [1] 184
#> 
#> [[54]]$mass
#> [1] 50
#> 
#> [[54]]$hair_color
#> [1] "none"
#> 
#> [[54]]$skin_color
#> [1] "dark"
#> 
#> [[54]]$eye_color
#> [1] "blue"
#> 
#> [[54]]$birth_year
#> [1] NA
#> 
#> [[54]]$sex
#> [1] "female"
#> 
#> [[54]]$gender
#> [1] "feminine"
#> 
#> [[54]]$homeworld
#> [1] "Coruscant"
#> 
#> [[54]]$species
#> [1] "Tholothian"
#> 
#> [[54]]$films
#> [[54]]$films[[1]]
#> [1] "The Phantom Menace"  "Revenge of the Sith"
#> 
#> 
#> [[54]]$vehicles
#> [[54]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[54]]$starships
#> [[54]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[55]]
#> [[55]]$name
#> [1] "Saesee Tiin"
#> 
#> [[55]]$height
#> [1] 188
#> 
#> [[55]]$mass
#> [1] NA
#> 
#> [[55]]$hair_color
#> [1] "none"
#> 
#> [[55]]$skin_color
#> [1] "pale"
#> 
#> [[55]]$eye_color
#> [1] "orange"
#> 
#> [[55]]$birth_year
#> [1] NA
#> 
#> [[55]]$sex
#> [1] "male"
#> 
#> [[55]]$gender
#> [1] "masculine"
#> 
#> [[55]]$homeworld
#> [1] "Iktotch"
#> 
#> [[55]]$species
#> [1] "Iktotchi"
#> 
#> [[55]]$films
#> [[55]]$films[[1]]
#> [1] "The Phantom Menace"  "Revenge of the Sith"
#> 
#> 
#> [[55]]$vehicles
#> [[55]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[55]]$starships
#> [[55]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[56]]
#> [[56]]$name
#> [1] "Yarael Poof"
#> 
#> [[56]]$height
#> [1] 264
#> 
#> [[56]]$mass
#> [1] NA
#> 
#> [[56]]$hair_color
#> [1] "none"
#> 
#> [[56]]$skin_color
#> [1] "white"
#> 
#> [[56]]$eye_color
#> [1] "yellow"
#> 
#> [[56]]$birth_year
#> [1] NA
#> 
#> [[56]]$sex
#> [1] "male"
#> 
#> [[56]]$gender
#> [1] "masculine"
#> 
#> [[56]]$homeworld
#> [1] "Quermia"
#> 
#> [[56]]$species
#> [1] "Quermian"
#> 
#> [[56]]$films
#> [[56]]$films[[1]]
#> [1] "The Phantom Menace"
#> 
#> 
#> [[56]]$vehicles
#> [[56]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[56]]$starships
#> [[56]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[57]]
#> [[57]]$name
#> [1] "Plo Koon"
#> 
#> [[57]]$height
#> [1] 188
#> 
#> [[57]]$mass
#> [1] 80
#> 
#> [[57]]$hair_color
#> [1] "none"
#> 
#> [[57]]$skin_color
#> [1] "orange"
#> 
#> [[57]]$eye_color
#> [1] "black"
#> 
#> [[57]]$birth_year
#> [1] 22
#> 
#> [[57]]$sex
#> [1] "male"
#> 
#> [[57]]$gender
#> [1] "masculine"
#> 
#> [[57]]$homeworld
#> [1] "Dorin"
#> 
#> [[57]]$species
#> [1] "Kel Dor"
#> 
#> [[57]]$films
#> [[57]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[57]]$vehicles
#> [[57]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[57]]$starships
#> [[57]]$starships[[1]]
#> [1] "Jedi starfighter"
#> 
#> 
#> 
#> [[58]]
#> [[58]]$name
#> [1] "Mas Amedda"
#> 
#> [[58]]$height
#> [1] 196
#> 
#> [[58]]$mass
#> [1] NA
#> 
#> [[58]]$hair_color
#> [1] "none"
#> 
#> [[58]]$skin_color
#> [1] "blue"
#> 
#> [[58]]$eye_color
#> [1] "blue"
#> 
#> [[58]]$birth_year
#> [1] NA
#> 
#> [[58]]$sex
#> [1] "male"
#> 
#> [[58]]$gender
#> [1] "masculine"
#> 
#> [[58]]$homeworld
#> [1] "Champala"
#> 
#> [[58]]$species
#> [1] "Chagrian"
#> 
#> [[58]]$films
#> [[58]]$films[[1]]
#> [1] "The Phantom Menace"   "Attack of the Clones"
#> 
#> 
#> [[58]]$vehicles
#> [[58]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[58]]$starships
#> [[58]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[59]]
#> [[59]]$name
#> [1] "Gregar Typho"
#> 
#> [[59]]$height
#> [1] 185
#> 
#> [[59]]$mass
#> [1] 85
#> 
#> [[59]]$hair_color
#> [1] "black"
#> 
#> [[59]]$skin_color
#> [1] "dark"
#> 
#> [[59]]$eye_color
#> [1] "brown"
#> 
#> [[59]]$birth_year
#> [1] NA
#> 
#> [[59]]$sex
#> [1] NA
#> 
#> [[59]]$gender
#> [1] NA
#> 
#> [[59]]$homeworld
#> [1] "Naboo"
#> 
#> [[59]]$species
#> [1] NA
#> 
#> [[59]]$films
#> [[59]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[59]]$vehicles
#> [[59]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[59]]$starships
#> [[59]]$starships[[1]]
#> [1] "Naboo fighter"
#> 
#> 
#> 
#> [[60]]
#> [[60]]$name
#> [1] "Cordé"
#> 
#> [[60]]$height
#> [1] 157
#> 
#> [[60]]$mass
#> [1] NA
#> 
#> [[60]]$hair_color
#> [1] "brown"
#> 
#> [[60]]$skin_color
#> [1] "light"
#> 
#> [[60]]$eye_color
#> [1] "brown"
#> 
#> [[60]]$birth_year
#> [1] NA
#> 
#> [[60]]$sex
#> [1] NA
#> 
#> [[60]]$gender
#> [1] NA
#> 
#> [[60]]$homeworld
#> [1] "Naboo"
#> 
#> [[60]]$species
#> [1] NA
#> 
#> [[60]]$films
#> [[60]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[60]]$vehicles
#> [[60]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[60]]$starships
#> [[60]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[61]]
#> [[61]]$name
#> [1] "Cliegg Lars"
#> 
#> [[61]]$height
#> [1] 183
#> 
#> [[61]]$mass
#> [1] NA
#> 
#> [[61]]$hair_color
#> [1] "brown"
#> 
#> [[61]]$skin_color
#> [1] "fair"
#> 
#> [[61]]$eye_color
#> [1] "blue"
#> 
#> [[61]]$birth_year
#> [1] 82
#> 
#> [[61]]$sex
#> [1] "male"
#> 
#> [[61]]$gender
#> [1] "masculine"
#> 
#> [[61]]$homeworld
#> [1] "Tatooine"
#> 
#> [[61]]$species
#> [1] "Human"
#> 
#> [[61]]$films
#> [[61]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[61]]$vehicles
#> [[61]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[61]]$starships
#> [[61]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[62]]
#> [[62]]$name
#> [1] "Poggle the Lesser"
#> 
#> [[62]]$height
#> [1] 183
#> 
#> [[62]]$mass
#> [1] 80
#> 
#> [[62]]$hair_color
#> [1] "none"
#> 
#> [[62]]$skin_color
#> [1] "green"
#> 
#> [[62]]$eye_color
#> [1] "yellow"
#> 
#> [[62]]$birth_year
#> [1] NA
#> 
#> [[62]]$sex
#> [1] "male"
#> 
#> [[62]]$gender
#> [1] "masculine"
#> 
#> [[62]]$homeworld
#> [1] "Geonosis"
#> 
#> [[62]]$species
#> [1] "Geonosian"
#> 
#> [[62]]$films
#> [[62]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[62]]$vehicles
#> [[62]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[62]]$starships
#> [[62]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[63]]
#> [[63]]$name
#> [1] "Luminara Unduli"
#> 
#> [[63]]$height
#> [1] 170
#> 
#> [[63]]$mass
#> [1] 56.2
#> 
#> [[63]]$hair_color
#> [1] "black"
#> 
#> [[63]]$skin_color
#> [1] "yellow"
#> 
#> [[63]]$eye_color
#> [1] "blue"
#> 
#> [[63]]$birth_year
#> [1] 58
#> 
#> [[63]]$sex
#> [1] "female"
#> 
#> [[63]]$gender
#> [1] "feminine"
#> 
#> [[63]]$homeworld
#> [1] "Mirial"
#> 
#> [[63]]$species
#> [1] "Mirialan"
#> 
#> [[63]]$films
#> [[63]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[63]]$vehicles
#> [[63]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[63]]$starships
#> [[63]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[64]]
#> [[64]]$name
#> [1] "Barriss Offee"
#> 
#> [[64]]$height
#> [1] 166
#> 
#> [[64]]$mass
#> [1] 50
#> 
#> [[64]]$hair_color
#> [1] "black"
#> 
#> [[64]]$skin_color
#> [1] "yellow"
#> 
#> [[64]]$eye_color
#> [1] "blue"
#> 
#> [[64]]$birth_year
#> [1] 40
#> 
#> [[64]]$sex
#> [1] "female"
#> 
#> [[64]]$gender
#> [1] "feminine"
#> 
#> [[64]]$homeworld
#> [1] "Mirial"
#> 
#> [[64]]$species
#> [1] "Mirialan"
#> 
#> [[64]]$films
#> [[64]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[64]]$vehicles
#> [[64]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[64]]$starships
#> [[64]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[65]]
#> [[65]]$name
#> [1] "Dormé"
#> 
#> [[65]]$height
#> [1] 165
#> 
#> [[65]]$mass
#> [1] NA
#> 
#> [[65]]$hair_color
#> [1] "brown"
#> 
#> [[65]]$skin_color
#> [1] "light"
#> 
#> [[65]]$eye_color
#> [1] "brown"
#> 
#> [[65]]$birth_year
#> [1] NA
#> 
#> [[65]]$sex
#> [1] "female"
#> 
#> [[65]]$gender
#> [1] "feminine"
#> 
#> [[65]]$homeworld
#> [1] "Naboo"
#> 
#> [[65]]$species
#> [1] "Human"
#> 
#> [[65]]$films
#> [[65]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[65]]$vehicles
#> [[65]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[65]]$starships
#> [[65]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[66]]
#> [[66]]$name
#> [1] "Dooku"
#> 
#> [[66]]$height
#> [1] 193
#> 
#> [[66]]$mass
#> [1] 80
#> 
#> [[66]]$hair_color
#> [1] "white"
#> 
#> [[66]]$skin_color
#> [1] "fair"
#> 
#> [[66]]$eye_color
#> [1] "brown"
#> 
#> [[66]]$birth_year
#> [1] 102
#> 
#> [[66]]$sex
#> [1] "male"
#> 
#> [[66]]$gender
#> [1] "masculine"
#> 
#> [[66]]$homeworld
#> [1] "Serenno"
#> 
#> [[66]]$species
#> [1] "Human"
#> 
#> [[66]]$films
#> [[66]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[66]]$vehicles
#> [[66]]$vehicles[[1]]
#> [1] "Flitknot speeder"
#> 
#> 
#> [[66]]$starships
#> [[66]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[67]]
#> [[67]]$name
#> [1] "Bail Prestor Organa"
#> 
#> [[67]]$height
#> [1] 191
#> 
#> [[67]]$mass
#> [1] NA
#> 
#> [[67]]$hair_color
#> [1] "black"
#> 
#> [[67]]$skin_color
#> [1] "tan"
#> 
#> [[67]]$eye_color
#> [1] "brown"
#> 
#> [[67]]$birth_year
#> [1] 67
#> 
#> [[67]]$sex
#> [1] "male"
#> 
#> [[67]]$gender
#> [1] "masculine"
#> 
#> [[67]]$homeworld
#> [1] "Alderaan"
#> 
#> [[67]]$species
#> [1] "Human"
#> 
#> [[67]]$films
#> [[67]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[67]]$vehicles
#> [[67]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[67]]$starships
#> [[67]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[68]]
#> [[68]]$name
#> [1] "Jango Fett"
#> 
#> [[68]]$height
#> [1] 183
#> 
#> [[68]]$mass
#> [1] 79
#> 
#> [[68]]$hair_color
#> [1] "black"
#> 
#> [[68]]$skin_color
#> [1] "tan"
#> 
#> [[68]]$eye_color
#> [1] "brown"
#> 
#> [[68]]$birth_year
#> [1] 66
#> 
#> [[68]]$sex
#> [1] "male"
#> 
#> [[68]]$gender
#> [1] "masculine"
#> 
#> [[68]]$homeworld
#> [1] "Concord Dawn"
#> 
#> [[68]]$species
#> [1] "Human"
#> 
#> [[68]]$films
#> [[68]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[68]]$vehicles
#> [[68]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[68]]$starships
#> [[68]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[69]]
#> [[69]]$name
#> [1] "Zam Wesell"
#> 
#> [[69]]$height
#> [1] 168
#> 
#> [[69]]$mass
#> [1] 55
#> 
#> [[69]]$hair_color
#> [1] "blonde"
#> 
#> [[69]]$skin_color
#> [1] "fair, green, yellow"
#> 
#> [[69]]$eye_color
#> [1] "yellow"
#> 
#> [[69]]$birth_year
#> [1] NA
#> 
#> [[69]]$sex
#> [1] "female"
#> 
#> [[69]]$gender
#> [1] "feminine"
#> 
#> [[69]]$homeworld
#> [1] "Zolan"
#> 
#> [[69]]$species
#> [1] "Clawdite"
#> 
#> [[69]]$films
#> [[69]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[69]]$vehicles
#> [[69]]$vehicles[[1]]
#> [1] "Koro-2 Exodrive airspeeder"
#> 
#> 
#> [[69]]$starships
#> [[69]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[70]]
#> [[70]]$name
#> [1] "Dexter Jettster"
#> 
#> [[70]]$height
#> [1] 198
#> 
#> [[70]]$mass
#> [1] 102
#> 
#> [[70]]$hair_color
#> [1] "none"
#> 
#> [[70]]$skin_color
#> [1] "brown"
#> 
#> [[70]]$eye_color
#> [1] "yellow"
#> 
#> [[70]]$birth_year
#> [1] NA
#> 
#> [[70]]$sex
#> [1] "male"
#> 
#> [[70]]$gender
#> [1] "masculine"
#> 
#> [[70]]$homeworld
#> [1] "Ojom"
#> 
#> [[70]]$species
#> [1] "Besalisk"
#> 
#> [[70]]$films
#> [[70]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[70]]$vehicles
#> [[70]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[70]]$starships
#> [[70]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[71]]
#> [[71]]$name
#> [1] "Lama Su"
#> 
#> [[71]]$height
#> [1] 229
#> 
#> [[71]]$mass
#> [1] 88
#> 
#> [[71]]$hair_color
#> [1] "none"
#> 
#> [[71]]$skin_color
#> [1] "grey"
#> 
#> [[71]]$eye_color
#> [1] "black"
#> 
#> [[71]]$birth_year
#> [1] NA
#> 
#> [[71]]$sex
#> [1] "male"
#> 
#> [[71]]$gender
#> [1] "masculine"
#> 
#> [[71]]$homeworld
#> [1] "Kamino"
#> 
#> [[71]]$species
#> [1] "Kaminoan"
#> 
#> [[71]]$films
#> [[71]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[71]]$vehicles
#> [[71]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[71]]$starships
#> [[71]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[72]]
#> [[72]]$name
#> [1] "Taun We"
#> 
#> [[72]]$height
#> [1] 213
#> 
#> [[72]]$mass
#> [1] NA
#> 
#> [[72]]$hair_color
#> [1] "none"
#> 
#> [[72]]$skin_color
#> [1] "grey"
#> 
#> [[72]]$eye_color
#> [1] "black"
#> 
#> [[72]]$birth_year
#> [1] NA
#> 
#> [[72]]$sex
#> [1] "female"
#> 
#> [[72]]$gender
#> [1] "feminine"
#> 
#> [[72]]$homeworld
#> [1] "Kamino"
#> 
#> [[72]]$species
#> [1] "Kaminoan"
#> 
#> [[72]]$films
#> [[72]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[72]]$vehicles
#> [[72]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[72]]$starships
#> [[72]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[73]]
#> [[73]]$name
#> [1] "Jocasta Nu"
#> 
#> [[73]]$height
#> [1] 167
#> 
#> [[73]]$mass
#> [1] NA
#> 
#> [[73]]$hair_color
#> [1] "white"
#> 
#> [[73]]$skin_color
#> [1] "fair"
#> 
#> [[73]]$eye_color
#> [1] "blue"
#> 
#> [[73]]$birth_year
#> [1] NA
#> 
#> [[73]]$sex
#> [1] "female"
#> 
#> [[73]]$gender
#> [1] "feminine"
#> 
#> [[73]]$homeworld
#> [1] "Coruscant"
#> 
#> [[73]]$species
#> [1] "Human"
#> 
#> [[73]]$films
#> [[73]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[73]]$vehicles
#> [[73]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[73]]$starships
#> [[73]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[74]]
#> [[74]]$name
#> [1] "R4-P17"
#> 
#> [[74]]$height
#> [1] 96
#> 
#> [[74]]$mass
#> [1] NA
#> 
#> [[74]]$hair_color
#> [1] "none"
#> 
#> [[74]]$skin_color
#> [1] "silver, red"
#> 
#> [[74]]$eye_color
#> [1] "red, blue"
#> 
#> [[74]]$birth_year
#> [1] NA
#> 
#> [[74]]$sex
#> [1] "none"
#> 
#> [[74]]$gender
#> [1] "feminine"
#> 
#> [[74]]$homeworld
#> [1] NA
#> 
#> [[74]]$species
#> [1] "Droid"
#> 
#> [[74]]$films
#> [[74]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[74]]$vehicles
#> [[74]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[74]]$starships
#> [[74]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[75]]
#> [[75]]$name
#> [1] "Wat Tambor"
#> 
#> [[75]]$height
#> [1] 193
#> 
#> [[75]]$mass
#> [1] 48
#> 
#> [[75]]$hair_color
#> [1] "none"
#> 
#> [[75]]$skin_color
#> [1] "green, grey"
#> 
#> [[75]]$eye_color
#> [1] "unknown"
#> 
#> [[75]]$birth_year
#> [1] NA
#> 
#> [[75]]$sex
#> [1] "male"
#> 
#> [[75]]$gender
#> [1] "masculine"
#> 
#> [[75]]$homeworld
#> [1] "Skako"
#> 
#> [[75]]$species
#> [1] "Skakoan"
#> 
#> [[75]]$films
#> [[75]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[75]]$vehicles
#> [[75]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[75]]$starships
#> [[75]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[76]]
#> [[76]]$name
#> [1] "San Hill"
#> 
#> [[76]]$height
#> [1] 191
#> 
#> [[76]]$mass
#> [1] NA
#> 
#> [[76]]$hair_color
#> [1] "none"
#> 
#> [[76]]$skin_color
#> [1] "grey"
#> 
#> [[76]]$eye_color
#> [1] "gold"
#> 
#> [[76]]$birth_year
#> [1] NA
#> 
#> [[76]]$sex
#> [1] "male"
#> 
#> [[76]]$gender
#> [1] "masculine"
#> 
#> [[76]]$homeworld
#> [1] "Muunilinst"
#> 
#> [[76]]$species
#> [1] "Muun"
#> 
#> [[76]]$films
#> [[76]]$films[[1]]
#> [1] "Attack of the Clones"
#> 
#> 
#> [[76]]$vehicles
#> [[76]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[76]]$starships
#> [[76]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[77]]
#> [[77]]$name
#> [1] "Shaak Ti"
#> 
#> [[77]]$height
#> [1] 178
#> 
#> [[77]]$mass
#> [1] 57
#> 
#> [[77]]$hair_color
#> [1] "none"
#> 
#> [[77]]$skin_color
#> [1] "red, blue, white"
#> 
#> [[77]]$eye_color
#> [1] "black"
#> 
#> [[77]]$birth_year
#> [1] NA
#> 
#> [[77]]$sex
#> [1] "female"
#> 
#> [[77]]$gender
#> [1] "feminine"
#> 
#> [[77]]$homeworld
#> [1] "Shili"
#> 
#> [[77]]$species
#> [1] "Togruta"
#> 
#> [[77]]$films
#> [[77]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[77]]$vehicles
#> [[77]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[77]]$starships
#> [[77]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[78]]
#> [[78]]$name
#> [1] "Grievous"
#> 
#> [[78]]$height
#> [1] 216
#> 
#> [[78]]$mass
#> [1] 159
#> 
#> [[78]]$hair_color
#> [1] "none"
#> 
#> [[78]]$skin_color
#> [1] "brown, white"
#> 
#> [[78]]$eye_color
#> [1] "green, yellow"
#> 
#> [[78]]$birth_year
#> [1] NA
#> 
#> [[78]]$sex
#> [1] "male"
#> 
#> [[78]]$gender
#> [1] "masculine"
#> 
#> [[78]]$homeworld
#> [1] "Kalee"
#> 
#> [[78]]$species
#> [1] "Kaleesh"
#> 
#> [[78]]$films
#> [[78]]$films[[1]]
#> [1] "Revenge of the Sith"
#> 
#> 
#> [[78]]$vehicles
#> [[78]]$vehicles[[1]]
#> [1] "Tsmeu-6 personal wheel bike"
#> 
#> 
#> [[78]]$starships
#> [[78]]$starships[[1]]
#> [1] "Belbullab-22 starfighter"
#> 
#> 
#> 
#> [[79]]
#> [[79]]$name
#> [1] "Tarfful"
#> 
#> [[79]]$height
#> [1] 234
#> 
#> [[79]]$mass
#> [1] 136
#> 
#> [[79]]$hair_color
#> [1] "brown"
#> 
#> [[79]]$skin_color
#> [1] "brown"
#> 
#> [[79]]$eye_color
#> [1] "blue"
#> 
#> [[79]]$birth_year
#> [1] NA
#> 
#> [[79]]$sex
#> [1] "male"
#> 
#> [[79]]$gender
#> [1] "masculine"
#> 
#> [[79]]$homeworld
#> [1] "Kashyyyk"
#> 
#> [[79]]$species
#> [1] "Wookiee"
#> 
#> [[79]]$films
#> [[79]]$films[[1]]
#> [1] "Revenge of the Sith"
#> 
#> 
#> [[79]]$vehicles
#> [[79]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[79]]$starships
#> [[79]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[80]]
#> [[80]]$name
#> [1] "Raymus Antilles"
#> 
#> [[80]]$height
#> [1] 188
#> 
#> [[80]]$mass
#> [1] 79
#> 
#> [[80]]$hair_color
#> [1] "brown"
#> 
#> [[80]]$skin_color
#> [1] "light"
#> 
#> [[80]]$eye_color
#> [1] "brown"
#> 
#> [[80]]$birth_year
#> [1] NA
#> 
#> [[80]]$sex
#> [1] "male"
#> 
#> [[80]]$gender
#> [1] "masculine"
#> 
#> [[80]]$homeworld
#> [1] "Alderaan"
#> 
#> [[80]]$species
#> [1] "Human"
#> 
#> [[80]]$films
#> [[80]]$films[[1]]
#> [1] "A New Hope"          "Revenge of the Sith"
#> 
#> 
#> [[80]]$vehicles
#> [[80]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[80]]$starships
#> [[80]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[81]]
#> [[81]]$name
#> [1] "Sly Moore"
#> 
#> [[81]]$height
#> [1] 178
#> 
#> [[81]]$mass
#> [1] 48
#> 
#> [[81]]$hair_color
#> [1] "none"
#> 
#> [[81]]$skin_color
#> [1] "pale"
#> 
#> [[81]]$eye_color
#> [1] "white"
#> 
#> [[81]]$birth_year
#> [1] NA
#> 
#> [[81]]$sex
#> [1] NA
#> 
#> [[81]]$gender
#> [1] NA
#> 
#> [[81]]$homeworld
#> [1] "Umbara"
#> 
#> [[81]]$species
#> [1] NA
#> 
#> [[81]]$films
#> [[81]]$films[[1]]
#> [1] "Attack of the Clones" "Revenge of the Sith" 
#> 
#> 
#> [[81]]$vehicles
#> [[81]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[81]]$starships
#> [[81]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[82]]
#> [[82]]$name
#> [1] "Tion Medon"
#> 
#> [[82]]$height
#> [1] 206
#> 
#> [[82]]$mass
#> [1] 80
#> 
#> [[82]]$hair_color
#> [1] "none"
#> 
#> [[82]]$skin_color
#> [1] "grey"
#> 
#> [[82]]$eye_color
#> [1] "black"
#> 
#> [[82]]$birth_year
#> [1] NA
#> 
#> [[82]]$sex
#> [1] "male"
#> 
#> [[82]]$gender
#> [1] "masculine"
#> 
#> [[82]]$homeworld
#> [1] "Utapau"
#> 
#> [[82]]$species
#> [1] "Pau'an"
#> 
#> [[82]]$films
#> [[82]]$films[[1]]
#> [1] "Revenge of the Sith"
#> 
#> 
#> [[82]]$vehicles
#> [[82]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[82]]$starships
#> [[82]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[83]]
#> [[83]]$name
#> [1] "Finn"
#> 
#> [[83]]$height
#> [1] NA
#> 
#> [[83]]$mass
#> [1] NA
#> 
#> [[83]]$hair_color
#> [1] "black"
#> 
#> [[83]]$skin_color
#> [1] "dark"
#> 
#> [[83]]$eye_color
#> [1] "dark"
#> 
#> [[83]]$birth_year
#> [1] NA
#> 
#> [[83]]$sex
#> [1] "male"
#> 
#> [[83]]$gender
#> [1] "masculine"
#> 
#> [[83]]$homeworld
#> [1] NA
#> 
#> [[83]]$species
#> [1] "Human"
#> 
#> [[83]]$films
#> [[83]]$films[[1]]
#> [1] "The Force Awakens"
#> 
#> 
#> [[83]]$vehicles
#> [[83]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[83]]$starships
#> [[83]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[84]]
#> [[84]]$name
#> [1] "Rey"
#> 
#> [[84]]$height
#> [1] NA
#> 
#> [[84]]$mass
#> [1] NA
#> 
#> [[84]]$hair_color
#> [1] "brown"
#> 
#> [[84]]$skin_color
#> [1] "light"
#> 
#> [[84]]$eye_color
#> [1] "hazel"
#> 
#> [[84]]$birth_year
#> [1] NA
#> 
#> [[84]]$sex
#> [1] "female"
#> 
#> [[84]]$gender
#> [1] "feminine"
#> 
#> [[84]]$homeworld
#> [1] NA
#> 
#> [[84]]$species
#> [1] "Human"
#> 
#> [[84]]$films
#> [[84]]$films[[1]]
#> [1] "The Force Awakens"
#> 
#> 
#> [[84]]$vehicles
#> [[84]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[84]]$starships
#> [[84]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[85]]
#> [[85]]$name
#> [1] "Poe Dameron"
#> 
#> [[85]]$height
#> [1] NA
#> 
#> [[85]]$mass
#> [1] NA
#> 
#> [[85]]$hair_color
#> [1] "brown"
#> 
#> [[85]]$skin_color
#> [1] "light"
#> 
#> [[85]]$eye_color
#> [1] "brown"
#> 
#> [[85]]$birth_year
#> [1] NA
#> 
#> [[85]]$sex
#> [1] "male"
#> 
#> [[85]]$gender
#> [1] "masculine"
#> 
#> [[85]]$homeworld
#> [1] NA
#> 
#> [[85]]$species
#> [1] "Human"
#> 
#> [[85]]$films
#> [[85]]$films[[1]]
#> [1] "The Force Awakens"
#> 
#> 
#> [[85]]$vehicles
#> [[85]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[85]]$starships
#> [[85]]$starships[[1]]
#> [1] "X-wing"
#> 
#> 
#> 
#> [[86]]
#> [[86]]$name
#> [1] "BB8"
#> 
#> [[86]]$height
#> [1] NA
#> 
#> [[86]]$mass
#> [1] NA
#> 
#> [[86]]$hair_color
#> [1] "none"
#> 
#> [[86]]$skin_color
#> [1] "none"
#> 
#> [[86]]$eye_color
#> [1] "black"
#> 
#> [[86]]$birth_year
#> [1] NA
#> 
#> [[86]]$sex
#> [1] "none"
#> 
#> [[86]]$gender
#> [1] "masculine"
#> 
#> [[86]]$homeworld
#> [1] NA
#> 
#> [[86]]$species
#> [1] "Droid"
#> 
#> [[86]]$films
#> [[86]]$films[[1]]
#> [1] "The Force Awakens"
#> 
#> 
#> [[86]]$vehicles
#> [[86]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[86]]$starships
#> [[86]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
#> [[87]]
#> [[87]]$name
#> [1] "Captain Phasma"
#> 
#> [[87]]$height
#> [1] NA
#> 
#> [[87]]$mass
#> [1] NA
#> 
#> [[87]]$hair_color
#> [1] "none"
#> 
#> [[87]]$skin_color
#> [1] "none"
#> 
#> [[87]]$eye_color
#> [1] "unknown"
#> 
#> [[87]]$birth_year
#> [1] NA
#> 
#> [[87]]$sex
#> [1] "female"
#> 
#> [[87]]$gender
#> [1] "feminine"
#> 
#> [[87]]$homeworld
#> [1] NA
#> 
#> [[87]]$species
#> [1] "Human"
#> 
#> [[87]]$films
#> [[87]]$films[[1]]
#> [1] "The Force Awakens"
#> 
#> 
#> [[87]]$vehicles
#> [[87]]$vehicles[[1]]
#> character(0)
#> 
#> 
#> [[87]]$starships
#> [[87]]$starships[[1]]
#> character(0)
#> 
#> 
#> 
```
