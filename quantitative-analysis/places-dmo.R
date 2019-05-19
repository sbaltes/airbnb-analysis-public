# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

source("../read-and-clean-data.R")
source("../colors.R")

library(stringr)
library(purrr)

# analysis of places mentioned by DMO

n <- nrow(reuterkiez)
n
# 750
descriptions <- reuterkiez$description

matches <- compact(str_extract_all(descriptions, "(?i:.*neuk(o|oe|ö)lln[^\n\t]*)"))
length(matches)
# 468
length(matches)/n*100
# 62.4
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*richard[^\n\t]*)"))
length(matches)
# 1
length(matches)/n*100
# 0.1333333
matches
# [[1]]
# [1] "Close to Weserstraße, Sonnenallee, Karl-Marx-Straße, Schiller- and Richardkiez."

matches <- compact(str_extract_all(descriptions, "(?i:.*britz[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*gutspark[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*crop[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*kindl[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 4 -> 0
matches
# not actual match

matches <- compact(str_extract_all(descriptions, "(?i:.*gropius[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 1
length(matches)/n*100
# 0.1333333
matches
# [[1]]
# [1] "You will find all the public transportation that you need in order to get through Berlin. In 2 minutes walk, you can reach the metro station U8, U7 and Bus M29 (which is quite special because it passes by the Jewish Museum, Checkpoint Charlie, the Martin Gropius Bau, Potsdamer Platz, the New National Gallery and the KuDamm)."

matches <- compact(str_extract_all(descriptions, "(?i:.*kreuzberg[^\n\t]*)"))
matches <- matches[!is.na(matches)]
length(matches)
# 227
length(matches)/n*100
# 30.26667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*kreuz( |-)?k(o|oe|ö)lln[^\n\t]*)"))
length(matches)
# 161
length(matches)/n*100
# 21.46667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*vecchia[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*schiller[^\n\t]*)"))
length(matches)
# 5
length(matches)/n*100
# 0.6666667
matches
# [[1]]
# [1] "Close to Weserstraße, Sonnenallee, Karl-Marx-Straße, Schiller- and Richardkiez."
# [[2]]
# [1] "The apartment is located close to Tempelhofer Flugfeld, Reuter Kiez, Schiller Kiez, Kreuzberg, Maybachufer and Hermannplatz. "
# [2] "The apartment is located close to Tempelhofer Flugfeld, Reuter Kiez, Schiller Kiez, Kreuzberg, Maybachufer and Hermannplatz." 
# [[3]]
# [1] "It's the perfect room for a couple or one person as well, very bright and quiet. The flat is situated in the center of Neukölln. it's easygoing but with lots of interesting places to explore! There are many venues, second hand shops, galleries, cafés, bars etc... And each of them with that special Berlin character -Reuterkiez, Schillerkiez and Weserstraße with this flat you are just in the middle of it! (:"
# [[4]]
# [1] "Another great neighbourhood to note nearby is the Schillerkiez, very close to the renowned Tempeholfer Feld — a must-see in the warmer months in Berlin! I encourage you to check out my recommended places to really get a sense of the locality and variety that the area provides!"
# [[5]]
# [1] "A private room with shared kitchen, bathroom and living room. Just around the corner from an entrance to the Hermannplatz U-Bahn station, within walking distance to such delights as Maybachufer, Klunkerkranich, Crossanterie and Templehofer Feld. Discover 4 awesome Kieze nearby: Reuterkiez, Graefekiez, Schillerkiez and Bergmannkiez. Good for couples, solo adventurers, and business travelers."

matches <- compact(str_extract_all(descriptions, "(?i:.*eins( |-)?44[^\n\t]*)"))
length(matches)
# 0


matches <- compact(str_extract_all(descriptions, "(?i:.landwehr[^\n\t]*)"))
length(matches)
# 92
length(matches)/n*100
# 12.26667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:maybach[^\n\t]*)"))
length(matches)
# 167
length(matches)/n*100
# 22.26667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:sonnenallee[^\n\t]*)"))
length(matches)
# 62
length(matches)/n*100
# 8.266667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:hermannstr[^\n\t]*)"))
length(matches)
# 7
length(matches)/n*100
# 0.9333333
matches
# [[1]]
# [1] "Hermannstrasse is about 9 minutes away.  Several buses a few minutes away as well."
# [[2]]
# [1] "Hermannstraße and there are several bus stops 2 walking minutes away. It's a 10 minutes U-Bahn ride to Alexanderplatz but renting a bike is also an option (there are several bike rental places right around the corner). Both Kotti and Görlitzer Park are in walking distance."
# [[3]]
# [1] "Hermannstrasse, Görlitzer Park, heaps of restaurants, bars and shops in this lovely area."
# [[4]]
# [1] "Hermannstrasse or a tube ride to Hermannplatz, or a bus and train ride (c. 45 minutes in total) from Tegel airport. Exit at U8 \"\"Schönleinstrasse\"\". I will explain you in detail how to find the place."
# [[5]]
# [1] "Hermannstraße"
# [[6]]
# [1] "Hermannstraße"
# [[7]]
# [1] "Hermannstraße (U7). It takes you 10 min by subway to Berlin Alexanderplatz and Berlin Mitte with all its famous sights. Bus M29 and M41 is nearby running to Potsdamer Platz and Ostbahnhof."

matches <- compact(str_extract_all(descriptions, "(?i:.*heimat( |-)?hafen[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*oper[^\n\t]*)"))
length(matches)
# 9
length(matches)/n*100
# 1.2
matches
# ...
# no actual matches

matches <- compact(str_extract_all(descriptions, "(?i:.* ä [^\n\t]*)"))
length(matches)
# 1
length(matches)/n*100
# 0.1333333
matches
# [[1]]
# [1] "Direkt Gegenüber von der legendären Tier -Bar. Im Haus der Ä Kneipe. Viele nette Restaurants und Bars in unmittelbarer Nachbarschaft. Landwehrkanal 2 min zu Fuss."

matches <- compact(str_extract_all(descriptions, "(?i:.*weser( |-)?str[^\n\t]*)"))
length(matches)
# 189
length(matches)/n*100
# 25.2
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.* tier [^\n\t]*)"))
length(matches)
# 3
length(matches)/n*100
# 0.4
matches
# [[1]]
# [1] "Direkt Gegenüber von der legendären Tier -Bar. Im Haus der Ä Kneipe. Viele nette Restaurants und Bars in unmittelbarer Nachbarschaft. Landwehrkanal 2 min zu Fuss."
# [[2]]
# [1] "The Canal is walking distance for a nice walk where you can also find with a lovely selection of bars and cafes. There is a shopping center called The Neukölln Arcaden just 5 minutes from the apartment where you can also do your groceries. The popular Weserstrasse is located just walking distance as well where you will find cool living-room bars like Tier or Ä. Next door is my favourite cafe/bar called Kindl Stuben. Kreuzberg is also a walking distance otherwise conveniently accessible by bus or Ubahn. And of course, a plethora of places to eat at; pizza, sushi, shawarma, burgers, tapas or walk towards Kreuzkölln (area somewhere between Neukölln/Kreuzberg) and go to California Breakfast Slam where they serve an awesome selection of American style breakfast yummies!"
# [[3]]
# [1] "Some nice places not so far from where we live(URL HIDDEN)K-Fetisch (directly at the corner Wildenbruch-/Weserstr. – left alternative café, nice, cosy(URL HIDDEN)Das Tier (Weser/Fulda – very good cocktails(URL HIDDEN)Californian Breakfast Slam (very good breakfast; (URL HIDDEN)Weser/Weichselstr. (good Tapans => Gaston, nice restaurant => Beusters; very good pizzas => (URL HIDDEN)Very good coffee on sonnenallee: Espera, Sonnenallee 3(URL HIDDEN)Very good and cheap food & nice bar: Kindl Stubn on Sonnenallee 92(URL HIDDEN)My absolut favourite: The “Knödelwirtschaft” in Fuldastr. (URL HIDDEN)"

matches <- compact(str_extract_all(descriptions, "(?i:.*vin aqua[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*k(o|oe|ö)rner[^\n\t]*)"))
length(matches)
# 1
length(matches)/n*100
# 0.1333333
matches
# [[1]]
# [1] "KÖRNERPARK:"


#-----------------------------------------------------------------------------------------------


n <- nrow(kudamm_kantstrasse)
n
# 210
descriptions <- kudamm_kantstrasse$description

matches <- compact(str_extract_all(descriptions, "(?i:.*charlottenburg[^\n\t]*)"))
length(matches)
# 90
length(matches)/n*100
# 42.85714
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*wilmersdorf[^\n\t]*)"))
length(matches)
# 43
length(matches)/n*100
# 20.47619
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*bikini[^\n\t]*)"))
length(matches)
# 19
length(matches)/n*100
# 9.047619
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*teufels( |-)?berg[^\n\t]*)"))
length(matches)
# 1
length(matches)/n*100
# 0.4761905
matches
# [[1]]
# [1] "As the flat is in the heart of Berlin, placed in Charlottenburg, it is very well connected to any public transport like bus (long (ZOB) and short distance, 2mins to walk), U-Bahn (metro, 4mins walk), S-Bahn und Fernbahn (train, long and short distance, 5mins walk) and well connected to the main highways (5mins drive) you can besides very well also just walk around as there are two street markets with fresh food (2+4mins by feet), the biggest car-free walking mile of Berlin with many stores (Wilmersdorfer Straße, 4mins by feet), the famous Kurfürstendamm / KuDamm (7 mins by feet) and some parks with lakes reachable (Tiergarten = 20 mins walk / 3 train stations=9min - there is also one of the best flee markets of Berlin, Schloss Charlottenburg = 25 mins walk, Lietzenseepark = 15 mins walk), the amazing huge and calm Grunewald (3 train stations = 13mins) - this is my special secret tip as well as Teufelsberg to really breath in and relax."

matches <- compact(str_extract_all(descriptions, "(?i:.*a( |-)?trane[^\n\t]*)"))
length(matches)
# 7
length(matches)/n*100
# 3.333333
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*br(o|oe|ö)han[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*r(u|ue|ü)desheimer[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*city( |-)?west[^\n\t]*)"))
length(matches)
# 16
length(matches)/n*100
# 7.619048
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*ku[^\n\t]+damm[^\n\t]*)"))
length(matches)
# 119
length(matches)/n*100
# 56.66667
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*grunewald[^\n\t]*)"))
length(matches)
# 3
length(matches)/n*100
# 1.428571
matches
# [[1]]
# [1] "The very center of the city west: KaDeWe, Kurfürstendamm, Grunewald, Tiergarten, Berlin Zoo, Shopping Malls (Bikini Berlin), Restaurants, Bars, Cafe's, Shopping-Streets, Markets (every wed. and sat.)"
# [[2]]
# [1] "Grunewald - a stroll through the woods "
# [[3]]
# [1] "As the flat is in the heart of Berlin, placed in Charlottenburg, it is very well connected to any public transport like bus (long (ZOB) and short distance, 2mins to walk), U-Bahn (metro, 4mins walk), S-Bahn und Fernbahn (train, long and short distance, 5mins walk) and well connected to the main highways (5mins drive) you can besides very well also just walk around as there are two street markets with fresh food (2+4mins by feet), the biggest car-free walking mile of Berlin with many stores (Wilmersdorfer Straße, 4mins by feet), the famous Kurfürstendamm / KuDamm (7 mins by feet) and some parks with lakes reachable (Tiergarten = 20 mins walk / 3 train stations=9min - there is also one of the best flee markets of Berlin, Schloss Charlottenburg = 25 mins walk, Lietzenseepark = 15 mins walk), the amazing huge and calm Grunewald (3 train stations = 13mins) - this is my special secret tip as well as Teufelsberg to really breath in and relax."

matches <- compact(str_extract_all(descriptions, "(?i:.*berlin( |-)wall[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*east( |-)berlin[^\n\t]*)"))
length(matches)
# 3
length(matches)/n*100
# 1.428571
matches
# [[1]]
# [1] "Charming Berlin 3room Apt, quiet+bright!  In the very center of Berlin West. 10min to airport (TXL), no car is needed to explore the city. 2 Bikes available and a very good public transportation (15 min to famous East Berlin) Have a look!"
# [[2]]
# [1] "Just off the Kurfürstendamm – main shopping artery in West Berlin – the street is quiet, leafy with lots of good restaurants, galleries, shops and cafes, as well as big department stores (e.g. KaDeWe). Everything you need is in walking distance. Public transportation gets you anywhere in the city in no time – 15 minutes to central East Berlin, Kreuzberg, etc. And a 24 hour taxi stand right on corner."
# [[3]]
# [1] "You have easy access to all you need, great connection to East Berlin and you are in the heart of West Berlin."

matches <- compact(str_extract_all(descriptions, "(?i:.*west( |-)berlin[^\n\t]*)"))
length(matches)
# 28
length(matches)/n*100
# 13.33333
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*(waldorf|astoria)[^\n\t]*)"))
length(matches)
# 2
length(matches)/n*100
# 0.952381
matches
# [[1]]
# [1] "There are plenty hotels (Steigenberger, Waldorf Astoria) and supermarkets in close proximity."
# [[2]]
# [1] "Very special ambience, quiet but in the middle of in the exciting life of berlin city west. Near Savignyplatz and Ku-damm, Park Area for sports, Waldorf Astoria, Bikini Haus, Paris Bar and C/O Berlin Gallery. Many Cafés and Restaurants close by. "
# [2] "and very close (5 min.) to the Hotel Waldorf Astoria, Bikinihaus und C/O Berlin Galerie"      

matches <- compact(str_extract_all(descriptions, "(?i:.*(schlo(ss?|ß)( |-)charlottenburg|charlottenburg( |-)palace)[^\n\t]*)"))
length(matches)
# 20
length(matches)/n*100
# 9.52381
matches
# ...
 
matches <- compact(str_extract_all(descriptions, "(?i:.*schlo(ss?|ß)-?str[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*fasanen[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*festspiele[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*vernunft[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*uhland-?str[^\n\t]*)"))
length(matches)
# 7
length(matches)/n*100
# 3.333333
matches
# ...

matches <- compact(str_extract_all(descriptions, "(?i:.*ludwig[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*havel[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*olymp[^\n\t]*)"))
length(matches)
# 5
length(matches)/n*100
# 2.380952
matches
# [[1]]
# [1] "- Olympiastadion"
# [[2]]
# [1] "There is access to U-Bahn (U7 leading to Kreuzberg/Neukölln) Busses (leading to Zoo and Olympiastadion) and the S-Bahn and Regional Trains (leading to City-Center or even to Potsdam). Sanssouci Palace is only half an hour away."
# [[3]]
# [1] "Ernst-Reuter-Platz (U2) via Prenzlauer Berg/Potsdamer Platz/Schöneberg/Olympiastadion"
# [[4]]
# [1] "My place is close to Zoologicher garten, Kuddam, Tiergarten, Olympiastadion Berlin and Schloss charlottenburg. You’ll love my place because of the coziness, the high ceilings, the views, the location, and the people."
# [[5]]
# [1] "Mitte, Hackescher Markt, Brandenburg Gate, Unter den Linden, Friedrichstraße, Museum Island, Potsdamer Platz, Alexanderplatz, Kreuzberg, Friedrichshain, Berghain, RAW (party location), Olympiastaion."

matches <- compact(str_extract_all(descriptions, "(?i:.*glocken[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*wald( |-)?b(u|ue|ü)hne[^\n\t]*)"))
length(matches)
# 0

matches <- compact(str_extract_all(descriptions, "(?i:.*funkturm|(television|radio)( |-)?tower[^\n\t]*)"))
length(matches)
# 2
length(matches)/n*100
# 0.952381
matches
# [[1]]
# [1] "television tower. "
# [[2]]
# [1] "- Funkturm"

matches <- compact(str_extract_all(descriptions, "(?i:.*wann( |-)?see[^\n\t]*)"))
length(matches)
# 2
length(matches)/n*100
# 0.952381
matches
# [[1]]
# [1] "S7 regional train direction S Wannsee Bhf (Berlin) "
# [[2]]
# [1] "Mit der S-Bahn ist man in etwa 15 Minuten am Wannsee bzw. in 10 Minuten am Zoo wo man wunderbar am Wasser und im Park entspannen kann."

matches <- compact(str_extract_all(descriptions, "(?i:.*grunewald( |-)?tower[^\n\t]*)"))
length(matches)
# 0
