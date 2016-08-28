# networkd3_for_d3
Shiny visualisation built in R using networkD3 (of the D3 visualisation library) to explore a dataset relating to Diablo® III (otherwise also known as D3). Other notable packages used: plotly, shinyjs.
 
Diablo® III
Diablo is a trademark or registered trademark of Blizzard Entertainment, Inc., in the U.S. and/or other countries.

## Overview:
Given some source data in a standard tabular format, that we want to create a network (graph) visualisation of the items (Weapons, Helmet, Gloves, Boots in our Example):

| Character Name  | ... | Weapon | Helmet |
| ------------- | ------------- | ------------- | ------------- |
| Billy the Wizard  | ...  | Wand of Zapping | Wizard's Hat |
| Conan the Barbarian | ... | Sword of Smashing | Dented Helmet |
| Sally the Crusader  | ...  | Flail of Flailing | Dented Helmet |

But needs to be transposed (pivot the column and rows) into format that we can work with in a graph:

| Character Name | ... | Wand of Zapping | Wizard's Hat | Sword of Smashing |Flail of Flailing | Dented Helmet |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| Billy the Wizard | ... |  Weapon | Helmet | | | |
| Conan the Barbarian | ... | | | Weapon | | Helmet |
| Sally the Crusader  | ... | | | | Weapon | Helmet |

And then further reshaped into:

| Start Node | End Node | Occurances |
| ------------- | ------------- | ------------- |
| Wand of Zapping | Wizard's Hat | 1 |
| Dented Helmet | Sword of Smashing | 1 |
| Dented Helmet | Flail of Flailing | 1 |

Which be used to generate a network graph along the lines of:
```
(Wand of Zapping)--(Wizard's Hat)
(Sword of Smashing)--(Dented Helmet)--(Flail of Flailing)
```

## Screenshots

### Distributions
![Distributions](/images/Distributions.png)!
Explore the distribution of the data

### Network
![Network](/images/Network.png)!
Create a network (graph) visualisation of the data. 

![Correlation](/images/Correlation.png)!

![Comparison](/images/Comparison.png)!


## Installation instructions

Developed and tested in RStudio Version 0.99.896

The following packages will need to be installed:
```R
install.packages(ggplot2)
install.packages(scales)
install.packages(RColorBrewer)
install.packages(networkD3)
install.packages(igraph)
install.packages(reshape2)
install.packages(plyr)
install.packages(dplyr)
install.packages(shiny)
install.packages(shinyjs)
install.packages(plotly)
```

## Usuage

Once you've got your Shiny webapp up and running the client ui is presented in two tabs:
* Distributions - For exploring the distribution of the data
* Networks - For exploring the network, and a correlation matrix of node in the network

## TODO aka "Things that could be better"
* Optimise the transposing of data
* Handle the resizing of ui elements a bit more gracefully
* Use the Blizzard API for querying item details rather than Diablohub.com
* Import live data from the Blizzard API rather than using static data

## Final Notes

If you find this useful, let me know. Even better if you're able to improve upon it, I'd love to hear from you. I'm by no means an expert R developer, so any pointers would be welcome.

@ryancollingwood - twitter
https://github.com/ryancollingwood - github
www.linkedin.com/in/ryancollingwood - LinkedIn



