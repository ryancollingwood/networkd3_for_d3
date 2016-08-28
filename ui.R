########################################################################
# networkd3 for D3 by Ryan Collingwood
#
# ui.R - Where the magic is presented
#
# Shiny visualisation built in R using networkd3 
# (of the D3 visualisation library) to explore a dataset 
# relating to Diablo III (otherwise also known as D3). 
# Other notable packages used: plotly, shinyjs.
#
# @ryancollingwood - twitter
# https://github.com/ryancollingwood - github
# www.linkedin.com/in/ryancollingwood - LinkedIn
# 
# Code is provided as is, under the MIT License
#
# Figured I would share this as finding functional and interesting examples
# of networkd3 visualisation in R
#
# Diablo® III
# Diablo is a trademark or registered trademark of Blizzard Entertainment, Inc., 
# in the U.S. and/or other countries.
#
########################################################################


library(networkD3)
library(plotly)

choices = list()
choices$character = c("Character Elites" = "character_elites", 
                      "Character Paragonlevel" = "character_paragonlevel", 
                      "Attributes Vitality" = "attributes_vitality", 
                      "Attributes Dexterity" = "attributes_dexterity", 
                      "Attributes Strength" = "attributes_strength", 
                      "Attributes Prime" = "attributes_prime", 
                      "Attributes Intelligence" = "attributes_intelligence",                       
                      "Combat Attackspeed" = "combat_attackspeed", 
                      "Combat Critchance" = "combat_critchance", 
                      "Combat Critdamage" = "combat_critdamage", 
                      "Combat Damage" = "combat_damage", 
                      "Combat Damageincrease" = "combat_damageincrease", 
                      "Combat Primaryresource" = "combat_primaryresource", 
                      "Combat Secondaryresource" = "combat_secondaryresource", 
                      "Defense Armor" = "defense_armor", 
                      "Defense Blockchance" = "defense_blockchance", 
                      "Defense Blockamountmin" = "defense_blockamountmin", 
                      "Defense Blockamountmax" = "defense_blockamountmax", 
                      "Defense Damagereduction" = "defense_damagereduction", 
                      "Defense Toughness" = "defense_toughness", 
                      "Defense Thorns" = "defense_thorns", 
                      "Restoration Life" = "restoration_life", 
                      "Restoration Healing" = "restoration_healing", 
                      "Restoration Lifesteal" = "restoration_lifesteal", 
                      "Restoration Lifeperkill" = "restoration_lifeperkill", 
                      "Restoration Lifeonhit" = "restoration_lifeonhit", 
                      "Adventure Goldfind" = "adventure_goldfind", 
                      "Adventure Magicfind" = "adventure_magicfind", 
                      "Count Set Items" = "count_set_items", 
                      "Count Ancient Items" = "count_ancient_items", 
                      "Count Legendary Items" = "count_legendary_items", 
                      "Count Sockets" = "count_sockets", 
                      "Count Gems" = "count_gems", 
                      "Count Jewels" = "count_jewels")


choices$groupBy = c("Character Class" = "character_class", 
                    "Character Dead" = "character_dead", 
                    "Character Gender" = "character_gender", 
                    "Character Hardcore" = "character_hardcore", 
                    "Character Seasonal" = "character_seasonal", 
                    "Adventure Act1 Completed" = "adventure_act1_completed", 
                    "Adventure Act2 Completed" = "adventure_act2_completed", 
                    "Adventure Act3 Completed" = "adventure_act3_completed", 
                    "Adventure Act4 Completed" = "adventure_act4_completed",
                    "Adventure Act5 Completed" = "adventure_act5_completed")

choices$sizeBy = append(choices$character, c("-NONE-" = NA), after = 0)

choices$character_class = c(
  "Barbarian" = "barbarian",
  "Crusader" = "crusader",
  "Demon Hunter" = "demon_hunter",
  "Monk" = "monk",
  "Witch Doctor" = "witch_doctor",
  "Wizard" = "wizard"
)

shinyUI(fluidPage( 
    
  headerPanel("Diablo 3 Characters"),
  #sidebat layout for choosing viz type
  
  tabsetPanel(
    tabPanel("Distributions", 
             fluidRow(
               column(3,  
                      inputPanel(
                        checkboxGroupInput("character_class", "Character Classes:",
                                           choices$character_class,
                                           selected = c("barbarian", "crusader", "demon_hunter", "monk", "witch_doctor", "wizard")
                        ),      
                        selectInput("xValue", "X Axis",
                                    choices = choices$character,
                                    selected = "character_paragonlevel"
                        ),
                        sliderInput("xValueRange", label = "",
                                    min = 0, max = 100, value = c(0,100)),
                        selectInput("yValue", "Y Axis",
                                    choices = choices$character,
                                    selected = "character_elites"
                        ),
                        sliderInput("yValueRange", label = "",
                                    min = 0, max = 100, value = c(0,100)),
                        selectInput("groupBy", "Group By",
                                    choices = choices$groupBy,
                                    selected = "character_class"
                        ),
                        selectInput("sizeBy", "Size By",
                                    choices = choices$sizeBy,
                                    selected = NA
                        ),
                        sliderInput("binValueSize", label = "Bin Size",
                                    min = 5, max = 100, value = 25)
                      )
               ),
               column(9,  
                      fluidRow(
                          plotlyOutput("scatterPlot", width = "90%")
                        ),
                      fluidRow(
                        plotlyOutput("bin2dPlot", width = "90%", height = "550px")  
                        )
                      )
             ),
             fluidRow(
               plotlyOutput("histoPlot", width = "90%")
             )
    ),
    tabPanel("Networks", 
             fluidRow(
               column(3,  
                      inputPanel(
                        checkboxGroupInput("network_character_class", "Character Classes:",
                                         choices$character_class,
                                         selected = c("barbarian", "crusader", "demon_hunter", "monk", "witch_doctor", "wizard"),
                                         inline = FALSE
                        ),
                        textInput("itemName", "Lookup:", placeholder = "Focus the network on an individual item", width = "100%"),
                        checkboxInput("networkEdges", "Display Network Edges", value = TRUE),
                        checkboxInput("networkEdgesQuarters", "Normalise Network Edges to Quantiles", value = TRUE),
                        sliderInput("networkWeightCutOff", label = "Weight Cut-Off %",
                                    min = 0, max = 100, value = 66),
                        sliderInput("networkSizeCutOff", label = "Size Cut-Off %",
                                    min = 0, max = 100, value = 66),                        
                        actionButton("getGraph", "Refresh Network")
                      )
               ),
               column(9,
                      forceNetworkOutput("itemsNetwork", width = "100%", height = "768px"),
                      #chordNetworkOutput("itemsChordDiagram", width = "100%"),
                      plotlyOutput("correlationMatrix", width = "100%", height = "768px" )
               )
             ),
             fluidRow(
               column(12,
                      verbatimTextOutput("click")
                      )
             ),
             fluidRow(
               column(6,
                      htmlOutput("networkFrameA")),
               column(6,
                      htmlOutput("networkFrameB"))
             )
      )
    )
  )
)