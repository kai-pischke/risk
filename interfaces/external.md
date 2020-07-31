# Interfacing with Server.hs

## What it sends you

A json file to represent the current state of board:
{
    "Board" : {dict from Country to (Player, NumTroops)}
    "Phase" : ({"Phase" :"Fortify" | "Reinforce" | "Attack Normal"}
            | {"Phase" : "WonInvade", "AttackingCountry" : ... ,"DefendingCountry" : ..., "NumAttackers" : ...}
    "Players" : List of players in play order with head being current player

}

A special request to get number of defenders when being attacked:


## What you send it
