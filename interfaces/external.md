
## Message.hs
```hs
Response = General Update | Special Question Player | Invalid Error
```
```hs
Update = WaitingRoom [Player] | Setup SetupState | Play GameState
```
```hs
Question = NumDefenders
```
```hs
Error = InvalidMove | NotTurn
```

```hs
Request = Request Player RequestType
```

```hs
RequestType =
    StartGame |
    PlaceTroop Country |
    Attack Country Country Attackers |
    Reinforce [(Country, Int)] |
    Fortify Country Country Int |
    Invade Int |
    ChooseDefenders Int
```

## Interface.hs

```hs
empty :: Game
```

```hs
receive :: Request -> Game -> (Response, Game)
```

```hs
addPlayer :: Game -> (Game, Player)
```

export

```
Response
```

```
Request
```

## Parse.hs
```
read :: Data.Text -> Maybe Request
```

```
show ::  Response -> Data.Text
```

## Server.hs

## JSON
### We receive

```
Msg = {"sender": String, "type": String, ...}
```

#### `"startGame"`
```
Msg = {...}
```

#### `"placeTroop"`
```
Msg = {..., "country": String}
```

### type can be:

#### `"attack"`
```
Msg = {..., "attackingCountry": String, "defendingCountry": String, "numberOfAttackers": Int}
```

#### `"reinforce"`
```
Msg = {..., "troops": {String: Int}}
```
(only ones being reinforced in dictionary)

#### `"fortify"`
```
Msg = {..., "fromCountry": String, "toCountry": String, "numberOfTroops": Int}
```

#### `"invade"`
```
Msg = {..., "numberOfTroops": Int}
```

#### `"chooseDefenders"`
```
Msg = {..., "numberOfDefenders": Int}
```

### We send
```
{"board": Board, "turnOrder": [String], "phase": Phase}
```

where:



```
Board :: {String: {"numberTroops": Int, "owner": String}}
```

so a dictionary from country -> (nTroops, owner)

```
Phase :: {"type": "simplePhase", "phase": String}
            | {"type": "miniPhase", "attackingCountry": String, "defendingCountry": String, "attackersRemaining": Int}
```
