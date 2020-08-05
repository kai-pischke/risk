
## Message.hs 
```
Response = General Update | Special Question Player | Invalid Error 
```

```
Request = ...
```

## Interface.hs

```
empty :: Game
```

```
receive :: Request -> Game -> (Response, Game)
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


