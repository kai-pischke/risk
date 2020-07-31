# Interfacing with Server.hs

## What it sends you

Here is the basic idea (will update soon):
```js
{
   "board": {
        "Alaska": [1, "Blue"],
        "Western Europe": [3, "Red"],
        ...
        "Western Australia": [7, "Green"]
    },
   "phase": {"type": "Reinforce"},
   "players": ["Blue", "Green", "Red"]
}
```
A special request to get number of defenders when being attacked:


## What you send it
