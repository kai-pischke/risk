## Draw
```
Class Draw
```
```
Draw.draw(state: board)
```
## Moves
```
async Moves.placeTroop(state: board) : country
async Moves.reinforce(state: board, troops: number) : Record<country, number>
async Moves.chooseDefenders(state: board, to: country, from: country, attackers: number) : number
async Moves.invade(state: board, to: country, from: country, attackers: number) : number
async Moves.attack(state: board) : (country, country, number) | EndAttack
async Moves.fortify(state: board) : (country, country, number)
```
## Main
Main is for passing messages back and forth from all the other components
 - All passing is done on the document
 - Mostly passing messages in from sock to moves
 - And passing messages out from moves to sock

```
board = new Board();
ui = new Draw();
conn, me = new Sock();
moves = new Moves(me, ui);
```

## Socks
```
Class Connection
```
### Functions
```
Connection.start() : Promise<string>
```
Returns the promise that it will tell you what colour you are

```
Connection.send(info:string)
```
Attempts to send info via the socket

```
Connection.start_game()
```
Attempts to send the message to start the game via the socket

### Events

## Board
```
Class Board
```
```
Board.getTroops(c: country) : number
Board.getOwner(c: country) : Player | "Empty"
Board.changeTroops(c :Country, t: number)
Board.changeOwner(c: Country, p: Player)
```

## Communicate
