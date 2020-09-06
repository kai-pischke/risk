## Draw
```
Class Draw
```
```
Draw.draw(state: board)
Draw.addEventToCountry()
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
```
board = new Board();
ui = new Draw();
conn, me = new Sock();
moves = new Moves(me, ui);
```

## Socks
```
you_got_mail = function(mail) {
	msg = mail.parse()
	if (msg.type == "SpecialQuestion") {
		
	}
}
```

## Board
```
Class Board
```
```
Board.update(new_board: json)
Board.getTroops(c: country) : number
Board.getOwner(c: country) : Player | "Empty"
```

## Communicate

