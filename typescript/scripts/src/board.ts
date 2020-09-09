import {Country, Player, Card, ALL_COUNTRIES} from "./elements";

interface BoardCell {
    player: Player | "Empty";
    troops: number;
}

type State = Record<Country, BoardCell>;

export class Board {

    private _board: State;
    private _players: Array<Player>;
    private _myCards: Array<Card>;

    constructor(ps : Array<Player>, cs : Array<Card>) {
        this._board = {} as State;
        this._players = ps
        this._myCards = cs
        ALL_COUNTRIES.forEach((key, index) => {this._board[key] = {player: "Empty", troops: 0}});
    }

    troops(c: Country) : number {
        return this._board[c].troops;
    }

    owner(c: Country) : Player | "Empty" {
        return this._board[c].player;
    }

    changeTroops(c: Country, t: number) {
        this._board[c].troops = t;
    }

    changeOwner(c: Country, p: Player) {
        this._board[c].player = p;
    }

    removeCard(c : Card) {
        const i = this._myCards.indexOf(c)
        if (i != -1){
            this._myCards = this._myCards.splice(i,1)
        }
    }

    get cards() {
        return this._myCards;
    }

    get players() {
        return this._players;
    }
}
