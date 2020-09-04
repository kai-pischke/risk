import {Country, Player, ALL_COUNTRIES} from "./elements";

interface BoardCell {
    player: Player | "Empty";
    troops: number;
}

type State = Record<Country, BoardCell>;

export class Board {

    private _board: State;

    constructor() {
        this._board = {} as State;
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
        console.log(p);
        this._board[c].player = p;
    }
}
