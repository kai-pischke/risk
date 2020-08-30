import { ALL_COUNTRIES } from "./elements";
export class Board {
    constructor() {
        this._board = {};
        ALL_COUNTRIES.forEach((key, index) => { this._board[key] = { player: "Empty", troops: 0 }; });
    }
    troops(c) {
        return this._board[c].troops;
    }
    owner(c) {
        return this._board[c].player;
    }
    changeTroops(c, t) {
        this._board[c].troops = t;
    }
    changeOwner(c, p) {
        this._board[c].player = p;
    }
}
