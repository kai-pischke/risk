define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Board = void 0;
    class Board {
        constructor(ps, cs) {
            this._board = {};
            this._players = ps;
            this._myCards = cs;
            elements_1.ALL_COUNTRIES.forEach((key, index) => { this._board[key] = { player: "Empty", troops: 0 }; });
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
        removeCard(c) {
            const i = this._myCards.indexOf(c);
            if (i != -1) {
                this._myCards = this._myCards.splice(i, 1);
            }
        }
        get cards() {
            return this._myCards;
        }
        get players() {
            return this._players;
        }
    }
    exports.Board = Board;
});
