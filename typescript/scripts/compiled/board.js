define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Board = void 0;
    class Board {
        constructor() {
            this._board = {};
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
    }
    exports.Board = Board;
});
