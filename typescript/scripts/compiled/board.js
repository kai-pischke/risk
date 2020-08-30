define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Board = void 0;
    var Board = /** @class */ (function () {
        function Board() {
            var _this = this;
            this._board = {};
            elements_1.ALL_COUNTRIES.forEach(function (key, index) { _this._board[key] = { player: "Empty", troops: 0 }; });
        }
        Board.prototype.troops = function (c) {
            return this._board[c].troops;
        };
        Board.prototype.owner = function (c) {
            return this._board[c].player;
        };
        Board.prototype.changeTroops = function (c, t) {
            this._board[c].troops = t;
        };
        Board.prototype.changeOwner = function (c, p) {
            this._board[c].player = p;
        };
        return Board;
    }());
    exports.Board = Board;
});
