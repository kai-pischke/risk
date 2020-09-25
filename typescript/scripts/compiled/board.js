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
        numberToReinforce(p) {
            const playersCountries = elements_1.ALL_COUNTRIES.filter((c) => { return this.owner(c) == p; });
            const northAmerica = ["Alaska", "Alberta", "Central America", "Eastern United States", "Greenland", "Northwest Territory", "Ontario", "Quebec", "Western United States"];
            const southAmerica = ["Argentina", "Brazil", "Peru", "Venezuela"];
            const europe = ["Great Britain", "Iceland", "Northern Europe", "Scandinavia", "Southern Europe", "Ukraine", "Western Europe"];
            const africa = ["Congo", "East Africa", "Egypt", "Madagascar", "North Africa", "South Africa"];
            const asia = ["Afghanistan", "China", "India", "Irkutsk", "Japan", "Kamchatka", "Middle East", "Mongolia", "Siam", "Siberia", "Ural", "Yakutsk"];
            const australia = ["Eastern Australia", "Indonesia", "New Guinea", "Western Australia"];
            const continents = [[5, northAmerica], [2, southAmerica], [5, europe], [3, africa], [7, asia], [2, australia]];
            const fromCountry = Math.floor(playersCountries.length / 3);
            var numTroops = Math.max(fromCountry, 3);
            console.log(numTroops);
            for (var i = 0; i < continents.length; i++) {
                if (continents[i][1].every((c) => { return playersCountries.includes(c); })) {
                    numTroops += continents[i][0];
                }
            }
            console.log(numTroops);
            return numTroops;
        }
    }
    exports.Board = Board;
});
