define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Moves = void 0;
    class Moves {
        constructor(whoAmI, whatIsUi) {
            this.me = whoAmI;
            this.ui = whatIsUi;
        }
        numberToReinforce(board, p) {
            const playersCountries = elements_1.ALL_COUNTRIES.filter((c) => { return board.owner(c); });
            const northAmerica = ["Alaska", "Alberta", "Central America", "Eastern United States", "Greenland", "Northwest Territory", "Ontario", "Quebec", "Western United States"];
            const southAmerica = ["Argentina", "Brazil", "Peru", "Venezuela"];
            const europe = ["Great Britain", "Iceland", "Northern Europe", "Scandinavia", "Southern Europe", "Ukraine", "Western Europe"];
            const africa = ["Congo", "East Africa", "Egypt", "Madagascar", "North Africa", "South Africa"];
            const asia = ["Afghanistan", "China", "India", "Irkutsk", "Japan", "Kamchatka", "Middle East", "Mongolia", "Siam", "Siberia", "Ural", "Yakutsk"];
            const australia = ["Eastern Australia", "Indonesia", "New Guinea", "Western Australia"];
            const continents = [[5, northAmerica], [2, southAmerica], [5, europe], [3, africa], [7, asia], [2, australia]];
            const fromCountry = Math.floor(playersCountries.length / 3);
            var numTroops = Math.max(fromCountry, 3);
            for (var i = 0; i < continents.length; i++) {
                if (continents[i][1].every((c) => { return playersCountries.includes(c); })) {
                    numTroops += continents[i][0];
                }
            }
            return numTroops;
        }
        async setup(board) {
            const me = this.me;
            const currentPlayer = board.players[0];
            this.ui.draw(board);
            if (this.me != currentPlayer) {
                return;
            }
            const incomplete = elements_1.ALL_COUNTRIES.some((c) => { return board.owner(c) == "Empty"; });
            function listenForSetup(e) {
                const country = e.detail;
                console.log(country + " | " + board.owner(country));
                if ((incomplete && board.owner(country) == "Empty") || ((!incomplete) && board.owner(country) == me)) {
                    document.removeEventListener("CountryClickedOn", listenForSetup);
                    document.dispatchEvent(new CustomEvent("Send", { detail: "{\"action\": \"PlaceTroop\", \"sender\":\"" + me + "\",\"country\":\"" + country + "\"}" }));
                }
            }
            document.addEventListener("CountryClickedOn", listenForSetup);
        }
        async reinforce() {
        }
    }
    exports.Moves = Moves;
});
