define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Moves = void 0;
    class Moves {
        constructor(whoAmI, whatIsUi) {
            this.me = whoAmI;
            this.ui = whatIsUi;
        }
        async setup(board, currentPlayer) {
            const me = this.me;
            this.ui.draw(board);
            if (this.me != currentPlayer) {
                return;
            }
            const incomplete = elements_1.ALL_COUNTRIES.some((c) => { return board.owner(c) == "Empty"; });
            function listenForSetup(e) {
                const country = e.detail;
                console.log(country + "  " + board.owner(country));
                if ((incomplete && board.owner(country) == "Empty") || ((!incomplete) && board.owner(country) == me)) {
                    document.removeEventListener("CountryClickedOn", listenForSetup);
                    document.dispatchEvent(new CustomEvent("Send", { detail: "{\"action\": \"PlaceTroop\", \"sender\":\"" + me + "\",\"country\":\"" + country + "\"}" }));
                }
            }
            document.addEventListener("CountryClickedOn", listenForSetup);
        }
    }
    exports.Moves = Moves;
});
