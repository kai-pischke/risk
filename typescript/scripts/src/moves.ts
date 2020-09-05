import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";
import {Draw} from "./draw";
import {COUNTRY_LOC} from "./map";
import {NEIGHBOURS} from "./neighbours";

export class Moves{
    public me: String;
    public ui: Draw

    constructor(whoAmI :String, whatIsUi : Draw) {
        this.me = whoAmI;
        this.ui = whatIsUi;
    }

    async setup(board : Board, currentPlayer : String){
        const me = this.me;
        this.ui.draw(board);

        if (this.me != currentPlayer){
            return;
        }
        const incomplete = ALL_COUNTRIES.some((c : Country) => {return board.owner(c) == "Empty";});

        function listenForSetup(e : CustomEvent) {
            const country = e.detail;
            console.log(country + "  " + board.owner(country));
            if ((incomplete && board.owner(country) == "Empty") || ((!incomplete) && board.owner(country) == me)){
                document.removeEventListener("CountryClickedOn", listenForSetup);
                document.dispatchEvent(new CustomEvent("Send", {detail: "{\"action\": \"PlaceTroop\", \"sender\":\"" + me + "\",\"country\":\"" + country + "\"}"}));

            }
        }
        document.addEventListener("CountryClickedOn", listenForSetup);
    }
}
