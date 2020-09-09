import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";
import {Draw} from "./draw";
import {COUNTRY_LOC} from "./map";
import {NEIGHBOURS} from "./neighbours";



export class Moves{
    public me: String;
    public ui: Draw;

    constructor(whoAmI :String, whatIsUi : Draw) {
        this.me = whoAmI;
        this.ui = whatIsUi;
    }

    private numberToReinforce(board : Board, p : Player): number {
        const playersCountries: Array<Country> = ALL_COUNTRIES.filter((c : Country) => {return board.owner(c)});
        const northAmerica: Array<Country> = ["Alaska", "Alberta", "Central America", "Eastern United States", "Greenland", "Northwest Territory", "Ontario", "Quebec", "Western United States"];
        const southAmerica: Array<Country> = ["Argentina", "Brazil", "Peru", "Venezuela"];
        const europe: Array<Country> = ["Great Britain", "Iceland", "Northern Europe", "Scandinavia", "Southern Europe", "Ukraine", "Western Europe"];
        const africa: Array<Country> = ["Congo", "East Africa", "Egypt", "Madagascar", "North Africa", "South Africa"];
        const asia: Array<Country> = ["Afghanistan", "China", "India", "Irkutsk", "Japan", "Kamchatka", "Middle East", "Mongolia", "Siam", "Siberia", "Ural", "Yakutsk"];
        const australia: Array<Country> = ["Eastern Australia", "Indonesia", "New Guinea", "Western Australia"];
        const continents: Array<[number, Array<Country>]> = [[5, northAmerica],[2, southAmerica],[5, europe],[3, africa],[7, asia],[2, australia]];

        const fromCountry = Math.floor(playersCountries.length/3)
        var numTroops = Math.max(fromCountry, 3)
        for (var i = 0;i<continents.length;i++){
            if (continents[i][1].every((c: Country) => {return playersCountries.includes(c);})){
                numTroops += continents[i][0]
            }
        }

        return numTroops;
    }

    async setup(board : Board){
        const me = this.me;
        const currentPlayer =  board.players[0]

        this.ui.draw(board);

        if (this.me != currentPlayer){
            return;
        }
        const incomplete = ALL_COUNTRIES.some((c : Country) => {return board.owner(c) == "Empty";});

        function listenForSetup(e : CustomEvent) {
            const country = e.detail;
            console.log(country + " | " + board.owner(country));
            if ((incomplete && board.owner(country) == "Empty") || ((!incomplete) && board.owner(country) == me)){
                document.removeEventListener("CountryClickedOn", listenForSetup);
                document.dispatchEvent(new CustomEvent("Send", {detail: "{\"action\": \"PlaceTroop\", \"sender\":\"" + me + "\",\"country\":\"" + country + "\"}"}));

            }
        }
        document.addEventListener("CountryClickedOn", listenForSetup);
    }

    async reinforce(){

    }


}
