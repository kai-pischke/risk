import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";
import {Draw} from "./draw";
import {COUNTRY_LOC} from "./map";
import {NEIGHBOURS} from "./neighbours";



export class Moves{
    public me: String;
    public ui: Draw;

    private attColour = "#ff0900";
    private defColour = "#0000ff";

    constructor(whoAmI :String, whatIsUi : Draw) {
        this.me = whoAmI;
        this.ui = whatIsUi;
    }

    private numberToReinforce(board : Board, p : Player): number {
        const playersCountries: Array<Country> = ALL_COUNTRIES.filter((c : Country) => {return board.owner(c) == this.me;});
        const northAmerica: Array<Country> = ["Alaska", "Alberta", "Central America", "Eastern United States", "Greenland", "Northwest Territory", "Ontario", "Quebec", "Western United States"];
        const southAmerica: Array<Country> = ["Argentina", "Brazil", "Peru", "Venezuela"];
        const europe: Array<Country> = ["Great Britain", "Iceland", "Northern Europe", "Scandinavia", "Southern Europe", "Ukraine", "Western Europe"];
        const africa: Array<Country> = ["Congo", "East Africa", "Egypt", "Madagascar", "North Africa", "South Africa"];
        const asia: Array<Country> = ["Afghanistan", "China", "India", "Irkutsk", "Japan", "Kamchatka", "Middle East", "Mongolia", "Siam", "Siberia", "Ural", "Yakutsk"];
        const australia: Array<Country> = ["Eastern Australia", "Indonesia", "New Guinea", "Western Australia"];
        const continents: Array<[number, Array<Country>]> = [[5, northAmerica],[2, southAmerica],[5, europe],[3, africa],[7, asia],[2, australia]];

        const fromCountry = Math.floor(playersCountries.length/3)
        var numTroops = Math.max(fromCountry, 3)

        console.log(numTroops)
        for (var i = 0;i<continents.length;i++){
            if (continents[i][1].every((c: Country) => {return playersCountries.includes(c);})){
                numTroops += continents[i][0]
            }
        }
        console.log(numTroops);
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
                document.dispatchEvent(new CustomEvent("Send",
                    {detail: JSON.stringify({
                        action: "PlaceTroop",
                        sender: me,
                        country: country
                    })}));
            }
        }
        document.addEventListener("CountryClickedOn", listenForSetup);
    }

    async reinforce(board : Board){
        console.log("---------------- starting reinforce ------------------------")

        const me = this.me;
        const currentPlayer = board.players[0];
        const ui = this.ui;

        this.ui.draw(board);
        if (this.me != currentPlayer){
            return;
        }
        var toReinforce = this.numberToReinforce(board, currentPlayer);
        var countryMap = {} as Record<Country, number>;

        function listenForReinforce(e : CustomEvent) {
            const country = e.detail;
            if (board.owner(country) == me){
                toReinforce -=1
                console.log(toReinforce)
                board.changeTroops(country, board.troops(country) + 1)
                ui.draw(board);
                if (country in countryMap){
                    countryMap[country] = countryMap[country] +1
                } else {
                    countryMap[country] = 1
                }

            }
            if (toReinforce == 0){
                document.dispatchEvent(new CustomEvent("Send",
                    {detail: JSON.stringify({
                        action: "Reinforce",
                        sender: me,
                        troops: countryMap,
                        trade_in: []
                    })}));
                document.removeEventListener("CountryClickedOn", listenForReinforce);
            }

        }

        document.addEventListener("CountryClickedOn", listenForReinforce);
    }

    async attack(board : Board){ //Doesn't check if they're neighbours
        console.log("---------------- starting attack ------------------------")
        const me = this.me;
        const ui = this.ui;
        const attColour = this.attColour;
        const defColour = this.defColour;
        const currentPlayer = board.players[0];

        this.ui.draw(board);
        if (this.me != currentPlayer){
            return;
        }
        var ac : Country = null;
        var dc : Country = null;



        function listenForAttack(e :CustomEvent){
            const country = e.detail;
            if (board.owner(country) == me && ac == null && board.troops(country) > 1){
                ac = country;
                ui.setColour(ac, attColour);
                ui.draw(board);
            } else if (ac == country){
                ui.setColour(ac, "white")
                ui.draw(board);
                ac = null;
            }

            if (board.owner(country) != me && dc == null){
                dc = country;
                ui.setColour(dc, defColour);
                ui.draw(board);
            } else if (dc == country){
                ui.setColour(dc, "white")
                ui.draw(board);
                dc = null;
            }

            if (dc != null && ac != null){
                document.getElementById("labelForTroopsToAttack").innerHTML = "Number of Troops to attack " + dc + " from " + ac;
                (document.getElementById("numberTroops") as HTMLInputElement).max = Math.min((board.troops(ac) - 1), 3).toString();
                document.getElementById("popupNumberTroops").style.display = "block";

            }
        }

        function listenForAttackSubmit(e: CustomEvent){
            if (dc != null && ac != null){
                document.dispatchEvent(new CustomEvent("Send",
                    {detail: JSON.stringify({
                        action: "Attack",
                        sender: me,
                        attacking_country: ac,
                        defending_country: dc,
                        number_of_attackers: parseInt((document.getElementById("numberTroops") as HTMLInputElement).value)
                    })}));
                attackEnded(new CustomEvent(""));
            }
        }

        function attackEnded(e : CustomEvent){
            document.removeEventListener("AttackEnded", attackEnded);
            document.removeEventListener("CountryClickedOn", listenForAttack);
            document.removeEventListener("SubmitNumberTroops", listenForAttackSubmit);
            document.getElementById("popupNumberTroops").style.display = "none";
            ui.clearColour()
        }

        document.addEventListener("EndAttack", attackEnded);
        document.addEventListener("CountryClickedOn", listenForAttack);
        document.addEventListener("SubmitNumberTroops", listenForAttackSubmit);

    }

    async chooseDefenders(board : Board, ac : Country, dc : Country, attackers : number){
        console.log("---------------- starting choose Defenders ------------------------")
        const me = this.me;
        const ui = this.ui;
        const currentPlayer = board.owner(dc);

        ui.setColour(ac, this.attColour);
        ui.setColour(dc, this.defColour);
        ui.draw(board);
        ui.clearColour();
        if (this.me != currentPlayer){
            return;
        }
        console.log((board.troops(dc)))
        document.getElementById("labelForTroopsToDefend").innerHTML = "Number of Troops to defend " + dc + " from the " + attackers + " attackers from " + ac;
        (document.getElementById("numberDef") as HTMLInputElement).max = Math.min((board.troops(dc)), 2).toString();
        document.getElementById("popupNumberDef").style.display = "block";

        function listenForNumberDefenders(e: CustomEvent){
            document.dispatchEvent(new CustomEvent("Send",
                {detail: JSON.stringify({
                    action: "ChooseDefenders",
                    sender: me,
                    number_of_defenders: parseInt((document.getElementById("numberDef") as HTMLInputElement).value)
                })}));
            document.getElementById("popupNumberDef").style.display = "none";
            document.removeEventListener("SubmitNumberDef", listenForNumberDefenders);
        }
        document.addEventListener("SubmitNumberDef", listenForNumberDefenders);
    }

    async invade(board : Board, ac : Country, dc : Country, remainingAttackers : number){
        console.log("---------------- starting Invade ------------------------")
        const me = this.me;
        const ui = this.ui;
        const currentPlayer = board.players[0];

        ui.setColour(ac, this.attColour);
        ui.setColour(dc, this.defColour);
        ui.draw(board);
        ui.clearColour();
        if (this.me != currentPlayer){
            return;
        }
        document.getElementById("labelForTroopsToInvade").innerHTML = "Number of Troops to invade " + ac + " from " + dc;
        (document.getElementById("numberInv") as HTMLInputElement).max = (board.troops(ac) -1).toString();
        (document.getElementById("numberInv") as HTMLInputElement).min = remainingAttackers.toString();
        document.getElementById("popupNumberInv").style.display = "block";

        function listenForNumberInvaders(e: CustomEvent){
            document.dispatchEvent(new CustomEvent("Send",
                {detail: JSON.stringify({
                    action: "Invade",
                    sender: me,
                    number_of_troops: parseInt((document.getElementById("numberInv") as HTMLInputElement).value)
                })}));
            document.getElementById("popupNumberInv").style.display = "none";
            document.removeEventListener("SubmitNumberInv", listenForNumberInvaders);
        }
        document.addEventListener("SubmitNumberInv", listenForNumberInvaders);

    }

    async fortify(board : Board){ //Doesn't check if they're neighbours
        const me = this.me;
        const ui = this.ui;
        const attColour = this.attColour;
        const defColour = this.defColour;
        const currentPlayer = board.players[0];

        this.ui.draw(board);
        if (this.me != currentPlayer){
            return;
        }
        var fc : Country = null;
        var tc : Country = null;



        function listenForFortify(e :CustomEvent){
            const country = e.detail;
            if (board.owner(country) == me && fc == null && board.troops(country) > 1){
                fc = country;
                ui.setColour(fc, attColour);
                ui.draw(board);
            } else if (fc == country){
                ui.setColour(fc, "white");
                ui.draw(board);
                fc = null;

            } else if (board.owner(country) == me && tc == null){
                tc = country;
                ui.setColour(tc, defColour);
                ui.draw(board);
            } else if (tc == country){
                ui.setColour(tc, "white");
                ui.draw(board);
                tc = null;

            }

            if (fc != null && tc != null){
                (document.getElementById("numberTroops") as HTMLInputElement).max = (board.troops(country) - 1).toString();
                document.getElementById("popupNumberFort").style.display = "block";
            }
        }

        function listenForFortifySubmit(e: CustomEvent){
            if (fc != null && tc != null){
                document.dispatchEvent(new CustomEvent("Send",
                    {detail: JSON.stringify({
                        action: "Fortify",
                        sender: me,
                        from_country: fc,
                        to_country: tc,
                        number_of_troops: parseInt((document.getElementById("numberFort") as HTMLInputElement).value)
                    })}));
                    skipFortify(new CustomEvent(""));
            }
        }
        function skipFortify(e: CustomEvent){
            document.removeEventListener("SkipFortify", skipFortify);
            document.removeEventListener("CountryClickedOn", listenForFortify);
            document.removeEventListener("submitNumberFort", listenForFortifySubmit);
            document.getElementById("popupNumberFort").style.display = "none";
            ui.clearColour()
        }

        document.addEventListener("SkipFortify", skipFortify);
        document.addEventListener("CountryClickedOn", listenForFortify);
        document.addEventListener("submitNumberFort", listenForFortifySubmit);

    }


}
