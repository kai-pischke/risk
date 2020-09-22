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

    private cardSetBonus(set){
      if (set.length < 3) return 0
      else if (set.length == 3){
        let sorted = set.sort()
        switch (sorted[2]){
          case "Artillery":
            if (sorted[0] == "Artillery" && sorted[1] == "Artillery") return 8;
            else return 0;
            break;
          case "Cavalry":
            if (sorted[0] == "Cavalry" && sorted[1] == "Cavalry") return 6
            else return 0;
            break;
          case "Infantry":
            if(sorted[0] == "Infantry" && sorted[1] == "Infantry") return 4;
            else if (sorted[0] == "Artillery" && sorted[1] == "Cavalry") return 10;
            else return 0;
            break;
          case "Wild":
            if (sorted[1] == "Wild" || sorted[0] != sorted[1]) return 10
            else if (sorted[0] == "Infantry" && sorted[1] == "Infantry") return 4
            else if (sorted[0] == "Cavalry" && sorted[1] == "Cavalry") return 6
            else if (sorted[0] == "Artillery" && sorted[1] == "Artillery") return 8
            else return 0
            break;
        }
      }
    }

    private tradeInBonus(t) {
      return this.cardSetBonus(t[0]) + this.cardSetBonus(t[1]);
    }

    private jsonifyTradeIn(t){
      if (t[0].length == 3 && t[1].length == 3) return t
      else if (t[0].length == 3) return [t[0]]
      else return []
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
        var cardsToTrade = [new Array(), new Array()]
        var tradeIn = [new Array(), new Array()]
        var self = this;


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
                        trade_in: self.jsonifyTradeIn(tradeIn)
                    })}));
                document.removeEventListener("CountryClickedOn", listenForReinforce);

                let i = 0;
                for (i = 0; i < board.cards.length; i++){
                  let elem = document.getElementById("card" + i.toString());
                  elem.removeEventListener("click", cardClickHandler);
                }
            }

        }

        function cardClickHandler(e:Event){
          let elemId = this.id;
          let whichSet = 0
          if (cardsToTrade[0].length == 3) whichSet = 1
          if (whichSet == 1 && cardsToTrade[1].length == 3) return;

          if (cardsToTrade[whichSet].includes(elemId) && toReinforce > self.tradeInBonus(tradeIn)) {
            toReinforce -= self.tradeInBonus(tradeIn)
            let i = cardsToTrade[whichSet].indexOf(elemId);
            cardsToTrade[whichSet].splice(i,1);
            tradeIn[whichSet].splice(i,1)
            document.getElementById(elemId).style.border = "none";
            toReinforce += self.tradeInBonus(tradeIn)
            console.log(JSON.stringify(tradeIn))
            }
            else {
              if (tradeIn[whichSet].length < 3){
                cardsToTrade[whichSet].push(elemId);
                document.getElementById(elemId).style.border = "3px solid #7fbf7f";
                let c = document.getElementById(elemId).getAttribute("data-type");
                tradeIn[whichSet].push(c)

                //Check the tradeIn is correct if it's complete
                if (self.cardSetBonus(tradeIn[whichSet]) == 0 && tradeIn[whichSet].length == 3){
                  console.log("Invalid combo for cardset")
                  let i = 0
                  for (i = 0; i < 3; i++){
                    document.getElementById(cardsToTrade[whichSet][i]).style.border = "none"
                  }
                  cardsToTrade[whichSet] = []
                  tradeIn[whichSet] = []
                }
                else if (tradeIn[whichSet].length == 3) {
                  toReinforce += self.cardSetBonus(tradeIn[whichSet])
                }
                console.log(JSON.stringify(tradeIn))
              }
            }
          }

        document.addEventListener("CountryClickedOn", listenForReinforce);

        let i = 0;
        for (i = 0; i < board.cards.length; i++){
          let elem = document.getElementById("card" + i.toString());
          elem.addEventListener("click", cardClickHandler);
        }

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
                })
            }));
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
                (document.getElementById("numberFort") as HTMLInputElement).max = (board.troops(country) - 1).toString();
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
            document.removeEventListener("SubmitNumberFort", listenForFortifySubmit);
            document.getElementById("popupNumberFort").style.display = "none";
            ui.clearColour()
        }

        document.addEventListener("SkipFortify", skipFortify);
        document.addEventListener("CountryClickedOn", listenForFortify);
        document.addEventListener("SubmitNumberFort", listenForFortifySubmit);

    }


}
