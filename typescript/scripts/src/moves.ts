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

    private addTroops(initTroops : number, phaseName : string, board : Board, ui : Draw){
      var toAdd = initTroops
      var countryMap = {} as Record<Country, number>;
      var cardsToTrade = [new Array(), new Array()]
      var tradeIn = [new Array(), new Array()]
      var self = this;
      var needToTrade = board.cards.length > 4
      const me = this.me

      ui.addRecruit(toAdd)

      function listenForReinforce(e : CustomEvent) {
          const country = e.detail;
          if (board.owner(country) == me){
              toAdd -=1
              board.changeTroops(country, board.troops(country) + 1)
              ui.draw(board);
              ui.addPhase(phaseName)
              ui.addRecruit(toAdd)
              if (country in countryMap){
                  countryMap[country] = countryMap[country] +1
              } else {
                  countryMap[country] = 1
              }

          }
          if (toAdd == 0){
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
              ui.clearCardColours();
          }

      }

      function cardClickHandler(e:Event){
        let elemId = this.id;
        let whichSet = 0
        if (cardsToTrade[0].length == 3) whichSet = 1
        if (whichSet == 1 && cardsToTrade[1].length == 3) return;

        if (cardsToTrade[whichSet].includes(elemId) && toAdd > self.tradeInBonus(tradeIn)) {
          toAdd -= self.tradeInBonus(tradeIn)
          let i = cardsToTrade[whichSet].indexOf(elemId);
          cardsToTrade[whichSet].splice(i,1);
          tradeIn[whichSet].splice(i,1)
          document.getElementById(elemId).style.border = "none";
          toAdd += self.tradeInBonus(tradeIn)
          console.log(JSON.stringify(tradeIn))
          }
          else {
            if (tradeIn[whichSet].length < 3){
              cardsToTrade[whichSet].push(elemId);
              ui.setCardColour(elemId, "#7fbf7f")
              let c = document.getElementById(elemId).getAttribute("data-type");
              tradeIn[whichSet].push(c)

              //Check the tradeIn is correct if it's complete
              if (self.cardSetBonus(tradeIn[whichSet]) == 0 && tradeIn[whichSet].length == 3){
                console.log("Invalid combo for cardset")
                let i = 0
                for (i = 0; i < 3; i++){
                  ui.removeCardColour(cardsToTrade[whichSet][i])
                }
                cardsToTrade[whichSet] = []
                tradeIn[whichSet] = []
              }
              else if (tradeIn[whichSet].length == 3) {
                toAdd += self.cardSetBonus(tradeIn[whichSet])
                ui.draw(board);
                ui.addPhase(phaseName)
                ui.addRecruit(toAdd)
                if (board.cards.length - 3 <= 4 && needToTrade) {
                  document.addEventListener("CountryClickedOn", listenForReinforce)
                  needToTrade = false
                }
              }
              console.log(JSON.stringify(tradeIn))
            }
          }
        }

      if (!needToTrade) document.addEventListener("CountryClickedOn", listenForReinforce);

      let i = 0;
      for (i = 0; i < board.cards.length; i++){
        let elem = document.getElementById("card" + i.toString());
        elem.addEventListener("click", cardClickHandler);
      }
      
    private setupNum(b : Board) {
        const init = 50 - 5*b.players.length;
        const owned = ALL_COUNTRIES.filter((c : Country) => {return b.owner(c) == this.me;})
        const used = owned.map((c : Country) => {return b.troops(c);}).reduce((a, b) => a + b, 0);

        return init - used;
    }

    async setup(board : Board){
        const me = this.me;
        const currentPlayer = board.players[0];
        const toPlace = this.setupNum(board);

        this.ui.draw(board);
        this.ui.addPhase("Setup")
        this.ui.addRecruit(toPlace)
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

        const currentPlayer = board.players[0];
        const ui = this.ui;

        ui.draw(board);
        ui.addPhase("Reinforce")
        if (this.me != currentPlayer){
            return;
        }

        var toReinforce = board.numberToReinforce(currentPlayer);

        this.addTroops(toReinforce, "Reinforce", board, ui)

    }

    async attack(board : Board){ //Doesn't check if they're neighbours
        console.log("---------------- starting attack ------------------------")
        const me = this.me;
        const ui = this.ui;
        const attColour = this.attColour;
        const defColour = this.defColour;
        const currentPlayer = board.players[0];

        ui.draw(board);
        ui.addPhase("Attack")

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
                ui.addPhase("Attack")
            } else if (ac == country){
                ui.setColour(ac, "white")
                ui.draw(board);
                ui.addPhase("Attack")
                ac = null;
            }

            if (board.owner(country) != me && dc == null){
                dc = country;
                ui.setColour(dc, defColour);
                ui.draw(board);
                ui.addPhase("Attack")
            } else if (dc == country){
                ui.setColour(dc, "white")
                ui.draw(board);
                ui.addPhase("Attack")
                dc = null;
            }

            if (dc != null && ac != null){
                ui.popup.label = "Number of Troops to attack " + dc + " from " + ac;
                ui.popup.max = Math.min((board.troops(ac) - 1), 3);
                ui.popup.default = ui.popup.max
                ui.popup.min = 1;
                ui.popup.visible = true;

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
                        number_of_attackers: ui.popup.value
                    })}));
                attackEnded(new CustomEvent(""));
            }
        }

        function attackEnded(e : CustomEvent){
            document.removeEventListener("AttackEnded", attackEnded);
            document.removeEventListener("CountryClickedOn", listenForAttack);
            document.removeEventListener("PopupSubmit", listenForAttackSubmit);
            document.removeEventListener("PopupCancel", popupCanceled);
            ui.popup.visible = false;
            ui.clearColour()
        }

        function popupCanceled(e : CustomEvent){
            ui.popup.visible = false;
        }

        document.addEventListener("PopupCancel", popupCanceled);
        document.addEventListener("EndAttack", attackEnded);
        document.addEventListener("CountryClickedOn", listenForAttack);
        document.addEventListener("PopupSubmit", listenForAttackSubmit);

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
        ui.addPhase("MidBattle")
        if (this.me != currentPlayer){
            return;
        }
        console.log((board.troops(dc)))
        ui.popup.label = "Number of Troops to defend " + dc + " from the " + attackers + " attackers from " + ac;
        ui.popup.max = Math.min((board.troops(dc)), 2);
        ui.popup.default = ui.popup.max
        ui.popup.min = 1;
        ui.popup.visible = true;

        function listenForNumberDefenders(e: CustomEvent){
            document.dispatchEvent(new CustomEvent("Send",
                {detail: JSON.stringify({
                    action: "ChooseDefenders",
                    sender: me,
                    number_of_defenders: ui.popup.value
                })}));
            ui.popup.visible = false;
            document.removeEventListener("PopupSubmit", listenForNumberDefenders);
        }
        document.addEventListener("PopupSubmit", listenForNumberDefenders);
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


        ui.popup.label = "Number of Troops to invade " + dc + " from " + ac;
        ui.popup.max = (board.troops(ac) -1);
        ui.popup.min = remainingAttackers;
        ui.popup.default = ui.popup.min;
        ui.popup.visible = true;



        function listenForNumberInvaders(e: CustomEvent){
            document.dispatchEvent(new CustomEvent("Send",
                {detail: JSON.stringify({
                    action: "Invade",
                    sender: me,
                    number_of_troops: ui.popup.value
                })
            }));
            ui.popup.visible = false;
            document.removeEventListener("PopupSubmit", listenForNumberInvaders);
        }
        document.addEventListener("PopupSubmit", listenForNumberInvaders);

    }

    async fortify(board : Board){ //Doesn't check if they're neighbours
        const me = this.me;
        const ui = this.ui;
        const attColour = this.attColour;
        const defColour = this.defColour;
        const currentPlayer = board.players[0];

        ui.draw(board);
        ui.addPhase("Fortify")
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
                ui.popup.label = "Number of Troops to fortify " + fc + " from " + tc;
                ui.popup.max = (board.troops(fc) -1);
                ui.popup.min = 1;
                ui.popup.visible = true;
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
                        number_of_troops: ui.popup.value
                    })}));
                    skipFortify(new CustomEvent(""));
            }
        }
        function skipFortify(e: CustomEvent){
            document.removeEventListener("SkipFortify", skipFortify);
            document.removeEventListener("CountryClickedOn", listenForFortify);
            document.removeEventListener("PopupSubmit", listenForFortifySubmit);
            document.removeEventListener("PopupCancel", popupCanceled);

            ui.popup.visible = false;
            ui.clearColour()
        }

        function popupCanceled(e : CustomEvent){
            ui.popup.visible = false;
        }

        document.addEventListener("PopupCancel", popupCanceled);
        document.addEventListener("SkipFortify", skipFortify);
        document.addEventListener("CountryClickedOn", listenForFortify);
        document.addEventListener("PopupSubmit", listenForFortifySubmit);

    }
}
