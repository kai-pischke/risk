define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Moves = void 0;
    class Moves {
        constructor(whoAmI, whatIsUi) {
            this.attColour = "#ff0900";
            this.defColour = "#0000ff";
            this.me = whoAmI;
            this.ui = whatIsUi;
        }
        numberToReinforce(board, p) {
            const playersCountries = elements_1.ALL_COUNTRIES.filter((c) => { return board.owner(c) == this.me; });
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
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "PlaceTroop",
                            sender: me,
                            country: country
                        }) }));
                }
            }
            document.addEventListener("CountryClickedOn", listenForSetup);
        }
        async reinforce(board) {
            console.log("---------------- starting reinforce ------------------------");
            const me = this.me;
            const currentPlayer = board.players[0];
            const ui = this.ui;
            this.ui.draw(board);
            if (this.me != currentPlayer) {
                return;
            }
            var toReinforce = this.numberToReinforce(board, currentPlayer);
            var countryMap = {};
            var cardsToTrade = [];
            var tradeIn = [];
            function listenForReinforce(e) {
                const country = e.detail;
                if (board.owner(country) == me) {
                    toReinforce -= 1;
                    console.log(toReinforce);
                    board.changeTroops(country, board.troops(country) + 1);
                    ui.draw(board);
                    if (country in countryMap) {
                        countryMap[country] = countryMap[country] + 1;
                    }
                    else {
                        countryMap[country] = 1;
                    }
                }
                if (toReinforce == 0) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "Reinforce",
                            sender: me,
                            troops: countryMap,
                            trade_in: []
                        }) }));
                    document.removeEventListener("CountryClickedOn", listenForReinforce);
                }
            }
            function cardClickHandler(e) {
                let elemId = this.id;
                if (cardsToTrade.includes(elemId)) {
                    let i = cardsToTrade.indexOf(elemId);
                    cardsToTrade.splice(i, 1);
                    tradeIn.splice(i, 1);
                    document.getElementById(elemId).style.border = "none";
                }
                else {
                    cardsToTrade.push(elemId);
                    document.getElementById(elemId).style.border = "3px solid green";
                    let c = document.getElementById(elemId).getAttribute("data-type");
                    tradeIn.push(elemId);
                }
            }
            document.addEventListener("CountryClickedOn", listenForReinforce);
            let i = 0;
            for (i = 0; i < board.cards.length; i++) {
                let elem = document.getElementById("card" + i.toString());
                elem.addEventListener("click", cardClickHandler);
            }
        }
        async attack(board) {
            console.log("---------------- starting attack ------------------------");
            const me = this.me;
            const ui = this.ui;
            const attColour = this.attColour;
            const defColour = this.defColour;
            const currentPlayer = board.players[0];
            this.ui.draw(board);
            if (this.me != currentPlayer) {
                return;
            }
            var ac = null;
            var dc = null;
            function listenForAttack(e) {
                const country = e.detail;
                if (board.owner(country) == me && ac == null && board.troops(country) > 1) {
                    ac = country;
                    ui.setColour(ac, attColour);
                    ui.draw(board);
                }
                else if (ac == country) {
                    ui.setColour(ac, "white");
                    ui.draw(board);
                    ac = null;
                }
                if (board.owner(country) != me && dc == null) {
                    dc = country;
                    ui.setColour(dc, defColour);
                    ui.draw(board);
                }
                else if (dc == country) {
                    ui.setColour(dc, "white");
                    ui.draw(board);
                    dc = null;
                }
                if (dc != null && ac != null) {
                    document.getElementById("labelForTroopsToAttack").innerHTML = "Number of Troops to attack " + dc + " from " + ac;
                    document.getElementById("numberTroops").max = Math.min((board.troops(ac) - 1), 3).toString();
                    document.getElementById("popupNumberTroops").style.display = "block";
                }
            }
            function listenForAttackSubmit(e) {
                if (dc != null && ac != null) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "Attack",
                            sender: me,
                            attacking_country: ac,
                            defending_country: dc,
                            number_of_attackers: parseInt(document.getElementById("numberTroops").value)
                        }) }));
                    attackEnded(new CustomEvent(""));
                }
            }
            function attackEnded(e) {
                document.removeEventListener("AttackEnded", attackEnded);
                document.removeEventListener("CountryClickedOn", listenForAttack);
                document.removeEventListener("SubmitNumberTroops", listenForAttackSubmit);
                document.getElementById("popupNumberTroops").style.display = "none";
                ui.clearColour();
            }
            document.addEventListener("EndAttack", attackEnded);
            document.addEventListener("CountryClickedOn", listenForAttack);
            document.addEventListener("SubmitNumberTroops", listenForAttackSubmit);
        }
        async chooseDefenders(board, ac, dc, attackers) {
            console.log("---------------- starting choose Defenders ------------------------");
            const me = this.me;
            const ui = this.ui;
            const currentPlayer = board.owner(dc);
            ui.setColour(ac, this.attColour);
            ui.setColour(dc, this.defColour);
            ui.draw(board);
            ui.clearColour();
            if (this.me != currentPlayer) {
                return;
            }
            console.log((board.troops(dc)));
            document.getElementById("labelForTroopsToDefend").innerHTML = "Number of Troops to defend " + dc + " from the " + attackers + " attackers from " + ac;
            document.getElementById("numberDef").max = Math.min((board.troops(dc)), 2).toString();
            document.getElementById("popupNumberDef").style.display = "block";
            function listenForNumberDefenders(e) {
                document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                        action: "ChooseDefenders",
                        sender: me,
                        number_of_defenders: parseInt(document.getElementById("numberDef").value)
                    }) }));
                document.getElementById("popupNumberDef").style.display = "none";
                document.removeEventListener("SubmitNumberDef", listenForNumberDefenders);
            }
            document.addEventListener("SubmitNumberDef", listenForNumberDefenders);
        }
        async invade(board, ac, dc, remainingAttackers) {
            console.log("---------------- starting Invade ------------------------");
            const me = this.me;
            const ui = this.ui;
            const currentPlayer = board.players[0];
            const box = document.getElementById("popupBox");
            const troopslabel = document.getElementById("ntroopslabel");
            const plusbutton = document.querySelector("#popupBox button[data-quantity='plus']");
            const minusbutton = document.querySelector("#popupBox button[data-quantity='minus']");
            ui.setColour(ac, this.attColour);
            ui.setColour(dc, this.defColour);
            ui.draw(board);
            ui.clearColour();
            if (this.me != currentPlayer) {
                return;
            }
            function changeTroops(d) {
                troopslabel.innerHTML = parseInt(troopslabel.innerHTML) + d;
            }
            plusbutton.onclick = (() => {
                changeTroops(1);
            });
            minusbutton.onclick = (() => {
                changeTroops(-1);
            });
            //document.getElementById("labelForTroopsToInvade").innerHTML = "Number of Troops to invade " + ac + " from " + dc;
            //(document.getElementById("numberInv") as HTMLInputElement).max = (board.troops(ac) -1).toString();
            //(document.getElementById("numberInv") as HTMLInputElement).min = remainingAttackers.toString();
            box.classList.add("show");
            function listenForNumberInvaders(e) {
                document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                        action: "Invade",
                        sender: me,
                        number_of_troops: parseInt(troopslabel.innerHTML)
                    })
                }));
                box.classList.remove("show");
                document.removeEventListener("SubmitNumberInv", listenForNumberInvaders);
            }
            document.addEventListener("SubmitNumberInv", listenForNumberInvaders);
        }
        async fortify(board) {
            const me = this.me;
            const ui = this.ui;
            const attColour = this.attColour;
            const defColour = this.defColour;
            const currentPlayer = board.players[0];
            this.ui.draw(board);
            if (this.me != currentPlayer) {
                return;
            }
            var fc = null;
            var tc = null;
            function listenForFortify(e) {
                const country = e.detail;
                if (board.owner(country) == me && fc == null && board.troops(country) > 1) {
                    fc = country;
                    ui.setColour(fc, attColour);
                    ui.draw(board);
                }
                else if (fc == country) {
                    ui.setColour(fc, "white");
                    ui.draw(board);
                    fc = null;
                }
                else if (board.owner(country) == me && tc == null) {
                    tc = country;
                    ui.setColour(tc, defColour);
                    ui.draw(board);
                }
                else if (tc == country) {
                    ui.setColour(tc, "white");
                    ui.draw(board);
                    tc = null;
                }
                if (fc != null && tc != null) {
                    document.getElementById("numberFort").max = (board.troops(country) - 1).toString();
                    document.getElementById("popupNumberFort").style.display = "block";
                }
            }
            function listenForFortifySubmit(e) {
                if (fc != null && tc != null) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "Fortify",
                            sender: me,
                            from_country: fc,
                            to_country: tc,
                            number_of_troops: parseInt(document.getElementById("numberFort").value)
                        }) }));
                    skipFortify(new CustomEvent(""));
                }
            }
            function skipFortify(e) {
                document.removeEventListener("SkipFortify", skipFortify);
                document.removeEventListener("CountryClickedOn", listenForFortify);
                document.removeEventListener("SubmitNumberFort", listenForFortifySubmit);
                document.getElementById("popupNumberFort").style.display = "none";
                ui.clearColour();
            }
            document.addEventListener("SkipFortify", skipFortify);
            document.addEventListener("CountryClickedOn", listenForFortify);
            document.addEventListener("SubmitNumberFort", listenForFortifySubmit);
        }
    }
    exports.Moves = Moves;
});
