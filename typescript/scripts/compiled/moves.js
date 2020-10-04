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
        cardSetBonus(set) {
            if (set.length < 3)
                return 0;
            else if (set.length == 3) {
                let sorted = set.sort();
                switch (sorted[2]) {
                    case "Artillery":
                        if (sorted[0] == "Artillery" && sorted[1] == "Artillery")
                            return 8;
                        else
                            return 0;
                        break;
                    case "Cavalry":
                        if (sorted[0] == "Cavalry" && sorted[1] == "Cavalry")
                            return 6;
                        else
                            return 0;
                        break;
                    case "Infantry":
                        if (sorted[0] == "Infantry" && sorted[1] == "Infantry")
                            return 4;
                        else if (sorted[0] == "Artillery" && sorted[1] == "Cavalry")
                            return 10;
                        else
                            return 0;
                        break;
                    case "Wild":
                        if (sorted[1] == "Wild" || sorted[0] != sorted[1])
                            return 10;
                        else if (sorted[0] == "Infantry" && sorted[1] == "Infantry")
                            return 4;
                        else if (sorted[0] == "Cavalry" && sorted[1] == "Cavalry")
                            return 6;
                        else if (sorted[0] == "Artillery" && sorted[1] == "Artillery")
                            return 8;
                        else
                            return 0;
                        break;
                }
            }
        }
        tradeInBonus(t) {
            return this.cardSetBonus(t[0]) + this.cardSetBonus(t[1]);
        }
        jsonifyTradeIn(t) {
            if (t[0].length == 3 && t[1].length == 3)
                return t;
            else if (t[0].length == 3)
                return [t[0]];
            else
                return [];
        }
        addTroops(initTroops, action, board, ui, lowerCardLimit) {
            var toAdd = initTroops;
            var countryMap = {};
            var cardsToTrade = [new Array(), new Array()];
            var tradeIn = [new Array(), new Array()];
            var self = this;
            var needToTrade = board.cards.length > 4;
            const me = this.me;
            ui.addRecruit(toAdd);
            function listenForReinforce(e) {
                const country = e.detail;
                if (board.owner(country) == me) {
                    toAdd -= 1;
                    board.changeTroops(country, board.troops(country) + 1);
                    ui.draw(board);
                    ui.addPhase(action);
                    ui.addRecruit(toAdd);
                    if (country in countryMap) {
                        countryMap[country] = countryMap[country] + 1;
                    }
                    else {
                        countryMap[country] = 1;
                    }
                }
                if (toAdd == 0) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: action,
                            sender: me,
                            troops: countryMap,
                            trade_in: self.jsonifyTradeIn(tradeIn)
                        }) }));
                    document.removeEventListener("CountryClickedOn", listenForReinforce);
                    let i = 0;
                    for (i = 0; i < board.cards.length; i++) {
                        let elem = document.getElementById("card" + i.toString());
                        elem.removeEventListener("click", cardClickHandler);
                    }
                    ui.clearCardColours();
                }
            }
            function cardClickHandler(e) {
                let elemId = this.id;
                let whichSet = 0;
                if (cardsToTrade[0].length == 3)
                    whichSet = 1;
                console.log("whichset: " + whichSet);
                if (whichSet == 1 && cardsToTrade[1].length == 3)
                    return;
                if (cardsToTrade[whichSet].includes(elemId) && cardsToTrade[whichSet].length < 3) {
                    toAdd -= self.tradeInBonus(tradeIn);
                    let i = cardsToTrade[whichSet].indexOf(elemId);
                    cardsToTrade[whichSet].splice(i, 1);
                    tradeIn[whichSet].splice(i, 1);
                    document.getElementById(elemId).style.border = "none";
                    toAdd += self.tradeInBonus(tradeIn);
                }
                else if (!cardsToTrade[0].includes(elemId) && !cardsToTrade[0].includes(elemId)) {
                    if (tradeIn[whichSet].length < 3) {
                        cardsToTrade[whichSet].push(elemId);
                        ui.setCardColour(elemId, "black");
                        let c = document.getElementById(elemId).getAttribute("data-type");
                        tradeIn[whichSet].push(c);
                        //Check the tradeIn is correct if it's complete
                        if (self.cardSetBonus(tradeIn[whichSet]) == 0 && tradeIn[whichSet].length == 3) {
                            console.log("Invalid combo for cardset");
                            let i = 0;
                            for (i = 0; i < 3; i++) {
                                ui.removeCardColour(cardsToTrade[whichSet][i]);
                            }
                            cardsToTrade[whichSet] = [];
                            tradeIn[whichSet] = [];
                        }
                        else if (tradeIn[whichSet].length == 3) {
                            toAdd += self.cardSetBonus(tradeIn[whichSet]);
                            ui.draw(board);
                            ui.addPhase(action);
                            ui.addRecruit(toAdd);
                            let i = 0;
                            for (i = 0; i < board.cards.length; i++) {
                                let elem = document.getElementById("card" + i.toString());
                                elem.addEventListener("click", cardClickHandler);
                                if (cardsToTrade[whichSet].includes(elem.id))
                                    ui.setCardColour(elem.id, "#7fbf7f");
                            }
                            if (board.cards.length - tradeIn[0].length - tradeIn[1].length <= lowerCardLimit && needToTrade) {
                                document.addEventListener("CountryClickedOn", listenForReinforce);
                                needToTrade = false;
                                ui.hideTradeMessage();
                            }
                        }
                        console.log(JSON.stringify(tradeIn));
                    }
                }
            }
            if (!needToTrade)
                document.addEventListener("CountryClickedOn", listenForReinforce);
            else
                ui.showTradeMessage();
            let i = 0;
            for (i = 0; i < board.cards.length; i++) {
                let elem = document.getElementById("card" + i.toString());
                elem.addEventListener("click", cardClickHandler);
            }
        }
        setupNum(b) {
            const init = 50 - 5 * b.players.length;
            const owned = elements_1.ALL_COUNTRIES.filter((c) => { return b.owner(c) == this.me; });
            const used = owned.map((c) => { return b.troops(c); }).reduce((a, b) => a + b, 0);
            return init - used;
        }
        async setup(board) {
            const me = this.me;
            const currentPlayer = board.players[0];
            const toPlace = this.setupNum(board);
            this.ui.draw(board);
            this.ui.addPhase("Setup");
            this.ui.addRecruit(toPlace);
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
            const currentPlayer = board.players[0];
            const ui = this.ui;
            ui.draw(board);
            ui.addPhase("Reinforce");
            if (this.me != currentPlayer) {
                return;
            }
            var toReinforce = board.numberToReinforce(currentPlayer);
            this.addTroops(toReinforce, "Reinforce", board, ui, board.cards.length);
        }
        async getTrade(board) {
            console.log("---------------- starting trade ------------------------");
            const currentPlayer = board.players[0];
            const ui = this.ui;
            ui.draw(board);
            ui.addPhase("Trade");
            if (this.me != currentPlayer) {
                return;
            }
            this.addTroops(0, "Trade", board, ui, 4);
        }
        async attack(board) {
            console.log("---------------- starting attack ------------------------");
            const me = this.me;
            const ui = this.ui;
            const attColour = this.attColour;
            const defColour = this.defColour;
            const currentPlayer = board.players[0];
            ui.draw(board);
            ui.addPhase("Attack");
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
                    ui.addPhase("Attack");
                }
                else if (ac == country) {
                    ui.setColour(ac, "white");
                    ui.draw(board);
                    ui.addPhase("Attack");
                    ac = null;
                }
                if (board.owner(country) != me && dc == null) {
                    dc = country;
                    ui.setColour(dc, defColour);
                    ui.draw(board);
                    ui.addPhase("Attack");
                }
                else if (dc == country) {
                    ui.setColour(dc, "white");
                    ui.draw(board);
                    ui.addPhase("Attack");
                    dc = null;
                }
                if (dc != null && ac != null) {
                    ui.popup.label = "Number of Troops to attack " + dc + " from " + ac;
                    ui.popup.max = Math.min((board.troops(ac) - 1), 3);
                    ui.popup.default = ui.popup.max;
                    ui.popup.min = 1;
                    ui.popup.visible = true;
                }
            }
            function listenForAttackSubmit(e) {
                if (dc != null && ac != null) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "Attack",
                            sender: me,
                            attacking_country: ac,
                            defending_country: dc,
                            number_of_attackers: ui.popup.value
                        }) }));
                    attackEnded(new CustomEvent(""));
                }
            }
            function attackEnded(e) {
                document.removeEventListener("AttackEnded", attackEnded);
                document.removeEventListener("CountryClickedOn", listenForAttack);
                document.removeEventListener("PopupSubmit", listenForAttackSubmit);
                document.removeEventListener("PopupCancel", popupCanceled);
                ui.popup.visible = false;
                ui.clearColour();
            }
            function popupCanceled(e) {
                ui.popup.visible = false;
            }
            document.addEventListener("PopupCancel", popupCanceled);
            document.addEventListener("EndAttack", attackEnded);
            document.addEventListener("CountryClickedOn", listenForAttack);
            document.addEventListener("PopupSubmit", listenForAttackSubmit);
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
            ui.addPhase("MidBattle");
            if (this.me != currentPlayer) {
                return;
            }
            console.log((board.troops(dc)));
            ui.popup.label = "Number of Troops to defend " + dc + " from the " + attackers + " attackers from " + ac;
            ui.popup.max = Math.min((board.troops(dc)), 2);
            ui.popup.default = ui.popup.max;
            ui.popup.min = 1;
            ui.popup.visible = true;
            function listenForNumberDefenders(e) {
                document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                        action: "ChooseDefenders",
                        sender: me,
                        number_of_defenders: ui.popup.value
                    }) }));
                ui.popup.visible = false;
                document.removeEventListener("PopupSubmit", listenForNumberDefenders);
            }
            document.addEventListener("PopupSubmit", listenForNumberDefenders);
        }
        async invade(board, ac, dc, remainingAttackers) {
            console.log("---------------- starting Invade ------------------------");
            const me = this.me;
            const ui = this.ui;
            const currentPlayer = board.players[0];
            ui.setColour(ac, this.attColour);
            ui.setColour(dc, this.defColour);
            ui.draw(board);
            ui.clearColour();
            if (this.me != currentPlayer) {
                return;
            }
            ui.popup.label = "Number of Troops to invade " + dc + " from " + ac;
            ui.popup.max = (board.troops(ac) - 1);
            ui.popup.min = remainingAttackers;
            ui.popup.default = ui.popup.min;
            ui.popup.visible = true;
            function listenForNumberInvaders(e) {
                document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
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
        async fortify(board) {
            const me = this.me;
            const ui = this.ui;
            const attColour = this.attColour;
            const defColour = this.defColour;
            const currentPlayer = board.players[0];
            ui.draw(board);
            ui.addPhase("Fortify");
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
                    ui.popup.label = "Number of Troops to fortify " + fc + " from " + tc;
                    ui.popup.max = (board.troops(fc) - 1);
                    ui.popup.min = 1;
                    ui.popup.visible = true;
                }
            }
            function listenForFortifySubmit(e) {
                if (fc != null && tc != null) {
                    document.dispatchEvent(new CustomEvent("Send", { detail: JSON.stringify({
                            action: "Fortify",
                            sender: me,
                            from_country: fc,
                            to_country: tc,
                            number_of_troops: ui.popup.value
                        }) }));
                    skipFortify(new CustomEvent(""));
                }
            }
            function skipFortify(e) {
                document.removeEventListener("SkipFortify", skipFortify);
                document.removeEventListener("CountryClickedOn", listenForFortify);
                document.removeEventListener("PopupSubmit", listenForFortifySubmit);
                document.removeEventListener("PopupCancel", popupCanceled);
                ui.popup.visible = false;
                ui.clearColour();
            }
            function popupCanceled(e) {
                ui.popup.visible = false;
            }
            document.addEventListener("PopupCancel", popupCanceled);
            document.addEventListener("SkipFortify", skipFortify);
            document.addEventListener("CountryClickedOn", listenForFortify);
            document.addEventListener("PopupSubmit", listenForFortifySubmit);
        }
    }
    exports.Moves = Moves;
});
