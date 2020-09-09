define(["require", "exports", "./elements", "./board"], function (require, exports, elements_1, board_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Connection = void 0;
    class Connection {
        constructor() {
            this.a = 10;
        }
        async start() {
            this._socket = new WebSocket("ws://localhost:9600");
            this._socket.onmessage = this.receive;
            return new Promise((resolve, reject) => this._socket.addEventListener('message', function (event) {
                const msg = JSON.parse(event.data);
                console.log(msg);
                if ("kind" in msg && msg.kind === "colour") {
                    const colour = msg.colour;
                    resolve(msg.colour);
                }
                else {
                    reject();
                }
            }));
        }
        async send(info) {
            this._socket.send(info);
        }
        async start_game(event) {
            this.send("{\"action\": \"StartGame\", \"sender\": \"" + this.me + "\"}");
        }
        receive(event) {
            const msg = JSON.parse(event.data);
            //console.log("Recieved : " + msg);
            if ("kind" in msg) {
                if (msg.kind === "colour") {
                    const colour = msg.colour;
                    console.log("I am " + colour);
                }
                else if (msg.kind === "State") {
                    var cards = [];
                    if (msg.state === "WaitingRoom") {
                        return;
                    }
                    else if (msg.state === "Play") {
                        cards = msg.cards[this.me].sort;
                    }
                    const temp = new board_1.Board(msg.players, cards);
                    elements_1.ALL_COUNTRIES.forEach((country, c_index) => {
                        const c = msg.board[country];
                        temp.changeTroops(country, c.number_of_troops);
                        if (elements_1.ALL_PLAYERS.includes(c.owner)) {
                            temp.changeOwner(country, c.owner);
                        }
                    });
                    if (msg.state === "Setup") {
                        console.log("sent");
                        document.dispatchEvent(new CustomEvent("Setup", { detail: temp }));
                    }
                    else if (msg.state === "Play") {
                        switch (msg.phase.kind) {
                            case "Simple":
                                document.dispatchEvent(new CustomEvent(msg.phase.phase, { detail: temp }));
                            case "BattleEnd":
                                document.dispatchEvent(new CustomEvent("BattleEnd", { detail: { board: temp,
                                        ac: msg.phase.attacking_country,
                                        dc: msg.phase.defending_country,
                                        attrem: msg.phase.attackers_remaining } }));
                            case "MidBattle":
                                document.dispatchEvent(new CustomEvent("BattleEnd", { detail: { board: temp,
                                        ac: msg.phase.attacking_country,
                                        dc: msg.phase.defending_country,
                                        att: msg.phase.attackers } }));
                        }
                    }
                }
                else if (msg.kind === "Won") {
                    document.dispatchEvent(new CustomEvent("Won", { detail: msg.winner }));
                }
                else if (msg.kind === "Question") {
                    document.dispatchEvent(new CustomEvent("Question", { detail: msg.question }));
                }
            }
        }
    }
    exports.Connection = Connection;
});
