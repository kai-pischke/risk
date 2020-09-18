define(["require", "exports", "./elements", "./board"], function (require, exports, elements_1, board_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Connection = void 0;
    class Connection {
        constructor() {
            this.a = 10;
            this.previousMessage = null;
        }
        async start() {
            this._socket = new WebSocket("ws://luna.smallserver.xyz:9600");
            this._socket.onmessage = this.receive.bind(this);
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
            //console.log(info)
            this._socket.send(info);
        }
        async start_game(event) {
            this.send("{\"action\": \"StartGame\", \"sender\": \"" + this.me + "\"}");
        }
        receive(event) {
            //console.log(event.data)
            const msg = JSON.parse(event.data);
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
                        cards = msg.cards[this.me];
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
                        this.previousMessage = event;
                        document.dispatchEvent(new CustomEvent("Setup", { detail: temp }));
                    }
                    else if (msg.state === "Play") {
                        if (msg.phase.kind === "Simple") {
                            this.previousMessage = event;
                            document.dispatchEvent(new CustomEvent(msg.phase.phase, { detail: temp }));
                        }
                        else if (msg.phase.kind === "BattleEnd") {
                            this.previousMessage = event;
                            console.log(temp);
                            document.dispatchEvent(new CustomEvent("BattleEnd", { detail: { board: temp,
                                    ac: msg.phase.attacking_country,
                                    dc: msg.phase.defending_country,
                                    attrem: msg.phase.attackers_remaining } }));
                        }
                        else if (msg.phase.kind === "MidBattle") {
                            this.previousMessage = event;
                            document.dispatchEvent(new CustomEvent("MidBattle", { detail: { board: temp,
                                    ac: msg.phase.attacking_country,
                                    dc: msg.phase.defending_country,
                                    att: msg.phase.attackers } }));
                        }
                    }
                }
                else if (msg.kind === "Won") {
                    this.previousMessage = event;
                    document.dispatchEvent(new CustomEvent("Won", { detail: msg.winner }));
                }
                else if (msg.kind === "Question") {
                    this.previousMessage = event;
                    document.dispatchEvent(new CustomEvent(msg.question, { detail: msg.player }));
                }
                else if (msg.kind === "Error") {
                    this.receive(this.previousMessage);
                }
            }
        }
    }
    exports.Connection = Connection;
});
