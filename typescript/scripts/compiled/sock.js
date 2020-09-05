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
            this._socket.send("{\"action\": \"StartGame\", \"sender\": \"" + this.me + "\"}");
        }
        receive(event) {
            const msg = JSON.parse(event.data);
            //console.log("Recieved : " + msg);
            if ("kind" in msg) {
                if (msg.kind === "colour") {
                    const colour = msg.colour;
                    console.log("I am " + colour);
                }
                if (msg.state === "Setup") {
                    const temp = new board_1.Board();
                    elements_1.ALL_COUNTRIES.forEach((country, c_index) => {
                        const c = msg.board[country];
                        temp.changeTroops(country, c.number_of_troops);
                        if (elements_1.ALL_PLAYERS.includes(c.owner)) {
                            temp.changeOwner(country, c.owner);
                        }
                    });
                    console.log("sent");
                    document.dispatchEvent(new CustomEvent("Setup", { detail: { board: temp, players: msg.players } }));
                    // new board temp
                }
            }
        }
    }
    exports.Connection = Connection;
});
