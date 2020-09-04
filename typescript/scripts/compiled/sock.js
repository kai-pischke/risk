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
        async start_game(event) {
            console.log(this);
            console.log(this.a);
            console.log("{\"action\": \"StartGame\", \"sender\": \"" + this.me + "\"}");
            this._socket.send("{\"action\": \"StartGame\", \"sender\": \"" + this.me + "\"}");
        }
        receive(event) {
            const msg = JSON.parse(event.data);
            console.log(msg);
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
                        if (c.owner in elements_1.ALL_PLAYERS) {
                            temp.changeOwner(country, c.owner);
                        }
                    });
                    let c_event = new CustomEvent("Setup", { detail: temp });
                    dispatchEvent(c_event);
                    // new board temp
                }
            }
        }
    }
    exports.Connection = Connection;
});
