define(["require", "exports", "./elements", "./board"], function (require, exports, elements_1, board_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Connection = void 0;
    class Connection {
        constructor() {
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
                        temp.changeOwner(country, c.owner);
                    });
                    // new board temp
                }
            }
        }
    }
    exports.Connection = Connection;
});
