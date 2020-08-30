define(["require", "exports", "./elements", "./board"], function (require, exports, elements_1, board_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Connection = void 0;
    var Connection = /** @class */ (function () {
        function Connection() {
            this._socket = new WebSocket("ws://localhost:9600");
            this._socket.onmessage = this.receive;
        }
        Connection.prototype.receive = function (event) {
            var msg = JSON.parse(event.data);
            console.log(msg);
            if ("state" in msg) {
                if (msg.state === "Setup") {
                    var temp_1 = new board_1.Board();
                    elements_1.ALL_COUNTRIES.forEach(function (country, c_index) {
                        var c = msg.board[country];
                        temp_1.changeTroops(country, c.number_of_troops);
                        temp_1.changeOwner(country, c.owner);
                    });
                    // new board temp
                }
            }
        };
        return Connection;
    }());
    exports.Connection = Connection;
});
