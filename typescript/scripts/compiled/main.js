define(["require", "exports", "./draw", "./board", "./sock"], function (require, exports, draw_1, board_1, sock_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var ui = new draw_1.Draw();
    var board = new board_1.Board();
    var conn = new sock_1.Connection();
    board.changeOwner("Siam", "Green");
    board.changeTroops("Siam", 3);
    ui.draw(board);
});
