define(["require", "exports", "./draw", "./board", "./sock"], function (require, exports, draw_1, board_1, sock_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    (async () => {
        console.log('before start');
        let ui = new draw_1.Draw();
        let board = new board_1.Board();
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        conn.me = colour;
        document.getElementById("startGame").onclick = conn.start_game;
        console.log(colour);
        board.changeOwner("Siam", "Green");
        board.changeTroops("Siam", 3);
        ui.draw(board);
        console.log('after start');
    })();
});
