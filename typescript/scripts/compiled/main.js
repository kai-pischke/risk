define(["require", "exports", "./draw", "./board", "./sock", "./moves", "./map"], function (require, exports, draw_1, board_1, sock_1, moves_1, map_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    //-- Global Variables -----------------------
    function countryClicked(e, r, canvas) {
        let country = map_1.countryClickedOn(e, r, canvas);
        if (country != null) {
            document.dispatchEvent(new CustomEvent("CountryClickedOn", { detail: country }));
        }
    }
    (async () => {
        let ui = new draw_1.Draw();
        let board = new board_1.Board();
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        let moves = new moves_1.Moves(colour, ui);
        conn.me = colour;
        const canvas = document.getElementById("canvas");
        //-- Listeners ------------------------------
        document.getElementById("startGame").onclick = conn.start_game.bind(conn);
        //-- That Pass Information In -----------
        canvas.onmousedown = function (e) { countryClicked(e, ui.outerRadius, canvas); };
        document.addEventListener('Setup', function (e) { moves.setup(e.detail.board, e.detail.players[0]); });
        //-- That Pass Information Out ----------
        document.addEventListener('Send', function (e) { conn.send(e.detail); });
        //---------------------------------------
        board.changeOwner("Siam", "Green");
        board.changeTroops("Siam", 3);
        ui.draw(board);
    })();
});
