define(["require", "exports", "./draw", "./board", "./sock", "./map"], function (require, exports, draw_1, board_1, sock_1, map_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    //-- Global Variables -----------------------
    function countryClicked(e, r, canvas) {
        let country = map_1.countryClickedOn(e, r, canvas);
        if (country != null) {
            dispatchEvent(new CustomEvent("CountryClickedOn", { detail: country }));
        }
    }
    (async () => {
        console.log('before start');
        let ui = new draw_1.Draw();
        let board = new board_1.Board();
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        conn.me = colour;
        const canvas = document.getElementById("canvas");
        //-- Listeners --------------------------
        document.getElementById("startGame").onclick = conn.start_game.bind(conn);
        canvas.onmousedown = function (e) { countryClicked(e, ui.outerRadius, canvas); };
        addEventListener('Setup', function (e) { console.log(e.detail); ui.draw(e.detail); }, false);
        //---------------------------------------
        board.changeOwner("Siam", "Green");
        board.changeTroops("Siam", 3);
        ui.draw(board);
        console.log('after start');
    })();
});
