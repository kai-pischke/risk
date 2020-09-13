define(["require", "exports", "./draw", "./board", "./sock", "./moves", "./map"], function (require, exports, draw_1, board_1, sock_1, moves_1, map_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    //-- Global Variables -----------------------
    function countryClickedOn(e, r, canvas) {
        const country = map_1.countryOn(e, r, canvas);
        if (country != null) {
            document.dispatchEvent(new CustomEvent("CountryClickedOn", { detail: country }));
        }
    }
    (async () => {
        let ui = new draw_1.Draw();
        let board = new board_1.Board([], []);
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        let moves = new moves_1.Moves(colour, ui);
        conn.me = colour;
        const canvas = document.getElementById("canvas");
        //-- Listeners ------------------------------
        document.getElementById("startGame").onclick = conn.start_game.bind(conn);
        document.getElementById("endAttack").onclick = (() => { document.dispatchEvent(new CustomEvent("EndAttack")); });
        document.getElementById("skipFortify").onclick = (() => { document.dispatchEvent(new CustomEvent("SkipFortify")); });
        document.getElementById("submitNumberTroops").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberTroops")); });
        document.getElementById("cancelNumberTroops").onclick = (() => { document.getElementById("popupNumberTroops").style.display = "none"; });
        document.getElementById("submitNumberDef").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberDef")); });
        document.getElementById("submitNumberInv").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberInv")); });
        document.getElementById("submitNumberFort").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberFort")); });
        //-- That Pass Information In -----------
        canvas.onmouseup = function (e) { countryClickedOn(e, ui.outerRadius, canvas); };
        canvas.onmousemove = function (e) {
            const country = map_1.countryOn(e, ui.outerRadius, canvas);
            let hover = false;
            let hoverID = "";
            if (country == null) {
                hover = false;
            }
            else {
                hover = true;
                hoverID = country;
            }
            document.getElementById("countryNameBadge").innerHTML = hover ? hoverID : "";
        };
        document.addEventListener('Setup', function (e) { moves.setup(e.detail); });
        document.addEventListener('Reinforce', function (e) { moves.reinforce(e.detail); });
        document.addEventListener('Attack', function (e) { moves.attack(e.detail); });
        document.addEventListener('Fortify', function (e) { moves.fortify(e.detail); });
        document.addEventListener('MidBattle', function (e) { moves.chooseDefenders(e.detail.board, e.detail.ac, e.detail.dc, e.detail.att); });
        document.addEventListener('BattleEnd', function (e) { moves.invade(e.detail.board, e.detail.ac, e.detail.dc, e.detail.attrem); });
        //-- That Pass Information Out ----------
        document.addEventListener('Send', function (e) { conn.send(e.detail); });
        document.addEventListener('EndAttack', function (e) {
            conn.send(JSON.stringify({
                action: "EndAttack",
                sender: colour
            }));
        });
        document.addEventListener('SkipFortify', function (e) {
            conn.send(JSON.stringify({
                action: "SkipFortify",
                sender: colour
            }));
        });
        //---------------------------------------
        board.changeOwner("Siam", "Green");
        board.changeTroops("Siam", 3);
        ui.draw(board);
    })();
});
