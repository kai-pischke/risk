define(["require", "exports", "./draw", "./board", "./sock", "./moves", "./map"], function (require, exports, draw_1, board_1, sock_1, moves_1, map_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    //-- Global Variables -----------------------
    function countryClicked(e, r, canvas) {
        let country = map_1.countryOn(e, r, canvas);
        if (country != null) {
            document.dispatchEvent(new CustomEvent("CountryClickedOn", { detail: country }));
        }
    }
    function changeTroops(d) {
        const label = document.getElementById("ntroopslabel");
        label.innerHTML = parseInt(label.innerHTML) + d;
    }
    (async () => {
        let board = new board_1.Board([], []);
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        let ui = new draw_1.Draw(colour);
        let moves = new moves_1.Moves(colour, ui);
        conn.me = colour;
        const canvas = document.getElementById("canvas");
        //const popup = document.getElementById("popupBox");
        //popup.classList.add("show");
        //-- Listeners ------------------------------
        document.getElementById("startGame").onclick = conn.start_game.bind(conn);
        document.getElementById("endAttack").onclick = (() => { document.dispatchEvent(new CustomEvent("EndAttack")); });
        document.getElementById("skipFortify").onclick = (() => { document.dispatchEvent(new CustomEvent("SkipFortify")); });
        document.getElementById("saveGame").onclick = (() => { document.dispatchEvent(new CustomEvent("SaveGame")); });
        document.getElementById("submitNumberTroops").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberTroops")); });
        document.getElementById("cancelNumberTroops").onclick = (() => { document.getElementById("popupNumberTroops").style.display = "none"; });
        document.getElementById("submitNumberDef").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberDef")); });
        document.getElementById("submitNumberInv").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberInv")); });
        document.getElementById("submitNumberFort").onclick = (() => { document.dispatchEvent(new CustomEvent("SubmitNumberFort")); });
        //-- That Pass Information In -----------
        canvas.onmouseup = function (e) { countryClicked(e, ui.outerRadius, canvas); };
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
        document.addEventListener('SaveGame', function (e) {
            alert("save");
            conn.send(JSON.stringify({
                action: "SaveGame",
                sender: colour
            }));
        });
        //---------------------------------------
        ui.draw(board);
    })();
});
