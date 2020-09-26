define(["require", "exports", "./draw", "./board", "./sock", "./moves", "./map"], function (require, exports, draw_1, board_1, sock_1, moves_1, map_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    //-- Global Variables -----------------------
    function countryClicked(e, r, xoff, yoff) {
        let country = map_1.countryOn(e, r, xoff, yoff);
        if (country != null) {
            document.dispatchEvent(new CustomEvent("CountryClickedOn", { detail: country }));
        }
    }
    (async () => {
        let board = new board_1.Board([], []);
        let conn = new sock_1.Connection();
        let colour = await conn.start();
        let ui = new draw_1.Draw(colour);
        let moves = new moves_1.Moves(colour, ui);
        conn.me = colour;
        const canvas = document.getElementById("canvas");
        //-- Listeners ------------------------------
        document.getElementById("startGame").onclick = conn.start_game.bind(conn);
        document.getElementById("endAttack").onclick = (() => { document.dispatchEvent(new CustomEvent("EndAttack")); });
        document.getElementById("skipFortify").onclick = (() => { document.dispatchEvent(new CustomEvent("SkipFortify")); });
        document.getElementById("saveGame").onclick = (() => { document.dispatchEvent(new CustomEvent("SaveGame")); });
        document.getElementById("popupSubmit").onclick = (() => { document.dispatchEvent(new CustomEvent("PopupSubmit")); });
        //document.getElementById("cancelNumberTroops").onclick = (() => {document.getElementById("popupNumberTroops").style.display = "none";});
        //-- That Pass Information In -----------
        document.addEventListener('Setup', function (e) { moves.setup(e.detail); });
        document.addEventListener('Reinforce', function (e) { moves.reinforce(e.detail); });
        document.addEventListener('Attack', function (e) { moves.attack(e.detail); });
        document.addEventListener('Fortify', function (e) { moves.fortify(e.detail); });
        document.addEventListener('MidBattle', function (e) { moves.chooseDefenders(e.detail.board, e.detail.ac, e.detail.dc, e.detail.att); });
        document.addEventListener('BattleEnd', function (e) { moves.invade(e.detail.board, e.detail.ac, e.detail.dc, e.detail.attrem); });
        const rect = canvas.getBoundingClientRect();
        // IDK why +1 but seems to make it recognise perfectly
        const xoff = rect.left + ui.borderSize + 1;
        const yoff = rect.top + ui.borderSize + 1;
        canvas.onmouseup = function (e) { countryClicked(e, ui.outerRadius, xoff, yoff); };
        canvas.onmousemove = function (e) {
            const country = map_1.countryOn(e, ui.outerRadius, xoff, yoff);
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
