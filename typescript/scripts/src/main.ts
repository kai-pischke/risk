import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";
import {Moves} from "./moves";
import {countryOn} from "./map";
import {Player} from "./elements";

//-- Global Variables -----------------------

function countryClicked(e : MouseEvent, ui: Draw, canvas : HTMLElement){
    const rect = canvas.getBoundingClientRect();
    const r = ui.outerRadius;
    // IDK why +1 but makes work
    const xoff = rect.left + ui.borderSize + 1;
    const yoff = rect.top + ui.borderSize + 1;

    let country = countryOn(e, r, xoff, yoff);
    if (country != null){
        document.dispatchEvent(new CustomEvent("CountryClickedOn",{detail: country}));
    }
}



(async() => {
    let board = new Board([],[]);
    let conn = new Connection();
    let colour : Player = await conn.start();
    let ui = new Draw(colour);
    let moves = new Moves(colour, ui)

    conn.me = colour;
    const canvas=document.getElementById("canvas");

    //-- Listeners ------------------------------
    document.getElementById("startGame").onclick = conn.start_game.bind(conn);
    document.getElementById("endAttack").onclick = (() => {document.dispatchEvent(new CustomEvent("EndAttack"))});
    document.getElementById("skipFortify").onclick = (() => {document.dispatchEvent(new CustomEvent("SkipFortify"))});
    document.getElementById("saveGame").onclick   = (() => {document.dispatchEvent(new CustomEvent("SaveGame"))});
    document.getElementById("loadGame").onclick   = (() => {document.dispatchEvent(new CustomEvent("LoadGame"))});
    
    document.getElementById("popupSubmit").onclick = (() => {document.dispatchEvent(new CustomEvent("PopupSubmit"))});
    document.getElementById("popupCancel").onclick = (() => {document.dispatchEvent(new CustomEvent("PopupCancel"))});
    //document.getElementById("cancelNumberTroops").onclick = (() => {document.getElementById("popupNumberTroops").style.display = "none";});

    //-- That Pass Information In -----------
    document.addEventListener('Setup', function (e : CustomEvent) {moves.setup(e.detail)});
    document.addEventListener('Reinforce', function (e : CustomEvent) {moves.reinforce(e.detail)});
    document.addEventListener('Attack', function (e : CustomEvent) {moves.attack(e.detail)});
    document.addEventListener('Fortify', function (e : CustomEvent) {moves.fortify(e.detail)});
    document.addEventListener('MidBattle', function (e : CustomEvent) {moves.chooseDefenders(e.detail.board, e.detail.ac, e.detail.dc, e.detail.att)});
    document.addEventListener('BattleEnd', function (e : CustomEvent) {moves.invade(e.detail.board, e.detail.ac, e.detail.dc, e.detail.attrem)});
    document.addEventListener('TimeToTrade', function (e : CustomEvent) {moves.getTrade(e.detail)});




    // IDK why +1 but seems to make it recognise perfectly
    canvas.onmouseup = function(e : MouseEvent){countryClicked(e, ui, canvas);};
    canvas.onmousemove = function(e : MouseEvent){
        const rect = canvas.getBoundingClientRect();
        const r = ui.outerRadius;
        // IDK why +1 but makes work
        const xoff = rect.left + ui.borderSize + 1;
        const yoff = rect.top + ui.borderSize + 1;

        const country = countryOn(e, r, xoff, yoff);
        let hover = false;
        let hoverID = "";
        if (country == null){
            hover = false;
        } else {
            hover = true;
            hoverID = country;
        }
        document.getElementById("countryNameBadge").innerHTML = hover ? hoverID : "";
    }

    //-- That Pass Information Out ----------
    document.addEventListener('Send', function (e : CustomEvent) {conn.send(e.detail)});
    document.addEventListener('EndAttack', function (e : CustomEvent) {
        conn.send(JSON.stringify({
            action: "EndAttack",
            sender: colour
        }));
    });
    document.addEventListener('SkipFortify', function (e : CustomEvent) {
        conn.send(JSON.stringify({
            action: "SkipFortify",
            sender: colour
        }));
    });
    document.addEventListener('SaveGame', function (e : CustomEvent) {
        alert("save");
        conn.send(JSON.stringify({
            action: "SaveGame",
            sender: colour
        }));
    });
    document.addEventListener('LoadGame', function (e : CustomEvent) {
        const response = prompt("Enter a Game Id", "0");
        if (response != null && response != "") {
            const i = parseInt(response, 10);
            if (i != null) {
                conn.send(JSON.stringify({
                    action: "LoadGame",
                    game_id: i,
                    sender: colour
                }));
            }
        }

        
    });
    //---------------------------------------

    ui.draw(board);

})();
