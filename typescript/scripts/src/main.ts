import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";
import {Moves} from "./moves";
import {countryOn} from "./map";
import {Player} from "./elements";

//-- Global Variables -----------------------

function countryClicked(e : MouseEvent, r : number, canvas : HTMLElement){
    let country = countryOn(e, r, canvas);
    if (country != null){
        document.dispatchEvent(new CustomEvent("CountryClickedOn",{detail: country}));
    }
}

function changeTroops(d) {
    const label = document.getElementById("ntroopslabel");
    label.innerHTML = parseInt(label.innerHTML) + d;
}

(async() => {
    let board = new Board([],[]);
    let conn = new Connection();
    let colour : Player = await conn.start();
    let ui = new Draw(colour);
    let moves = new Moves(colour, ui)

    conn.me = colour;
    const canvas=document.getElementById("canvas");
    
    //const popup = document.getElementById("popupBox");
    //popup.classList.add("show");
    
//-- Listeners ------------------------------
    document.getElementById("startGame").onclick = conn.start_game.bind(conn);
    document.getElementById("endAttack").onclick = (() => {document.dispatchEvent(new CustomEvent("EndAttack"))});
    document.getElementById("skipFortify").onclick = (() => {document.dispatchEvent(new CustomEvent("SkipFortify"))});
    document.getElementById("saveGame").onclick   = (() => {document.dispatchEvent(new CustomEvent("SaveGame"))});
    
    document.getElementById("submitNumberTroops").onclick = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberTroops"))});
    document.getElementById("cancelNumberTroops").onclick = (() => {document.getElementById("popupNumberTroops").style.display = "none";});
    document.getElementById("submitNumberDef").onclick    = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberDef"))});
    document.getElementById("submitNumberInv").onclick    = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberInv"))});
    document.getElementById("submitNumberFort").onclick   = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberFort"))});
    



    //-- That Pass Information In -----------
    canvas.onmouseup = function(e : MouseEvent){countryClicked(e, ui.outerRadius, canvas);};
    document.addEventListener('Setup', function (e : CustomEvent) {moves.setup(e.detail)});
    document.addEventListener('Reinforce', function (e : CustomEvent) {moves.reinforce(e.detail)});
    document.addEventListener('Attack', function (e : CustomEvent) {moves.attack(e.detail)});
    document.addEventListener('Fortify', function (e : CustomEvent) {moves.fortify(e.detail)});
    document.addEventListener('MidBattle', function (e : CustomEvent) {moves.chooseDefenders(e.detail.board, e.detail.ac, e.detail.dc, e.detail.att)});
    document.addEventListener('BattleEnd', function (e : CustomEvent) {moves.invade(e.detail.board, e.detail.ac, e.detail.dc, e.detail.attrem)});

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
    //---------------------------------------

    ui.draw(board);

})();
