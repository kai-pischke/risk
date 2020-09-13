import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";
import {Moves} from "./moves";
import {countryOn} from "./map";
import {Player} from "./elements";

//-- Global Variables -----------------------

function countryClickedOn(e : MouseEvent, r : number, canvas : HTMLElement){
    const country = countryOn(e, r, canvas);
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

    document.getElementById("submitNumberTroops").onclick = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberTroops"))});
    document.getElementById("cancelNumberTroops").onclick = (() => {document.getElementById("popupNumberTroops").style.display = "none";});
    document.getElementById("submitNumberDef").onclick    = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberDef"))});
    document.getElementById("submitNumberInv").onclick    = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberInv"))});
    document.getElementById("submitNumberFort").onclick   = (() => {document.dispatchEvent(new CustomEvent("SubmitNumberFort"))});




    //-- That Pass Information In -----------
    canvas.onmouseup = function(e : MouseEvent){countryClickedOn(e, ui.outerRadius, canvas);};

    canvas.onmousemove = function(e : MouseEvent){
        const country = countryOn(e, ui.outerRadius, canvas);
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

    //---------------------------------------

    board.changeOwner("Siam", "Green");
    board.changeTroops("Siam", 3);

    ui.draw(board);

})();
