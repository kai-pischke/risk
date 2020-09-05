import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";
import {Moves} from "./moves";
import {countryClickedOn} from "./map";

//-- Global Variables -----------------------

function countryClicked(e : MouseEvent, r : number, canvas : HTMLElement){
    let country = countryClickedOn(e, r, canvas);
    if (country != null){
        document.dispatchEvent(new CustomEvent("CountryClickedOn",{detail: country}));
    }
}

(async() => {
    let ui = new Draw();
    let board = new Board();
    let conn = new Connection();
    let colour : string = await conn.start();
    let moves = new Moves(colour, ui)

    conn.me = colour;
    const canvas=document.getElementById("canvas");

//-- Listeners ------------------------------
    document.getElementById("startGame").onclick = conn.start_game.bind(conn);

    //-- That Pass Information In -----------
    canvas.onmousedown = function(e : MouseEvent){countryClicked(e, ui.outerRadius, canvas);};
    document.addEventListener('Setup', function (e : CustomEvent) {moves.setup(e.detail.board, e.detail.players[0])});

    //-- That Pass Information Out ----------
    document.addEventListener('Send', function (e : CustomEvent) {conn.send(e.detail)});

    //---------------------------------------

    board.changeOwner("Siam", "Green");
    board.changeTroops("Siam", 3);

    ui.draw(board);

})();
