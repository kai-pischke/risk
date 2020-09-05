import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";
import {countryClickedOn} from "./map";

//-- Global Variables -----------------------

function countryClicked(e : MouseEvent, r : number, canvas : HTMLElement){
    let country = countryClickedOn(e, r, canvas);
    if (country != null){
        dispatchEvent(new CustomEvent("CountryClickedOn",{detail: country}));
    }
}

(async() => {
    console.log('before start');



    let ui = new Draw();
    let board = new Board();
    let conn = new Connection();
    let colour : string = await conn.start();
    conn.me = colour;

    const canvas=document.getElementById("canvas");
    //-- Listeners --------------------------
    document.getElementById("startGame").onclick = conn.start_game.bind(conn);

    canvas.onmousedown = function(e : MouseEvent){countryClicked(e, ui.outerRadius, canvas);};
    addEventListener('Setup', function (e : CustomEvent) {console.log(e.detail); ui.draw(e.detail);}, false);
    //---------------------------------------

    board.changeOwner("Siam", "Green");
    board.changeTroops("Siam", 3);

    ui.draw(board);

    console.log('after start');
})();
