import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";

let ui = new Draw();
let board = new Board();
let conn = new Connection();
let colour = await conn.start();
console.log(colour);
board.changeOwner("Siam", "Green");
board.changeTroops("Siam", 3);

ui.draw(board);
