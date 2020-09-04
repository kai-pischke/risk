import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";


(async() => {
  console.log('before start');

  let ui = new Draw();
  let board = new Board();
  let conn = new Connection();
  let colour : string = await conn.start();
  conn.me = colour;

  document.getElementById("startGame").onclick = conn.start_game.bind(conn);


  board.changeOwner("Siam", "Green");
  board.changeTroops("Siam", 3);

  ui.draw(board);
  addEventListener('Setup', function (e : CustomEvent) {console.log(e.detail); ui.draw(e.detail);}, false);
  console.log('after start');
})();
