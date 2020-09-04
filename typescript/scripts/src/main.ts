import {Draw} from "./draw";
import {Board} from "./board";
import {Connection} from "./sock";


(async() => {
  console.log('before start');

  let ui = new Draw();
  let board = new Board();
  let conn = new Connection();
  let colour = await conn.start();
  
  document.getElementById("startGame").onclick(conn.start_game);
  
  console.log(colour);
  board.changeOwner("Siam", "Green");
  board.changeTroops("Siam", 3);

  ui.draw(board);
  
  console.log('after start');
})();
