import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";


export class Connection {
    private _socket: WebSocket;

    constructor() {
        this._socket = new WebSocket("ws://localhost:9600");
        this._socket.onmessage = this.receive
    } 
    
    private receive(event) {
        const msg = JSON.parse(event.data);
        console.log(msg);
        if ("state" in msg) {
            if (msg.state === "Setup") {
                const temp = new Board();
                ALL_COUNTRIES.forEach((country, c_index) => {
                    const c = msg.board[country];
                    temp.changeTroops(country, c.number_of_troops);
                    temp.changeOwner(country, c.owner);
                }); 
                // new board temp
            }
        }
    }
}