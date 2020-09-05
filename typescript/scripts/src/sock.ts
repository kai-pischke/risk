import {Country, Player, ALL_COUNTRIES, ALL_PLAYERS} from "./elements";
import {Board} from "./board";


export class Connection {
    private _socket: WebSocket;
    public a = 10;
    public me : string;

    constructor() {

    }
    async start() : Promise<string> {
        this._socket = new WebSocket("ws://localhost:9600");
        this._socket.onmessage = this.receive;
        return new Promise((resolve,reject) =>
            this._socket.addEventListener('message', function(event) {
                const msg = JSON.parse(event.data);
                console.log(msg);
                if ("kind" in msg && msg.kind === "colour") {
                    const colour = msg.colour;
                    resolve(msg.colour);
                } else {
                    reject();
                }
            })
        );
    }

    async send(info : string){
        this._socket.send(info);
    }


    async start_game(event : MouseEvent) {
        this._socket.send("{\"action\": \"StartGame\", \"sender\": \""+this.me+"\"}");
    }

    private receive(event) {
        const msg = JSON.parse(event.data);
        //console.log("Recieved : " + msg);
        if ("kind" in msg) {
            if (msg.kind === "colour") {
                const colour = msg.colour;
                console.log("I am " + colour);
            }
            if (msg.state === "Setup") {
                const temp = new Board();
                ALL_COUNTRIES.forEach((country, c_index) => {
                    const c = msg.board[country];
                    temp.changeTroops(country, c.number_of_troops);
                    if (ALL_PLAYERS.includes(c.owner)) {
                        temp.changeOwner(country, c.owner);
                    }
                });
                console.log("sent")
                document.dispatchEvent(new CustomEvent("Setup",{detail: {board :temp, players : msg.players}}));
                // new board temp
            }
        }
    }
}
