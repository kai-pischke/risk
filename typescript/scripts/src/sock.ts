import {Country, Player, Card, ALL_COUNTRIES, ALL_PLAYERS} from "./elements";
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
        this.send("{\"action\": \"StartGame\", \"sender\": \""+this.me+"\"}");
    }

    private receive(event) {
        const msg = JSON.parse(event.data);
        //console.log("Recieved : " + msg);
        if ("kind" in msg) {
            if (msg.kind === "colour") {
                const colour = msg.colour;
                console.log("I am " + colour);
            } else if (msg.kind === "State"){
                var cards : Array<Card> = []
                if (msg.state === "WaitingRoom"){
                    return;
                } else if (msg.state === "Play"){
                    cards = msg.cards[this.me].sort;
                }

                const temp = new Board(msg.players, cards);
                ALL_COUNTRIES.forEach((country, c_index) => {
                    const c = msg.board[country];
                    temp.changeTroops(country, c.number_of_troops);
                    if (ALL_PLAYERS.includes(c.owner)) {
                        temp.changeOwner(country, c.owner);
                    }
                });
                if (msg.state === "Setup"){
                    console.log("sent")
                    document.dispatchEvent(new CustomEvent("Setup",{detail: temp}));

                } else if (msg.state === "Play"){
                    switch (msg.phase.kind) {
                        case "Simple":
                            document.dispatchEvent(new CustomEvent(msg.phase.phase,
                                                  {detail: temp}));
                        case "BattleEnd":
                            document.dispatchEvent(new CustomEvent("BattleEnd",
                                                  {detail: {board :temp,
                                                            ac: msg.phase.attacking_country,
                                                            dc: msg.phase.defending_country,
                                                            attrem: msg.phase.attackers_remaining}}));
                        case "MidBattle":
                            document.dispatchEvent(new CustomEvent("BattleEnd",
                                                  {detail: {board :temp,
                                                            ac: msg.phase.attacking_country,
                                                            dc: msg.phase.defending_country,
                                                            att: msg.phase.attackers}}));
                    }
                }
            } else if (msg.kind === "Won") {
                document.dispatchEvent(new CustomEvent("Won", {detail: msg.winner}));
            } else if (msg.kind === "Question") {
                document.dispatchEvent(new CustomEvent("Question", {detail: msg.question}));
            }
        }
    }
}
