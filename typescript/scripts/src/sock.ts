import {Country, Player, Card, ALL_COUNTRIES, ALL_PLAYERS} from "./elements";
import {Board} from "./board";


export class Connection {
    private _socket: WebSocket;
    public a = 10;
    public me : string;
    private previousMessage = null;

    constructor() {

    }
    async start() : Promise<Player> {
        this._socket = new WebSocket("ws://localhost:9600");
        this._socket.onmessage = this.receive.bind(this);
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
        //console.log(info)
        this._socket.send(info);
    }


    async start_game(event : MouseEvent) {
        this.send("{\"action\": \"StartGame\", \"sender\": \""+this.me+"\"}");
    }



    private receive(event) {
        //console.log(event.data)
        const msg = JSON.parse(event.data);
        if ("kind" in msg) {
            if (msg.kind === "colour") {
                const colour = msg.colour;
                console.log("I am " + colour);
            } else if (msg.kind === "State"){
                var cards : Array<Card> = []
                if (msg.state === "WaitingRoom"){
                    return;
                } else if (msg.state === "Play"){
                    cards = msg.cards[this.me];
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
                    this.previousMessage = event;
                    document.dispatchEvent(new CustomEvent("Setup",{detail: temp}));

                } else if (msg.state === "Play"){
                    if (msg.phase.kind === "Simple") {
                        this.previousMessage = event;
                        document.dispatchEvent(new CustomEvent(msg.phase.phase,
                                              {detail: temp}));
                    } else if (msg.phase.kind === "BattleEnd") {
                        this.previousMessage = event;
                        console.log(temp);
                        document.dispatchEvent(new CustomEvent("BattleEnd",
                                              {detail: {board :temp,
                                                        ac: msg.phase.attacking_country,
                                                        dc: msg.phase.defending_country,
                                                        attrem: msg.phase.attackers_remaining}}));
                    } else if (msg.phase.kind === "MidBattle") {
                        this.previousMessage = event;
                        document.dispatchEvent(new CustomEvent("MidBattle",
                                              {detail: {board :temp,
                                                        ac: msg.phase.attacking_country,
                                                        dc: msg.phase.defending_country,
                                                        att: msg.phase.attackers}}));
                    }
                }
            } else if (msg.kind === "Won") {
                this.previousMessage = event;
                document.dispatchEvent(new CustomEvent("Won", {detail: msg.winner}));
            } else if (msg.kind === "Question") {
                this.previousMessage = event;
                document.dispatchEvent(new CustomEvent(msg.question, {detail: msg.player}));
            } else if (msg.kind === "Error") {
                this.receive(this.previousMessage);
            }

        }

    }
}
