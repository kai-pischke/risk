import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";
import {COUNTRY_LOC} from "./map";
import {NEIGHBOURS} from "./neighbours";

class Popup{
    private _box: HTMLElement;
    private _ntroops: HTMLElement;
    private _plusbutton: HTMLElement;
    private _minusbutton: HTMLElement;
    private _min: number = 0;
    private _max: number = 0;

    constructor() {
        this._box = document.getElementById("popupBox");

        this._ntroops = document.getElementById("popupntroops");
        this._plusbutton = document.querySelector("#popupBox button[data-quantity='plus']") as HTMLElement;
        this._minusbutton = document.querySelector("#popupBox button[data-quantity='minus']") as HTMLElement;

        this._plusbutton.onclick = (() => {
          this.changeTroops(1);
        });
        this._minusbutton.onclick = (() => {
          this.changeTroops(-1);
        });
        this.submit = "Submit";
    }

    private changeTroops(d) {
        const n = parseInt(this._ntroops.innerHTML)
        if (n+d <= this._max && n+d >= this._min){
            this._ntroops.innerHTML = (n + d).toString();
        }

    }

    public get value(): number {
        return parseInt(this._ntroops.innerHTML);
    }

    public set visible(b: Boolean){
        if (b){
            this._box.classList.add("show");
        } else {
            this._box.classList.remove("show");
        }
    }

    public set label(l: string){
        document.getElementById("popupLabel").innerHTML = l;
    }

    public set submit(l: string){
        document.getElementById("popupSubmit").innerHTML = l;
    }

    public set min(newmin: number){
        this._min = newmin;
        this._ntroops.innerHTML = newmin.toString();
    }

    public set max(newmax: number){
        const n = parseInt(this._ntroops.innerHTML);
        this._max = newmax;
        if (n> newmax){
            this._ntroops.innerHTML = newmax.toString();
        }
    }
}

export class Draw{

    private _ctx: CanvasRenderingContext2D;
    private colour: string;
    public outerRadius = 35;
    public innerRadius = 25;
    public borderSize = 3;
    public popup: Popup;
    private _countryColour = {} as Record<Country, string>;

    constructor(player:Player) {
        const canvas = document.getElementById("canvas");

        this.popup = new Popup();

        if (!(canvas instanceof HTMLCanvasElement)) {
            throw new Error("The element of is not an HTMLCanvasElement.");
        }
        this.colour = this.playerToColour(player)
        canvas.style.border = this.borderSize.toString() + "px solid " + this.colour;
        console.log(player);

        this._ctx = canvas.getContext("2d");
        this._ctx.textBaseline = "middle";
        this._ctx.textAlign = "center";

        ALL_COUNTRIES.forEach((key, index) => {this._countryColour[key] = "white"});

    }

    public setColour(c : Country, s: string){
        this._countryColour[c] = s;
    }

    public clearColour(){
        ALL_COUNTRIES.forEach((key, index) => {this._countryColour[key] = "white"});
    }

    public draw(state: Board) {
        // draw lines
        this._ctx.fillStyle = "white";
        ALL_COUNTRIES.forEach((country, c_index) => {
            const myloc = COUNTRY_LOC[country];
            NEIGHBOURS[country].forEach((neighbour, n_index) => {
                const neighbourloc = COUNTRY_LOC[neighbour];
                if (!(country === "Alaska" && neighbour === "Kamchatka"
                  || country  === "Kamchatka" && neighbour === "Alaska")) {
                    this._ctx.beginPath();
                    this._ctx.moveTo(myloc.x, myloc.y);
                    this._ctx.lineTo(neighbourloc.x, neighbourloc.y);
                    this._ctx.stroke();
                }
            });
        });

        //draw special lines
        const ala = COUNTRY_LOC["Alaska"];
        this._ctx.beginPath();
        this._ctx.moveTo(ala.x, ala.y);
        this._ctx.lineTo(ala.x-60, ala.y);
        this._ctx.stroke();

        const kam = COUNTRY_LOC["Kamchatka"];
        this._ctx.beginPath();
        this._ctx.moveTo(kam.x, kam.y);
        this._ctx.lineTo(kam.x+60, kam.y);
        this._ctx.stroke();

        ALL_COUNTRIES.forEach((country, c_index) => {

            const myloc = COUNTRY_LOC[country];

            // draw outer circle
            this._ctx.beginPath();
            this._ctx.arc(myloc.x, myloc.y, this.outerRadius, 0, Math.PI*2, false);

            // get fill
            const fill = this.playerToColour(state.owner(country));

            this._ctx.fillStyle = fill;
            this._ctx.fill();
            this._ctx.closePath();

            this._ctx.beginPath();
            this._ctx.arc(myloc.x, myloc.y, this.innerRadius, 0, Math.PI*2, false);
            this._ctx.fillStyle = this._countryColour[country];
            this._ctx.fill();
            this._ctx.fillStyle = "black";
            this._ctx.font = "14px 'Helvetica'";
            this._ctx.fillText(state.troops(country)+'', myloc.x, myloc.y);
            this._ctx.closePath();
        });


        let hand = document.getElementById("hand");
        hand.innerHTML = "";
        let i;
        for (i = 0; i < state.cards.length; i++){
            const c = state.cards[i];
            let imgstr = ""

            switch (c) {
                case "Wild":
                    imgstr = "Wild.png"
                    break;
                    case "Infantry":
                        imgstr = "Infantry.png"
                        break;
                    case "Artillery":
                        imgstr = "Artillery.jpg"
                        break;
                    case "Cavalry":
                        imgstr = "Cavalry.jpg"
            }

            let str = ""
            str = "<div class = \"card\" id = \"card" + i.toString() + "\" data-type = \"" + c + "\"><img src = \"" + imgstr + "\" width = 100%> <div class = \"container\"><h4><b>" + c + "</b></h4></div>";
            hand.innerHTML += str;
        }

        const numPlayers = state.players.length
        const s = 600/numPlayers

        for (i=0; i< numPlayers; i++){
            this._ctx.fillStyle = this.playerToColour(state.players[i]);
            this._ctx.fillRect(1200, 200 + i*s, 200, s);
            this._ctx.fillStyle = "white";
            this._ctx.font = "40px 'Helvetica'";
            this._ctx.fillText(state.numberToReinforce(state.players[i]).toString(), 1300, 200 + (i+.5)*s);
        }

        this._ctx.fillStyle = "white";
        this._ctx.fillRect(1200, 0, 200, 200);
    }

    private playerToColour(p : Player | "Empty") : string {
      let fill;
      switch (p) {
          case "Black":
              fill = "black";
              break;
          case "Blue":
              fill = "#63ace5";
              break;
          case "Green":
              fill = "#7fbf7f";
              break;
          case "Red":
              fill = "#ff6f69";
              break;
          case "Yellow":
              fill = "#ffcc5c";
              break;
          case "Empty":
              fill = "#b266b2";
              break;
          default:
              const _exhaustiveCheck: never = p;
      }
      return fill;
    }

    public addPhase(p : string){
        this._ctx.fillStyle = "black";
        this._ctx.font = "40px 'Helvetica'";
        this._ctx.fillText(p, 1300, 50);
    }

    public addRecruit(toRecruit: (number | null)){
        if (toRecruit != null){
            this._ctx.fillStyle = "black";
            this._ctx.font = "40px 'Helvetica'";
            this._ctx.fillText(toRecruit.toString(), 1300, 150);
        }
    }

}
