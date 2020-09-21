import {Country, Player, ALL_COUNTRIES} from "./elements";
import {Board} from "./board";
import {COUNTRY_LOC} from "./map";
import {NEIGHBOURS} from "./neighbours";

export class Draw{

    private _ctx: CanvasRenderingContext2D;
    public outerRadius = 35;
    public innerRadius = 25;
    private _countryColour = {} as Record<Country, string>;

    constructor(player:Player) {
        const canvas = document.getElementById("canvas");

        if (!(canvas instanceof HTMLCanvasElement)) {
            throw new Error("The element of is not an HTMLCanvasElement.");
        }
        canvas.style.border = "3px solid " + this.playerToColour(player);
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
            let fill;
            const owner = state.owner(country);
            switch (owner) {
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
                    const _exhaustiveCheck: never = owner;
            }

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

}
