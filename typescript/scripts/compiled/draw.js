define(["require", "exports", "./elements", "./map", "./neighbours"], function (require, exports, elements_1, map_1, neighbours_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Draw = void 0;
    class Popup {
        constructor() {
            this._min = 0;
            this._max = 0;
            this._box = document.getElementById("popupBox");
            this._ntroops = document.getElementById("popupntroops");
            this._plusbutton = document.querySelector("#popupBox button[data-quantity='plus']");
            this._minusbutton = document.querySelector("#popupBox button[data-quantity='minus']");
            this._plusbutton.onclick = (() => {
                this.changeTroops(1);
            });
            this._minusbutton.onclick = (() => {
                this.changeTroops(-1);
            });
            this.submit = "Submit";
        }
        changeTroops(d) {
            const n = parseInt(this._ntroops.innerHTML);
            if (n + d <= this._max && n + d >= this._min) {
                this._ntroops.innerHTML = (n + d).toString();
            }
        }
        get value() {
            return parseInt(this._ntroops.innerHTML);
        }
        set visible(b) {
            if (b) {
                this._box.classList.add("show");
            }
            else {
                this._box.classList.remove("show");
            }
        }
        set label(l) {
            document.getElementById("popupLabel").innerHTML = l;
        }
        set submit(l) {
            document.getElementById("popupSubmit").innerHTML = l;
        }
        set min(newmin) {
            this._min = newmin;
            this._ntroops.innerHTML = newmin.toString();
        }
        set max(newmax) {
            const n = parseInt(this._ntroops.innerHTML);
            this._max = newmax;
            if (n > newmax) {
                this._ntroops.innerHTML = newmax.toString();
            }
        }
    }
    class Draw {
        constructor(player) {
            this.outerRadius = 35;
            this.innerRadius = 25;
            this._countryColour = {};
            const canvas = document.getElementById("canvas");
            this.popup = new Popup();
            if (!(canvas instanceof HTMLCanvasElement)) {
                throw new Error("The element of is not an HTMLCanvasElement.");
            }
            canvas.style.border = "3px solid " + this.playerToColour(player);
            console.log(player);
            this._ctx = canvas.getContext("2d");
            this._ctx.textBaseline = "middle";
            this._ctx.textAlign = "center";
            elements_1.ALL_COUNTRIES.forEach((key, index) => { this._countryColour[key] = "white"; });
        }
        setColour(c, s) {
            this._countryColour[c] = s;
        }
        clearColour() {
            elements_1.ALL_COUNTRIES.forEach((key, index) => { this._countryColour[key] = "white"; });
        }
        draw(state) {
            // draw lines
            elements_1.ALL_COUNTRIES.forEach((country, c_index) => {
                const myloc = map_1.COUNTRY_LOC[country];
                neighbours_1.NEIGHBOURS[country].forEach((neighbour, n_index) => {
                    const neighbourloc = map_1.COUNTRY_LOC[neighbour];
                    if (!(country === "Alaska" && neighbour === "Kamchatka"
                        || country === "Kamchatka" && neighbour === "Alaska")) {
                        this._ctx.beginPath();
                        this._ctx.moveTo(myloc.x, myloc.y);
                        this._ctx.lineTo(neighbourloc.x, neighbourloc.y);
                        this._ctx.stroke();
                    }
                });
            });
            //draw special lines
            const ala = map_1.COUNTRY_LOC["Alaska"];
            this._ctx.beginPath();
            this._ctx.moveTo(ala.x, ala.y);
            this._ctx.lineTo(ala.x - 60, ala.y);
            this._ctx.stroke();
            const kam = map_1.COUNTRY_LOC["Kamchatka"];
            this._ctx.beginPath();
            this._ctx.moveTo(kam.x, kam.y);
            this._ctx.lineTo(kam.x + 60, kam.y);
            this._ctx.stroke();
            elements_1.ALL_COUNTRIES.forEach((country, c_index) => {
                const myloc = map_1.COUNTRY_LOC[country];
                // draw outer circle
                this._ctx.beginPath();
                this._ctx.arc(myloc.x, myloc.y, this.outerRadius, 0, Math.PI * 2, false);
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
                        const _exhaustiveCheck = owner;
                }
                this._ctx.fillStyle = fill;
                this._ctx.fill();
                this._ctx.closePath();
                this._ctx.beginPath();
                this._ctx.arc(myloc.x, myloc.y, this.innerRadius, 0, Math.PI * 2, false);
                this._ctx.fillStyle = this._countryColour[country];
                this._ctx.fill();
                this._ctx.fillStyle = "black";
                this._ctx.font = "14px 'Helvetica'";
                this._ctx.fillText(state.troops(country) + '', myloc.x, myloc.y);
                this._ctx.closePath();
            });
            let hand = document.getElementById("hand");
            hand.innerHTML = "";
            let i;
            for (i = 0; i < state.cards.length; i++) {
                const c = state.cards[i];
                let imgstr = "";
                switch (c) {
                    case "Wild":
                        imgstr = "Wild.png";
                        break;
                    case "Infantry":
                        imgstr = "Infantry.png";
                        break;
                    case "Artillery":
                        imgstr = "Artillery.jpg";
                        break;
                    case "Cavalry":
                        imgstr = "Cavalry.jpg";
                }
                let str = "";
                str = "<div class = \"card\" id = \"card" + i.toString() + "\" data-type = \"" + c + "\"><img src = \"" + imgstr + "\" width = 100%> <div class = \"container\"><h4><b>" + c + "</b></h4></div>";
                hand.innerHTML += str;
            }
        }
        playerToColour(p) {
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
                    const _exhaustiveCheck = p;
            }
            return fill;
        }
    }
    exports.Draw = Draw;
});
