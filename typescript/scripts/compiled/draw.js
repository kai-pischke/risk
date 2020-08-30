define(["require", "exports", "./elements", "./map", "./neighbours"], function (require, exports, elements_1, map_1, neighbours_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.Draw = void 0;
    var Draw = /** @class */ (function () {
        function Draw() {
            var canvas = document.getElementById("canvas");
            if (!(canvas instanceof HTMLCanvasElement)) {
                throw new Error("The element of is not an HTMLCanvasElement.");
            }
            this._ctx = canvas.getContext("2d");
            this._ctx.textBaseline = "middle";
            this._ctx.textAlign = "center";
        }
        Draw.prototype.draw = function (state) {
            var _this = this;
            // draw lines
            elements_1.ALL_COUNTRIES.forEach(function (country, c_index) {
                var myloc = map_1.COUNTRY_LOC[country];
                neighbours_1.NEIGHBOURS[country].forEach(function (neighbour, n_index) {
                    var neighbourloc = map_1.COUNTRY_LOC[neighbour];
                    if (!(country === "Alaska" && neighbour === "Kamchatka"
                        || country === "Kamchatka" && neighbour === "Alaska")) {
                        _this._ctx.beginPath();
                        _this._ctx.moveTo(myloc.x, myloc.y);
                        _this._ctx.lineTo(neighbourloc.x, neighbourloc.y);
                        _this._ctx.stroke();
                    }
                });
            });
            //draw special lines
            var ala = map_1.COUNTRY_LOC["Alaska"];
            this._ctx.beginPath();
            this._ctx.moveTo(ala.x, ala.y);
            this._ctx.lineTo(ala.x - 60, ala.y);
            this._ctx.stroke();
            var kam = map_1.COUNTRY_LOC["Kamchatka"];
            this._ctx.beginPath();
            this._ctx.moveTo(kam.x, kam.y);
            this._ctx.lineTo(kam.x + 60, kam.y);
            this._ctx.stroke();
            elements_1.ALL_COUNTRIES.forEach(function (country, c_index) {
                var myloc = map_1.COUNTRY_LOC[country];
                // draw outer circle
                _this._ctx.beginPath();
                _this._ctx.arc(myloc.x, myloc.y, 35, 0, Math.PI * 2, false);
                // get fill
                var fill;
                var owner = state.owner(country);
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
                        var _exhaustiveCheck = owner;
                }
                _this._ctx.fillStyle = fill;
                _this._ctx.fill();
                _this._ctx.closePath();
                _this._ctx.beginPath();
                _this._ctx.arc(myloc.x, myloc.y, 25, 0, Math.PI * 2, false);
                _this._ctx.fillStyle = "white";
                _this._ctx.fill();
                _this._ctx.fillStyle = "black";
                _this._ctx.font = "14px 'Helvetica'";
                _this._ctx.fillText(state.troops(country) + '', myloc.x, myloc.y);
                _this._ctx.closePath();
            });
        };
        return Draw;
    }());
    exports.Draw = Draw;
});
