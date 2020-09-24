define(["require", "exports", "./elements"], function (require, exports, elements_1) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.countryOn = exports.COUNTRY_LOC = void 0;
    exports.COUNTRY_LOC = {
        "Afghanistan": { x: 800, y: 300 },
        "Alaska": { x: 100, y: 100 },
        "Alberta": { x: 100, y: 200 },
        "Argentina": { x: 200, y: 700 },
        "Brazil": { x: 268, y: 625 },
        "Central America": { x: 200, y: 400 },
        "China": { x: 900, y: 300 },
        "Congo": { x: 500, y: 600 },
        "East Africa": { x: 600, y: 600 },
        "Eastern Australia": { x: 900, y: 700 },
        "Eastern United States": { x: 248, y: 300 },
        "Egypt": { x: 600, y: 500 },
        "Great Britain": { x: 450, y: 200 },
        "Greenland": { x: 300, y: 100 },
        "Iceland": { x: 500, y: 100 },
        "India": { x: 900, y: 400 },
        "Indonesia": { x: 900, y: 550 },
        "Irkutsk": { x: 1000, y: 200 },
        "Japan": { x: 1100, y: 200 },
        "Kamchatka": { x: 1100, y: 100 },
        "Madagascar": { x: 600, y: 700 },
        "Middle East": { x: 800, y: 400 },
        "Mongolia": { x: 1000, y: 300 },
        "New Guinea": { x: 968, y: 625 },
        "North Africa": { x: 500, y: 500 },
        "Northern Europe": { x: 550, y: 200 },
        "Northwest Territory": { x: 200, y: 100 },
        "Ontario": { x: 200, y: 200 },
        "Peru": { x: 131, y: 625 },
        "Quebec": { x: 300, y: 200 },
        "Scandinavia": { x: 600, y: 100 },
        "Siam": { x: 1000, y: 400 },
        "Siberia": { x: 900, y: 200 },
        "South Africa": { x: 500, y: 700 },
        "Southern Europe": { x: 600, y: 300 },
        "Ukraine": { x: 650, y: 200 },
        "Ural": { x: 800, y: 200 },
        "Venezuela": { x: 200, y: 550 },
        "Western Australia": { x: 831, y: 625 },
        "Western Europe": { x: 500, y: 300 },
        "Western United States": { x: 151, y: 300 },
        "Yakutsk": { x: 1000, y: 100 },
    };
    function countryOn(e, r, xoff, yoff) {
        e.preventDefault();
        var mouseX = e.clientX - xoff;
        var mouseY = e.clientY - yoff;
        // iterate each shape in the shapes array
        for (var i = 0; i < elements_1.ALL_COUNTRIES.length; i++) {
            let country = elements_1.ALL_COUNTRIES[i];
            let loc = exports.COUNTRY_LOC[country];
            if ((mouseX - loc.x) ** 2 + (mouseY - loc.y) ** 2 < r ** 2) {
                return country;
            }
            ;
        }
        ;
        return null;
    }
    exports.countryOn = countryOn;
    ;
});
