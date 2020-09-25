import {Country, Player, Card, ALL_COUNTRIES} from "./elements";

interface BoardCell {
    player: Player | "Empty";
    troops: number;
}

type State = Record<Country, BoardCell>;

export class Board {

    private _board: State;
    private _players: Array<Player>;
    private _myCards: Array<Card>;

    constructor(ps : Array<Player>, cs : Array<Card>) {
        this._board = {} as State;
        this._players = ps
        this._myCards = cs
        ALL_COUNTRIES.forEach((key, index) => {this._board[key] = {player: "Empty", troops: 0}});
    }

    troops(c: Country) : number {
        return this._board[c].troops;
    }

    owner(c: Country) : Player | "Empty" {
        return this._board[c].player;
    }

    changeTroops(c: Country, t: number) {
        this._board[c].troops = t;
    }

    changeOwner(c: Country, p: Player) {
        this._board[c].player = p;
    }

    removeCard(c : Card) {
        const i = this._myCards.indexOf(c)
        if (i != -1){
            this._myCards = this._myCards.splice(i,1)
        }
    }

    get cards() {
        return this._myCards;
    }

    get players() {
        return this._players;
    }

    public numberToReinforce(p : Player): number {
        const playersCountries: Array<Country> = ALL_COUNTRIES.filter((c : Country) => {return this.owner(c) == p;});
        const northAmerica: Array<Country> = ["Alaska", "Alberta", "Central America", "Eastern United States", "Greenland", "Northwest Territory", "Ontario", "Quebec", "Western United States"];
        const southAmerica: Array<Country> = ["Argentina", "Brazil", "Peru", "Venezuela"];
        const europe: Array<Country> = ["Great Britain", "Iceland", "Northern Europe", "Scandinavia", "Southern Europe", "Ukraine", "Western Europe"];
        const africa: Array<Country> = ["Congo", "East Africa", "Egypt", "Madagascar", "North Africa", "South Africa"];
        const asia: Array<Country> = ["Afghanistan", "China", "India", "Irkutsk", "Japan", "Kamchatka", "Middle East", "Mongolia", "Siam", "Siberia", "Ural", "Yakutsk"];
        const australia: Array<Country> = ["Eastern Australia", "Indonesia", "New Guinea", "Western Australia"];
        const continents: Array<[number, Array<Country>]> = [[5, northAmerica],[2, southAmerica],[5, europe],[3, africa],[7, asia],[2, australia]];

        const fromCountry = Math.floor(playersCountries.length/3)
        var numTroops = Math.max(fromCountry, 3)

        console.log(numTroops)
        for (var i = 0;i<continents.length;i++){
            if (continents[i][1].every((c: Country) => {return playersCountries.includes(c);})){
                numTroops += continents[i][0]
            }
        }
        console.log(numTroops);
        return numTroops;
    }
}
