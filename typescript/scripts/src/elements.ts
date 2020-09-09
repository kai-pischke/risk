export const ALL_PHASE = ["Waiting", "Setup", "Playing"] as const;

type PhaseList = typeof ALL_PHASE;

export type Phase = PhaseList[number];


export const ALL_COUNTRIES = ["Afghanistan","Alaska","Alberta","Argentina","Brazil","Central America","China","Congo","East Africa","Eastern Australia","Eastern United States","Egypt","Great Britain","Greenland","Iceland","India","Indonesia","Irkutsk","Japan","Kamchatka","Madagascar","Middle East","Mongolia","New Guinea","North Africa","Northern Europe","Northwest Territory","Ontario","Peru","Quebec","Scandinavia","Siam","Siberia","South Africa","Southern Europe","Ukraine","Ural","Venezuela","Western Australia","Western Europe","Western United States","Yakutsk"] as const;

type CountryList = typeof ALL_COUNTRIES;

export type Country = CountryList[number];

export const ALL_PLAYERS = ["Black", "Blue", "Green", "Red", "Yellow"] as const;
type PlayerTuple = typeof ALL_PLAYERS;
export type Player = PlayerTuple[number];

export const ALL_CARDS = ["Artillery", "Cavalry", "Infantry", "Wild"] as const;
type CardTuple = typeof ALL_CARDS;
export type Card = CardTuple[number];
