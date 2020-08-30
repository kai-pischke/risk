export type Phase = "Waiting" | "Setup" | "Playing";

export const ALL_COUNTRIES = ["Afghanistan","Alaska","Alberta","Argentina","Brazil","Central America","China","Congo","East Africa","Eastern Australia","Eastern United States","Egypt","Great Britain","Greenland","Iceland","India","Indonesia","Irkutsk","Japan","Kamchatka","Madagascar","Middle East","Mongolia","New Guinea","North Africa","Northern Europe","Northwest Territory","Ontario","Peru","Quebec","Scandenavia","Siam","Siberia","South Africa","Southern Europe","Ukraine","Ural","Venezuela","Western Australia","Western Europe","Western United States","Yakutsk"] as const;

type CountryList = typeof ALL_COUNTRIES;

export type Country = CountryList[number];
                   
export type Player = "Black" | "Blue" | "Green" | "Red" | "Yellow";