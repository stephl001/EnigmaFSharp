type RotorId = I|II|III|IV|V|VI|VII|VIII
type ReflectorId = A|B|C|ETW
type Letter = 
    |A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    static member index = function
        | A -> 0 | B -> 1 | C -> 2 | D -> 3 | E -> 4 | F -> 5 | G -> 6 | H -> 7 | I -> 8
        | J -> 9 | K -> 10 | L -> 11 | M -> 12 | N -> 13 | O -> 14 | P -> 15 | Q -> 16 | R -> 17
        | S -> 18 | T -> 19 | U -> 20 | V -> 21 | W -> 22 | X -> 23 | Y -> 24 | Z -> 25
    static member fromIndex = function
        | 0 -> A | 1 -> B | 2 -> C | 3 -> D | 4 -> E | 5 -> F | 6 -> G | 7 -> H | 8 -> I
        | 9 -> J | 10 -> K | 11 -> L | 12 -> M | 13 -> N | 14 -> O | 15 -> P | 16 -> Q | 17 -> R
        | 18 -> S | 19 -> T | 20 -> U | 21 -> V | 22 -> W | 23 -> X | 24 -> Y | 25 -> Z
        | _ -> failwith "Indoex out of range"

type Mapping = 
    private Mapping of Letter array
        static member Create letters =
            let lettersCount = letters |> Array.distinct |> Array.length
            if lettersCount <> 26
            then failwith "A mapping must be composed of 26 distinct letters"
            else Mapping letters
          
        static member id =
            Mapping [|A;B;C;D;E;F;G;H;I;J;K;L;M;N;O;P;Q;R;S;T;U;V;W;X;Y;Z|]

        static member Get (Mapping m) = m


type Rotor = {
    Id: RotorId
    Notch: Letter
    Mapping: Mapping
}

type Reflector = {
    Id: ReflectorId
    Mapping: Mapping
}

type Wheel = {
    Rotor: Rotor
    RingSetting: Letter
}

type Plugboard = {
    Mapping: Mapping
}

type EnigmaMachine = {
    Plugboard: Plugboard
    Wheel1: Wheel
    Wheel2: Wheel
    Wheel3: Wheel
    Reflector: Reflector
}

type Encoder = EnigmaMachine -> Letter -> Letter

let rotorI = {Id=RotorId.I; Notch=Q; Mapping=Mapping.Create [|E;K;M;F;L;G;D;Q;V;Z;N;T;O;W;Y;H;X;U;S;P;A;I;B;R;C;J|]}
let rotorII = {Id=II; Notch=E; Mapping=Mapping.Create [|A;J;D;K;S;I;R;U;X;B;L;H;W;T;M;C;Q;G;Z;N;P;Y;F;V;O;E|]}
let rotorIII = {Id=III; Notch=V; Mapping=Mapping.Create [|B;D;F;H;J;L;C;P;R;T;X;V;Z;N;Y;E;I;W;G;A;K;M;U;S;Q;O|]}
let rotorIV = {Id=IV; Notch=J; Mapping=Mapping.Create [|E;S;O;V;P;Z;J;A;Y;Q;U;I;R;H;X;L;N;F;T;G;K;D;C;M;W;B|]}
let rotorV = {Id=RotorId.V; Notch=Z; Mapping=Mapping.Create [|V;Z;B;R;G;I;T;Y;U;P;S;D;N;H;L;X;A;W;M;J;Q;O;F;E;C;K|]}

let reflectorA = {Id=ReflectorId.A; Mapping=Mapping.Create [|E;J;M;Z;A;L;Y;X;V;B;W;F;C;R;Q;U;O;N;T;S;P;I;K;H;G;D|]}
let reflectorB = {Id=ReflectorId.B; Mapping=Mapping.Create [|Y;R;U;H;Q;S;L;D;P;X;N;G;O;K;M;I;E;B;F;Z;C;W;V;J;A;T|]}
let reflectorC = {Id=ReflectorId.C; Mapping=Mapping.Create [|F;V;P;J;I;A;O;Y;E;D;R;Z;X;W;G;C;T;K;U;Q;S;B;N;M;H;L|]}
let reflectorETW = {Id=ETW; Mapping=Mapping.id}

let inverseMapping (mapping:Mapping) =
    (Mapping.Get Mapping.id, Mapping.Get mapping) 
    ||> Array.zip 
    |> Array.sortBy snd
    |> Array.map fst
    |> Mapping.Create

let offsetLetter offset letter =
    let indexLetter = Letter.index letter
    (indexLetter + offset) % 26 |> Letter.fromIndex

let reverseOffsetLetter offset =
    offsetLetter (-offset)

let mapLetter mapping letter =
    let arrayMapping = Mapping.Get mapping
    arrayMapping.[Letter.index letter]

let offsetMapping offset mapping = 
    offsetLetter offset >> mapLetter mapping >> reverseOffsetLetter offset