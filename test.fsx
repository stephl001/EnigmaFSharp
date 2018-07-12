type RotorId = I|II|III|IV|V|VI|VII|VIII
type ReflectorId = A|B|C|ETW
type Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
type Mapping = private Mapping of Letter array

type Rotor = private {
    Notch: Letter
    Mapping: Mapping
    InnerRingOffset: Letter
}

type Reflector = {
    Mapping: Mapping
}

type Wheel = private {
    Rotor: Rotor
    RotorPosition: Letter
    IsInNotchPosition: bool
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

module Letter =
    [<Literal>] 
    let LettersCount = 26

    let (|IndexLetter|) = function
        | A -> 0 | B -> 1 | C -> 2 | D -> 3 | E -> 4 | F -> 5 | G -> 6 | H -> 7 | I -> 8
        | J -> 9 | K -> 10 | L -> 11 | M -> 12 | N -> 13 | O -> 14 | P -> 15 | Q -> 16 | R -> 17
        | S -> 18 | T -> 19 | U -> 20 | V -> 21 | W -> 22 | X -> 23 | Y -> 24 | Z -> 25
    
    let fromIndex = function
        | 0 -> A | 1 -> B | 2 -> C | 3 -> D | 4 -> E | 5 -> F | 6 -> G | 7 -> H | 8 -> I
        | 9 -> J | 10 -> K | 11 -> L | 12 -> M | 13 -> N | 14 -> O | 15 -> P | 16 -> Q | 17 -> R
        | 18 -> S | 19 -> T | 20 -> U | 21 -> V | 22 -> W | 23 -> X | 24 -> Y | 25 -> Z
        | _ -> failwith "Index out of range"

    let (%+) m n = (((m % n) + n) % n)
    let modAlphabet x = x %+ LettersCount
    let fromModIndex = modAlphabet >> fromIndex

    let offsetLetter offset (IndexLetter letterIndex) =
        (letterIndex + offset) |> fromModIndex

    let reverseOffsetLetter offset = offsetLetter (-offset)

module Mapping =
    open Letter

    let create letters =
        let lettersCount = letters |> Array.distinct |> Array.length
        if lettersCount <> 26
        then failwith "A mapping must be composed of 26 distinct letters"
        else Mapping letters
          
    let id = Mapping [|A;B;C;D;E;F;G;H;I;J;K;L;M;N;O;P;Q;R;S;T;U;V;W;X;Y;Z|]

    let private inverseMapping' (Mapping idMap) (Mapping mapping) =
        (idMap, mapping) 
        ||> Array.zip
        |> Array.sortBy snd
        |> Array.map fst
        |> create

    let inverseMapping = inverseMapping' id

    let private rotate n (Mapping m) =
        m |> Array.splitAt (n % m.Length) |> (fun (a,b) -> [|b;a|]) |> Array.concat |> create

    let map f (Mapping m) = Array.map f m |> create

    let shiftMapping (IndexLetter shiftIndex) = 
        rotate shiftIndex >> map (Letter.reverseOffsetLetter shiftIndex)

    let mapLetter (Mapping mapping) (IndexLetter letterIndex) =
        mapping.[letterIndex]

    let offsetMapping offset mapper = 
        offsetLetter offset >> mapper >> reverseOffsetLetter offset

module Rotor =
    let rotorI = {Notch=Q; Mapping=Mapping.create [|E;K;M;F;L;G;D;Q;V;Z;N;T;O;W;Y;H;X;U;S;P;A;I;B;R;C;J|]; InnerRingOffset=A}
    let rotorII = {Notch=E; Mapping=Mapping.create [|A;J;D;K;S;I;R;U;X;B;L;H;W;T;M;C;Q;G;Z;N;P;Y;F;V;O;E|]; InnerRingOffset=A}
    let rotorIII = {Notch=V; Mapping=Mapping.create [|B;D;F;H;J;L;C;P;R;T;X;V;Z;N;Y;E;I;W;G;A;K;M;U;S;Q;O|]; InnerRingOffset=A}
    let rotorIV = {Notch=J; Mapping=Mapping.create [|E;S;O;V;P;Z;J;A;Y;Q;U;I;R;H;X;L;N;F;T;G;K;D;C;M;W;B|]; InnerRingOffset=A}
    let rotorV = {Notch=Z; Mapping=Mapping.create [|V;Z;B;R;G;I;T;Y;U;P;S;D;N;H;L;X;A;W;M;J;Q;O;F;E;C;K|]; InnerRingOffset=A}

module Reflector = 
    let reflectorA = {Mapping=Mapping.create [|E;J;M;Z;A;L;Y;X;V;B;W;F;C;R;Q;U;O;N;T;S;P;I;K;H;G;D|]}
    let reflectorB = {Mapping=Mapping.create [|Y;R;U;H;Q;S;L;D;P;X;N;G;O;K;M;I;E;B;F;Z;C;W;V;J;A;T|]}
    let reflectorC = {Mapping=Mapping.create [|F;V;P;J;I;A;O;Y;E;D;R;Z;X;W;G;C;T;K;U;Q;S;B;N;M;H;L|]}
    let reflectorETW = {Mapping=Mapping.id}

module Wheel =
    open Letter

    let setup startPos rotor = {
        Rotor = rotor
        RotorPosition = startPos
        IsInNotchPosition = (rotor.Notch = startPos)
    } 

    let setupDefault = setup A

    let private offsetOneLetter = offsetLetter 1
    let advance wheel = 
        let newPos = wheel.RotorPosition |> offsetOneLetter
        { wheel with 
            RotorPosition=newPos
            IsInNotchPosition = (wheel.Rotor.Notch = newPos)
        }

    let private innerRingMapper wheel mapping = 
        mapping
        |> Mapping.shiftMapping wheel.Rotor.InnerRingOffset
        |> Mapping.mapLetter

    let private mapLetterWithMapping wheel mapping = 
        let (IndexLetter indLetter) = wheel.RotorPosition
        mapping
        |> innerRingMapper wheel
        |> Mapping.offsetMapping indLetter

    let mapLetter wheel =
        mapLetterWithMapping wheel wheel.Rotor.Mapping

    let reverseMapLetter wheel =
        wheel.Rotor.Mapping 
        |> Mapping.inverseMapping
        |> mapLetterWithMapping wheel