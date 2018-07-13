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

    let charToLetter (c:char) = int c - int 'A' |> fromIndex

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

    let fromString (strMapping:string) = 
        strMapping 
        |> (Seq.map charToLetter >> Array.ofSeq >> create)
          
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

    let reverseMapLetter = inverseMapping >> mapLetter

    let offsetMapping offset mapper = 
        offsetLetter offset >> mapper >> reverseOffsetLetter offset

module Rotor =
    let rotorI = {Notch=Q; Mapping=Mapping.fromString "EKMFLGDQVZNTOWYHXUSPAIBRCJ"; InnerRingOffset=A}
    let rotorII = {Notch=E; Mapping=Mapping.fromString "AJDKSIRUXBLHWTMCQGZNPYFVOE"; InnerRingOffset=A}
    let rotorIII = {Notch=V; Mapping=Mapping.fromString "BDFHJLCPRTXVZNYEIWGAKMUSQO"; InnerRingOffset=A}
    let rotorIV = {Notch=J; Mapping=Mapping.fromString "ESOVPZJAYQUIRHXLNFTGKDCMWB"; InnerRingOffset=A}
    let rotorV = {Notch=Z; Mapping=Mapping.fromString "VZBRGITYUPSDNHLXAWMJQOFECK"; InnerRingOffset=A}

module Reflector = 
    let (reflectorA:Reflector) = {Mapping=Mapping.fromString "EJMZALYXVBWFCRQUONTSPIKHGD"}
    let (reflectorB:Reflector) = {Mapping=Mapping.fromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"}
    let (reflectorC:Reflector) = {Mapping=Mapping.fromString "FVPJIAOYEDRZXWGCTKUQSBNMHL"}
    let (reflectorETW:Reflector) = {Mapping=Mapping.id}

    let mapLetter (reflector:Reflector) = reflector.Mapping |> Mapping.mapLetter

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

    let private innerRingMapper mapping wheel = 
        mapping
        |> Mapping.shiftMapping wheel.Rotor.InnerRingOffset
        |> Mapping.mapLetter

    let private mapLetterWithMapping mapping wheel = 
        let (IndexLetter indLetter) = wheel.RotorPosition
        wheel
        |> innerRingMapper mapping
        |> Mapping.offsetMapping indLetter

    let mapLetter wheel =
        mapLetterWithMapping wheel.Rotor.Mapping wheel 

    let reverseMapLetter wheel =
        let inverseMapping = wheel.Rotor.Mapping |> Mapping.inverseMapping
        wheel |> mapLetterWithMapping inverseMapping

module Plugboard =
    let mapLetter plugboard = plugboard.Mapping |> Mapping.mapLetter
    let reverseMapLetter plugboard = plugboard.Mapping |> Mapping.reverseMapLetter

module EnigmaMachine =
    let mapLetter machine =
        [
            machine.Plugboard |> Plugboard.mapLetter
            machine.Wheel3 |> Wheel.mapLetter
            machine.Wheel2 |> Wheel.mapLetter
            machine.Wheel1 |> Wheel.mapLetter
            machine.Reflector |> Reflector.mapLetter
            machine.Wheel1 |> Wheel.reverseMapLetter
            machine.Wheel2 |> Wheel.reverseMapLetter
            machine.Wheel3 |> Wheel.reverseMapLetter
            machine.Plugboard |> Plugboard.reverseMapLetter
        ]
        |> List.reduce (>>)