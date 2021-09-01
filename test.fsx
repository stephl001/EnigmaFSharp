type Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
open System
type Mapping = private Mapping of Letter array
type Mapper = Letter -> Letter

type Rotor = private {
    Notch: Letter
    Mapper: Mapper
    InnerRingOffset: Letter
}

type Wheel = private {
    Rotor: Rotor
    RotorPosition: Letter
    IsInNotchPosition: bool
}

type EnigmaMachine = {
    Plugboard: Mapper
    SlowSocket: Wheel
    MiddleSocket: Wheel
    FastSocket: Wheel
    Reflector: Mapper
}

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
    let offsetLetter offset (IndexLetter letterIndex) = (letterIndex + offset) |> fromModIndex
    let reverseOffsetLetter = (~-) >> offsetLetter
    let strLetters (str:string) = 
        str |> List.ofSeq |> List.filter Char.IsLetter |> List.map charToLetter

module Mapping =
    open Letter

    let create letters =
        let lettersCount = letters |> Array.distinct |> Array.length
        if lettersCount <> 26
        then failwith "A mapping must be composed of 26 distinct letters"
        else Mapping letters

    let fromString = Seq.map charToLetter >> Array.ofSeq >> create
    let id = fromString {'A' .. 'Z'}
    let map f (Mapping m) = Array.map f m

    let mapLetter (Mapping mapping) (IndexLetter letterIndex) =
        mapping.[letterIndex]

module Mapper =
    open Letter
    open Mapping

    let fromArray = create >> mapLetter

    let reverseMapper (mapper:Mapper) : Mapper =
        id
        |> map (fun l -> (l,mapper l)) 
        |> Array.sortBy snd 
        |> Array.map fst
        |> fromArray

    let offsetMapper (IndexLetter offset) (mapper:Mapper) : Mapper = 
        offsetLetter offset >> mapper >> reverseOffsetLetter offset

    let fromString = Mapping.fromString >> mapLetter
    let id = Mapping.id |> mapLetter

module Rotor =
    let create ringOffset notch mapping =
        {Notch=notch; Mapper=Mapper.fromString mapping; InnerRingOffset=ringOffset}
    let createDefault = create A
    let rotorI = createDefault Q "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
    let rotorII = createDefault E "AJDKSIRUXBLHWTMCQGZNPYFVOE"
    let rotorIII = createDefault V "BDFHJLCPRTXVZNYEIWGAKMUSQO"
    let rotorIV = createDefault J "ESOVPZJAYQUIRHXLNFTGKDCMWB"
    let rotorV = createDefault Z "VZBRGITYUPSDNHLXAWMJQOFECK"

module Reflector = 
    let reflectorA = Mapper.fromString "EJMZALYXVBWFCRQUONTSPIKHGD"
    let reflectorB = Mapper.fromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    let reflectorC = Mapper.fromString "FVPJIAOYEDRZXWGCTKUQSBNMHL"
    let reflectorETW = Mapper.id

module Socket =
    open Letter
    open Mapper

    let setup startPos rotor = {
        Rotor = rotor
        RotorPosition = startPos
        IsInNotchPosition = (rotor.Notch = startPos)
    } 

    let setupDefault = setup A

    let advance socket = 
        let newPos = socket.RotorPosition |> offsetLetter 1
        { socket with 
            RotorPosition=newPos
            IsInNotchPosition = (socket.Rotor.Notch = newPos)
        }

    let getMapper (socket:Wheel) = 
        socket.Rotor.Mapper 
        |> offsetMapper socket.Rotor.InnerRingOffset
        |> offsetMapper socket.RotorPosition

module EnigmaMachine =
    let private wheelMappers machine = 
        [machine.FastSocket;machine.MiddleSocket;machine.SlowSocket]
        |> List.map Socket.getMapper
        |> List.reduce (>>)

    let mapLetter machine =
        let forwardMapper = machine.Plugboard >> wheelMappers machine
        let reverseMapper = forwardMapper |> Mapper.reverseMapper
        
        forwardMapper >> machine.Reflector >> reverseMapper

    let advanceRotors (machine:EnigmaMachine) =
        let (ms,fs) = (machine.MiddleSocket,machine.FastSocket)
        match (ms.IsInNotchPosition,fs.IsInNotchPosition) with
        | (true,_) -> { machine with 
                            MiddleSocket=ms|>Socket.advance
                            FastSocket=fs|>Socket.advance }
        | _ -> { machine with FastSocket=fs|>Socket.advance }

    let encodeLetter machine letter =
        let newMachine = machine |> advanceRotors
        let encodedLetter = mapLetter newMachine letter
        (newMachine, encodedLetter)

    let nextState (machine,encoded) letter =
        let (newMachine,encodedLetter) = encodeLetter machine letter
        (newMachine, encodedLetter::encoded)

    let encodeMessage machine =
        List.fold nextState (machine,[]) >> snd >> List.rev

    let private encodableString : (string->string) = 
        Seq.filter Char.IsLetter 
        >> Seq.map Char.ToUpperInvariant 
        >> String.Concat
    
    let encodeString machine = 
        encodableString 
        >> Letter.strLetters 
        >> encodeMessage machine 
        >> String.Concat

    let defaultMachine = 
       { 
           FastSocket = Socket.setupDefault Rotor.rotorIII
           MiddleSocket = Socket.setupDefault Rotor.rotorII
           SlowSocket = Socket.setupDefault Rotor.rotorI
           Plugboard = Mapper.id
           Reflector = Reflector.reflectorB
       }