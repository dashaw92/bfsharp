(*
Author: Daniel Shaw (dashaw92)
Date: 10/15/2023
BF interpreter in F# (BF#)

*)

open System

/// Represents all possible Bf instructions:
///     IncPtr:     >
///         Increments the memory pointer by 1
///     DecPtr:     <
///         Decrements the memory pointer by 1
///     Inc:        +
///         Increments the pointed to memory cell by 1
///     Dec:        -
///         Decrements the pointed to memory cell by 1
///     MarkLoop:   [
///         If the current memory cell is non-zero, start a loop
///         Otherwise, skip to the matching ']' instruction
///     JmpLoop:    ]
///         If the current memory cell is non-zero, jump to the matching '[' instruction
///         Otherwise, continue to the next instruction
///     Print:      .
///         Print the current memory cell's value as an ASCII char
///     Input:      ,
///         Read a single ASCII char from input and store it at the current
///         memory cell as an int
/// 
type Instr =
    | IncPtr
    | DecPtr
    | Inc
    | Dec
    | MarkLoop
    | JmpLoop
    | Print
    | Input

/// Decode chars to an Instr option or None
let instrFromChar = function
    | '>' -> Some IncPtr
    | '<' -> Some DecPtr
    | '+' -> Some Inc
    | '-' -> Some Dec
    | '[' -> Some MarkLoop
    | ']' -> Some JmpLoop
    | '.' -> Some Print
    | ',' -> Some Input
    | _ -> None

/// Represents a BF CPU
type State =
    {
        /// Memory pointer
        Ptr: int
        /// Jump lists (loops)
        Stack: int list
        /// A simulated infinite tape
        /// The key is just a simple index for lookups by Ptr
        Memory: Map<int, int>
    }

/// Define an infix operator for defaultArg
let (??=) optA def = defaultArg optA def

/// Apply `fn` to the cell at `ptr` in `memory`
/// If the memory map does not contain the key already,
/// apply `fn` to 0. Returns the new map
let changeMemory memory ptr (fn: int -> int) =
    let wrapOptFn cell = Some <| fn (cell ??= 0)
    Map.change ptr wrapOptFn memory

/// Get the value of the current cell or 0
let cellValue state = (Map.tryFind state.Ptr state.Memory) ??= 0 
    
/// Find the index in the instrs tape where the matching loop instruction is,
/// or None if there is none (error case)
let rec findEndOfLoop ip instrs =
    match Map.tryFind ip instrs with
    //Intentionally set to 1 beyond the closing ']' so as not to try
    //to jmp back to the beginning of a loop when we shouldn't.
    //Bounds are handled by Map.tryFind, so it's completely safe to return
    //unchecked indexes.
    | Some value when value = JmpLoop -> Some (ip + 1)
    | Some _ -> findEndOfLoop (ip + 1) instrs
    | _ -> None

/// Executes the input (instruction list) until either
/// an error occurs (unmatched loop start/end) or the input has been
/// exhausted.
let run state (input: Instr list) =
    //Build up an index-able map of the instructions. aux will refer to this when
    //jumping back at the end of loops.
    let instrTape =
        input
        |> List.mapi (fun el idx -> (el, idx))
        |> Map
    
    //Helper that closes over instrTape so it doesn't need to be
    //provided with every call
    let rec aux state ip =
        //Clean up code
        let nextIp = ip + 1
        //Use tryFind to manually handle out of bounds indexing
        match Map.tryFind ip instrTape with
        //ip is either invalid now, or we've reached the end of the input.
        | None -> state
        | Some instr ->
            match instr with
            //Basic ops, inc/dec the Ptr
            | IncPtr -> aux { state with Ptr = state.Ptr + 1 } nextIp
            | DecPtr -> aux { state with Ptr = state.Ptr - 1 } nextIp
            //Inc/dec the current cell's value. Note: Dec's fn is not ((-) 1) because it causes
            //strange behavior with negatives: ((-) 1) n = abs n???
            | Inc -> aux { state with Memory = changeMemory state.Memory state.Ptr ((+) 1) } nextIp
            | Dec -> aux { state with Memory = changeMemory state.Memory state.Ptr ((+) -1) } nextIp
            | MarkLoop ->
                //If the current cell is 0, find the matching loop end (']') and jmp to 1 past it.
                //If there is no matching ']', execution cannot continue because the program is invalid. 
                if cellValue state = 0 then
                    match findEndOfLoop ip instrTape with
                    | Some endLoop -> aux state endLoop
                    | None -> state
                else
                    //Save ip to the stack and begin the loop (go to the next instr like normal)
                    aux { state with Stack = nextIp :: state.Stack } nextIp
            | JmpLoop ->
                match state.Stack with
                //An empty stack here means there's an unmatched ']'.
                //Execution cannot continue in this case because the program is invalid.
                | [] -> state
                //jmp is the most recent IP to return to
                | jmp :: jmps ->
                    //If the current cell is 0, the loop is done and we can continue to the
                    //next instruction like normal.
                    if cellValue state = 0 then
                        aux { state with Stack = jmps } nextIp
                    //Otherwise, return back to the start of the loop (1 beyond the '[' instruction so as to not
                    //keep starting a new loop with every jmp)
                    else
                        aux state jmp
            | Print ->
                //Read the current cell and print it as a char
                let ch = char <| cellValue state
                printf $"%c{ch}"
                aux state nextIp
            | Input ->
                //Read a single char from stdin and place it at the current
                //memory cell as an int
                aux { state with Memory = changeMemory state.Memory state.Ptr (fun _ -> Console.Read ()) } nextIp
    //Run the program! IP = 0, state is user provided
    aux state 0

/// Convert a program in string form into a Instr list, filtering out invalid BF commands.
let toInstr: char seq -> Instr list = Seq.choose instrFromChar >> Seq.toList

//Helper for running a program
//runProgram "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." []
//-> Hello World!
let runProgram program =
    let st = { Ptr = 0; Stack = []; Memory = Map.empty }
    let instrs = toInstr program
    run st instrs 