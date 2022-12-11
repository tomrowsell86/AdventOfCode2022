module partB

let uniqueChecker (input: List<char>) =
    let rec Outer (chars: List<char>) =
        let rec Inner (candidate: char) (remaining: List<char>) =
            match remaining with
            | hd :: tl  -> if candidate = hd then false else Inner candidate tl
            |[] -> true
        match chars with
        | hd :: tl  -> if not (Inner hd tl) then false else Outer tl 
        | [] -> true
    Outer input
