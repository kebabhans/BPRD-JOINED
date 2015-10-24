module trans

    type state = int
    
    type sym = 
          SEps 
        | SChar of char
        
    type nfa =
        { 
            start  : state;
            accept : state;
            trans  : (state * sym * state) list
        }
        
    let order nfa = 
        let extract (a, b, _) = a, b
        let sorted = Seq.sortBy (fun s -> extract s) nfa.trans 
                     |> List.ofSeq
                    
        { start = nfa.start; accept = nfa.accept; trans = sorted }
    
    let isDfa nfa = 
        let sym = nfa.sym
        if sym = SEps then false
        
        List.filter (fun s -> )
        
    
    let test = 
        let before = { start = 0; 
                       accept = 1; 
                       trans = [ (1, SChar 'b', 2); 
                                 (1, SChar 'a', 2); 
                                 (0, SChar 'c', 0); 
                                 (0, SChar 'a', 3) ] 
                     }
        order before


// Samples
let s1 = {start = 0; accept = 1; trans = [(0,SChar 'b',1);(0,SChar 'a',2);(1,SChar 'p',4);(4,SChar 'x',1);(4,SChar 'y',5);(2,SChar 'e',3);(2,SChar 'i',1);(3,SChar 'i',2);(3,SChar 'a',4)]}
