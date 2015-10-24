type state = int

type sym = 
      SEps 
    | SChar of char
    
type nfa =
  { start  : state;
    accept : state;
    trans  : (state * sym * state) list
  }


// Samples
let s1 = {start = 0; accept = 1; trans = [(0,SChar 'b',1);(0,SChar 'a',2);(1,SChar 'p',4);(4,SChar 'x',1);(4,SChar 'y',5);(2,SChar 'e',3);(2,SChar 'i',1);(3,SChar 'i',2);(3,SChar 'a',4)]}let s2 = {start = 0; accept = 1; trans = [(0,SChar 'a',1);(0,SChar 'a',2);(1,SChar 'p',4);(4,SChar 'x',1);(4,SChar 'y',5);(2,SChar 'e',3);(2,SChar 'i',1);(3,SChar 'i',2);(3,SChar 'a',4)]}