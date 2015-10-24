type state = int
type sym = SEps | SChar of char
type nfa =
  { start  : state;
    accept : state;
    trans  : (state * sym * state) list
  }