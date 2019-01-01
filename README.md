# cluedo

A solver for the table-top game [Cluedo](https://en.wikipedia.org/wiki/Cluedo) written in Clojure. Given a set of moves and players it will provide a list of the possible envelopes. It uses a few tricks to limit the search space and then does a total-search of all possible assignments of cards to provide the most restrictuve set of possible envelopes that can be infered from the given game.

The solver uses the Swedish names for all suspects, weapons and rooms. Two predefined games are included for demo purposes.