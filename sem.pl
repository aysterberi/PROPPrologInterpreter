sentence(sentence(NP, VP)) -->
    nounphrase(NP), verbphrase(VP).

nounphrase --> determiner, noun.

verbphrase -->verb, nounphrase.

determiner --> [a].

noun --> [cat].
noun --> [mouse].

verb --> [scares].
verb --> [hates].


