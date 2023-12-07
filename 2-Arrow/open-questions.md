# Open questions

## Exercise 4

Happy handles left recursion more efficiently when it comes down to keeping the size of the stack in check. A left-recursive production results in a parser which consumes constant space on the stack. In contrast, a right-recursive rule requires an amount of space proportional to the length of the list of tokens being parsed.

In contrast, using parser combinators, grammars with left-recursive productions are often transformed using the 'removing left recursion' tranfomrmation, as applying the parser combinators directly on the grammar produces loops. This is because, if you were to have the following production:

  S -> Ss | s

you could naively implement this using parser combinators as:

data S = Multi S | Single

parseSs = (\m _ -> Multi m) <$> parseSs <*> symbol 's'
      <|> const Single <$> symbol 's'

However, this parser will just call itself on the input string, causing a loop.


## Exercise 10
