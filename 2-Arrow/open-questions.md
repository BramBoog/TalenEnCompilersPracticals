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

Whether a recursive rule call is at the end of a command sequence or in the middle has a clear effect on the size of the command stack as a whole, and therefore the memory use of the program. We can illustrate this with the following examples:

Take the following command sequence, where 'r' is the recursive call, and 'c' and 'a' are arbitrary non-recursive calls: [c, c, r, a, a].
Suppose this sequence is on a command stack as follows: [c, c, r, a, a, b, b].
When the program reaches 'r', the command stack looks like: [r, a, a, b, b].
After evaluating 'r', the command stack becomes: [c, c, r, a, a, a, a, b, b].
Again, after reaching 'r' and evaluating, the command stack is: [c, c, r, a, a, a, a, a, a, b, b].
We can see that the size of the command stack keeps growing, increasing memory usage all the while.

However, if you have recursion at the end of a sequence, i.e., [c, c, a, a, r], the size of the stack doesn't grow the same way.
For example, again using the command stack [c, c, a, a, r, b, b], when reaching the 'r', the command stack will look like: [r, b, b].
After evaluating 'r', the command stack becomes [c, c, a, a, r, b, b].
Hence, the size of the command stack never grows beyond its original size.

In conclusion, there is a difference in the sizes of the stacks; having recursion in the middle of a command sequence can cause a memory leak because of a growing stack size. Having the recursion at the end, however, preserves the size of the stack.
