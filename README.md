# Superpos

Defines a Monad which can behave as multiple values at once with probabilities. Unwrapping behaves much like the list Monad in that it just applies an operation to all possible outcomes. If an operation generates more superpositions, then those are combined with the inputs.

At any point, you can "collapse" the superposition into a single value by using the `collapse` function.

This is not my original idea, you can find the inspiration [here](http://learnyouahaskell.com/for-a-few-monads-more).