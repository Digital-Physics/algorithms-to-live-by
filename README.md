# algorithms-to-live-by
Code related to concepts in the book "algorithms to live by: the computer science of human decisions" by Brian Christian and Tom Griffiths.

python lookThenLeap.py 
This code relates to a situation where you are only interested in finding *the best* ranked object in a set of n objects.
you see the objects one at a time. you learn the current observation's ordinal relationship to all the objects that have come before it.
if you look for too long, you may pass over the best of the lot. if you leap too soon, you may not have "explored" enough. 
(Note: This is not a Bayesian updating "exploration" but rather the optimal balance in a Platonic math object.)

node exploreExploit.js 
This code relates to the multi-arm bandit problem where each slot machine has a binary payoff (not a Real/float Gaussian Random Variable payout).
You can look at how variations of "win-stay, lose-switch" policy/strategy performs on slot machines with various win/lose probabiliies.
You can modify parts of the program to handle, say, the switching protocol differently. (i.e. epsilon-greedy, changing epsilon over time, etc.)
You could also do something akin to grid search of hyperparameters to exhaustively map out some space of results (using a for loop over scenarios or a .map().

"g++ sorting.cpp -o sorting_compiled" or "clang++ -std=c++11 sorting.cpp -o sorting_compiled" to compile it 
"./sorting_compiled" to run the executable
This code does MergeSort. In MergeSort, you first break your list down into n separate lists. They are sorted singleton lists to start. 
You then merge sorted lists, which isn't that bad since you can do this in one pass of the lists (linear time). To get n/2 sorted lists for the next round.

Haskell
Scala
C#
typeScript

