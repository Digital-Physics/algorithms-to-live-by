# algorithms-to-live-by
This repository has code related to concepts explored in the book "Algorithms to Live By: The Computer Science of Human Decisions" by Brian Christian and Tom Griffiths. The book was given to me and my wife by my mother (after she read it!).

The goal of this repository is to write at least one piece of code related to each chapter, with each piece of code being in a different language/technology.

Some of the code has notes on how the language's syntax and structure compares with Python (or JavaScript).

![chapter listings](img/IMG_5568.jpg)
![chapter listings](img/IMG_5569.jpg)

Chapter 1 "Optimal Stopping": 

python lookThenLeap.py 

This code relates to a situation where you are only interested in finding *the best* ranked object in a set of n objects.
you see the objects one at a time. you learn the current observation's ordinal relationship to all the objects that have come before it.
if you look for too long, you may pass over the best of the lot. if you leap too soon, you may not have "explored" enough. 
(Note: This is not a Bayesian updating "exploration" but rather the optimal balance in a Platonic math object.)

Chapter 2 "Explore/Exploit":

node exploreExploit.js 

This code relates to the multi-arm bandit problem where each slot machine has a binary payoff (not a Real/float Gaussian Random Variable payout).
You can look at how variations of "win-stay, lose-switch" policy/strategy performs on slot machines with various win/lose probabiliies.
You can modify parts of the program to handle, say, the switching protocol differently. (i.e. epsilon-greedy, decreasing epsilon/exploration over time, etc.) You could also do something akin to grid search of hyperparameters to exhaustively map out some space of results (using a for loop over scenarios or a .map()).

Chapter 3 "Sorting":

"g++ sorting.cpp -o sorting_compiled" or "clang++ -std=c++11 sorting.cpp -o sorting_compiled" to compile it 
"./sorting_compiled" to run the executable

This code does MergeSort. In MergeSort, you first break your list down into n separate lists. They are sorted singleton lists to start. You then merge those sorted lists two at a time (which you can do in one pass of the lists (linear time)) to get n/2 sorted lists for the next recursive call or iteration. The overall time complexity is O(n*log(n))

Chapter 4 "Caching":

python Redis_caching.py

This code explores Redis for caching. Redis allows us to save data in key-value form in-memory (so it has fast retrieval, although it can be persisted over time unlike normal in-memory RAM) outside our main web service which makes the main web application stateless (so there's no problem restarting your server as you won't lose data) and easy to scale since the Redis db can be replicated and be made highly available (see Redis Sentinel). Redis caching seems to be most useful when you are dealing with many client sessions where the data they are querying/computing takes a little while to retrieve and the requested data will likely be requested buy that user or another user sometime soon. All queries from the different client sessions can reference the same Redis database, sharing a common cache. To see a Least Recently Used cache where you can appreciate the fundamental data structures, check this code out:

https://github.com/Digital-Physics/algorithms/blob/main/lru_cache.py

Chapter 5 "Scheduling":

Chapter 6 "Bayes's Rule":

Chapter 7 "Overfitting":

Chapter 8 "Relaxation":

Chapter 9 "Randomness":

Chapter 10 "Networking":

Chapter 11 "Game Theory":

Possible future languages:


C#
Haskell
Scala
typeScript
Rust


