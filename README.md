# algorithms-to-live-by
code related to concepts in the book "algorithms to live by: the computer science of human decisions"

lookThenLeap.py relates to a situation where you are only interested in finding *the best* ranked object in a set of n objects.
you see the objects one at a time. you learn their ordinal relationship to all the objects that have come before them.
if you look for too long, you may pass over the best of the lot. if you leap too soon, you may not have "explored" enough. 
(Note: This is not a Bayesian updating exploration but rather the optimal balance in a Platonic object.)

exploreExploit.js has code related to the multi-arm bandit problem. 
It looks at how variations of "win-stay, lose-switch" strategy perform on slot machines with various win/lose probabiliies.
