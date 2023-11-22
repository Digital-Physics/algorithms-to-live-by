from itertools import permutations
from math import factorial

# length of ordinal list
n = 3
look_period = 0
choices = range(n)
perms = factorial(len(choices))
# look then leap strategy parameter
counter = 0

#### brute force calculation #####
# sum over each possibility
for perm in permutations(choices):
    # starting at the first person after the look window...
    # (note: look period + 1 is the first consideration, but Python has an initial index of 0, so range starts at look_period)
    for candidate_i in range(look_period, len(choices)):
        # print("idx", candidate_i, "in", perm, "has val" perm[candidate_i], "max before", max(perm[:candidate_i]), "max after", max(perm[candidate_i + 1:]))
        # ...we stop at the first candidate that scores better than what we've seen so far...
        # (note: we first check if the look window is 0 (and therefore the first element will definitely pass) to avoide the error on max([]))
        if candidate_i == 0 or perm[candidate_i] > max(perm[:candidate_i]):
            # ...and check whether its ranking is greater than all the remaining rankings not seen yet
            if candidate_i == len(choices) - 1 or perm[candidate_i] > max(perm[candidate_i + 1:]):
                # if this all checks, your look than leap strategy worked in finding the highest ordinal ranked object in the set
                counter += 1
            break

print(f"Look then leap strategy for {n} choices:")
print(f"Look window: {look_period}")
print(f"This strategy results in {counter} successes in the {perms} ranking permutations, which translates to a {counter/perms} probability of picking the highest ranked.")


#### algebraic calculation ####

#### simplified algebraic calculation ####