from itertools import permutations
from math import factorial

# length of ordinal list
n = 10
choices = range(n)
perms = factorial(n)

# look then leap strategy parameter
look_period = 8

counter, counter2, total_prob = 0, 0, 0

#### brute force calculation that looks at every permutation possibility and counts ones that work #####
# for each possible ranking, each equally likely, does the the method work out such that the best choice is chosen? if so we'll count it.
for perm in permutations(choices):
    # starting at the first person after the look window...
    # (note: look period + 1 is the first consideration, but Python has an initial index of 0, so range starts at look_period)
    for candidate_i in range(look_period, len(choices)):
        # print("idx", candidate_i, "in", perm, "has val" perm[candidate_i], "max before", max(perm[:candidate_i]), "max after", max(perm[candidate_i + 1:]))
        # ...we stop at the first candidate that scores better than what we've seen so far...
        # (note: our first our operand is evaluatate first to see if the look window is 0 (and therefore the first element will definitely pass) to avoide the error on max([]))
        if candidate_i == 0 or perm[candidate_i] > max(perm[:candidate_i]):
            # ...and check whether its ranking is greater than all the remaining rankings not seen yet
            if candidate_i == len(choices) - 1 or perm[candidate_i] > max(perm[candidate_i + 1:]):
                # if this all checks, your look than leap strategy worked in finding the highest ordinal ranked object in the set
                counter += 1
            break

print(f"Look then leap strategy for {n} choices:")
print(f"Look window: {look_period}")
print(f"This strategy results in {counter} successes in {perms} ranking permutations, which translates to a {counter/perms} probability of picking the highest ranked.")

#### brute force simplified ####
for perm in permutations(choices):
    for candidate_i in range(look_period, n):
        if candidate_i == 0 or perm[candidate_i] > max(perm[:candidate_i]):
            # note: the agent in this look then leap situation does not have access to the final ranking so it can't use this small algorithmic shortcut 
            if perm[candidate_i] == n - 1:
                counter2 += 1
            break

print(counter2/perms)

def lookThenLeap(n, look_period, total_prob=0):
    """algebraic probabilities algorithm"""
    for q_idx in range(look_period, n):
        term = 1/(q_idx + 1) # prob of decrementing at that period (i.e. being the highest ordinal rank after q_idx observations) 
        for p_idx in range(look_period, n):
            if p_idx != q_idx:
                term *= (1 - 1/(p_idx + 1)) #times prob of survival
        total_prob += term
    return total_prob

test_n = 100
test_results = []

for l in range(test_n):
    test_results.append(lookThenLeap(test_n, l))

print("Now let's try every possible look window from length 0 to the number of choices we have...")
print()
print("Does the success probability peak around 37% of the way through the list?")
print()
print(test_results)
print()
print(f"look_window_arg_index_max for an ordering of size {test_n}: {test_results.index(max(test_results))}")
print(f"look window percent {test_results.index(max(test_results))/test_n}")
print()
print("Is the success probability itself about 37%?")
print()
print(f"(success) likelihood (at arg max): {max(test_results)}")

