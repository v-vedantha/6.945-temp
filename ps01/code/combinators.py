import math
arity_map = {}

def compose(f, g):
    def the_combination(args):
        g_args = args
        f_args = g(*g_args)
        return f(*f_args)
    return the_combination

def spread_combine(h, f, g):
    def the_combination(args):
        f_args = args[:arity_map[f]]
        g_args = args[arity_map[f]:]
        h_args = f(*f_args) + g(*g_args)
        return h(*h_args)
    return the_combination

def discard_argument(i):
    def caller(f):
        def call(args):
            actual_args = args[:i] + args[i+1:]
            return f(*actual_args)
        return call

    return caller

def q(a, b):
    return a, b, a+1
def r(a, b, c):
    return a, b, c

def h(a, b, c, d, e, f):
    return a, b, c, d,f, e
arity_map[q] = 2
arity_map[r] = 3

print(compose(r, q)([1, 2]))

print(discard_argument(1)(q)([1, 2, 3]))
print(spread_combine(h, q, r)([1, 2, 3, 4, 5]))

def list_sin(x):
    return [math.sin(x)]
def list_cos(x):
    return [math.sin(x)]

print(compose(list_sin, list_cos)([3]))
