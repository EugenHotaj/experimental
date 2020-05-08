"""Simple Scheme (Lisp) interpreter in Python.

Shamelessly stolen from https://norvig.com/lispy.html with a few updates to
work with modern python3.
"""

import math
import operator as op

Symbol = str
Number = (int, float)
Atom = (Symbol, Number)
List = list
Exp = (Atom, List)


class Env(dict):
  """An environment: a dict of {'var': val} pairs, with an outer Env."""

  def __init__(self, params=(), args=(), outer=None):
    self.update(zip(params, args))
    self._outer = outer

  def find(self, v):
    """Find the innermost Env where v appears."""
    return self if (v in self) else self._outer.find(v)


class Procedure(object):
  """A user-defined Scheme procedure."""

  def __init__(self, params, body, env):
    self._params = params
    self._body = body
    self._env = env

  def __call__(self, *args):
    return evall(self._body, Env(self._params, args, self._env))


def standard_env():
  """An environment with some standard Scheme procedures."""
  env = Env()
  env.update(vars(math))
  env.update({
      '+': op.add,
      '-': op.sub,
      '*': op.mul,
      '/': op.truediv,
      '>': op.gt,
      '<': op.lt,
      '>=': op.ge,
      '<=': op.le,
      '=': op.eq,
      'abs': abs,
      'append': lambda x, y: x + y,  # Assumes at least 'x' is a list. 
      'apply': lambda proc, args: proc(*args),
      'begin': lambda *x: x[-1],
      'car': lambda x: x[0],
      'cdr': lambda x: x[1:],
      'cons': lambda x, y: [x] + y,
      'eq?': op.is_,
      'expt': pow,
      'equal?': op.eq,
      'length': len,
      'list': lambda *x: List(x),
      'list?': lambda x: isinstance(x, List),
      'map': lambda x: list(map(x)),
      'max': max,
      'min': min,
      'not': op.not_,
      'null?': lambda x: x == [],
      'number?': lambda x: isinstance(x, Number),
      'print': lambda x: print(x),
      'procedure?': callable,
      'round': round,
      'symbol?': lambda x: isinstance(x, Symbol)
  })
  return env


global_env = standard_env()


def tokenize(chars):
  """Convert a string of characters into a list of tokens."""
  return chars.replace('(', ' ( ').replace(')', ' ) ').split()


def atomize(token):
  """Numbers become numbers; every other token is a symbol."""
  for num_type in Number:
    try:
      return num_type(token)
    except ValueError:
      continue
  return Symbol(token)


def read_from_tokens(tokens):
  """Read an expression from a sequence of tokens."""
  if not tokens:
    raise SyntaxError('Unexpected EOF')
  token = tokens.pop(0)
  if token == '(':
    L = []
    while tokens[0] != ')':
      L.append(read_from_tokens(tokens))
    tokens.pop(0)
    return L
  elif token == ')':
    raise SyntaxError('Mismatched parentheses.')
  else:
    return atomize(token)


def parse(program):
  """Read a Scheme expression from a string."""
  return read_from_tokens(tokenize(program))


# Calling evall to not shadow python eval.
def evall(x, env=global_env):
  """Evaluate Scheme expressions in an environment."""
  if isinstance(x, Symbol):
    return env.find(x)[x]
  elif not isinstance(x, List):
    return x

  op, *args = x
  if op == 'quote':
    return args[0]
  elif op == 'if':
    (test, conseq, alt) = args
    exp = (conseq if evall(test, env) else alt)
    return evall(exp, env)
  elif op == 'define':
    (symbol, exp) = args
    env[symbol] = evall(exp, env)
  elif op == 'set!':
    (symbol, exp) = args
    env.find(symbol)[symbol] = evall(exp, env)
  elif op == 'lambda':
    (params, body) = args
    return Procedure(params, body, env)
  else:
    proc = evall(op, env)
    vals = [evall(arg, env) for arg in args]
    return proc(*vals)


def schemestr(exp):
  """Convert a Python object back into Scheme-readable string."""
  if isinstance(exp, List):
    return '(' + ' '.join(map(schemestr, exp)) + ')'
  else:
    return str(exp)


def repl(prompt='lis.py>'):
  """A prompt-read-eval-print loop."""
  while True:
    val = evall(parse(input(prompt)))
    if val is not None:
      print(schemestr(val))


if __name__ == '__main__':
  repl()
