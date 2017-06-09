import numpy.random as random

from enum import Enum


def draw_card(only_black):
  """Draws a red suit card with probability 1/3 or a black
  suit card with probability 2/3. 

  Args:
    only_black: only draw a black card.
  Returns:
    The card if the suit is black or -card if the suit is red.
  """
  if only_black:
    return random.randint(1,11)
  else:
    # First get the suit of the card
    suit = random.random()
    # Then get the card
    card = random.randint(1,11)
    return card if suit < 2/3 else -card


class State:
  """A representation of a state.
  
    player_total: the player's score in [0, 21].
    dealer_card: the dealer's face up card in [0, 10]; 
  """   
  def __init__(self):
   self.re_init() 

  def re_init(self):
    self.player_total = draw_card(True)
    self.dealer_total = draw_card(True)


class Action(Enum):
  """The actions a player can take at a given state."""
  STAY = 0
  HIT = 1


def step(s, a):
  """Performs an action to determine the next state. Returns the
  reward at the next state, if any.
      
  (s, a) -> s', r 

  Args:
    s: the current state
    a: the action to take in state s
  Returns:
    The rewards at the next state:
    -1 -- if the dealer wins.
     0 -- if the game is a draw, or no terminal state has been
          reached.
     1 -- if the player wins.
  """
  if a == Action.STAY:
    while s.dealer_total >= 0 and s.dealer_total < 17:
      s.dealer_total = s.dealer_total + draw_card(False)
    if s.dealer_total < 0 or s.dealer_total > 21:
      return 1 
    elif s.dealer_total < s.player_total:
      return 1
    elif s.dealer_total > s.player_total:
      return -1
    else: 
      return 0
  elif a == Action.HIT:
    s.player_total = s.player_total + draw_card(False)
    if s.player_total < 0 or s.player_total > 21:
      return -1
    else:
      return 0
  else:
    raise Exception("Invalid action:", a)
