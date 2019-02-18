"""Softmax."""

import math
import numpy as np
import matplotlib.pyplot as plt

def softmax(x):
  """Computes softmax values for each sets of scores in x."""
  x = np.array(x)
  if x.ndim == 1:
    return column_softmax(x)
  else:
    ret_arr = np.array([column_softmax(col) for col in x.T])
    return ret_arr.T
    
def column_softmax(col):
  """Computes softmax values for a single vector."""
  total = np.sum([np.exp(cell) for cell in col])
  return np.array([np.exp(cell)/total for cell in col])

# Quick test
scores = np.array([[1, 2, 3, 6],
                   [2, 4, 5, 6],
                   [3, 8, 7, 6]])
print(softmax(scores))
