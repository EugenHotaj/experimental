import math

THRESHOLDS = [2.69, 5.978, 5.667, 5.830]

def build_gaussian_pdf(mu, sig):
  """Returns a Gaussian PDF for the given mean (mu) and standard deviation (sig)."""
  var = sig**2
  coef = 1/math.sqrt(2*math.pi*var)
  return lambda x: coef*math.exp(-(x - mu)**2/(2*var))

# "Evaluate" which triggering threshold should be s.t. the penalaty for misidentifying
# s2 is twice that of s1. (Answer: 5.978).
s1 = build_gaussian_pdf(5, .5)
s2 = build_gaussian_pdf(7, 1)

for x in THRESHOLDS:
  print('x={}: {}'.format(x, s1(x)/s2(x)))

 
