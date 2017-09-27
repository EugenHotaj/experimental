import math

def build_gaussian_pdf(mu, sig):
  """Returns a Gaussian PDF for the given mean (mu) and standard deviation (sig)."""
  var = sig**2
  coef = 1/math.sqrt(2*math.pi*var)
  return lambda x: coef*math.exp(-(x - mu)**2/(2*var))

g1 = build_gaussian_pdf(5, .5)
g2 = build_gaussian_pdf(7, 1)

xs = [2.69, 5.978, 5.667, 5.830]
for x in xs:
  print('x={}: {}'.format(x, g1(x)/g2(x)))

 
