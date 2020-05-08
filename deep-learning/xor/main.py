"""Trains an LSTM to solve the XOR problem.

Produced for OpenAI's Requests for Research 2.0. The problem statement:
    "Train an LSTM to solve the XOR problem: that is, given a sequence of bits,
    determine its parity. The LSTM should consume the sequence, one bit at a
    time, and then output the correct answer at the sequence’s end."

To run:
    python main.py

Reference:
    https://github.com/SeanBae/xor-lstm/blob/master/xor-lstm.ipynb
"""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import numpy as np

from absl import app
from functools import reduce
from keras import models
from keras import layers


_SAMPLE_LENGTH = 50
_NUM_TRAIN_SAMPLES = 100000
_NUM_TEST_SAMPLES = 10000
_BATCH_SIZE = 128
_EPOCHS = 7


def _generate_samples(sample_length, num_samples):
  low = 0
  high = 1 << sample_length

  X = np.zeros((num_samples, sample_length, 2))
  Y = np.zeros((num_samples, sample_length, 2))
  for i in range(num_samples):
    num = np.random.randint(low, high)
    bits = np.binary_repr(num, width=sample_length)

    x = np.zeros((sample_length, 2))
    y = np.zeros((sample_length, 2))
    parity = 0
    for j, bit in enumerate(bits):
      bit_ = int(bit)
      parity ^= bit_
      x[j, bit_] = 1
      y[j, parity] = 1

    X[i, :, :] = x
    Y[i, :, :] = y
  return X, Y


def _build_model():
  model = models.Sequential()
  model.add(layers.LSTM(1, return_sequences=True,
                        input_shape=(_SAMPLE_LENGTH, 2)))
  model.add(layers.Dense(2, activation='softmax'))
  model.compile(loss='binary_crossentropy',
                optimizer='adam', metrics=['accuracy'])
  return model


def main(argv):
  del argv  # Unused.

  train_X, train_Y = _generate_samples(_SAMPLE_LENGTH, _NUM_TRAIN_SAMPLES)
  test_X, test_Y = _generate_samples(_SAMPLE_LENGTH, _NUM_TEST_SAMPLES)
  model = _build_model()
  print(model.summary())
  model.fit(train_X, train_Y, validation_data=(test_X, test_Y),
            epochs=_EPOCHS, batch_size=_BATCH_SIZE)


if __name__ == '__main__':
  app.run(main)
