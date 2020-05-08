"""Neural network which tries to learn additon between 0 and 10."""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function


import tensorflow as tf
import numpy as np

from absl import app

# Dataset Variables
_INPUT_SIZE = 2
_NUM_TRAIN_EXAMPLES = 2000

# NN Variables
_NUM_LABELS = 10  # only supporting addition from 0-9
_NUM_HIDDEN_NODES = 1024
_BATCH_SIZE = 512
_NUM_TRAINING_STEPS = 3001


def _GenerateTrainingData():
  x = []
  for i in range(_NUM_TRAIN_EXAMPLES):
    left = np.random.randint(0, _NUM_LABELS)
    right = np.random.randint(0, 10 - left)
    x.append([left, right])
  x = np.array(x)

  # Turn answer vector into matrix of one-hot vectors
  y = x[:, 0] + x[:, 1]
  y = (np.arange(_NUM_LABELS) == y[:, None]).astype(np.float32)

  return x, y


def main(argv):
  del argv  # Unused

  x, y = _GenerateTrainingData()
  graph = tf.Graph()
  with graph.as_default():
    train_set = tf.placeholder(tf.float32, shape=(_BATCH_SIZE, _INPUT_SIZE))
    train_labels = tf.placeholder(tf.float32, shape=(_BATCH_SIZE, _NUM_LABELS))

    # Two layer architecture with _NUM_HIDDEN_NODES
    layer1_weights = tf.Variable(
        tf.truncated_normal([_INPUT_SIZE, _NUM_HIDDEN_NODES]))
    layer1_biases = tf.Variable(tf.zeros([_NUM_HIDDEN_NODES]))
    layer2_weights = tf.Variable(
        tf.truncated_normal([_NUM_HIDDEN_NODES, _NUM_LABELS]))
    layer2_biases = tf.Variable(tf.zeros([_NUM_LABELS]))

    # Training
    layer1 = tf.nn.relu(tf.matmul(train_set, layer1_weights) + layer1_biases)
    logits = tf.matmul(layer1, layer2_weights) + layer2_biases
    loss = tf.reduce_mean(
        tf.nn.softmax_cross_entropy_with_logits(
            labels=train_labels, logits=logits))

    # Optimizer
    optimizer = tf.train.GradientDescentOptimizer(0.5).minimize(loss)

    # Predictions
    train_prediction = tf.nn.softmax(logits)

  with tf.Session(graph=graph) as session:
    tf.global_variables_initializer().run()
    print("Initialized")
    for step in range(_NUM_TRAINING_STEPS):
      # Pick an offset within the training data, which has been randomized.
      offset = (step * _BATCH_SIZE) % (y.shape[0] - _BATCH_SIZE)

      # Generate a minibatch.
      batch_data = x[offset:(offset + _BATCH_SIZE), :]
      batch_labels = y[offset:(offset + _BATCH_SIZE), :]

      # Prepare a dictionary telling the session where to feed the minibatch.
      # The key of the dictionary is the placeholder node of the graph to be fed,
      # and the value is the numpy array to feed to it.
      feed_dict = {train_set: batch_data, train_labels: batch_labels}

      _, l, predictions = session.run(
          [optimizer, loss, train_prediction], feed_dict=feed_dict)
      if (step % 500 == 0):
        print("Loss at step %d: %f" % (step, l))
        print("Accuracy: %.1f%%" % _Accuracy(predictions, batch_labels))


def _Accuracy(predictions, labels):
  return (100.0 * np.sum(np.argmax(predictions, 1) == np.argmax(labels, 1)) /
          predictions.shape[0])


if __name__ == '__main__':
  app.run(main)
