"""Train a CNN on the CIFAR10 image dataset.

Results: ~77% validation accuracy.

To run:
    python main.py
"""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import pickle
import matplotlib.pyplot as plt
import numpy as np

from absl import app
from keras import layers
from keras import models
from keras.preprocessing import image

_TRAIN_FILE_PATHS = ['./data/data_batch_%d' % i for i in range(1, 6)]
_TEST_FILE_PATH = './data/test_batch'

_IMAGE_SIZE = 32
_IMAGE_CHANNELS = 3

_TRAIN_SIZE = 50000
_TEST_SIZE = 10000
_BATCH_SIZE = 100
_EPOCHS = 50

_NUM_CLASSES = 10


def _load_data():
    """Load all the training and test data."""
    train_set = {}
    test_set = {}

    # Load the train data.
    for file_path in _TRAIN_FILE_PATHS:
        data, labels = _read_data_and_labels_from_file(file_path)
        if 'data' not in train_set.keys():
            train_set['data'] = data
            train_set['labels'] = labels
        else:
            train_set['data'] = np.append(train_set['data'], data, axis=0)
            train_set['labels'] += labels

    # Load the test data.
    data, labels = _read_data_and_labels_from_file(_TEST_FILE_PATH)
    test_set['labels'] = labels
    test_set['data'] = data

    return train_set, test_set


def _read_data_and_labels_from_file(file_path):
    """Read the file with the given path; extract 'data' and 'label' cols."""
    with open(file_path, 'rb') as opened_file:
        data_dict = pickle.load(opened_file, encoding='bytes')
        data = data_dict[b'data']
        labels = data_dict[b'labels']
    return data, labels


def _reshape(data):
    """Reshape the given input data into RGB images.

    The input data consists of 50k rows of 3072 values. Each row represents
    a 32x32 RGB image (i.e. 32 * 32 * 3 = 3072). Each row is reshaped into the
    image dimensions and normalized to be in [0, 1].
    """
    def _transform_func(vec):
        return np.transpose(
            np.reshape(
                vec, (_IMAGE_CHANNELS, _IMAGE_SIZE, _IMAGE_SIZE)), (1, 2, 0))
    return np.apply_along_axis(_transform_func, 1, data)


def _to_one_hot(labels):
    """Convert a list of labels into corresponding one-hot vectors."""
    size = len(labels)
    one_hot = np.zeros((size, _NUM_CLASSES))
    one_hot[np.arange(size), labels] = 1
    return one_hot


def _build_model():
    """Build the CNN."""
    model = models.Sequential()

    model.add(layers.Conv2D(16, 3, activation='relu', input_shape=(32, 32, 3)))
    model.add(layers.MaxPooling2D(2))
    model.add(layers.Conv2D(32, 3, activation='relu'))
    model.add(layers.MaxPooling2D(2))
    model.add(layers.Conv2D(64, 3, activation='relu'))
    model.add(layers.MaxPooling2D(2))

    # One fully connected layer and output layer.
    model.add(layers.Flatten())
    model.add(layers.Dense(512, activation='relu'))
    model.add(layers.Dropout(.2))
    model.add(layers.Dense(_NUM_CLASSES, activation='softmax'))

    model.compile(loss='categorical_crossentropy',
                  optimizer='adam', metrics=['accuracy'])
    return model


def main(argv):
    del argv  # Unused.

    train_set, test_set = _load_data()

    train_data = _reshape(train_set['data'])
    train_labels = _to_one_hot(train_set['labels'])
    train_datagen = image.ImageDataGenerator(
        rotation_range=40,
        width_shift_range=.2,
        height_shift_range=.2,
        shear_range=.2,
        zoom_range=.2,
        fill_mode='nearest',
        horizontal_flip=True,
        rescale=1./255.)
    train_generator = train_datagen.flow(
        train_data, train_labels, batch_size=_BATCH_SIZE)

    test_data = _reshape(test_set['data'])
    test_labels = _to_one_hot(test_set['labels'])
    test_datagen = image.ImageDataGenerator(rescale=1./255.)
    test_generator = test_datagen.flow(
        test_data, test_labels, batch_size=_BATCH_SIZE)

    model = _build_model()
    history = model.fit_generator(
        train_generator,
        steps_per_epoch=_TRAIN_SIZE // _BATCH_SIZE,
        epochs=_EPOCHS,
        validation_data=test_generator,
        validation_steps=_TEST_SIZE // _BATCH_SIZE)

    train_acc = history.history['acc']
    test_acc = history.history['val_acc']
    train_loss = history.history['loss']
    test_loss = history.history['val_loss']
    eps = range(len(train_acc))

    plt.figure(1)
    plt.subplot(211)
    plt.plot(eps, train_acc)
    plt.plot(eps, test_acc)

    plt.subplot(212)
    plt.plot(eps, train_loss)
    plt.plot(eps, test_loss)

    plt.show()


if __name__ == '__main__':
    app.run(main)
