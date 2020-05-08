"""TODO(eugenhotaj)."""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import os
import sys
sys.path.append(os.path.abspath('..'))

from absl import app
from auto_agent import binance_client
from auto_agent import config
from auto_agent import flashcrash_agent


def main(argv):
    """Driver."""
    del argv  # Unused.

    client = binance_client.BinanceClient(
        config.API_KEY, config.API_SECRET)
    agent = flashcrash_agent.FlashCrashAgent(
        client, name='agent', market='IOTAETH', quantity=1)
    agent.run()


if __name__ == '__main__':
    app.run(main)
