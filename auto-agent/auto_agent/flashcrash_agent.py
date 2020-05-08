"""An agent which tries to capitalize on flash crashes."""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import decimal
import time

from enum import Enum
from absl import logging

_TICK_TIME_S = .5


class Status(Enum):
    """The agent status."""

    IDLE = 0  # The has no orders pending and is not holding any assets.
    BUYING = 1  # The agent has placed a buy order, but it may not be filled.
    HODLING = 2  # The agent is holding assets.
    SELLING = 3  # The agent has placed a sell order, but it may not be filled.
    DONE = 4  # The agent is done trading and will be destroyed.


class FlashCrashAgent(object):
    """An autonomous trading agent which capitalizes on flash crashes.

    The agent acts as follows:
        1. Check the price of the current market. Set limit buy at -10% price.
        2. While the order is not filled:
            2.1 If the price steadily decreases to -5% of the original price,
                cancel our order and set another limit buy at -10% (i.e. -15%
                of the original price).
            2.2 If the price steadily increases to +5% of the original price,
                cancel our order and set another limit buy at -10% (i.e. -5% of
                the original price).
            2.3 Otherwise do nothing.
        3. While the order is filled:
            3.1 If the price increases by 5% of the buy price, sell the current
                position.
            3.2 If the price decreases by 10% of the buy price, sell the
                current position at a loss -- this was not a flash crash and we
                ended up losing money.
            3.3 Otherwise do nothing.
    """

    def __init__(self, client, name, market, quantity):
        """Constructor.

        Args:
            name: The name of the agent.
            client: The binance_client.BinanceClient to make orders with.
            market: The market for the agent to operate in.
            quantity: The quantity of assets to trade.
        """
        self._name = name
        self._client = client
        self._market = market
        self._quantity = quantity

        self._status = Status.IDLE

        # Buy price modifier is relative to current price.
        self._buy_price_modifier = decimal.Decimal(.001)  # Low for testing.

        self._order_id = None
        self._buy_price = None

    def _act(self):
        """Perform one clock tick.

        Returns:
            A boolean indicating whether the agent is finished with all
                activities.

        """
        if self._status == Status.DONE:
            logging.info('Finished all activities.')
            return True

        current_price = self._client.get_current_price(self._market)

        if self._status == Status.IDLE:
            buy_price = current_price * self._buy_price_modifier
            order_id = self._client.place_buy_order(
                market=self._market,
                quantity=self._quantity,
                price=buy_price)
            logging.info('Agent %s placed order %d to buy %s for %s' % (
                self._name, self._order_id, self._market, self._buy_price))
            self._order_id = order_id
            self._buy_price = buy_price
            self._status = Status.BUYING
        elif self._status == Status.BUYING:
            # TODO(eugenhotaj): may need to wait for order to fill before
            # proceeding.
            self._status = Status.HODLING
        elif self._status == Status.HODLING:
            # Just cancel the order for now as we test.
            self._client.cancel_order(self._order_id)
            logging.info('Agent %s cancelled order %d' %
                         (self._name, self._order_id))
            self._order_id = None
            self._buy_price = None
            self._status = Status.DONE
        elif self._status == Status.SELLING:
            # TODO(eugenhotaj): may need to wait for order to fill before
            # proceeding.
            self._status = Status.DONE
        else:
            # TODO(eugenhotaj): This could be dangerous if the agent is in the
            # middle of a cycle. Might consider more robust exception handling.
            raise ValueError('Unrecognized agent status.')

        return False

    def run(self):
        """Run main loop for the agent.

        Note: This function takes care of all the low level loop timing, and,
        for simplicity, should not contain any business logic. All business
        logic should be implemented in #_act.
        """
        is_finished = False
        while not is_finished:
            start_time = time.time()
            is_finished = self._act()
            end_time = time.time()
            delta = end_time - start_time

            # Sleep if there is extra time in the tick.
            if delta < _TICK_TIME_S:
                time_to_sleep = _TICK_TIME_S - delta
                time.sleep(time_to_sleep)
            else:
                logging.warning('Could not keep up the tick rate.')
