"""Manager which handles making request to Binance."""

import decimal

from binance import client


def _parse_order_resp(resp):
    return resp['orderId']


class BinanceClient(object):
    """An interface which delegates request to the Binance Client."""

    def __init__(self, api_key, api_secret):
        """Constructor."""

        self._client = client.Client(api_key, api_secret)

    def get_current_price(self, market):
        """Return the current price for the given market."""
        market_info = self._client.get_ticker(symbol=market)
        return decimal.Decimal(market_info['lastPrice'])

    def place_buy_order(self, market, quantity, price=0):
        """Send a buy request to binance.

        Args:
            market: The string market to place the order in.
            quantity: The amount of the assed to buy.
            price: The bid price. If positive, a limit buy order will be
                placed.
        Returns:
            The numeric order id of the placed order.

        """
        if price > 0:
            resp = self._client.order_limit_buy(
                symbol=market, quantity=1000, price='0.0001')
        else:
            resp = self._client.order_market_buy(
                symbol=market, quantity=quantity)
        return _parse_order_resp(resp)

    def place_sell_order(self, market, quantity, price=0):
        """Send a sell request to binance.

        Args:
            market: The string market to place the order in.
            quantity: The amount of the assed to buy.
            price: The ask price. If positive, a limit sell order will be
                placed.
        Returns:
            The numeric order id of the placed order.

        """
        if price > 0:
            resp = self._client.order_limit_sell(
                symbol=market, quantity=quantity, price=str(price))
        else:
            resp = self._client.order_market_sell(
                symbol=market, quantity=quantity)
        return _parse_order_resp(resp)

    def cancel_order(self, market, order_id):
        """Cancel an order.

        Args:
            market: The string market to place the order in.
            order_id: The numeric order id of the order to cancel.
        """
        return self._client.cancel_order(symbol=market, order=order_id)
