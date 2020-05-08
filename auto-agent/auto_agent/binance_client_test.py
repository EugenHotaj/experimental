"""Unit tests for binance_client.py."""

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

from unittest import mock

from absl.testing import absltest
from auto_agent import binance_client

FAKE_ORDER_RESP = {"orderId": 1}


class TestBinanceClient(absltest.TestCase):

    def setUp(self):
        self.patcher = mock.patch('binance.client.Client')
        self.mock_api = self.patcher.start()
        self.binance_client = binance_client.BinanceClient(
            'fake_api_key', 'fake_api_secret')

    def tearDown(self):
        self.patcher.stop()

    def test_get_current_price(self):
        expected_price = 400
        fake_ticker_resp = {'lastPrice': expected_price}
        self.mock_api.return_value.get_ticker.return_value = fake_ticker_resp

        actual_price = self.binance_client.get_current_price('AAABBB')

        assert expected_price == actual_price

    def test_pace_buy_order_with_price_places_limit_order(self):
        self.mock_api.return_value.order_limit_buy.return_value = FAKE_ORDER_RESP

        actual_order_id = self.binance_client.place_buy_order(
            'AAABBB', 1, price=.1)

        expected_order_id = FAKE_ORDER_RESP['orderId']
        self.assertEqual(expected_order_id, actual_order_id)
        self.mock_api.return_value.order_limit_buy.assert_called_once()
        self.mock_api.return_value.order_market_buy.assert_not_called()

    def test_place_buy_order_without_price_places_market_order(self):
        self.mock_api.return_value.order_market_buy.return_value = FAKE_ORDER_RESP

        actual_order_id = self.binance_client.place_buy_order('AAABBB', 1)

        expected_order_id = FAKE_ORDER_RESP['orderId']
        self.assertEqual(expected_order_id, actual_order_id)
        self.mock_api.return_value.order_limit_buy.assert_not_called()
        self.mock_api.return_value.order_market_buy.assert_called_once()

    def test_place_sell_order_with_price_places_limit_order(self):
        self.mock_api.return_value.order_limit_sell.return_value = FAKE_ORDER_RESP

        actual_order_id = self.binance_client.place_sell_order(
            'AAABBB', 1, price=.1)

        expected_order_id = FAKE_ORDER_RESP['orderId']
        self.assertEqual(expected_order_id, actual_order_id)
        self.mock_api.return_value.order_limit_sell.assert_called_once()
        self.mock_api.return_value.order_market_sell.assert_not_called()

    def test_place_sell_order_without_price_places_market_order(self):
        self.mock_api.return_value.order_market_sell.return_value = FAKE_ORDER_RESP

        actual_order_id = self.binance_client.place_sell_order('AAABBB', 1)

        expected_order_id = FAKE_ORDER_RESP['orderId']
        self.assertEqual(expected_order_id, actual_order_id)
        self.mock_api.return_value.order_limit_sell.assert_not_called()
        self.mock_api.return_value.order_market_sell.assert_called_once()

    def test_cancel_order(self):
        self.binance_client.cancel_order('AAABBB', 'order_id')

        self.mock_api.return_value.cancel_order.assert_called_once()

if __name__ == '__main__':
    absltest.main()
