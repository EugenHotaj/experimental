"""An Algorithmic trader using Robinhood's private API.

As this API is private the client is likley to break often. If the client
breaks, we break.
""" 

import contextlib
import getpass

from absl import app
import matplotlib.pyplot as plt
import pandas as pd
import robin_stocks as client

ONE_HOUR= 60 * 60

@contextlib.contextmanager
def authenticated_session(expiration=ONE_HOUR):
    """Creates an authenticated Robinhood session.
    
    Logs in at the start of the context and out at the end. 

    Args:
        expiration: How long to keep the session alive for. 
    """
    username = input("Username: ")
    password = getpass.getpass("Password: ")
    try:
        client.login(username, password, expiresIn=expiration)
        yield
    finally:
        client.logout()


def get_historicals_as_dataframe():
    data = client.get_historicals(['aapl'], span='year', bounds='regular')
    df = pd.DataFrame(data)
    df = df[['begins_at', 'close_price']]
    df['begins_at'] = pd.to_datetime(df['begins_at'])
    df['close_price'] = df['close_price'].astype('float64')
    df = df.set_index('begins_at')
    return df


def main(argv):
    del argv  # Unused.

    with authenticated_session():
        df = get_historicals_as_dataframe()
        rdf = df.rolling(20)
        df['rolling_mean'] = rdf.mean().fillna(method='bfill')['close_price']
        df['rolling_std'] = rdf.std().fillna(method='bfill')['close_price']
        df['+2sig'] = df['rolling_mean'] + 2 * df['rolling_std']
        df['-2sig'] = df['rolling_mean'] - 2 * df['rolling_std']

        df.plot(y=['close_price', 'rolling_mean', '+2sig', '-2sig'])
        plt.show()


if __name__ == '__main__':
    app.run(main)


