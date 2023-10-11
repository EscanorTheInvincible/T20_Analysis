import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

import os
for dirname, _, filenames in os.walk('./ball_by_ball_it20.csv'):
    for filename in filenames:
        print(os.path.join(dirname, filename))

t20=pd.read_csv('./ball_by_ball_it20.csv')

t20.head()

