import numpy as np
import pandas as pd
import seaborn as sb
from datetime import datetime
import matplotlib.pyplot as plt

# import os
# for dirname, _, filenames in os.walk('/'):
#     for filename in filenames:
#         print(os.path.join(dirname, filename))

t20i = pd.read_csv('ball_by_ball_it20.csv')
t20i.sample(5)

t20i.columns

full_name = ''
for name in t20i['Batter'].unique():
    if 'kohli' in name.lower():
        full_name = name
        break

full_name
kohli_t20i = t20i[t20i['Batter'] == full_name]
kohli_t20i.sample(6)

recommended_columns = ['Match ID', 'Date', 'Venue', 'Bat First', 'Bat Second', 'Innings', 'Over', 'Ball','Bowler', 
                       'Batter Runs', 'Runs From Ball', 'Batter Balls Faced', 'Wicket', 'Method', 'Player Out',
                       'Player Out Runs', 'Player Out Balls Faced', 'Winner', 'Chased Successfully']
kohli_t20i = kohli_t20i[recommended_columns]
kohli_t20i.sample(5)

match_id_mapping = {}
match_id_counter = 1

for match_id in kohli_t20i['Match ID'].unique():
    match_id_mapping[match_id] = f"Match {match_id_counter}"
    match_id_counter += 1

kohli_t20i['Match ID'] = kohli_t20i['Match ID'].map(match_id_mapping)

kohli_t20i

def get_info(dataframe):
    
    info = pd.DataFrame()
    
    info['Columns'] = kohli_t20i.columns
    info['Data Type'] = kohli_t20i.dtypes.values
    
    info['Missing Values'] = kohli_t20i.isnull().sum().values
    info['Percentage Missing'] = info['Missing Values'] / len(kohli_t20i)
    
    return info

get_info(kohli_t20i)

debut_date = min(kohli_t20i['Date'])
date_datetime = datetime.strptime(debut_date, '%Y-%m-%d')

formatted_date = date_datetime.strftime('%d %B %Y')
formatted_date      

print("Matches Played: ", kohli_t20i['Match ID'].nunique()) 

total_runs_scored = kohli_t20i['Batter Runs'].sum()
print('Runs Scored:', total_runs_scored)
