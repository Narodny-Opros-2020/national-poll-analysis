import pandas as pd
import numpy as np
# get_ipython().run_line_magic('matplotlib', 'inline')

import warnings
warnings.filterwarnings("ignore")

#read raw data
df = pd.read_csv('all_viber_data_raw.csv', encoding='UTF-8')

# Viber poll has been stopped at 2020-08-06 19:12 UTC,
# so we exclude people who joint later
df['created_at'] = pd.to_datetime(df['created_at'])
df = df[df['created_at'] < '2020-08-06 19:12']

# exclude non-Belarusian devices and citizens and underaged people
df_valid = df[(df['country'] == 'BY') & (df['property.isBelarus'] == 'Да') & (df['property.age'] != 'младше 18')]

# omit service fields (select only meaningful)
df_valid = df_valid[['candidate', 'property.age', 'property.gender', 'property.residence_location', 'property.residence_location_type', 'property.income_total', 'property.education_level', 'property.vote_day']]
# rename for simplicity
df_valid.columns = ['candidate', 'age', 'gender', 'region', 'location_type', 'income', 'education', 'vote_day']

# exclude people who weren't going to vote
df_valid.drop(df_valid.loc[df_valid['vote_day'] == 'Не пойду голосовать'].index, inplace=True)

# everyone from Viber poll uses Viber
df_valid['use_viber'] = True

# eliminate wrong values from the beginning of the poll
df_valid.loc[df_valid['age'] == 'старше 70', 'age'] = 'старше 60'

df_valid.loc[df_valid['age'] == '18-25', 'age'] = '18-30'

df_valid.drop(df_valid.loc[(df_valid['age'] == '26-40') | (df_valid['age'] == '55-70') | (df_valid['age'] == '41-55')].index, inplace=True)

df_valid.loc[df_valid['region'] == 'Минск', 'location_type'] = 'Областной центр / Минск'

# exclude people outside of Belarus
df_valid.drop(df_valid.loc[df_valid['region'] == 'Проживаю за пределами РБ'].index, inplace=True)
df_valid.drop(df_valid.loc[df_valid['location_type'] == 'Проживаю за пределами РБ'].index, inplace=True)

# create field for area of residence
df_valid['countryside'] = df_valid['location_type'].map(lambda x: 'Село' if x == 'Агрогородок / Село / Деревня' else 'Город')

df_valid['location_type'] = df_valid['countryside']

df_valid.drop('countryside', axis=1, inplace=True)

# drop na values
df_valid = df_valid.dropna(subset=['age', 'gender', 'region', 'location_type', 'income', 'education', 'use_viber', 'vote_day'])

# df_valid.info()

# introduce na for those who didn't choose a candidate
df_valid['candidate'] = df_valid['candidate'].map(lambda x: np.nan if ((x == 'Затрудняюсь ответить') | (x == 'Не пойду голосовать')) else x)

# create early voting field
df_valid['early_voting'] = df_valid['vote_day'].map({'В день выборов (9 августа)': 'Нет', 'Досрочно (4-8 августа)': 'Да'})

df_valid.drop(['vote_day'], axis = 1, inplace=True)

# write out the data to .csv file
df_valid.to_csv('/prepared_vote_days_release.csv', index=False)
