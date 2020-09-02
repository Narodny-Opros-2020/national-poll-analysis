import pandas as pd
import numpy as np
import warnings
warnings.filterwarnings("ignore")

# read raw street poll data
street_poll = pd.read_csv('all_street_data_raw.csv')

# rename columns for simplicity
street_poll.columns = ['time', 'location', 'age', 'gender', 'will_vote', 'early_voting', 'candidate', 'education','income', 'inf_source', 'code', 'comment', 'location_type', 'region']

# introduce use_viber field based on means of communications that were picked by respondent
street_poll['use_viber'] = street_poll['inf_source'].map(lambda s: (s.find('Viber')>=0), na_action='ignore')

# leave only meaningful columns
street_valid = street_poll[['age', 'gender', 'location_type', 'region', 'candidate', 'use_viber', 'education', 'income', 'early_voting']]

# convert street poll values to Viber poll ones for consistency
street_valid['gender'] = street_valid['gender'].map(lambda x: "Женский" if x == "Жен" else "Мужской")

street_valid.loc[street_valid['location_type'] == 'город', 'location_type'] = 'Город'
street_valid.loc[street_valid['location_type'] == 'село', 'location_type'] = 'Село'

street_valid['region'] = street_valid['region'].astype('str').map({'1.0': 'Брестская', 
                                           '2.0': 'Витебская', 
                                           '3.0': 'Гомельская', 
                                           '4.0': 'Гродненская', 
                                           '5.0': 'Минская', 
                                           '6.0': 'Могилевская', 
                                           '7.0': 'Минск'}, na_action='ignore')

edu_map = {'Высшее': 'Высшее',
           'Среднее специальное': 'Среднее специальное',
           'Базовое \ Среднее':'Базовое / Среднее общее (школа)',
           'Профессионально-техническое':'Профессионально-техническое',
           'Другое \ не хочу отвечать': 'Другое'}
street_valid['education'] = street_valid['education'].map(edu_map, na_action='ignore')

income_map = {'1000 - 2000': '1000 - 2000 бел. руб.',              
            '500 - 1000':'500 - 1000 бел. руб.',               
            'выше 2000': 'Выше 2000 бел.руб.',                 
            'Не хочу отвечать на этот вопрос': 'Не хочу отвечать на этот вопрос',    
            'До 500':'До 500 бел. руб.'}
street_valid['income'] = street_valid['income'].map(income_map, na_action='ignore')

# some people had already voted early, so early_voting field should be Yes
street_valid.loc[street_valid['early_voting'] == 'Уже проголосовал (-а) досрочно', 'early_voting'] = 'Да'

# introduce na for those who didn't choose a candidate
street_valid['candidate'] = street_valid['candidate'].map(lambda x: np.nan if ((x == 'Затрудняюсь ответить') | (x == 'Не пойду')) else x)

# drop na values except candidate column
street_valid = street_valid.dropna(subset=['age', 'gender', 'region', 'location_type', 'income', 'education', 'use_viber', 'early_voting'])
# street_valid.info()

# write out data to .csv file
street_valid.to_csv('prepared_street_vote_day_release.csv', index=False, encoding='UTF-8')

