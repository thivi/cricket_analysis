

import pandas as pd

def sanitize_data(data):

    df = data[data.isna().sum(axis=1) <= 42].copy()
    print("number of rows removed",len(data)-len(df))


    # Convert columns to the proper types
    df['batting_team'] = df['batting_team'].astype(str) #1 
    df['batsman_id'] = df['batsman_id'].astype(str) #2
    df['batsman_name'] = df['batsman_name'].astype(str) #3
    df['batsman_is_rhb'] = df['batsman_is_rhb'].astype(bool) #4
    df['non_striker_id'] = df['non_striker_id'].astype(str) #5
    df['non_striker_name'] = df['non_striker_name'].astype(str) #6
    df['non_striker_is_rhb'] = df['non_striker_is_rhb'].astype(bool) #7
    df['bowling_team'] = df['bowling_team'].astype(str) #8
    df['bowler_id'] = df['bowler_id'].astype(str) #9
    df['bowler_name'] = df['bowler_name'].astype(str) #10
    df['bowler_is_rhb'] = df['bowler_is_rhb'].astype(bool) #11
    df['delivery_type'] = df['delivery_type'].astype(str) #12
    # df['innings'] = df['innings'].astype(int) #13
    df['innings'] = df['innings'].astype('Int64')  # Capital I
    df['ball'] = df['ball'].astype(int) #14
    df['over'] = df['over'].astype(int) #15
    df['shot_is_attacked'] = df['shot_is_attacked'].astype(bool) #16
    df['shot_is_played'] = df['shot_is_played'].astype(bool) #17
    df['shot_type'] = df['shot_type'].astype(str) #18
    # df['bounce_above_stumps'] = df['bounce_above_stumps'].astype(str) #19
    df['bounce_angle'] = df['bounce_angle'].astype(float) #20
    df['bounce_pos_x'] = df['bounce_pos_x'].astype(float) #21
    df['bounce_pos_y'] = df['bounce_pos_y'].astype(float) #22
    df['boucne_pos_z'] = df['boucne_pos_z'].astype(float) #23
    df['crease_pos_x'] = df['crease_pos_x'].astype(float) #24
    # Note: In the original code, crease_pos_y is set from crease_pos_z.
    df['crease_pos_y'] = df['crease_pos_z'].astype(float) #25
    df['crease_pos_z'] = df['crease_pos_z'].astype(float) #26
    df['deviation'] = df['deviation'].astype(float) #27
    df['drop_angle'] = df['drop_angle'].astype(float) #28
    df['hit_stumps'] = df['hit_stumps'].astype(bool) #29
    df['impact_pos_x'] = df['impact_pos_x'].astype(float) #30
    df['impact_pos_y'] = df['impact_pos_y'].astype(float) #31
    df['impact_pos_z'] = df['impact_pos_z'].astype(float)
    df['initial_angle'] = df['initial_angle'].astype(float)
    df['landing_pos_x'] = df['landing_pos_x'].astype(float)
    df['landing_pos_y'] = df['landing_pos_y'].astype(float)
    df['landing_pos_z'] = df['landing_pos_z'].astype(float)
    df['pbr'] = df['pbr'].astype(float)
    # Replace "None" strings with 0 before converting
    df['react_time_to_crease'] = df['react_time_to_crease'].replace("None", 0).astype(float)
    df['react_time_to_intercept'] = df['react_time_to_intercept'].replace("None", 0).astype(float)
    df['real_distance'] = df['real_distance'].astype(float)
    df['release_pos_x'] = df['release_pos_x'].astype(float)
    df['release_pos_y'] = df['release_pos_y'].astype(float)
    df['release_pos_z'] = df['release_pos_z'].astype(float)
    df['release_speed'] = df['release_speed'].astype(float)
    df['spin_rate'] = df['spin_rate'].astype(float)
    df['stump_pos_x'] = df['stump_pos_x'].astype(float)
    df['stump_pos_y'] = df['stump_pos_y'].astype(float)
    df['stump_pos_z'] = df['stump_pos_z'].astype(float)
    df['swing'] = df['swing'].astype(float)
    
    df['match_id'] = df['match_id'].astype(int)
    print("the data type after conversion is ",df['match_id'].dtype)
    df['ball_id'] = df['ball_id'].astype(str)
    df['is_single'] = df['is_single'].astype(bool)
    df['is_double'] = df['is_double'].astype(bool)
    df['is_three'] = df['is_three'].astype(bool)
    df['is_dot'] = df['is_dot'].astype(bool)
    df['is_wide'] = df['is_wide'].astype(bool)
    df['is_no_ball'] = df['is_no_ball'].astype(bool)
    df['is_bye'] = df['is_bye'].astype(bool)
    df['is_leg_bye'] = df['is_leg_bye'].astype(bool)
    df['is_four'] = df['is_four'].astype(bool)
    df['is_six'] = df['is_six'].astype(bool)
    df['is_wicket'] = df['is_wicket'].astype(bool)
    df['wicket_type'] = df['wicket_type'].astype(str)
    df['is_bowler_wicket'] = df['is_bowler_wicket'].astype(bool)
    df['ball_type'] = df['ball_type'].astype(str)
    # df['shot_type_b'] = df['shot_type_b'].astype(str)
    df['pitch_x'] = df['pitch_x'].astype(float)
    df['pitch_y'] = df['pitch_y'].astype(float)
    df['ball_line'] = df['ball_line'].astype(str)
    df['ball_length'] = df['ball_length'].astype(str)
    df['runs'] = df['runs'].astype(float) 
    df['actual_runs'] = df['actual_runs'].astype(float)
    df['extras'] = df['extras'].astype(float)
    # df['ball_runs'] = df['ball_runs'].astype(float)
    df['is_bouncer'] = df['is_bouncer'].astype(bool)
    df['is_free_hit'] = df['is_free_hit'].astype(bool)
    df['innings_no'] = df['innings_no'].astype(int)
    df['ground'] = df['ground'].astype(str)
    df['date'] = df['date'].astype(str)

    return df
# Read and sanitize data
data2022 = pd.read_csv("./datasets/data2022.csv", index_col=None,low_memory=False)
#print(data2022.info())
#print(len(data2022))
data2022 = sanitize_data(data2022)
# # print(data2022 number of rows
#print(len(data2022))
data2023 = pd.read_csv("./datasets/data2023.csv", index_col=None,low_memory=False)
data2023 = sanitize_data(data2023)

data2024 = pd.read_csv("./datasets/data2024.csv", index_col=None,low_memory=False)
data2024 = sanitize_data(data2024)

data2025 = pd.read_csv("./datasets/data2025.csv", index_col=None,low_memory=False)
data2025 = sanitize_data(data2025)
# # Combine datasets
data = pd.concat([data2022, data2023, data2024,data2025], ignore_index=True)

# Write to CSV
# print(df.info())
data.to_csv("./datasets/ipl_hawkeye_data.csv", index=False)

