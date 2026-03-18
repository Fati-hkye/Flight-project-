def calculate_good_conditions(df):
    def smooth_scale(value, max_val, min_val=0):
        if value <= min_val:
            return 1.0
        elif value >= max_val:
            return 0.0
        else:
            return 1 - (value - min_val) / (max_val - min_val)

    def wind_direction_score_loose(direction):
        mapping = {
            'Nord': 1.0,
            'Nord Nord Ouest': 0.85,
            'Nord Ouest': 0.7,
            'Ouest Nord Ouest': 0.5,
        }
        return mapping.get(direction, 0.3)

    df['wave_score'] = df['mean_wave'].apply(lambda x: smooth_scale(x, max_val=2, min_val=0))
    df['wind_speed_score'] = df['wind_kmh'].apply(lambda x: smooth_scale(x, max_val=70, min_val=0))
    df['wind_dir_score'] = df['wind_direction'].apply(wind_direction_score_loose)

    weights = {'wave': 0.4, 'wind_speed': 0.4, 'wind_dir': 0.2}

    df['good_conditions_scale'] = (
        df['wave_score'] * weights['wave'] +
        df['wind_speed_score'] * weights['wind_speed'] +
        df['wind_dir_score'] * weights['wind_dir']
    )

    df['good_conditions_pct'] = df['good_conditions_scale'] * 100

    return df
