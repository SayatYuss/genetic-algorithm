import pandas as pd
import random

INPUT_FILE = "../data/coords.csv"
OUTPUT_FILE = "../data/vrp_data.csv"
DEPO_LAT = 51.215708
DEPO_LON = 71.508529
N_TRUCKS = 5

try:
    df = pd.read_csv(INPUT_FILE, header=None)
except Exception as ex:
    print(f"error: {ex}")
    exit(1)

final_data = []

final_data.append({
    "id": 0,
    "type": "depot",
    "lat": DEPO_LAT,
    "lon": DEPO_LON,
    "demand": 0,            
    "ready_time": 360,        
    "due_time": 1260,       
    "service_time": 0
})

for index, row in df.iterrows():
    start_hour = random.randint(7, 14)
    window_len = random.randint(2, 5)

    ready_min = start_hour * 60
    due_min = ready_min + (window_len * 60)

    final_data.append({
        "id": index + 1,
        "type": "store",
        "lat": float(row[0]),
        "lon": float(row[1]),
        "demand": random.randint(1, 5) * 10 + random.randint(0, 1) * 5,
        "ready_time": ready_min,
        "due_time": due_min,
        "service_time": random.randint(1, 3) * 10 + random.randint(0, 1) * 5
    })

df_final = pd.DataFrame(final_data)
df_final.to_csv(OUTPUT_FILE, index=False)

