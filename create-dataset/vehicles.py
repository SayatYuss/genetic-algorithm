import csv
import random
import math

def round_to_5(x):
    return int(round(x / 5) * 5)

def round_to_100(x):
    return int(round(x / 100) * 100)

def generate_vehicles_csv(num_vehicles, filename='vehicles.csv'):
    distribution_percent = {150: 0.2, 100: 0.3, 70: 0.2, 50: 0.3}
    type_map = {50: "Car", 70: "Minivan", 100: "Van", 150: "SmallTruck"}

    distribution_count = {cap: math.floor(num_vehicles * pct) for cap, pct in distribution_percent.items()}
    total = sum(distribution_count.values())
    diff = num_vehicles - total
    if diff > 0:
        for cap in sorted(distribution_percent, key=distribution_percent.get, reverse=True):
            if diff == 0:
                break
            distribution_count[cap] += 1
            diff -= 1

    vehicles = []

    for capacity, count in distribution_count.items():
        for _ in range(count):
            if capacity <= 50:
                fuel_consumption = round(random.uniform(0.1, 0.2), 2)
                tank_capacity = round_to_5(random.randint(40, 90))
                fixed_cost = round_to_100(random.randint(1000, 3000))
                cost_per_km = int(round(random.uniform(5, 10), 2))
            elif capacity <= 70:
                fuel_consumption = round(random.uniform(0.2, 0.4), 2)
                tank_capacity = round_to_5(random.randint(60, 110))
                fixed_cost = round_to_100(random.randint(3000, 5000))
                cost_per_km = int(round(random.uniform(10, 15), 2))
            elif capacity <= 100:
                fuel_consumption = round(random.uniform(0.5, 0.7), 2)
                tank_capacity = round_to_5(random.randint(80, 140))
                fixed_cost = round_to_100(random.randint(5000, 7000))
                cost_per_km = int(round(random.uniform(15, 20), 2))
            else:
                fuel_consumption = round(random.uniform(0.8, 1.2), 2)
                tank_capacity = round_to_5(random.randint(120, 200))
                fixed_cost = round_to_100(random.randint(7000, 10000))
                cost_per_km = int(round(random.uniform(20, 30), 2))

            vehicle = {
                'type': f'V{len(vehicles)+1}',
                'vehicle_type': type_map[capacity],
                'capacity': capacity,
                'fixed_cost': fixed_cost,
                'cost_per_km': cost_per_km,
                'required_end_fuel': round(tank_capacity / 2, 2),
                'fuel_consumption_per_km': fuel_consumption,
                'tank_capacity': tank_capacity
            }
            vehicles.append(vehicle)

    with open("../data/" + filename, 'w', newline='') as csvfile:
        fieldnames = ['type', 'vehicle_type', 'capacity', 'fixed_cost', 'cost_per_km', 
                      'required_end_fuel', 'fuel_consumption_per_km', 'tank_capacity']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for v in vehicles:
            writer.writerow(v)

    print(f"CSV файл '{filename}' создан с {num_vehicles} машинами с типами на английском.")

# Пример использования
generate_vehicles_csv(12)
