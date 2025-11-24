# --- БЛОК 1: НАСТРОЙКИ ---
library(leaflet)
library(osrm)
library(dplyr)
library(GA)
library(htmlwidgets)

# Параметры штрафов
penalty_late <- 10000
penalty_overload <- 10000
penalty_truck <- 500

# --- БЛОК 2: ЗАГРУЗКА ДАННЫХ ---
vrp_data <- read.csv("../data/vrp_data.csv")
vehicles <- read.csv("../data/vehicles.csv")
fuel_stations <- read.csv("../data/fuel_stations.csv")

max_id <- max(vrp_data$id)
fuel_stations$id <- (max_id + 1):(max_id + nrow(fuel_stations))


options(osrm.server = "http://127.0.0.1:5000/")
options(osrm.profile = "car")

# Депо
depot_coords <- c(vrp_data$lat[1], vrp_data$lon[1])
customers <- vrp_data[-1, ]
n_customers <- nrow(customers)

all_locations <- rbind(
  data.frame(id = 0, lon = depot_coords[2], lat = depot_coords[1]),
  customers %>% select(id, lon, lat),
  fuel_stations %>% select(id, lon, lat)
)

# --- БЛОК 3: МАТРИЦА РАССТОЯНИЙ И ВРЕМЕНИ ---
dist_obj <- osrmTable(
  src = all_locations[c("lon", "lat")],
  dst = all_locations[c("lon", "lat")],
  measure = c("duration", "distance")
)

time_matrix <- dist_obj$durations
dist_matrix <- dist_obj$distances

print("Матрицы расстояний и времени построены.")

# --- БЛОК 4: ФИТНЕС-ФУНКЦИЯ С ЗАПРАВКАМИ ---
fitness_vrptw <- function(tour) {
  total_cost <- 0
  total_penalties <- 0
  trucks_used <- 0

  truck_load <- 0
  truck_time <- 0
  current_node_idx <- 1
  truck_idx <- 1
  vehicle <- vehicles[truck_idx, ]
  current_fuel <- vehicle$tank_capacity

  for (i in 1:length(tour)) {
    client_id <- tour[i]
    matrix_idx <- client_id + 1
    row_idx <- which(customers$id == client_id)

    dem <- customers$demand[row_idx]
    ready <- customers$ready_time[row_idx]
    due <- customers$due_time[row_idx]
    service <- customers$service_time[row_idx]

    # --- Проверка вместимости ---
    if (truck_load + dem > vehicle$capacity) {
      total_cost <- total_cost + vehicle$cost_per_km * (dist_matrix[current_node_idx, 1] / 1000)
      trucks_used <- trucks_used + 1
      truck_idx <- ((truck_idx) %% nrow(vehicles)) + 1
      vehicle <- vehicles[truck_idx, ]
      truck_load <- 0
      truck_time <- 0
      current_node_idx <- 1
      current_fuel <- vehicle$tank_capacity
    }

    # --- Расчет пути к клиенту ---
    travel_dist <- dist_matrix[current_node_idx, matrix_idx]
    travel_time <- time_matrix[current_node_idx, matrix_idx]
    fuel_needed <- (travel_dist / 1000) * vehicle$fuel_consumption_per_km

    # --- Проверка топлива и заправка ---
    if (fuel_needed + vehicle$required_end_fuel > current_fuel) {
      # ищем ближайшую заправку
      station_idx <- which.min(sqrt((fuel_stations$lat - customers$lat[row_idx])^2 +
        (fuel_stations$lon - customers$lon[row_idx])^2))
      station <- fuel_stations[station_idx, ]
      station_matrix_idx <- n_customers + 2 + station_idx

      dist_to_station <- dist_matrix[current_node_idx, station_matrix_idx]
      time_to_station <- time_matrix[current_node_idx, station_matrix_idx]
      fuel_cost <- (vehicle$tank_capacity - current_fuel) * station$fuel_price

      total_cost <- total_cost + vehicle$cost_per_km * (dist_to_station / 1000) + fuel_cost

      # Обновление позиции и топлива
      current_node_idx <- station_matrix_idx
      current_fuel <- vehicle$tank_capacity
      truck_time <- truck_time + time_to_station
    }

    arrival_time <- truck_time + travel_time
    if (arrival_time > due) total_penalties <- total_penalties + penalty_late
    if (arrival_time < ready) arrival_time <- ready

    # --- Обновление состояния грузовика ---
    truck_load <- truck_load + dem
    truck_time <- arrival_time + service
    current_fuel <- current_fuel - fuel_needed

    if (truck_load == dem) total_cost <- total_cost + vehicle$fixed_cost
    total_cost <- total_cost + vehicle$cost_per_km * (travel_dist / 1000)
    current_node_idx <- matrix_idx
  }

  total_cost <- total_cost + vehicle$cost_per_km * (dist_matrix[current_node_idx, 1] / 1000)
  trucks_used <- trucks_used + 1
  total_penalties <- total_penalties + (trucks_used * penalty_truck)

  return(-(total_cost + total_penalties)) # nolint: return_linter.
}

# --- БЛОК 5: ЗАПУСК GA ---
ga_result <- ga(
  type = "permutation",
  fitness = fitness_vrptw,
  lower = 1,
  upper = n_customers,
  popSize = 500,
  maxiter = 1000,
  run = 50,
  pmutation = 0.4,
  monitor = TRUE
)

best_solution <- ga_result@solution[1, ]
print("Алгоритм завершен!")

# --- БЛОК 6: ВИЗУАЛИЗАЦИЯ МАРШРУТОВ С ЗАПРАВКАМИ ---
get_detailed_routes <- function(tour) {
  routes_sf <- list()
  truck_load <- 0
  truck_idx <- 1
  vehicle <- vehicles[truck_idx, ]
  current_fuel <- vehicle$tank_capacity
  curr_pos <- c(depot_coords[2], depot_coords[1])
  depot_pos <- curr_pos
  trip_segments <- list()
  current_node_idx <- 1 # начнем с депо

  for (i in 1:length(tour)) {
    client_id <- tour[i]
    client_row <- customers[customers$id == client_id, ]
    target_pos <- c(client_row$lon, client_row$lat)
    matrix_idx <- client_id + 1
    travel_dist <- dist_matrix[current_node_idx, matrix_idx]
    fuel_needed <- (travel_dist / 1000) * vehicle$fuel_consumption_per_km

    # --- Смена грузовика или превышение объема ---
    if (truck_load + client_row$demand > vehicle$capacity) {
      # маршрут к депо
      route_segment <- osrmRoute(src = curr_pos, dst = depot_pos, overview = "full", returnclass = "sf")
      trip_segments[[length(trip_segments) + 1]] <- route_segment
      routes_sf[[truck_idx]] <- do.call(rbind, trip_segments)

      truck_idx <- ((truck_idx) %% nrow(vehicles)) + 1
      vehicle <- vehicles[truck_idx, ]
      truck_load <- 0
      current_fuel <- vehicle$tank_capacity
      curr_pos <- depot_pos
      trip_segments <- list()
      current_node_idx <- 1
    }

    # --- Дозаправка, если топлива не хватает ---
    if (fuel_needed + vehicle$required_end_fuel > current_fuel) {
      station_idx <- which.min(sqrt((fuel_stations$lat - client_row$lat)^2 +
        (fuel_stations$lon - client_row$lon)^2))
      station <- fuel_stations[station_idx, ]
      station_pos <- c(station$lon, station$lat)
      station_matrix_idx <- n_customers + 2 + station_idx

      # маршрут до заправки
      route_segment <- osrmRoute(src = curr_pos, dst = station_pos, overview = "full", returnclass = "sf")
      trip_segments[[length(trip_segments) + 1]] <- route_segment

      curr_pos <- station_pos
      current_fuel <- vehicle$tank_capacity
      current_node_idx <- station_matrix_idx
    }

    # --- Маршрут до клиента ---
    route_segment <- osrmRoute(src = curr_pos, dst = target_pos, overview = "full", returnclass = "sf")
    trip_segments[[length(trip_segments) + 1]] <- route_segment

    truck_load <- truck_load + client_row$demand
    current_fuel <- current_fuel - fuel_needed
    curr_pos <- target_pos
    current_node_idx <- matrix_idx
  }

  # --- Возврат в депо ---
  route_segment <- osrmRoute(src = curr_pos, dst = depot_pos, overview = "full", returnclass = "sf")
  trip_segments[[length(trip_segments) + 1]] <- route_segment
  routes_sf[[truck_idx]] <- do.call(rbind, trip_segments)

  return(routes_sf)
}


# --- БЛОК 7: ВИЗУАЛИЗАЦИЯ КАРТЫ ---
truck_routes_sf <- get_detailed_routes(best_solution)
colors <- colorFactor(palette = "Set1", domain = 1:length(truck_routes_sf))

depot_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/664/664468.png",
  iconWidth = 40,
  iconHeight = 40
)
fuel_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/1505/1505662.png",
  iconWidth = 30,
  iconHeight = 30
)
customer_icon <- makeIcon(
  iconUrl = "   https://cdn-icons-png.flaticon.com/512/126/126122.png ",
  iconWidth = 30,
  iconHeight = 30
)


map <- leaflet() %>%
  addTiles() %>%
  addMarkers(
    lng = depot_coords[2], lat = depot_coords[1],
    popup = "<b>СКЛАД (DEPOT)</b>",
    icon = depot_icon
  ) %>%
  addMarkers(
    data = customers,
    lng = ~lon, lat = ~lat,
    popup = ~ paste("<b>Client ID:</b>", id, "<br>Demand:", demand, "кг"),
    icon = customer_icon,
    group = "Clients"
  ) %>%
  addMarkers(
    data = fuel_stations, lng = ~lon, lat = ~lat,
    popup = ~ paste("<b>Fuel Station ID:</b>", id, "<br>Price:", fuel_price),
    icon = fuel_icon,
    group = "Fuel Stations"
  ) %>%
  addLayersControl(
    overlayGroups = c("Clients", "Fuel Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )


for (i in 1:length(truck_routes_sf)) {
  if (!is.null(truck_routes_sf[[i]])) {
    map <- map %>% addPolylines(
      data = truck_routes_sf[[i]],
      color = colors(i), weight = 4, opacity = 0.8,
      group = paste("Truck", i),
      popup = paste("Маршрут машины №", i)
    )
  }
}

map <- map %>% addLayersControl(
  overlayGroups = c(paste("Truck", 1:length(truck_routes_sf)), "Fuel Stations"),
  options = layersControlOptions(collapsed = FALSE)
)

print(map)

# --- БЛОК 8: МАНИФЕСТ ---
format_time <- function(minutes) {
  hours <- floor(minutes / 60)
  mins <- round(minutes %% 60)
  sprintf("%02d:%02d", hours %% 24, mins)
}

print_manifest <- function(tour) {
  cat("\n==================== РАСПИСАНИЕ ====================\n")
  current_trip_clients <- c()
  current_trip_load <- 0
  truck_num <- 1
  vehicle <- vehicles[truck_num, ]
  current_fuel <- vehicle$tank_capacity

  process_truck <- function(clients_list, t_num) {
    if (length(clients_list) == 0) {
      return()
    }
    cat(paste0("\n[ ГРУЗОВИК № ", t_num, " | Тип: ", vehicle$vehicle_type, " ]\n"))
    cat("ID | Статус | Прибытие | Окно | Разгрузка | Груз | Стоимость | Топливо\n")

    truck_time <- 360
    curr_loc <- 1
    total_cost <- 0
    current_fuel <- vehicle$tank_capacity

    for (client_id in clients_list) {
      row_idx <- which(customers$id == client_id)
      dem <- customers$demand[row_idx]
      ready <- customers$ready_time[row_idx]
      due <- customers$due_time[row_idx]
      service <- customers$service_time[row_idx]

      travel <- time_matrix[curr_loc, client_id + 1]
      travel_dist <- dist_matrix[curr_loc, client_id + 1]
      fuel_needed <- (travel_dist / 1000) * vehicle$fuel_consumption_per_km

      fuel_event <- ""
      if (fuel_needed + vehicle$required_end_fuel > current_fuel) {
        station_idx <- which.min(sqrt((fuel_stations$lat - customers$lat[row_idx])^2 +
          (fuel_stations$lon - customers$lon[row_idx])^2))
        station <- fuel_stations[station_idx, ]
        station_matrix_idx <- n_customers + 2 + station_idx

        dist_to_station <- dist_matrix[curr_loc, station_matrix_idx]
        time_to_station <- time_matrix[curr_loc, station_matrix_idx]
        fuel_cost <- (vehicle$tank_capacity - current_fuel) * station$fuel_price

        total_cost <- total_cost + vehicle$cost_per_km * (dist_to_station / 1000) + fuel_cost
        truck_time <- truck_time + time_to_station
        current_fuel <- vehicle$tank_capacity
        curr_loc <- station_matrix_idx
        fuel_event <- paste0("Заправка на станции ID ", station$id)
      }

      arrival <- truck_time + travel
      status <- "OK"
      if (arrival > due) status <- "! LATE"
      if (arrival < ready) arrival <- ready

      cost <- vehicle$cost_per_km * (travel_dist / 1000)
      total_cost <- total_cost + cost
      current_fuel <- current_fuel - fuel_needed
      curr_loc <- client_id + 1
      truck_time <- arrival + service

      cat(sprintf(
        "%-4d | %-7s | %-8s | %-13s | %-10s | %-6d | %-8.2f | %s\n",
        client_id, status,
        format_time(arrival),
        format_time(ready),
        format_time(due),
        dem, cost, fuel_event
      ))
    }

    cat(paste(">> Итого стоимость маршрута:", total_cost, "\n"))
  }

  for (i in 1:length(tour)) {
    client_id <- tour[i]
    row_idx <- which(customers$id == client_id)
    dem <- customers$demand[row_idx]

    if (current_trip_load + dem > vehicle$capacity) {
      process_truck(current_trip_clients, truck_num)
      truck_num <- min(truck_num + 1, nrow(vehicles))
      vehicle <- vehicles[truck_num, ]
      current_trip_clients <- c()
      current_trip_load <- 0
      current_fuel <- vehicle$tank_capacity
    }

    current_trip_clients <- c(current_trip_clients, client_id)
    current_trip_load <- current_trip_load + dem
  }
  process_truck(current_trip_clients, truck_num)
}

print_manifest(best_solution)
