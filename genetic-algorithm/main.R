# --- БЛОК 1: НАСТРОЙКИ ---
library(leaflet)
library(osrm)
library(dplyr)
library(GA)
library(htmlwidgets)

# Константы ограничений
truck_capacity <- 100 # вместимость
max_trucks <- 10 # максимум машин
penalty_late <- 10000 # штраф за опоздание
penalty_overload <- 10000 # штраф за перегруз по весу
penalty_truck <- 500 # штраф за использование лишней машины

# --- БЛОК 2: ДАННЫЕ И OSRM ---
vrp_data <- read.csv("../data/vrp_data.csv")

# Координаты депо
depot_coords <- c(vrp_data$lat[1], vrp_data$lon[1])
options(digits = 15)

# Клиенты
customers <- vrp_data[-1, ]
n_customers <- nrow(customers)

locations <- vrp_data %>% select(id, lon, lat)

dist_obj <- osrmTable(
  src = locations[c("lon", "lat")],
  dst = locations[c("lon", "lat")],
  measure = c("duration", "distance")
)

time_matrix <- dist_obj$durations # время (мин)
dist_matrix <- dist_obj$distances # расстояние (м)

print("Данные загружены и матрицы построены.")

# --- БЛОК 3: ФИТНЕС-ФУНКЦИЯ ---
fitness_vrptw <- function(tour) {
  total_dist <- 0
  total_penalties <- 0

  truck_load <- 0
  truck_time <- 0
  current_node_idx <- 1
  trucks_used <- 1

  for (i in 1:length(tour)) {
    client_id <- tour[i]
    matrix_idx <- client_id + 1

    row_idx <- which(customers$id == client_id)

    dem <- customers$demand[row_idx]
    ready <- customers$ready_time[row_idx]
    due <- customers$due_time[row_idx]
    service <- customers$service_time[row_idx]

    # 1. Проверка: Влезет ли груз?
    if (truck_load + dem > truck_capacity) {
      total_dist <- total_dist + dist_matrix[current_node_idx, 1]
      trucks_used <- trucks_used + 1
      truck_load <- 0
      truck_time <- 0
      current_node_idx <- 1
    }

    # 2. Едем к клиенту
    travel_time <- time_matrix[current_node_idx, matrix_idx]
    travel_dist <- dist_matrix[current_node_idx, matrix_idx]

    arrival_time <- truck_time + travel_time

    # 3. Проверка времени
    if (arrival_time > due) {
      total_penalties <- total_penalties + penalty_late
    }
    if (arrival_time < ready) {
      arrival_time <- ready
    }

    # 4. Обслуживаем
    truck_load <- truck_load + dem
    truck_time <- arrival_time + service
    total_dist <- total_dist + travel_dist
    current_node_idx <- matrix_idx
  }

  # Возврат последней машины в депо
  total_dist <- total_dist + dist_matrix[current_node_idx, 1]

  # Штрафы за лишние машины
  if (trucks_used > max_trucks) {
    total_penalties <- total_penalties + penalty_overload
  }
  total_penalties <- total_penalties + (trucks_used * penalty_truck)

  return(-(total_dist + total_penalties))
}

# --- БЛОК 4: ЗАПУСК GA ---
print("Запуск алгоритма... Пожалуйста, подождите.")

ga_result <- ga(
  type = "permutation",
  fitness = fitness_vrptw,
  lower = 1,
  upper = n_customers,
  popSize = 2000,
  maxiter = 5000,
  run = 100,
  pmutation = 0.4,
  monitor = TRUE
)

# Извлечение лучшего решения
best_solution <- ga_result@solution[1, ]
print("Алгоритм завершен!")

# --- БЛОК 5: КАРТА С РЕАЛЬНЫМИ ДОРОГАМИ (OSRM) ---

print("Построение детальных маршрутов... Это может занять время.")

# 1. Функция получения детальной геометрии маршрутов
get_detailed_routes <- function(tour) {
  routes_sf <- list() # Список для хранения sf объектов (линий)
  truck_load <- 0
  truck_id <- 1
  
  # Координаты склада (lon, lat) для osrm
  curr_pos <- c(depot_coords[2], depot_coords[1]) 
  depot_pos <- c(depot_coords[2], depot_coords[1])
  
  # Временный список сегментов для одной машины
  trip_segments <- list()
  
  for (i in 1:length(tour)) {
    client_id <- tour[i]
    client_row <- customers[customers$id == client_id, ]
    target_pos <- c(client_row$lon, client_row$lat)
    dem <- client_row$demand
    
    # Проверка вместимости (как в фитнес-функции)
    if (truck_load + dem > truck_capacity) {
      # 1. Едем обратно в депо
      route_segment <- osrmRoute(src = curr_pos, dst = depot_pos, overview = "full", returnclass = "sf")
      trip_segments[[length(trip_segments) + 1]] <- route_segment
      
      # Сохраняем маршрут текущего грузовика (объединяем сегменты)
      if(length(trip_segments) > 0) {
        routes_sf[[truck_id]] <- do.call(rbind, trip_segments)
      }
      
      # 2. Сброс для нового грузовика
      truck_id <- truck_id + 1
      truck_load <- 0
      curr_pos <- depot_pos
      trip_segments <- list()
    }
    
    # Едем к клиенту (запрос реальной дороги)
    # overview = "full" дает полную геометрию поворотов
    route_segment <- osrmRoute(src = curr_pos, dst = target_pos, overview = "full", returnclass = "sf")
    trip_segments[[length(trip_segments) + 1]] <- route_segment
    
    truck_load <- truck_load + dem
    curr_pos <- target_pos
  }
  
  # Возврат последнего грузовика в депо
  route_segment <- osrmRoute(src = curr_pos, dst = depot_pos, overview = "full", returnclass = "sf")
  trip_segments[[length(trip_segments) + 1]] <- route_segment
  routes_sf[[truck_id]] <- do.call(rbind, trip_segments)
  
  return(routes_sf)
}

# 2. Получение данных (это займет время на запросы к API)
truck_routes_sf <- get_detailed_routes(best_solution)
colors <- colorFactor(palette = "Set1", domain = 1:length(truck_routes_sf))

# 3. Инициализация карты
map <- leaflet() %>%
  addTiles() %>%
  addMarkers(
    lng = depot_coords[2], lat = depot_coords[1],
    popup = "<b>СКЛАД (DEPOT)</b>",
    icon = makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/664/664468.png", iconWidth = 40, iconHeight = 40)
  ) %>%
  addCircleMarkers(
    data = customers, lng = ~lon, lat = ~lat,
    radius = 6, color = "navy", fillOpacity = 0.8,
    popup = ~ paste("<b>Client ID:</b>", id, "<br>Demand:", demand, "kg")
  )

# 4. Добавление слоев с маршрутами
for (i in 1:length(truck_routes_sf)) {
  # Проверяем, есть ли данные для маршрута
  if (!is.null(truck_routes_sf[[i]])) {
    map <- map %>%
      addPolylines(
        data = truck_routes_sf[[i]],
        color = colors(i),
        weight = 4,
        opacity = 0.8,
        group = paste("Truck", i),
        popup = paste("Маршрут машины №", i)
      )
  }
}

# Управление слоями
map <- map %>% addLayersControl(
  overlayGroups = paste("Truck", 1:length(truck_routes_sf)),
  options = layersControlOptions(collapsed = FALSE)
)

print(map)
print("Карта с дорогами построена.")

# --- БЛОК 6: ОТЧЕТ (MANIFEST) ---
print_manifest <- function(tour) {
  cat("\n======================================================================\n")
  cat("                     ЛОГИСТИЧЕСКОЕ РАСПИСАНИЕ                         \n")
  cat("======================================================================\n")

  current_trip_clients <- c()
  current_trip_load <- 0
  truck_num <- 1

  process_truck <- function(clients_list, t_num) {
    if (length(clients_list) == 0) {
      return()
    }

    # ФИКСИРОВАННЫЙ СТАРТ (06:00)
    start_time <- 360
    start_h <- floor(start_time / 60)
    start_m <- round(start_time %% 60)

    cat(paste0("\n[ ГРУЗОВИК № ", t_num, " ]\n"))
    cat("-------------------------------------------------------------------------------------\n")
    cat(sprintf("%-4s | %-10s | %-10s | %-13s | %-10s | %-10s\n", "ID", "Статус", "Прибытие", "Окно", "Разгрузка", "Груз"))
    cat("-------------------------------------------------------------------------------------\n")

    truck_time <- start_time
    curr_loc <- 1

    for (client_id in clients_list) {
      row_idx <- which(customers$id == client_id)
      dem <- customers$demand[row_idx]
      ready <- customers$ready_time[row_idx]
      due <- customers$due_time[row_idx]
      service <- customers$service_time[row_idx]

      travel <- time_matrix[curr_loc, client_id + 1]
      arrival <- truck_time + travel

      status <- "OK"
      if (arrival > due) status <- "! LATE"
      if (arrival < ready) {
        status <- "Wait"
        arrival <- ready
      }

      arr_rounded <- round(arrival)
      time_str <- sprintf("%02d:%02d", floor(arr_rounded / 60), arr_rounded %% 60)

      ready_rounded <- round(ready)
      due_rounded <- round(due)
      window_str <- sprintf("%02d:%02d-%02d:%02d", floor(ready_rounded / 60), ready_rounded %% 60, floor(due_rounded / 60), due_rounded %% 60)

      cat(sprintf(
        "%-4d | %-10s | %-10s | %-13s | %-10s | %-10s\n",
        client_id, status, time_str, window_str, paste(service, "мин"), paste(dem, "кг")
      ))

      truck_time <- arrival + service
      curr_loc <- client_id + 1
    }
    cat("-------------------------------------------------------------------------------------\n")
    cat(sprintf(">> ВОЗВРАТ В ДЕПО. Итого загрузка: %d / %d кг\n", current_trip_load, truck_capacity))
  }

  for (i in 1:length(tour)) {
    client_id <- tour[i]
    row_idx <- which(customers$id == client_id)
    dem <- customers$demand[row_idx]

    if (current_trip_load + dem > truck_capacity) {
      process_truck(current_trip_clients, truck_num)
      truck_num <- truck_num + 1
      current_trip_clients <- c()
      current_trip_load <- 0
    }

    current_trip_clients <- c(current_trip_clients, client_id)
    current_trip_load <- current_trip_load + dem
  }
  process_truck(current_trip_clients, truck_num)
  cat("\n======================================================================\n")
}

# Запуск отчета
print_manifest(best_solution)
