#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at
#' a given speed
#' @param cdrag coefficient due to drag default=0.3
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

autopower = function(V, m, A, cdrag=0.3, crolling=0.015,pair=1.2,g=9.8) {
  P = crolling*m*g*V + 1/2*A*pair*cdrag*V**3
  return(P)
}

# arrange parameters in a structured data frame
possible_cars = data.frame(name = c("A","B","C","D"),mass=c(10000,65000,38000,5000), area = c(22,30,22,10))
#Sample size is 150 (a highway with 150 cars)
nsample <- 150

#Now, let's generate random speeds for the 150 cars and take into account the given average speed and standard deviation.

#Method 1: pmap for repetition

powerall_pmap <- pmap(list(A = possible_cars$area, m = possible_cars$mass), autopower, V = speeds)
# Convert results into a data frame
powerall_pmap <- as.data.frame(powerall_pmap, col.names = possible_cars$name)

# Manipulate data for plotting: structuring data frame into a total of 1 row
powerall_pmap <- powerall_pmap %>% pivot_longer(cols = everything(), names_to = "car", values_to = "power")

# Method 2: For looping for repetition
powerall_loop <- as.data.frame(matrix(nrow = length(speeds), ncol = nrow(possible_cars)), col.names = possible_cars$name)

# Set up FOR loops
for (i in 1:nrow(possible_cars)) {
  powerall_loop[, i] <- autopower(A = possible_cars$area[i], m = possible_cars$mass[i], V = speeds)
}

colnames(powerall_loop) <- possible_cars$name

# Pivot for plotting
powerall_loop <- powerall_loop %>% pivot_longer(cols = everything(), names_to = "car", values_to = "power")

# Add boxplots from above methods that show the range of power consumption across sampled speed for each car type
# Boxplot for pmap
pmap_plot <- ggplot(powerall_pmap, aes(car, power, fill = car)) + geom_boxplot() + ggtitle("Power Consumption with pmap")

# Boxplot for looping
looping_plot <- ggplot(powerall_loop, aes(car, power, fill = car)) + geom_boxplot() + ggtitle("Power Consumption with For Loop")

# Side by side comparison
ggarrange(pmap_plot, looping_plot)

# Check for answers
# Mean Power for all
powerall_pmap %>% map(mean)  # Using pmap
powerall_loop %>% map(mean)  # Using loop
# Max Power for all
Maxall_p <- powerall_pmap %>% map(max)  # Using pmap
Maxall_l <- powerall_loop %>% map(max)  # Using loop

Maxall_l
Maxall_p

# Max and Mean by types of car
# Max and Mean Power for A
A_mean <- mean(powerall_pmap$power[powerall_pmap$car == "A"], na.rm = TRUE)  # Results using pmap are the same as using loop
A_max <- max(powerall_pmap$power[powerall_pmap$car == "A"], na.rm = TRUE)

# Max and Mean Power for B
B_mean <- mean(powerall_pmap$power[powerall_pmap$car == "B"], na.rm = TRUE)  # Results using pmap are the same as using loop
B_max <- max(powerall_pmap$power[powerall_pmap$car == "B"], na.rm = TRUE)

# Max and Mean Power for C
C_mean <- mean(powerall_pmap$power[powerall_pmap$car == "C"], na.rm = TRUE)  # Results using pmap are the same as using loop
C_max <- max(powerall_pmap$power[powerall_pmap$car == "C"], na.rm = TRUE)

# Max and Mean Power for D
D_mean <- mean(powerall_pmap$power[powerall_pmap$car == "D"], na.rm = TRUE)  # Results using pmap are the same as using loop
D_max <- max(powerall_pmap$power[powerall_pmap$car == "D"], na.rm = TRUE)

# Create a Dataframe to store final results
Summary_Power <- data.frame(
  Car_Type = c("A", "B", "C", "D"),
  Mean_Power = c(A_mean, B_mean, C_mean, D_mean),
  Max_Power = c(A_max, B_max, C_max, D_max)
)

# Display results
Summary_Power
