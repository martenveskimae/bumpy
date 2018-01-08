# Bumpy
Pavement quality measured by Starship delivery robots. Visualization &amp; route optimization

## Authors
Märten Veskimäe, Kevin Kanarbik and Tõnis Kasekamp

Institute of Computer Science, University of Tartu

# Business problem
Most modern-day delivery services rely on automated route optimization algorithms, that estimate optimal delivery routes, given the number of delivery vehicles, payload, and time constraints. While these algorithms can become very complex, they often discount pavement quality as one of the important conditional variables. Pavement quality is an important factor in route optimization, since it affects delivery time, energy consumption, and maintenance costs. In case of fragile packages, it may also affect the condition of the package.
So far, measuring pavement quality in a dynamic manner has been too difficult to justify measurement costs. Luckily, it has become more feasible with modern technology. Given gyroscope, accelerometer and GPS input from delivery vehicles, such as Starship delivery robots, companies are now able to periodically measure pavement quality in an automated and relatively low-cost manner. This input can be used to enhance route optimization algorithms, save delivery time, lower maintenance costs, and improve client satisfaction.

# Data minging
## From JSON to Data Frame
The original JSON data was composed of 40GB of JSON files which had to be read in and processed line by line (rjson). The key element in this task was speed, which is why the algorithm relied on data.table and avoided pipes. The original data consisted of numerous measurements for each second, but was aggregated to a one-second level for further analysis.

## Analyzing pavement quality
For measuring pavement quality a custom formula was used, focusing on the up-down movement of the robot (vibration intensity). The formula used variation in the accelerometer and gyroscope z-axis for that purpose. The results were normalized for easier interpretation.

## Route optimization & visualization
In order to visualize pavement quality and perform route optimization, the raw GPS data from delivery robots had to be linked with Tallinn’s road network. For this OpenStreetMap data was used (osmar) and visualized via leaflet (mapview; leaflet). For route optimization, Road network’s nodes were weighed by the road quality indicator and transformed into a graph object, after which the shortest path was calculated (igraph). The final application was  implemented using shiny.
