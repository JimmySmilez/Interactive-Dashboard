# Interactive Dashboard: Kenyan Human Gut Virome

An interactive web-based dashboard developed using **R Shiny** to dynamically visualize viral clusters and taxonomy within the Kenyan human gut virome.

🔗 **Live Demo:** [Access the Dashboard here](https://igmr.org/software/kenyavirocat)  

## Dashboard Preview

![Kenyan Gut Virome Dashboard Landing Page](https://github.com/JimmySmilez/Interactive-Dashboard/blob/main/files/Screenshot%202026-03-16%20080602.png?raw=true)

*Figure 1: The interactive landing page showing viral cluster distributions and regional data.*

---

## Key Features
* **Taxonomic Classification:** Visualize the distribution of classified vs. unclassified viral clusters.
* **Regional Analysis:** Explore data across different Kenyan regions (Kilifi, Kwale, Nairobi).
* **Functional Insights:** Observe viral clusters per sample by regions.
* **Interactive Data Explorer:** Filter and search through raw datasets directly in the UI.

## Tech Stack
* **Language:** R
* **Framework:** Shiny, Shinydashboard
* **Visualizations:** ggplot2 and plotly 

## How to Run Locally
To run this dashboard on your own machine:

1. **Clone the repo:**
   ```bash
   git clone [https://github.com/JimmySmilez/Interactive-Dashboard.git](https://github.com/JimmySmilez/Interactive-Dashboard.git)

2. **Install Dependancies:**
   ```r
   install.packages(c("shiny", "shinydashboard", "plotly", "ggplot2", "dplyr", "readr"))
3. **Launch the Dashboard:**
   ```r
   shiny::runApp('dashboard/')

