library(hector)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Read in v2.5 data
v25 <- as.data.frame(read.csv("C:\\Users\\pres520\\Desktop\\Hector\\version25_output.csv"))

# Remove first column of numbers (1, 2, 3...)
v25 <- v25 %>% select(scenario, year, variable, value, units)
v25 <- v25 %>% mutate(version = "v2.5.0")

# Read in files for four RCPs - v3
inputdir <- system.file("input", package = "hector")
rcp26 <- file.path(inputdir, "hector_rcp26.ini")
rcp45 <- file.path(inputdir, "hector_rcp45.ini")
rcp60 <- file.path(inputdir, "hector_rcp60.ini")
rcp85 <- file.path(inputdir, "hector_rcp85.ini")

# Variables of interest
vars <- c(ATMOSPHERIC_C(), GLOBAL_TEMP(), RF_TOTAL())
rcps <- c(rcp26, rcp45, rcp60, rcp85)
scenarios <- c("rcp26", "rcp45", "rcp60", "rcp85")

run_core <- function(scenario, vars) {
    ini <- file.path(inputdir, paste0("hector_", scenario, ".ini"))
    core <- newcore(ini)
    run(core)
    sd <- core$strtdate
    ed <- core$enddate
    fetchvars(core, sd:ed, vars, scenario)
}

v3 <- lapply(scenarios, run_core, vars)
v3 <- bind_rows(v3)
v3 <- v3 %>% mutate(version = "v3.0.0")

# Find differences between versions
differences <- v25 %>% mutate(diff = v3$value - v25$value)
differences <- differences %>% select(scenario, year, variable, units, version, diff)

diff_plot <- ggplot(differences, aes(year, diff, color = variable)) +
    geom_line() +
    facet_grid(variable~scenario, scales = "free") +
    theme(axis.text = element_text(size = 7)) +
    scale_color_manual(labels = c("atmos_c (Pg C)", "Ftot (W/m2)", "Tgav (degC)"), values = c("plum4", "lightpink3", "goldenrod")) +
    ggtitle("Differences in Hector outputs in v3.0.0 relative to v2.5.0") +
    labs(x = "Year", y = "Difference in value", col = "Variable")


# ggsave("Hector output differences.jpg", diff_plot, width = 10, height = 6)

# Plot two versions against each other

both_versions <- rbind(v25, v3)

versions_plot <- ggplot(both_versions, aes(year, value, color = version)) +
    geom_line() +
    facet_grid(variable~scenario, scales = "free") +
    theme(axis.text = element_text(size = 7)) +
    scale_color_manual(labels = c("v2.5.0", "v3.0.0"), values = c("black", "white")) +
    ggtitle("Hector version 2.5.0 against version 3.0.0") +
    labs(x = "Year", y = "Value", col = "Variable")

v25_plot <- ggplot(v25, aes(year, value, color = variable)) +
    geom_line() +
    facet_grid(variable~scenario, scales = "free") +
    theme(axis.text = element_text(size = 7)) +
    scale_color_manual(labels = c("atmos_c (Pg C)", "Ftot (W/m2)", "Tgav (degC)"),
                         values = c("plum4", "lightpink3", "goldenrod")) +
    ggtitle("Hector version 2.5.0 outputs") +
    labs(x = "Year", y = "Value", col = "Variable")

v3_plot <- ggplot(v3, aes(year, value, color = variable)) +
    geom_line() +
    facet_grid(variable~scenario, scales = "free") +
    theme(axis.text = element_text(size = 7)) +
    scale_color_manual(labels = c("atmos_c (Pg C)", "Ftot (W/m2)", "Tgav (degC)"),
                       values = c("aquamarine4", "darkblue", "darkseagreen4")) +
    ggtitle("Hector version 3.0.0 outputs") +
    labs(x = "Year", y = "Value", col = "Variable")

comp_plot <- grid.arrange(v25_plot, v3_plot)

all_plots <- grid.arrange(comp_plot, versions_plot, diff_plot)

version_diff_plot <- grid.arrange(versions_plot, diff_plot)
# ggsave("Hector output differences by version.jpg", version_diff_plot, height = 16, width = 16)

#######

test <- differences %>% mutate(fraction = differences$diff / v3$value)

test_plot <- ggplot(test, aes(year, fraction, color = variable)) +
    geom_point() +
    facet_grid(variable~scenario, scales = "free") +
    theme(axis.text = element_text(size = 7)) +
    scale_color_manual(labels = c("atmos_c (Pg C)", "Ftot (W/m2)", "Tgav (degC)"),
                       values = c("aquamarine4", "darkblue", "darkseagreen4")) +
    ggtitle("Percentage difference from v3.0.0") +
    labs(x = "Year", y = "% difference", col = "Variable") +
    scale_y_continuous(labels = scales::percent)

