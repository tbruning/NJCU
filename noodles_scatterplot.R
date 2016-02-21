require(dplyr)
require(tidyr)
require(ggvis)
dt <- read.csv("noodles.csv")
dt %>% ggvis(~Salads, ~RiceKrispies) %>% 
    layer_points() %>% 
    layer_model_predictions(model = "lm",
                            se = TRUE)