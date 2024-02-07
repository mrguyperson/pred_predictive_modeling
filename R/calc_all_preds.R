# a function to do the predator calculations

calc_all_preds = function(species, pred_parm, predator_predictions, variable_habitat, fish_length, habitat_parm, cover_fra_model, dis_to_cover_model){
  p_id <- which(pred_parm$species == species)
  hab_ratings <- predator_predictions %>%
    filter(pred_species == species) %>%
    select(hab_rating)

  # Get the pred and prey length
  pred_length = exp(pred_parm$pred_length_mean[[p_id]])
  prey_length = exp(pred_parm$gape_a[[p_id]] +
                      pred_parm$gape_b[[p_id]] * pred_length^2)
  # Check if it is vulnerable to predation
  length_pred_bonous = fifelse(fish_length >= prey_length, 0, 1)
  
  pred_habitat <- bind_cols(variable_habitat, hab_ratings)  %>% 
    group_by(date) %>% 
    mutate(# Place predators
      predators = replace_na(round((hab_rating * wetted_area) /
                                     sum(hab_rating * wetted_area) *
                                     reach_preds), 0)) %>% 
    ungroup() %>% 
    mutate(# Calculate the temp effect 
      temp_effect = 1 / (1 + exp(-(temp* pred_parm$area_pred_b[[p_id]] +
                                     pred_parm$area_pred_a[[p_id]]))),
      # Calculate the fraction of area the predators occupy
      porp_area = pmin(predators * habitat_parm$reaction_distance^2 *
                         pi * temp_effect / (wetted_area), predators),
      # calculate the distance to cover
      dis_to_cover_m = pmax(sqrt(area) * predict(cover_fra_model, newdata = data.frame(pct_cover = cover_fra)), 0),
      # Calculate the survival bonuses 
      survival_bonus = 1/(1+exp(-(dis_to_cover_model$coefficients[1] +
                                    dis_to_cover_model$coefficients[2] * dis_to_cover_m))),
      turb_bonus = 1 / (1 + exp(-1 *(habitat_parm$turbidity_int +
                                       turb * habitat_parm$turbidity_slope))),
      survival_prob = 1 - habitat_parm$pred_success +
        (habitat_parm$pred_success * (1 - (1 - survival_bonus) * (1 - turb_bonus))),
      # Calculate the predation risk and include the length bonus
      pred_mort_risk = fifelse(porp_area > 1,
                               1 - (survival_prob ^ porp_area),
                               porp_area * (1 - survival_prob)) * length_pred_bonous) %>% 
    # Remove temporary columns
    select(hab_rating, pred_mort_risk) %>% 
    rename_with( ~ paste0(.x, paste0("_" , p_id)))
}
