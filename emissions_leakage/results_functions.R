# graphing functions
library(ggrepel)

plot_source_breakdown <- function(all_results, results_loc = results_loc){
 
  tot_co2 <- sum(all_results$co2eq)
   
  df <- all_results %>% select(source, co2eq) %>% 
    mutate(co2eq = round(100 *co2eq/tot_co2, digits =1 )) %>% 
    mutate(source = replace(source, source == "ent_ferm", "Enteric Fermentation"),
           source = replace(source, source == "fertilizer", "Fertilizer"),
           source = replace(source, source == "fuel", "Fuel"),
           source = replace(source, source == "manure", "Manure Deposition (CH4)"),
           source = replace(source, source == "n_fixing", "N-fixing species"),
           source = replace(source, source == "manure", "Urine and Dung (N20)"),
    ) %>%  arrange(co2eq) %>%    
    mutate(source=factor(source, levels=source))%>% 
    mutate(csum = rev(cumsum(rev(co2eq))), 
           pos = co2eq/2 + lead(csum, 1),
           pos = if_else(is.na(pos), co2eq/2, pos))  
  
  es_plot <- ggplot(data = df, aes( x ="", y = co2eq, fill = (source))) +
    geom_col()+
    coord_polar(theta = "y")+
    geom_label_repel(data = df,
                     aes(y = pos, label = paste0(co2eq, "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Emission Source"))+
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank())+
    scale_fill_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02"))
  
  ggsave(es_plot, filename = file.path(results_loc, "lca_emission_source.png"))
  
}
