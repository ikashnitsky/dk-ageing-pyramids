

# # This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, load the Rdata into a new environment to avoid side effects
# LoadToEnvironment <- function(RData, env=new.env()) {
#     load(RData, env)
#     return(env)
# }

# plot function
draw_pyramid <- function(proj_year = 2070, cut_off_age = 65){
    
    library(tidyverse)
    library(magrittr)
    
    Age = 0:110
    H = proj_year %>% subtract(2016)
    xpos = 105 - 1:3 * (110 - cut_off_age)*.07
    
    
    gg <- ggplot() +
        
        geom_rect(aes(xmin = 110, xmax = Inf, ymin = -Inf, ymax = Inf), color = NA, fill = "white")+
        geom_rect(aes(xmin = 110 + (110 - cut_off_age)*0.17, xmax = Inf, ymin = -Inf, ymax = Inf), color = NA, fill = "#35978f")+
        
        geom_bar(aes(x = Age, y = pop.OVcu$projF[,H]), stat = "identity", fill="darkgoldenrod1",  width = 1) +
        geom_bar(aes(x = Age, y = -pop.OVcu$projM[,H]), stat = "identity", fill="darkgoldenrod1", width = 1) +
        
        geom_bar(aes(x = Age, y = pop.OV2$projF[,H]), stat = "identity", fill="orangered", width = 1) +
        geom_bar(aes(x = Age, y = -pop.OV2$projM[,H]), stat = "identity", fill="orangered", width = 1) +
        
        geom_bar(aes(x = Age, y = pop.Dream$projF[,H]), stat = "identity", fill="darkorchid4", width = 1) +
        geom_bar(aes(x = Age, y = -pop.Dream$projM[,H]), stat = "identity", fill="darkorchid4",  width = 1) +
        
        geom_bar(aes(x = Age, y = pop.Cons$projF[,H]), stat = "identity", fill="midnightblue",  width = 1) +
        geom_bar(aes(x = Age, y = -pop.Cons$projM[,H]), stat = "identity", fill="midnightblue", width = 1) +
        
        geom_segment(aes(y = 0, yend = 0, x = 0, xend = 111), color = "white", size = .1)+
        
        scale_x_continuous(breaks = seq(0, 110, 10), labels = seq(0, 110, 10))+
        scale_y_continuous(breaks = seq(-30e3, 30e3, 10e3), 
                           labels = seq(-30, 30, 10) %>% abs %>% paste0(., "K"))+
        
        coord_flip(
            xlim = c(cut_off_age, 110 + (110 - cut_off_age)*.32), 
            ylim = c(-max(pop.Cons$projM), max(pop.Dream$projM)),
            expand = F
        )+
        
        # annotate MALES
        geom_text(
            aes(
                y = -max(pop.Cons$projM) + 3e3,
                x = 105,
                label = "MALES"
            ),
            col = "gray25", size = 7, 
            family = font_rc, fontface = 2, hjust = 0, vjust = 1
        ) +
        geom_text(
            aes(
                y = -max(pop.Cons$projM) + 3000,
                x = xpos[1],
                label = (popO65.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "darkgoldenrod1", size = 6, 
            family = font_rc, fontface = 2, hjust = 0, vjust = 1
        ) +
        
        geom_text(
            aes(
                y = -max(pop.Cons$projM) + 3000,
                x = xpos[2],
                label = (pop265.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "orangered", size = 6, 
            family = font_rc, fontface = 2, hjust = 0, vjust = 1
        ) +
        
        geom_text(
            aes(
                y = -max(pop.Cons$projM) + 3000,
                x = xpos[3],
                label = (popC65.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "midnightblue", size = 6, 
            family = font_rc, fontface = 2, hjust = 0, vjust = 1
        )+
        
        # annotate FEMALES
        geom_text(
            aes(
                y = max(pop.Cons$projM) - 3e3,
                x = 105,
                label = "FEMALES"
            ),
            col = "gray25", size = 7, 
            family = font_rc, fontface = 2, hjust = 1, vjust = 1
        ) +
        geom_text(
            aes(
                y = max(pop.Cons$projM) - 3e3,
                x = xpos[1],
                label = (popO65.f - popD65.f)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "darkgoldenrod1", size = 6, 
            family = font_rc, fontface = 2, hjust = 1, vjust = 1
        ) +
        
        geom_text(
            aes(
                y = max(pop.Cons$projM) - 3e3,
                x = xpos[2],
                label = (pop265.f - popD65.f)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "orangered", size = 6, 
            family = font_rc, fontface = 2, hjust = 1, vjust = 1
        ) +
        
        geom_text(
            aes(
                y = max(pop.Cons$projM) - 3e3,
                x = xpos[3],
                label = (popC65.f - popD65.f)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0(., "K")
            ),
            col = "midnightblue", size = 6, 
            family = font_rc, fontface = 2, hjust = 1, vjust = 1
        )+
        
        # annotate TOTAL
        geom_text(
            aes(
                y = 0,
                x = 110 + (110 - cut_off_age)*0.25,
                label = paste0("Denmark, ", 2016+H, ", Population 65+\ndifference from DREAM projection")
            ),
            col = "gray95", size = 8, 
            family = font_rc, fontface = 2, lineheight = .9
        ) +
        geom_text(
            aes(
                y = max(pop.Cons$projM)*2/3,
                x = 110 + (110 - cut_off_age)*0.15,
                label = (popO65.f - popD65.f + popO65.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0("Oeppen-Vaupel Catch Up\n", ., "K")
            ),
            col = "darkgoldenrod1", size = 6, 
            family = font_rc, fontface = 2, hjust = 0.5, vjust = 1, lineheight = .9
        ) +
        
        geom_text(
            aes(
                y = 0,
                x = 110 + (110 - cut_off_age)*0.15,
                label = (pop265.f - popD65.f + pop265.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0("Oeppen-Vaupel 2.2\n", ., "K")
            ),
            col = "orangered", size = 6, 
            family = font_rc, fontface = 2, hjust = 0.5, vjust = 1, lineheight = .9
        ) +
        
        geom_text(
            aes(
                y = -max(pop.Cons$projM)*2/3,
                x = 110 + (110 - cut_off_age)*0.15,
                label = (popC65.f - popD65.f + popC65.m - popD65.m)[H] %>% 
                    divide_by(1000) %>% round(1) %>% 
                    paste0("Zero change\n", ., "K")
            ),
            col = "midnightblue", size = 7, 
            family = font_rc, fontface = 2, hjust = 0.5, vjust = 1, lineheight = .9
        )+
        
        # theme
        labs(
            x = "Age", y = "Population"
        )+
        
        theme_bw(base_family = font_rc, base_size = 16)+
        theme(panel.grid.minor.x = element_blank())
    
    return(gg)
}
