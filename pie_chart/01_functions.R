### make bar plots

colorblind_friendly_colors = RColorBrewer::brewer.pal(3,"Set2")

make_bar = function(dat) {
                dat %>% as_tibble() %>% 
        # if 0: tacos
        # if 1: burritos
        # if 2: nachos
        mutate(Food = ifelse(value==0, 'Tacos', 'Burritos'),
               Food = ifelse(value==2, 'Nachos', Food)) %>%
        #group_by(value) %>%
        #summarize(count = n()) %>% 
        ggplot(aes(x = Food, fill = Food)) +
        geom_bar()  +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.text=element_text(size=24),
              legend.title=element_text(size=32),
              axis.text.x = element_text(size=18),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=24)) +
                scale_fill_manual(values = colorblind_friendly_colors)
        }

make_pie = function(dat){
        dat %>% as_tibble() %>%
                mutate(Food = ifelse(value==0, 'Tacos', 'Burritos'),
                       Food = ifelse(value==2, 'Nachos', Food)) %>%
                group_by(Food) %>%
                summarize(value = n()) %>%
                ggplot(aes(x="", y=value, fill=Food)) +
                geom_bar(width = 1, stat = "identity") +
                coord_polar("y", start=0) +
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid  = element_blank(),
                      axis.title.y=element_blank(),
                      axis.text.y =element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.title.x=element_blank(),
                      legend.text=element_text(size=24),
                      legend.title=element_text(size=32)) +
                scale_fill_manual(values = colorblind_friendly_colors)
}