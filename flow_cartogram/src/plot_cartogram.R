make_carto_grid <- function(){
  us_state_grid1 %>% 
    add_row(row = 7, col = 11, code = "PR", name = "Puerto Rico") %>% # add PR
    filter(code != "DC") # remove DC (only has 3 gages)
}
get_state_fips <- function(){
  maps::state.fips %>% 
    distinct(fips, abb) %>%
    add_row(fips = 02, abb = 'AK')%>%
    add_row(fips = 15, abb = 'HI')%>%
    add_row(fips = 72, abb = 'PR') %>%
    mutate(state_cd = str_pad(fips, 2, "left", pad = "0"))
}

theme_flowfacet <- function(base = 14, color_bknd){
  theme_classic(base_size = base) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 14, vjust = 1),
          strip.placement = "inside",
          strip.background.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 18, face = "bold"),
          plot.background = element_rect(fill = color_bknd,
                                         color = color_bknd),
          panel.background = element_rect(fill = color_bknd,
                                          color = color_bknd),
          panel.spacing = unit(0, "pt"),
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "pt"))
          
 }

plot_state_cartogram <- function(state_data, fips, pal, usa_grid, color_bknd){
  state_data %>% 
    left_join(fips) %>% # to bind to cartogram grid
    ggplot(aes(date, prop)) +
    with_shadow(
      geom_area(aes(fill = cond)),
      colour = "black",
      x_offset = 2,
      y_offset = 2,
      sigma = 3,
      stack = TRUE
    ) +
    scale_fill_manual(values = rev(pal)) +
    facet_geo(~abb, grid = usa_grid, move_axes = FALSE) +
    scale_y_continuous(trans = "reverse") +
    theme_flowfacet(base = 14, color_bknd) +
    theme(plot.margin = margin(50, 50, 50, 50, "pt"),
          panel.spacing.y = unit(0, "pt"),
          panel.spacing.x = unit(6, "pt"),
          strip.text = element_text(vjust = -1))+
    coord_fixed(ratio = 28)
}

plot_national_area <- function(national_data, date_start, date_end, pal, color_bknd){
  # to label flow categories
  sec_labels <- national_data  %>%
    filter(date == max(national_data$date)) %>%
    distinct(cond, prop) %>%
    mutate(prop = cumsum(prop))
  
  plot_nat <- national_data %>% 
    ggplot(aes(date, prop)) +
    geom_area(aes(fill = cond)) +
    theme_classic() +
    labs(x=month(date_end - 30, label = TRUE, abbr = FALSE),
         y="") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse",
                       breaks = rev(c(0.05,0.08, 0.15, 0.5, 0.75, 0.92, 0.95)), 
                       labels = c("0%","","","","", "","100%"),
                       sec.axis = dup_axis(
                         labels = sec_labels$cond
                       )) +
    theme_flowfacet(base = 14, color_bknd) +
    theme(axis.text.y = 
            element_text(size = 14, 
                         vjust = c(1, 0), # align yaxis labels to plot bounds
                         hjust = 1),
          axis.title.x.bottom = element_text(size = 26,
                                             vjust = 2),
          axis.title.x.top = element_text(size = 26,
                                          vjust = -1),
          axis.text.x.bottom = element_text(size = 14,
                                            vjust = 4)) +
    scale_x_date(breaks = seq.Date(date_start, date_end, "1 week"),
                 position = "bottom",
                 labels = day(seq.Date(date_start, date_end, "1 week")),
                 sec.axis = dup_axis(
                   name = "National"
                 )) +
    guides(fill = guide_legend("")) +
    coord_fixed(ratio = 28)
  
  return(plot_nat)
}

combine_plots <- function(file_out, plot_left, plot_right, width, height, color_bknd){
  
  # import fonts
  font_legend <- 'Cutive Mono'
  font_fam = 'Train One'
  font_add_google(font_legend) 
  font_add_google(font_fam) 
  showtext_opts(dpi = 300)
  showtext_auto(enable = TRUE)
  
  # logo
  usgs_logo <- magick::image_read('../logo/usgs_logo_white.png') %>%
    magick::image_colorize(100, "black")
  
  plot_margin <- 0.025
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 9, width = 16,
              hjust = 0, vjust = 1) +
    draw_plot(plot_left,
              x = plot_margin,
              y = 0.25,
              height = 0.5 ,
              width = 0.3-plot_margin) +
    draw_plot(plot_right,
              x = 0.32,
              y = 0+plot_margin,
              height = 1- plot_margin*2, 
              width = 1-(0.3-plot_margin)) +
    # draw title
   draw_label(sprintf('%s %s\nSTREAMFLOW', "April", "2022"),
              x = plot_margin, y = 1-plot_margin, 
              fontface = "bold", 
              size = 48, 
              hjust = 0, 
              vjust = 1,
              fontfamily = font_fam,
              lineheight = 1)  +
   # add logo
    draw_image(usgs_logo, x = plot_margin, y = plot_margin, width = 0.15, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  
  ggsave(file_out, width = width, height = height, dpi = 300)
  return(file_out)
  
}