

metrics<-function(file){

  file %>%
  mutate(r=row_number(.)) %>%
  arrange(r) %>%
  dplyr::select(sample,everything(),-r)  %>%
  mutate(ratio2=radius_max/radius_min,
         angle2=ifelse(angle>0,angle/(pi/360),360-(-angle)/(pi/360))) %>%
  filter(area<8000,
         area>5) %>%
  select(-center_z,-center_x,-center_y,-id) %>%
  pivot_longer(cols=2:length(.)) %>%
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~name,scales = "free")


}

