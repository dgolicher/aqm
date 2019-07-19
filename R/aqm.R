dt<-function(x) DT::datatable(x, 
                              filter = "top",                         
                              extensions = c('Buttons'), options = list(
                                dom = 'Blfrtip',
                                buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                              ))