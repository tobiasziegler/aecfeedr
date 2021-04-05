# unpack_xml works

    Code
      unpack_xml(df, .data$pp_xml, unpack_formula)
    Output
      # A tibble: 4 x 4
        feed_id contest_name pp_id pp_name        
        <chr>   <chr>        <chr> <chr>          
      1 101     Contest 1    PP1   Polling Place 1
      2 101     Contest 1    PP2   Polling Place 2
      3 101     Contest 2    PP3   Polling Place 3
      4 101     Contest 2    PP4   Polling Place 4

