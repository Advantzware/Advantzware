/* clearFilterValues.i */

    ASSIGN 
        begin_cust-no = ""
        begin_ship    = ""
        vi_part-no    = ""
        vi_part-dscr1 = ""
        vi_stock-no   = ""
        vi_style      = ""
        vi_die-no     = ""
        vi_plate-no   = ""
        vi_plate-no   = ""
        vi_len        = 0
        vi_len-2      = 0
        vi_wid        = 0
        vi_wid-2      = 0
        vi_dep        = 0
        vi_dep-2      = 0
        vi_est-date   = ?
        .
    DISPLAY 
        begin_cust-no
        begin_ship
        vi_part-no
        vi_part-dscr1
        vi_stock-no
        vi_style
        vi_die-no
        vi_plate-no
        vi_plate-no
        vi_len
        vi_len-2
        vi_wid
        vi_wid-2
        vi_dep
        vi_dep-2
        vi_est-date
            WITH FRAME {&FRAME-NAME}.
