/* custom/resizmn.i */
ASSIGN CURRENT-WINDOW:MAX-WIDTH = SESSION:WIDTH-CHARS
       CURRENT-WINDOW:MAX-height = SESSION:HEIGHT-CHARS.
ASSIGN hcol = CURRENT-WINDOW NO-ERROR.
      CREATE tt_size.
      ASSIGN tt_size.wg_name = STRING(hcol)
             tt_size.wg_width = hcol:WIDTH-PIXELS
             tt_size.wg_height = hcol:HEIGHT-PIXELS
             tt_size.wg_xpos = hcol:X
             tt_size.wg_ypos = hcol:Y NO-ERROR.
