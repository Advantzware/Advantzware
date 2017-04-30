/* custom/resizmx.i  for non container widgets  - no folder resize */
    
  ASSIGN winstate = 1 NO-ERROR.
  ASSIGN hcol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.
            

  FIND FIRST bf_size WHERE bf_size.wg_name = STRING(CURRENT-WINDOW) NO-ERROR.
  IF AVAIL bf_size THEN DO:           
      
       FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
       IF NOT AVAIL tt_size THEN DO:
           CREATE tt_size.
           assign tt_size.wg_name = STRING(hcol)
                  tt_size.wg_width = hcol:WIDTH-PIXELS
                  tt_size.wg_height = hcol:height-PIXELS
                  tt_size.wg_xpos = hcol:X
                  tt_size.wg_ypos = hcol:Y NO-ERROR.
  
       END.
       ASSIGN hcol:HEIGHT-PIXELS = (hcol:HEIGHT-PIXELS * CURRENT-WINDOW:HEIGHT-PIXELS) 
                         / bf_size.wg_height
              hcol:width-PIXELS = (hcol:width-PIXELS * CURRENT-WINDOW:WIDTH-PIXELS) 
                         / bf_size.wg_width .


       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       assign hcol = hcol:FIRST-CHILD NO-ERROR. 
       DO WHILE VALID-HANDLE(hcol):
          FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
          IF NOT AVAIL tt_size THEN DO:
             CREATE tt_size.
             assign tt_size.wg_name = STRING(hcol)
                  tt_size.wg_width = hcol:WIDTH-PIXELS
                  tt_size.wg_height = hcol:height-PIXELS
                  tt_size.wg_xpos = hcol:X
                  tt_size.wg_ypos = hcol:Y NO-ERROR.
          END.
          ASSIGN hcol:HEIGHT-PIXELS = (hcol:HEIGHT-PIXELS * CURRENT-WINDOW:HEIGHT-PIXELS) 
                             / bf_size.wg_height
                 hcol:width-PIXELS = (hcol:width-PIXELS * CURRENT-WINDOW:WIDTH-PIXELS) 
                             / bf_size.wg_width 
                 hcol:X = (hcol:X * CURRENT-WINDOW:WIDTH-PIXELS) / bf_size.wg_width
                 hcol:Y = (hcol:Y * CURRENT-WINDOW:height-PIXELS) / bf_size.wg_height NO-ERROR.
          ASSIGN hcol = hcol:NEXT-SIBLING NO-ERROR.
       END. /* do while */             
  END.

