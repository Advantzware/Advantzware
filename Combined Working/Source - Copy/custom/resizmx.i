/* custom/resizmx.i*/
ON 'window-maximized':U OF {&WINDOW-NAME} /*CURRENT-WINDOW */
DO:

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
                         / bf_size.wg_width
              hcol:X = (hcol:X * CURRENT-WINDOW:WIDTH-PIXELS) / bf_size.wg_width
              hcol:Y = (hcol:Y * CURRENT-WINDOW:height-PIXELS) / bf_size.wg_height NO-ERROR.      
       
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
       
       RUN set-size IN h_folder ( FRAME {&frame-name}:HEIGHT ,FRAME {&frame-name}:WIDTH ) NO-ERROR.
       RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-target",OUTPUT charsiz-hdl).            
       DO ii = 1 TO NUM-ENTRIES(charsiz-hdl):
                 RUN max-widget IN WIDGET-HANDLE(ENTRY(ii,charsiz-hdl)) NO-ERROR.
       END.

      
  END.
END.
