/* custom/resizrs2.i  for non container widgets  - no folder */
  
IF winstate = 3 THEN ASSIGN winstate = 0 NO-ERROR.
ELSE DO:
       IF winstate = 1 THEN DO:
           ASSIGN winstate = 0 NO-ERROR.
       END.
       ASSIGN hcol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.
       FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
       IF AVAIL tt_size THEN DO:
           ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
               /*   hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos */ NO-ERROR.
             
       END.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       DO WHILE VALID-HANDLE(hcol):
          FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
          IF AVAIL tt_size THEN DO:
             ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos  NO-ERROR.
             
          END.
          ASSIGN hcol = hcol:NEXT-SIBLING NO-ERROR.
       END.
     

END.

