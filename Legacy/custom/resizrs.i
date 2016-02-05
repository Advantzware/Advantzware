/* custom/resizrs.i*/
ON 'window-restored':U OF {&WINDOW-NAME}
DO:

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
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos NO-ERROR.
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

       /*RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR. /*default tool kit*/
       RUN set-size IN h_folder ( 22.14 , 159.00 ) NO-ERROR. /*w-order.w*/
       RUN set-size IN h_folder ( 21.67 , 150.00 ) NO-ERROR. /* w-est.w*/
       */

       RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-target",OUTPUT charsiz-hdl).
       DO ii = 1 TO NUM-ENTRIES(charsiz-hdl):
          RUN restore-widget IN WIDGET-HANDLE(ENTRY(ii,charsiz-hdl)) NO-ERROR.
       END.
END.

END.
