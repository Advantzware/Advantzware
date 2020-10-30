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



ON 'WINDOW-RESIZED':U OF {&WINDOW-NAME}
    DO:
        DEFINE VARIABLE smartObjList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE icnt         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE temphand     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE deRowPos     AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deColPos     AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deWidth      AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE delta        AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deHeight     AS DECIMAL   NO-UNDO.   
            
        delta =  {&WINDOW-NAME}:WIDTH - FRAME {&FRAME-NAME}:WIDTH.
        FRAME {&FRAME-NAME}:WIDTH = {&WINDOW-NAME}:WIDTH.
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS.
        temphand = FRAME {&FRAME-NAME}:handle.
        temphand  = temphand:FIRST-CHILD .
        
        IF temphand:TYPE = "Field-group" THEN  
            temphand  = temphand:FIRST-CHILD .
            
        REPEAT WHILE VALID-HANDLE(temphand):
            IF temphand:TYPE = "frame"  AND temphand:NAME = "OPTIONS-FRAME" THEN
                ASSIGN
                    temphand:WIDTH                 = {&WINDOW-NAME}:WIDTH
                    temphand:VIRTUAL-HEIGHT-PIXELS = temphand:HEIGHT-PIXELS
                    temphand:VIRTUAL-WIDTH-PIXELS  = temphand:WIDTH-PIXELS no-error.
            temphand = temphand:NEXT-SIBLING.
        END.
           
        RUN getlinktable IN adm-broker-hdl(INPUT THIS-PROCEDURE:UNIQUE-ID, OUTPUT smartObjList).
        DO icnt = 1 TO NUM-ENTRIES(smartObjList,","): 
            temphand = HANDLE(ENTRY(icnt,smartObjList,",")).
            &IF DEFINED (h_Object01) <> 0 &THEN
            IF STRING({&h_Object01}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object02) <> 0 &THEN
            IF STRING({&h_Object02}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object03) <> 0 &THEN
            IF STRING({&h_Object03}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object04) <> 0 &THEN
            IF STRING({&h_Object04}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object05) <> 0 &THEN
            IF STRING({&h_Object05}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object06) <> 0 &THEN
            IF STRING({&h_Object06}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object07) <> 0 &THEN
            IF STRING({&h_Object07}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object08) <> 0 &THEN
            IF STRING({&h_Object08}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object09) <> 0 &THEN
            IF STRING({&h_Object09}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object10) <> 0 &THEN
            IF STRING({&h_Object05}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object11) <> 0 &THEN
            IF STRING({&h_Object11}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object12) <> 0 &THEN
            IF STRING({&h_Object12}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object13) <> 0 &THEN
            IF STRING({&h_Object13}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object14) <> 0 &THEN
            IF STRING({&h_Object14}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object15) <> 0 &THEN
            IF STRING({&h_Object15}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object16) <> 0 &THEN
            IF STRING({&h_Object16}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object17) <> 0 &THEN
            IF STRING({&h_Object17}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object18) <> 0 &THEN
            IF STRING({&h_Object18}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object19) <> 0 &THEN
            IF STRING({&h_Object19}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object20) <> 0 &THEN
            IF STRING({&h_Object20}) = entry(icnt,smartObjList,",")  
                THEN NEXT.
            &ENDIF
            
            
            
    
            IF VALID-HANDLE(temphand) AND
               (LOOKUP( "nav-browse-identIFier", temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "browse-identifier",     temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "panel-identifier",      temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "viewer-identifier",     temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "count-buttons",         temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                THIS-PROCEDURE:FILE-NAME =  temphand:NAME                             OR
                temphand:NAME = "smartobj/smartmsg.w")
                THEN NEXT.   
            IF INDEX(temphand:INTERNAL-ENTRIES, "folder")  > 0   THEN
            DO:
                RUN get-size IN temphand (OUTPUT deHeight ,OUTPUT deWidth ) NO-ERROR. 
                RUN set-size IN temphand ( deHeight , {&WINDOW-NAME}:WIDTH ) NO-ERROR.
            END.
            ELSE IF LOOKUP( "browse-identifier",     temphand:INTERNAL-ENTRIES, ",")  > 0 THEN
            DO:
                RUN get-size IN temphand (OUTPUT deHeight ,OUTPUT deWidth ) NO-ERROR. 
                RUN set-size IN temphand ( deHeight , deWidth + delta ) NO-ERROR.
            END.
            ELSE 
            DO:
                RUN get-position IN temphand ( OUTPUT deRowPos ,OUTPUT deColPos) NO-ERROR.         
                RUN get-size IN temphand (OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            //    resizeval = resizeval -  (deWidth + 2 ).
                RUN set-position IN temphand ( deRowPos , deColPos + delta ) NO-ERROR. 
            END.   

        END.  
    END. 


