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
        
        DEFINE VARIABLE cSmartObjList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCnt          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hTempHand     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE deRowPos      AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deColPos      AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deWidth       AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deDelta       AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deHeight      AS DECIMAL   NO-UNDO.  
        
        IF deOrigWinWidth > {&WINDOW-NAME}:WIDTH THEN
            {&WINDOW-NAME}:WIDTH = deOrigWinWidth.
          
        deDelta =  {&WINDOW-NAME}:WIDTH - FRAME {&FRAME-NAME}:WIDTH.
        
        FRAME {&FRAME-NAME}:WIDTH                 = {&WINDOW-NAME}:WIDTH.
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS.
        
        hTempHand = FRAME {&FRAME-NAME}:handle.
        hTempHand = hTempHand:FIRST-CHILD .
        IF hTempHand:TYPE = "FIELD-GROUP" THEN  
            hTempHand  = hTempHand:FIRST-CHILD.
            
        REPEAT WHILE VALID-HANDLE(hTempHand):
            IF hTempHand:TYPE = "frame" AND 
               hTempHand:NAME = "OPTIONS-FRAME"  THEN
                ASSIGN
                    hTempHand:WIDTH                 = {&WINDOW-NAME}:WIDTH
                    hTempHand:VIRTUAL-HEIGHT-PIXELS = hTempHand:HEIGHT-PIXELS
                    hTempHand:VIRTUAL-WIDTH-PIXELS  = hTempHand:WIDTH-PIXELS 
                    no-error.
            hTempHand = hTempHand:NEXT-SIBLING.
        END.
           
        RUN getlinktable IN adm-broker-hdl(
            INPUT THIS-PROCEDURE:UNIQUE-ID, 
            OUTPUT cSmartObjList
            ).
            
        DO iCnt = 1 TO NUM-ENTRIES(cSmartObjList,","): 
                   
            &IF DEFINED (h_Object01) <> 0 &THEN
                IF STRING({&h_Object01}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object02) <> 0 &THEN
                IF STRING({&h_Object02}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object03) <> 0 &THEN
                IF STRING({&h_Object03}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object04) <> 0 &THEN
                IF STRING({&h_Object04}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object05) <> 0 &THEN
                IF STRING({&h_Object05}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object06) <> 0 &THEN
                IF STRING({&h_Object06}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object07) <> 0 &THEN
                IF STRING({&h_Object07}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object08) <> 0 &THEN
                IF STRING({&h_Object08}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object09) <> 0 &THEN
                IF STRING({&h_Object09}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object10) <> 0 &THEN
                IF STRING({&h_Object05}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object11) <> 0 &THEN
                IF STRING({&h_Object11}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object12) <> 0 &THEN
                IF STRING({&h_Object12}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object13) <> 0 &THEN
                IF STRING({&h_Object13}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object14) <> 0 &THEN
                IF STRING({&h_Object14}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object15) <> 0 &THEN
                IF STRING({&h_Object15}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object16) <> 0 &THEN
                IF STRING({&h_Object16}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object17) <> 0 &THEN
                IF STRING({&h_Object17}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object18) <> 0 &THEN
                IF STRING({&h_Object18}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object19) <> 0 &THEN
                IF STRING({&h_Object19}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object20) <> 0 &THEN
                IF STRING({&h_Object20}) = entry(iCnt,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF     
    
            hTempHand = HANDLE(ENTRY(iCnt,cSmartObjList,",")).
      
            IF VALID-HANDLE(hTempHand) AND
               (LOOKUP( "nav-browse-identIFier", hTempHand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "panel-identifier",      hTempHand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "viewer-identifier",     hTempHand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "count-buttons",         hTempHand:INTERNAL-ENTRIES, ",")  > 0 OR
                THIS-PROCEDURE:FILE-NAME =  hTempHand:NAME                             OR
                hTempHand:NAME = "smartobj/smartmsg.w")
                THEN NEXT.   
                
            IF INDEX(hTempHand:INTERNAL-ENTRIES, "folder")  > 0   THEN
            DO:
                RUN get-size IN hTempHand (
                    OUTPUT deHeight ,
                    OUTPUT deWidth 
                    ) NO-ERROR. 
                RUN set-size IN hTempHand ( 
                    INPUT deHeight , 
                    INPUT {&WINDOW-NAME}:WIDTH 
                    ) NO-ERROR.
            END.
            ELSE IF LOOKUP( "browse-identifier",hTempHand:INTERNAL-ENTRIES, ",") > 0 THEN
            DO:              
                RUN get-size IN hTempHand (
                    OUTPUT deHeight ,
                    OUTPUT deWidth   
                    ) NO-ERROR.
                &IF DEFINED(h_Browse01) NE 0 &THEN
                    IF VALID-HANDLE(HANDLE(STRING({&h_Browse01}))) THEN 
                        RUN winReSize IN HANDLE(STRING({&h_Browse01})) (0,deDelta) NO-ERROR.
                &ENDIF
                &IF DEFINED(h_Browse02) NE 0 &THEN
                    IF VALID-HANDLE(HANDLE(STRING({&h_Browse02}))) THEN 
                        RUN winReSize IN HANDLE(STRING({&h_Browse02})) (0,deDelta) NO-ERROR.
                &ENDIF
                &IF DEFINED(h_Browse03) NE 0 &THEN
                    IF VALID-HANDLE(HANDLE(STRING({&h_Browse03}))) THEN 
                        RUN winReSize IN HANDLE(STRING({&h_Browse03})) (0,deDelta) NO-ERROR.
                &ENDIF
                                
              /*  RUN set-size IN hTempHand ( 
                    INPUT deHeight , 
                    INPUT deWidth + deDelta 
                    ) NO-ERROR.  */              
            END.
            ELSE 
            DO:            
                RUN get-position IN hTempHand ( 
                    OUTPUT deRowPos ,
                    OUTPUT deColPos
                    ) NO-ERROR. 
                RUN set-position IN hTempHand ( 
                    INPUT deRowPos , 
                    INPUT deColPos + deDelta 
                    ) NO-ERROR. 
            END.   
        END.  
    END. 


