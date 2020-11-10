/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

DEFINE VARIABLE hTemp           AS HANDLE    NO-UNDO.
DEFINE VARIABLE deResizeVal     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cSmartObjList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCntWidHand            AS INTEGER   NO-UNDO.
DEFINE VARIABLE deRowPos        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deColPos        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deWidth         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deHeight        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deTempColPos    AS DECIMAL   NO-UNDO.


DEFINE TEMP-TABLE toreposition 
    FIELDS widhand  AS CHARACTER 
    FIELDS colpos   AS DECIMAL
    FIELDS rowpos   AS DECIMAL  
    FIELDS widwidth AS DECIMAL.

IF VALID-HANDLE({&WINDOW-NAME}) THEN DO:
    ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       {&WINDOW-NAME}:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
    deOrigWinWidth = {&WINDOW-NAME}:WIDTH.
    hTemp  = FRAME {&FRAME-NAME}:handle.
    IF VALID-HANDLE(hTemp) THEN
        hTemp  = hTemp:FIRST-CHILD .       
    IF hTemp:TYPE = "FIELD-GROUP" THEN  
        hTemp  = hTemp:FIRST-CHILD .
    REPEAT WHILE VALID-HANDLE(hTemp):
        IF hTemp:NAME = "OPTIONS-FRAME"  THEN
        ASSIGN
            hTemp:ROW                   = 1
            hTemp:COLUMN                = 1
            hTemp:BGCOLOR               = 21
            hTemp:WIDTH                 = {&WINDOW-NAME}:width
            hTemp:VIRTUAL-HEIGHT-PIXELS = hTemp:HEIGHT-PIXELS
            hTemp:VIRTUAL-WIDTH-PIXELS  = hTemp:WIDTH-PIXELS
            . 
        hTemp = hTemp:NEXT-SIBLING.
    END.
    
    hTemp = ?.
     
    /* The CLOSE event can be used from inside or outside the procedure to  */
    /* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE 
       RUN dispatch IN THIS-PROCEDURE ('destroy':U).

    RUN dispatch ('create-objects':U).

/* Execute this code only if not being run PERSISTENT, i.e., if in test mode
   of one kind or another or if this is a Main Window. Otherwise postpone 
   'initialize' until told to do so. */

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
&ENDIF
    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
       ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
       // MESSAGE 0 STRING(h_import) VIEW-AS ALERT-BOX.
    /* Now enable the interface and wait for the exit condition.            */
       RUN dispatch ('initialize':U).
       
       RUN getlinktable IN adm-broker-hdl(
           INPUT THIS-PROCEDURE:UNIQUE-ID, 
           OUTPUT cSmartObjList
           ).

        DO iCntWidHand = 1 TO NUM-ENTRIES(cSmartObjList,","): 
            
            &IF DEFINED (h_Object01) <> 0 &THEN
                IF STRING({&h_Object01}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object02) <> 0 &THEN
                IF STRING({&h_Object02}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object03) <> 0 &THEN
                IF STRING({&h_Object03}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object04) <> 0 &THEN
                IF STRING({&h_Object04}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object05) <> 0 &THEN
                IF STRING({&h_Object05}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object06) <> 0 &THEN
                IF STRING({&h_Object06}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object07) <> 0 &THEN
                IF STRING({&h_Object07}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object08) <> 0 &THEN
                IF STRING({&h_Object08}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object09) <> 0 &THEN
                IF STRING({&h_Object09}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object10) <> 0 &THEN
                IF STRING({&h_Object05}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object11) <> 0 &THEN
                IF STRING({&h_Object11}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object12) <> 0 &THEN
                IF STRING({&h_Object12}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object13) <> 0 &THEN
                 IF STRING({&h_Object13}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object14) <> 0 &THEN
                IF STRING({&h_Object14}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object15) <> 0 &THEN
                IF STRING({&h_Object15}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object16) <> 0 &THEN
                IF STRING({&h_Object16}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object17) <> 0 &THEN
                IF STRING({&h_Object17}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object18) <> 0 &THEN
                IF STRING({&h_Object18}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object19) <> 0 &THEN
                IF STRING({&h_Object19}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF
            &IF DEFINED (h_Object20) <> 0 &THEN
                IF STRING({&h_Object20}) = entry(iCntWidHand,cSmartObjList,",")  
                    THEN NEXT.
            &ENDIF          
    
            hTemp = HANDLE(ENTRY(iCntWidHand,cSmartObjList,",")).

            IF NOT VALID-HANDLE(hTemp) THEN 
                NEXT.
    
            IF LOOKUP( "nav-browse-identIFier", hTemp:INTERNAL-ENTRIES, ",")  > 0 OR
               LOOKUP( "browse-identifier",     hTemp:INTERNAL-ENTRIES, ",")  > 0 OR
               LOOKUP( "panel-identifier",      hTemp:INTERNAL-ENTRIES, ",")  > 0 OR
               LOOKUP( "viewer-identifier",     hTemp:INTERNAL-ENTRIES, ",")  > 0 OR
               LOOKUP( "count-buttons",         hTemp:INTERNAL-ENTRIES, ",")  > 0 OR
               INDEX(hTemp:INTERNAL-ENTRIES, "folder")                        > 0 OR
               THIS-PROCEDURE:FILE-NAME =  hTemp:NAME                             OR
               hTemp:NAME = "smartobj/smartmsg.w"
                THEN NEXT.   
            
            RUN get-position IN hTemp ( 
                OUTPUT deRowPos ,
                OUTPUT deColPos
                ) NO-ERROR.
            RUN get-size IN hTemp (
                OUTPUT deHeight , 
                OUTPUT deWidth 
                ) NO-ERROR. 
            
            CREATE toreposition.
            ASSIGN 
                toreposition.widhand   = ENTRY(iCntWidHand,cSmartObjList,",")
                toreposition.colpos    = deColPos
                toreposition.rowpos    = deRowPos
                toreposition.widwidth  = deWidth
                .
        END.  
        IF VALID-HANDLE({&WINDOW-NAME}) THEN 
            deResizeVal = {&WINDOW-NAME}:WIDTH.
       
        FOR EACH toreposition BY toreposition.colpos DESCENDING :
            IF deTempColPos NE toreposition.colpos THEN 
                deResizeVal = deResizeVal -  (toreposition.widwidth + 2 ).
            RUN set-position IN WIDGET-HANDLE(toreposition.widhand) ( 
                INPUT toreposition.rowpos , 
                INPUT deResizeVal 
                ) NO-ERROR. 
            deTempColPos = toreposition.colpos.
        END.
       
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
END.
&ENDIF
END.


