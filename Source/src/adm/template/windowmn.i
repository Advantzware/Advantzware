/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

DEFINE VARIABLE temphand     AS HANDLE    NO-UNDO.
DEFINE VARIABLE resizeval    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE smartObjList AS CHARACTER NO-UNDO.
DEFINE VARIABLE icnt         AS INTEGER   NO-UNDO.
DEFINE VARIABLE deRowPos     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deColPos     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deWidth      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deHeight     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tempcolpos   AS DECIMAL   NO-UNDO.
DEFINE TEMP-TABLE toreposition 
 FIELDS widhand AS CHARACTER 
 FIELDS colpos AS DECIMAL
 FIELDS rowpos AS DECIMAL  
 FIELD widwidth AS DECIMAL.

IF VALID-HANDLE({&WINDOW-NAME}) THEN DO:
    ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       {&WINDOW-NAME}:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

    temphand  = FRAME {&FRAME-NAME}:handle.
    temphand  = temphand:FIRST-CHILD .       
    IF temphand:TYPE = "FIELD-GROUP" THEN  
        temphand  = temphand:FIRST-CHILD .
    REPEAT WHILE VALID-HANDLE(temphand):
        IF temphand:NAME = "OPTIONS-FRAME"  THEN
        ASSIGN
            temphand:ROW = 1
            temphand:COLUMN = 1
            temphand:BGCOLOR = 21
            temphand:width = {&WINDOW-NAME}:width
            temphand:VIRTUAL-HEIGHT-PIXELS = temphand:HEIGHT-PIXELS
            temphand:VIRTUAL-WIDTH-PIXELS  = temphand:WIDTH-PIXELS. 
        temphand = temphand:NEXT-SIBLING.
    END.
    
     
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
 
    /* Now enable the interface and wait for the exit condition.            */
       RUN dispatch ('initialize':U).
       
       RUN getlinktable IN adm-broker-hdl(INPUT THIS-PROCEDURE:UNIQUE-ID, OUTPUT smartObjList).

        DO icnt = 1 TO NUM-ENTRIES(smartObjList,","): 
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
        
            temphand = HANDLE(ENTRY(icnt,smartObjList,",")).
    
            IF LOOKUP( "nav-browse-identIFier", temphand:INTERNAL-ENTRIES, ",")   > 0 OR
                LOOKUP( "browse-identifier",     temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "panel-identifier",      temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "viewer-identifier",     temphand:INTERNAL-ENTRIES, ",")  > 0 OR
                LOOKUP( "count-buttons",     temphand:INTERNAL-ENTRIES, ",")      > 0     OR
            INDEX(temphand:INTERNAL-ENTRIES, "folder")  > 0 OR
                THIS-PROCEDURE:FILE-NAME =  temphand:NAME                             OR
                temphand:NAME = "smartobj/smartmsg.w"
                THEN NEXT.   
            
            RUN get-position IN temphand ( OUTPUT deRowPos ,OUTPUT deColPos) NO-ERROR.
            RUN get-size IN temphand (OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            
            CREATE toreposition.
            ASSIGN 
                toreposition.widhand   = ENTRY(icnt,smartObjList,",")
                toreposition.colpos    = deColPos
                toreposition.rowpos = deRowPos
                toreposition.widwidth  = deWidth.

        END.  
        
       resizeval = {&WINDOW-NAME}:WIDTH.
       FOR EACH toreposition BY toreposition.colpos DESCENDING :
           IF tempcolpos <> toreposition.colpos THEN 
            resizeval = resizeval -  (toreposition.widwidth + 2 ).
           RUN set-position IN WIDGET-HANDLE(toreposition.widhand) ( toreposition.rowpos , resizeval ) NO-ERROR. 
           tempcolpos = toreposition.colpos.
       END.
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
END.
&ENDIF
END.


