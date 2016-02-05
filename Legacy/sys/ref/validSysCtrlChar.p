DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSVName AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSVLabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSVLogFld AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcSVCharFld AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcNameFldList AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcValidList AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcEntryTo AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplResult AS LOGICAL     NO-UNDO.
  
  DEF VAR thisOne AS CHAR NO-UNDO.
  DEF VAR comp-char-val AS CHAR NO-UNDO.
  DEF VAR j AS INT NO-UNDO.  

  IF NOT (ipcSVName = "VendXfer" OR
     ipcSVName = "CustXfer" OR  ipcSVName = "ORDERXFER")
  THEN DO: 
      /* valid-char-fld.i */

    DEF VAR ls-name AS cha NO-UNDO.
    DEF VAR ls-name-value AS cha NO-UNDO.
    DEF VAR lv-msg AS CHAR NO-UNDO.
    DEF VAR i AS INT NO-UNDO.
    oplResult = YES.
    DO WITH FRAME F-Main:
      ls-name = ipcSVName.  
     
      IF CAN-DO(ipcNameFldList,ls-name) THEN DO:
        ls-name-value = ipcValidList /* str-init[LOOKUP(ls-name,ipcNameFldList)]*/.
         
        IF NOT CAN-DO(ls-name-value,ipcSVCharFld) THEN
          lv-msg = TRIM(ipcSVCharFld) + " is not on lookup".
      END.
      
      IF lv-msg NE "" AND ls-name EQ "FGWHSBIN" THEN DO:
        lv-msg = "".
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ ipcCompany
              AND fg-bin.i-no    EQ ""
              AND fg-bin.loc     EQ SUBSTR(ipcSVCharFld,1,5)
              AND fg-bin.loc-bin EQ SUBSTR(ipcSVCharFld,6)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL fg-bin THEN lv-msg = "FG Bin does not exist".
      END.
    
      IF lv-msg NE "" AND ls-name EQ "RMWHSBIN" THEN DO:
        lv-msg = "".
        IF ipcSVCharFld NE "RMITEM" THEN DO:
          FIND FIRST rm-bin
              WHERE rm-bin.company EQ ipcCompany
                AND rm-bin.loc     EQ SUBSTR(ipcSVCharFld,1,5)
                AND rm-bin.i-no    EQ ""
                AND rm-bin.loc-bin EQ SUBSTR(ipcSVCharFld,6)
              NO-LOCK NO-ERROR.
          IF NOT AVAIL rm-bin THEN lv-msg = "RM Bin does not exist".
        END.
      END.
    
      IF lv-msg EQ "" THEN
        IF ls-name EQ "TSPOST"                        AND
           ipcSVLogFld EQ "no"      AND
           ipcSVCharFld EQ "Actual" THEN
          ASSIGN
           ipcSVCharFld = "Standard"
           lv-msg = "Actual Direct Labor Rate Update may be done only from Touch Screen Data Collection".
    
      IF lv-msg = "" AND ls-name = "TSPOSTFG" THEN DO:
         DO i = 1 TO NUM-ENTRIES(ipcSVCharFld):
            FIND FIRST mach WHERE mach.company = ipcCompany AND mach.loc = ipcLoc
                              AND mach.m-code = ENTRY(i,ipcSVCharFld) 
                              NO-LOCK NO-ERROR.
            IF NOT AVAIL mach THEN 
               lv-msg = "Machine: " + ENTRY(i,ipcSVCharFld) .
            IF lv-msg <> "" THEN LEAVE.
         END.
      END.
    
      IF lv-msg EQ "" AND ls-name EQ "FGMASTER" AND
         ipcSVCharFld NE "FGITEM" AND
         NOT CAN-FIND(FIRST itemfg
                      WHERE itemfg.company EQ ipcCompany
                        AND itemfg.i-no    EQ ipcSVCharFld) THEN
        lv-msg = "FG Item# does not exist, try help...".
      
      IF lv-msg NE "" THEN DO:
        MESSAGE TRIM(ipcSVLabel) + " is not valid " +
                "(" + TRIM(lv-msg) + ")" +
                ", please try help..."
            VIEW-AS ALERT-BOX ERROR.
        /* APPLY "entry" TO sys-ctrl.char-fld. */
        opcEntryTo = "Char".
        oplResult = NO.
      END. 
    END.
 
  END.
      ELSE DO:
          
          ASSIGN comp-char-val = ipcSVCharFld.          

          DO J = 1 TO NUM-ENTRIES(comp-char-val):
              ASSIGN thisOne = ENTRY(j,comp-char-val).          
     
              FIND FIRST company WHERE company.company = thisOne NO-LOCK NO-ERROR.
              IF NOT AVAIL company THEN DO:         
                  MESSAGE   thisOne  "is not a valid company" VIEW-AS ALERT-BOX ERROR.
                  /* APPLY 'ENTRY':U TO sys-ctrl.char-fld. */
                  opcEntryTo = "Char".
                  oplResult = NO.
              END.
             
      END.
  END.


  IF ipcSvName EQ "SSRMISSUE" THEN DO:
      IF ipcSVCharFld = "ScanTagOnly" AND
          ipcSVLogFld = "yes" THEN DO:
          MESSAGE "Value 'ScanTagOnly' cannot be used with auto-post functionality (logical value YES)."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          /* APPLY 'ENTRY':U TO sys-ctrl.log-fld. */
          opcEntryTo = "Log".
          oplResult = NO.
      END.
  END.
