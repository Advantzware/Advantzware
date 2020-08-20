/* dynFuncs.i - rstark - 1.25.2019 */

FUNCTION fCreateLabel RETURNS HANDLE
  (ipcPool AS CHARACTER, iphFrame AS HANDLE, ipcLabel AS CHARACTER, ipdRow AS DECIMAL, ipdHeight AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.
    
    IF ipdHeight GT 1 THEN
    ipdHeight = 1.
    ipcLabel = TRIM(ipcLabel) + ":".
    CREATE TEXT hLabel IN WIDGET-POOL ipcPool
      ASSIGN
        FRAME = iphFrame
        AUTO-RESIZE = YES
        HEIGHT = ipdHeight
        WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ipcLabel,iphFrame:FONT) + .5
        ROW = ipdRow
        FORMAT = "x(" + STRING(LENGTH(ipcLabel)) + ")"
        SCREEN-VALUE = ipcLabel
        SENSITIVE = YES
        HIDDEN = NO
        .
  RETURN hLabel.

END FUNCTION.

FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER, ipcFormat AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: format field value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx  AS INTEGER   NO-UNDO.

    IF INDEX(ipcField,"[") NE 0 THEN
    ASSIGN
        cStr = SUBSTRING(ipcField,INDEX(ipcField,"[") + 1)
        cStr = REPLACE(cStr,"]","")
        idx  = INTEGER(cStr)
        ipcField = SUBSTRING(ipcField,1,INDEX(ipcField,"[") - 1)
        .
    IF ipcFormat EQ "" THEN
    ipcFormat = iphTable:BUFFER-FIELD(ipcField):FORMAT.
    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(idx),ipcFormat) NO-ERROR.
    /* error raised if invalid format for field value */
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR
       iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(idx).
    
    RETURN LEFT-TRIM(TRIM(cStr)).

END FUNCTION.
