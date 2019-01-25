/* dynFuncs.i - rstark - 1.25.2019 */

FUNCTION fCreateLabel RETURNS HANDLE
  (ipcPool AS CHARACTER, iphFrame AS HANDLE, ipcLabel AS CHARACTER, ipdRow AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.
    
    ipcLabel = TRIM(ipcLabel) + ":".
    CREATE TEXT hLabel IN WIDGET-POOL ipcPool
      ASSIGN
        FRAME = iphFrame
        AUTO-RESIZE = YES
        HEIGHT = 1
        WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ipcLabel,iphFrame:FONT) + .5
        ROW = ipdRow
        FORMAT = "x(" + STRING(LENGTH(ipcLabel)) + ")"
        SCREEN-VALUE = ipcLabel
        SENSITIVE = YES
        .
  RETURN hLabel.

END FUNCTION.

FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: format field value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.

    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(),
                  iphTable:BUFFER-FIELD(ipcField):FORMAT) NO-ERROR.
    /* error raised if invalid format for field value */
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR
       iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE().
    
    RETURN LEFT-TRIM(TRIM(cStr)).

END FUNCTION.
