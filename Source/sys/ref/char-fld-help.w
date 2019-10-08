/*******************************************************************************
Author: Gilbert Marquez
TASKID: 11050804     
*******************************************************************************/

DEF INPUT  PARAMETER ip_comp   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ip_chrfld AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op_chrfld AS CHAR NO-UNDO.

DEF VAR v_chrfld AS CHAR NO-UNDO.
DEF VAR v_chFile AS CHAR NO-UNDO.
DEF VAR v_okflg  AS LOG  NO-UNDO.
DEF VAR v_chPos  AS INT  NO-UNDO.

DEF VAR v_path AS CHAR NO-UNDO.

DEF BUFFER bf_sys-ctrl FOR sys-ctrl.

IF ip_chrfld EQ "" THEN DO:

    FIND FIRST bf_sys-ctrl NO-LOCK                       
        WHERE bf_sys-ctrl.company EQ ip_comp
          AND bf_sys-ctrl.name EQ "BARDIR" NO-ERROR.     
    IF AVAIL bf_sys-ctrl                                 
      THEN ASSIGN ip_chrfld = TRIM(bf_sys-ctrl.descrip). 
      ELSE ASSIGN ip_chrfld = 'c:\'.
END.
    
/* gdm - 11110806 */
IF ip_chrfld EQ "GRAPHIC" THEN ASSIGN ip_chrfld = "".

IF TRIM(ip_chrfld) EQ "" THEN ASSIGN ip_chrfld = 'c:\'.

IF LENGTH(TRIM(ip_chrfld)) EQ 1 THEN ASSIGN ip_chrfld = TRIM(ip_chrfld) + ':\'.

IF ip_chrfld = 'c:\' /*OR INDEX(TRIM(ip_chrfld),':') GT 0)*/
  THEN 
    ASSIGN 
        v_path = TRIM(ip_chrfld).
       
  ELSE
    ASSIGN
        ip_chrfld  = REPLACE(ip_chrfld,'/','\')
        v_chPos    = R-INDEX(ip_chrfld,'\')
        v_path     = SUBSTRING(ip_chrfld,1,v_chPos).


SYSTEM-DIALOG GET-FILE v_chFile 
    TITLE "Select Label Matrix Label File"
    FILTERS "Label Matrix (*.qdf) " "*.qdf"
    INITIAL-DIR v_path
    MUST-EXIST
    USE-FILENAME
    UPDATE v_okflg.

ASSIGN 
    op_chrfld = v_chFile.

  
