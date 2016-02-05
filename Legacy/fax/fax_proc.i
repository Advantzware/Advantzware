/* fax_proc.i  Procedures definition for RightFax */

 ROCEDURE RFaxServerHandle EXTERNAL "rfwin32.dll" :
    DEF INPUT PARAM RFaxServerName AS cha.    
    DEF INPUT PARAM CommProtoclo AS int.
    DEF OUTPUT PARAM RFaxError AS LONG.
    DEF OUTPUT PARAM RFaxServerHandle AS LONG.
 END PROCEDURE.               

 PROCEDURE RFaxInitFax EXTERNAL "rfwin32.dll" :
    DEF INPUT PARAM RFaxServerHandle AS long.    
    DEF INPUT PARAM RFaxInfo AS long.
    DEF INPUT PARAM RfaxStr AS cha.
    DEF OUTPUT PARAM RFaxRet AS LONG.
 END PROCEDURE.

 PROCEDURE RFaxSendCVL EXTERNAL "rfwin32.dll" :
    DEF INPUT PARAM RFaxServerHandle AS long.
    DEF INPUT PARAM RFaxCVLFileName AS cha.
    DEF INPUT PARAM RFaxInfo AS long.
    DEF INPUT PARAM NoteInfo AS LONG.
    DEF INPUT PARAM RfaxStr AS cha.
    DEF OUTPUT PARAM faxhandle AS long.

 END PROCEDURE.
