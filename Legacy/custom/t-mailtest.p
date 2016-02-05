  DEF VAR retcode AS INT NO-UNDO.
  DEF VAR ls-to-list AS cha NO-UNDO.
  DEF VAR lv-mailto AS cha NO-UNDO.
  DEF VAR lv-mailsubject AS cha NO-UNDO.
  DEF VAR lv-mailbody AS cha NO-UNDO.
  DEF VAR lv-mailattach AS cha NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.

  

   ls-to-list = "".

   /*RUN mail("TO:abcde@advantzware.com,12345@advantzware.com","Mail Test ",lv-mailbody,lv-mailattach,1,OUTPUT retcode).*/

   RUN mail("TO:abcd@advantzware.com CC:1234@advantzware.com","Mail Test ",lv-mailbody,lv-mailattach,1,OUTPUT retcode).

PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEF INPUT PARAM mailTo AS CHAR.
    DEF INPUT PARAM mailsubject AS CHAR.
    DEF INPUT PARAM mailText AS CHAR.
    DEF INPUT PARAM mailFiles AS CHAR.
    DEF INPUT PARAM mailDialog AS LONG.
    DEF OUTPUT PARAM retCode AS LONG.
END.
