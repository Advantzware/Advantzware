/* cec/get-vend.i */
def var cevendor like sys-ctrl.log-fld no-undo.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-valid AS LOG INIT YES NO-UNDO.
DEF VAR v-cebrowse-secur-group AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exc-char AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exclude-cat AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exclude-vend AS CHAR NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CEVENDOR"
             no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEVENDOR"
   sys-ctrl.descrip = "Select Board Vendor during Whatif?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

cevendor = sys-ctrl.log-fld.

if cevendor AND vprint then
DO:
   IF sys-ctrl.char-fld NE "" THEN
   DO:
      ASSIGN
         v-valid = NO
         v-cebrowse-secur-group = SUBSTRING(sys-ctrl.char-fld,1,INDEX(TRIM(sys-ctrl.char-fld)," ") - 1)
         v-cebrowse-exc-char = SUBSTRING(sys-ctrl.char-fld,INDEX(TRIM(sys-ctrl.char-fld)," ") + 1)
         v-cebrowse-exc-char = SUBSTRING(v-cebrowse-exc-char,INDEX(v-cebrowse-exc-char,"(") + 1)
         v-cebrowse-exc-char = TRIM(v-cebrowse-exc-char,")")
         v-cebrowse-exclude-cat = SUBSTRING(v-cebrowse-exc-char,1, INDEX(v-cebrowse-exc-char,"=") - 1)
         v-cebrowse-exclude-vend = SUBSTRING(v-cebrowse-exc-char,INDEX(v-cebrowse-exc-char,"=") + 1).

      REPEAT v-count = 1 TO NUM-ENTRIES(v-cebrowse-secur-group):
        
         IF TRIM(ENTRY(v-count,v-cebrowse-secur-group)) NE "" THEN
         DO:
            FIND FIRST usergrps WHERE
                 usergrps.usergrps = ENTRY(v-count,v-cebrowse-secur-group)
                 NO-LOCK NO-ERROR.
           
            IF AVAIL usergrps AND
               (CAN-DO(usergrps.users,USERID("NOSWEAT")) OR
                TRIM(usergrps.users) EQ "*") THEN
               DO:
                  v-valid = YES.
                  LEAVE.
               END.
         END.
      END.

      IF v-valid = NO AND v-cebrowse-exclude-cat NE "" THEN
         RUN cec/get-exclude-vend.p(INPUT ROWID(xest),
                                    INPUT v-cebrowse-exclude-cat,
                                    INPUT v-cebrowse-exclude-vend,
                                    OUTPUT v-vend-no).
   END.
   
   IF v-valid THEN
      run cec/est-vend.w (recid(xest),output v-vend-no,output lv-error ) no-error.
END.

if lv-error then do:
   return error.
end.
/*  
find first e-item-vend where recid(e-item-vend) eq fil_id no-lock no-error.
v-vend-no = if avail e-item-vend then e-item-vend.vend-no else "".
*/
