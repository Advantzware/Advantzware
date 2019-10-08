/* emailList.p - rstark 7.15.2005 */

DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipEmailCode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opEmailList AS CHARACTER NO-UNDO.

opEmailList = ''.
FOR EACH {&nosweat}phone NO-LOCK
    WHERE {&nosweat}phone.table_rec_key EQ ipRecKey BREAK BY {&nosweat}phone.e_mail:
  IF CAN-FIND(FIRST emaildtl
              WHERE (emaildtl.emailcod EQ 'EHOTS'
                 OR emaildtl.emailcod EQ ipEmailCode)
                AND emaildtl.table_rec_key EQ {&nosweat}phone.rec_key)
     OR {&nosweat}phone.titlcode EQ 'EHOTS' THEN
  IF {&nosweat}phone.e_mail NE '' AND
     NOT CAN-DO(opEmailList,{&nosweat}phone.e_mail) THEN
  opEmailList = opEmailList + (IF opEmailList NE '' THEN ',' ELSE '') + {&nosweat}phone.e_mail.
END. /* each phone */
