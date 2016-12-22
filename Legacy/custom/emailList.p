/* emailList.p - rstark 7.15.2005 */

DEFINE INPUT PARAMETER ipRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipEmailCode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opEmailList AS CHARACTER NO-UNDO.

opEmailList = ''.
FOR EACH {&ASI.phone NO-LOCK
    WHERE {&ASI.phone.table_rec_key EQ ipRecKey BREAK BY {&ASI.phone.e_mail:
  IF CAN-FIND(FIRST emaildtl
              WHERE (emaildtl.emailcod EQ 'EHOTS'
                 OR emaildtl.emailcod EQ ipEmailCode)
                AND emaildtl.table_rec_key EQ {&ASI.phone.rec_key)
     OR {&ASI.phone.titlcode EQ 'EHOTS' THEN
  IF {&ASI.phone.e_mail NE '' AND
     NOT CAN-DO(opEmailList,{&ASI.phone.e_mail) THEN
  opEmailList = opEmailList + (IF opEmailList NE '' THEN ',' ELSE '') + {&ASI.phone.e_mail.
END. /* each phone */
