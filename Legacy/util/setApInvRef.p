/* Fixes ambiguity of reftable values.  The reftable was not defined per */
/* company, therefore, storing company in spar-char-1 so as not to have */
/* specify the index in all queries of the reftable as would be the case */
/* if company field was used */

 MESSAGE "Correct problem with missing PO records?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE "" UPDATE lSendTheFile AS LOGICAL.

for each  reftable                
              WHERE reftable.reftable EQ "AP-INVL"         
                AND reftable.company  EQ ""                
              AND reftable.loc      EQ ""                
             /* AND reftable.code     EQ STRING(po-ordl.po-no,"9999999999") /* OUTER-JOIN */ */
    EXCLUSIVE-LOCK:

  FIND FIRST  ap-invl                                      
                WHERE ap-invl.i-no              EQ int(reftable.code2)               
             /*     AND ap-invl.po-no             EQ po-ordl.po-no                     */
             /*     AND (ap-invl.line + ap-invl.po-no * -1000) EQ po-ordl.LINE */ /* OUTER-JOIN */
    NO-LOCK NO-ERROR.

  find first po-ordl where po-ordl.company eq ap-invl.company
     and po-ordl.po-no eq int(reftable.code) 
  no-lock no-error.

  if avail ap-invl AND AVAIL(po-ordl) then 
    reftable.spare-char-1 = ap-invl.company.
  

END.

MESSAGE "Done!"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
