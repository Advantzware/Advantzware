DEF PARAM BUFFER io-po-ordl FOR po-ordl.
DEF PARAM BUFFER io-ap-invl FOR ap-invl.


IF AVAIL io-po-ordl                                             AND
   (NOT AVAIL io-ap-invl OR
    NOT CAN-FIND(FIRST ap-invl
                 WHERE ap-invl.i-no       EQ io-ap-invl.i-no
                   AND ap-invl.po-no      EQ io-po-ordl.po-no
                   AND {ap/invlline.i -1} EQ io-po-ordl.line
                   AND ROWID(ap-invl)     NE ROWID(io-ap-invl)
                 USE-INDEX i-no))                               AND
                               
  io-po-ordl.stat      NE "X"   /* not deleted or cancelled */  AND            
  io-po-ordl.stat      NE "F"   /* not deleted or cancelled */  AND            
  (io-po-ordl.t-rec-qty NE 0 OR
   (io-po-ordl.item-type AND
    CAN-FIND(FIRST item
             WHERE item.company EQ io-po-ordl.company
               AND item.i-no    EQ io-po-ordl.i-no
               AND item.i-code  EQ "R"
               /*AND item.stocked EQ NO*/ /* task 06301508 */
             USE-INDEX i-no)))                                  THEN.
ELSE RELEASE io-po-ordl.
