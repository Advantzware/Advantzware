/* ------------------------------------------------- oe/get-tckt#.p 06/09 GDM */
/* Order Entry - Get the next release ticket #                                */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-cocode   LIKE oe-relh.company  NO-UNDO.
DEF OUTPUT PARAM op-r-no     LIKE oe-relh.r-no     NO-UNDO.
DEF OUTPUT PARAM op-release# LIKE oe-relh.release# NO-UNDO.

DEF VAR v-cnt AS INT NO-UNDO.

ASSIGN v-cnt = 1.

DO TRANSACTION:

 FIND LAST reftable EXCLUSIVE-LOCK 
   WHERE reftable.reftable EQ "oe-relh.r-no" USE-INDEX CODE NO-WAIT NO-ERROR.
 IF NOT AVAIL reftable THEN DO:
   
   CREATE reftable.
   ASSIGN reftable.reftable = "oe-relh.r-no"
          reftable.company = ip-cocode.

   FIND LAST oe-relh USE-INDEX r-no NO-LOCK NO-ERROR.

   ASSIGN v-cnt = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 1
          v-cnt = v-cnt + IF v-cnt NE 1 THEN 1 ELSE 0.

 END.

  ASSIGN reftable.code = STRING(INT(reftable.CODE) + v-cnt)
         op-r-no       = INT(reftable.code).

 RELEASE reftable.

END.

ASSIGN v-cnt = 1.

DO TRANSACTION:

  FIND LAST reftable EXCLUSIVE-LOCK 
   WHERE reftable.reftable EQ "oe-relh.release#" 
      USE-INDEX CODE NO-WAIT NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
   
   CREATE reftable.
   ASSIGN reftable.reftable = "oe-relh.release#"
          reftable.company = ip-cocode.

   RUN oe/release#.p (ip-cocode, OUTPUT v-cnt).

   ASSIGN reftable.code = STRING(v-cnt)
          op-release#   = INT(reftable.code).

  END.
  ELSE ASSIGN reftable.code = STRING(INT(reftable.code) + v-cnt)
              op-release#   = INT(reftable.code).

 RELEASE reftable.

END.


 

