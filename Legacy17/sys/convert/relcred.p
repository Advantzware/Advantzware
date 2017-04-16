/* sys/convert/relcred.p*/

IF CAN-FIND(first reftable where
   reftable.reftable = "relcredconv") THEN
   RETURN.

DO TRANSACTION:

   FOR EACH company NO-LOCK,
       EACH oe-relh WHERE
       oe-relh.company EQ company.company AND
       oe-relh.deleted EQ NO AND
       oe-relh.w-ord:
       oe-relh.w-ord = NO.
   END.

   FOR EACH reftable WHERE
       reftable.reftable = "oe-relh.hold":

       FIND FIRST oe-relh WHERE
            oe-relh.rec_key = reftable.rec_key
            NO-ERROR.

       IF AVAIL oe-relh THEN
          oe-relh.w-ord = reftable.company EQ "hld".

       DELETE reftable.
   END.

   CREATE reftable.
   ASSIGN reftable.reftable = "relcredconv"
          reftable.code2    = STRING(TIME) + " " + USERID("nosweat").
END.
