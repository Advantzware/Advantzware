

DEF VAR cRtnCharBol AS CHAR NO-UNDO.
DEFINE VARIABLE lRecFoundBol AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "RELPOST", "L" /* Logical */, YES /* check by cust */, 
    INPUT YES /* use cust not vendor */, oe-relh.cust-no /* cust */, oe-relh.ship-id /* ship-to*/,
OUTPUT cRtnCharBol, OUTPUT lRecFoundBol).
     IF lRecFoundBol THEN
       relpost-log = LOGICAL(cRtnCharBol) NO-ERROR.
     

RUN sys/ref/nk1look.p (INPUT cocode, "RELPOST", "C" /* Logical */, YES /* check by cust */, 
    INPUT YES /* use cust not vendor */, oe-relh.cust-no  /* cust */, oe-relh.ship-id /* ship-to*/,
OUTPUT cRtnCharBol, OUTPUT lRecFoundBol).
 IF lRecFoundBol THEN
    relpost-chr = cRtnCharBol NO-ERROR. 

RUN oe/oe-bolno.p (cocode, OUTPUT v-n-bol).

  /* check if more than 1 oe-rell and multiple frt pay/fob */
  ASSIGN rell-ctr = 0 
         vfrt-list = "" 
         vfob-list = "".
  FOR EACH bf-rell WHERE bf-rell.r-no EQ oe-relh.r-no
                         USE-INDEX r-no NO-LOCK:
                                                                                                      

    IF bf-rell.lot-no <> "" THEN
       ASSIGN vfrt-list = (IF LOOKUP(bf-rell.frt-pay,vfrt-list) = 0 THEN vfrt-list + "," + bf-rell.frt-pay ELSE vfrt-list)
              vfob-list = (IF LOOKUP(bf-rell.fob-code,vfob-list) = 0 THEN vfob-list + "," + bf-rell.fob-code ELSE vfob-list).
  END.    

  IF length(vfrt-list) > 0 THEN
      ASSIGN vfrt-list = SUBSTR(vfrt-list,2).
  IF length(vfob-list) > 0 THEN
      ASSIGN vfob-list = SUBSTR(vfob-list,2).

  IF NUM-ENTRIES(vfrt-list) > 1 THEN
     ASSIGN rell-ctr = NUM-ENTRIES(vfrt-list).
  ELSE
  IF NUM-ENTRIES(vfob-list) > 1 THEN
     ASSIGN rell-ctr = NUM-ENTRIES(vfob-list).

  /* get first order for release */
  FIND FIRST bf-rell WHERE bf-rell.r-no EQ oe-relh.r-no
      USE-INDEX r-no NO-LOCK NO-ERROR.
  find first xoe-ord where xoe-ord.company eq oe-relh.company
                       and xoe-ord.ord-no  eq bf-rell.ord-no
                     no-lock no-error.

/*
find first oe-ctrl where oe-ctrl.company eq cocode exclusive.

do while true:
  assign
   v-n-bol       = oe-ctrl.n-bol
   oe-ctrl.n-bol = v-n-bol + 1.
   
  if oe-ctrl.n-bol gt 999999 then oe-ctrl.n-bol = 1.
   
  find first {1}oe-bolh
      where {1}oe-bolh.company eq cocode
        and {1}oe-bolh.bol-no  eq v-n-bol
      use-index bol-no no-lock no-error.
  if not avail {1}oe-bolh then leave.
end.*/

  /* get the frt pay from oe-rel when filled in on the release browse */
  /* ah 03-15-10 */              
  ASSIGN vfrt-pay = (IF vfrt-list <> "" THEN SUBSTR(vfrt-list,1,1) ELSE "")
         vfob-code = (IF vfob-list <> "" THEN SUBSTR(vfob-list,1,1) ELSE "").


    IF bf-rell.lot-no <> "" THEN DO:   
     ASSIGN vfrt-pay = (IF vfrt-pay = "" THEN bf-rell.frt-pay ELSE vfrt-pay)
            vfob-code = (IF vfob-code = "" THEN bf-rell.fob-code ELSE vfob-code).        

     IF vfrt-pay = "P" THEN vfrt-pay = "Prepaid". 
     ELSE IF vfrt-pay = "C" THEN vfrt-pay = "Collect".
     ELSE IF vfrt-pay = "B" THEN vfrt-pay = "Bill". 
     ELSE IF vfrt-pay = "T" THEN vfrt-pay = "ThirdParty".

     IF vfob-code = "O" THEN vfob-code = "Origin". 
     ELSE IF vfob-code = "D" THEN vfob-code = "Destination".

     IF rell-ctr > 1 THEN DO:
         MESSAGE "Default Freight Pay to 1st Release?"
                 UPDATE vfrt-pay FORM "x(10)".
         IF NOT can-do("P,C,B,T",substr(vfrt-pay,1,1)) THEN
             ASSIGN vfrt-pay =  bf-rell.frt-pay.
         
         MESSAGE "Default FOB Code to 1st Release?"
                 UPDATE vfob-code FORM "x(12)".
         IF NOT can-do("O,D",substr(vfob-code,1,1)) THEN 
             ASSIGN vfob-code = bf-rell.fob-code.
     END.
     IF length(vfrt-pay) > 1 THEN
        ASSIGN vfrt-pay =  substr(vfrt-pay,1,1).
     IF length(vfob-code) > 1 THEN
        ASSIGN vfob-code =  substr(vfob-code,1,1).
  END.
  RELEASE reftable.
  /***************************************************************************/
  

  FOR EACH oe-bolh
      WHERE oe-bolh.company  EQ oe-relh.company
        AND oe-bolh.release# EQ oe-relh.release#
        AND NOT CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company
                                         AND oe-boll.b-no    EQ oe-bolh.b-no):
    DELETE oe-bolh.
  END.

  x = 1.
  find last oe-bolh use-index b-no no-lock no-error.
  if avail oe-bolh then x = oe-bolh.b-no + 1.

  create oe-bolh.
  assign
   oe-bolh.company  = oe-relh.company
   oe-bolh.loc      = locode
   oe-bolh.b-no     = x
   oe-bolh.bol-no   = v-n-bol
   oe-bolh.release# = oe-relh.release#
   oe-bolh.bol-date = IF boldate-chr EQ "Current" THEN TODAY ELSE oe-relh.rel-date
   oe-bolh.cust-no  = oe-relh.cust-no
   /*oe-bolh.po-no    = oe-relh.po-no  */
   oe-bolh.ship-no  = oe-relh.ship-no
   oe-bolh.ship-id  = oe-relh.ship-id
   oe-bolh.carrier  = oe-relh.carrier
   oe-bolh.stat     = STRING(relpost-log AND relpost-chr BEGINS "BOL","H/R")
   oe-bolh.b-ord-no = oe-relh.b-ord-no
   oe-bolh.rel-date = oe-relh.rel-date
   oe-bolh.r-no     = oe-relh.r-no
   oe-bolh.posted   = no
   oe-bolh.printed  = no
   oe-bolh.deleted  = no
   /*oe-bolh.frt-pay  = if avail xoe-ord then xoe-ord.frt-pay else ""*/
   oe-bolh.frt-pay  = IF vfrt-pay <> "" THEN vfrt-pay 
                      ELSE if avail xoe-ord then xoe-ord.frt-pay 
                      else ""
   oe-bolh.trailer  = oe-relh.trailer
   oe-bolh.upd-date = TODAY
   oe-bolh.upd-time = TIME
   oe-bolh.user-id  = USERID("nosweat").

  /*task 01121106 disable trigger oe-bolh preventing this from happening*/
  IF oe-bolh.rec_key EQ "" THEN
  DO:
     CREATE rec_key.
     ASSIGN
        oe-bolh.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey")
        rec_key.rec_key = oe-bolh.rec_key
        rec_key.table_name = "oe-bolh".
     RELEASE rec_key.
  END.

  IF vfob-code <> "" THEN DO:
     FIND FIRST reftable WHERE
          reftable.reftable EQ "oe-bolh.lot-no" AND
          reftable.rec_key  EQ oe-bolh.rec_key
          USE-INDEX rec_key
          NO-LOCK NO-ERROR.
     IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN reftable.reftable = "oe-bolh.lot-no" 
               reftable.rec_key  = oe-bolh.rec_key
               reftable.CODE     = vfob-code.
     END.         
     RELEASE reftable.
  END.

  RUN oe/custxship.p (oe-bolh.company,
                      oe-bolh.cust-no,
                      oe-bolh.ship-id,
                      BUFFER shipto).

  if not avail shipto then
  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq oe-bolh.cust-no
      no-lock no-error.

  if avail shipto then do:
    assign
     oe-bolh.ship-no      = shipto.ship-no
     oe-bolh.ship-id      = shipto.ship-id
     oe-bolh.ship-i[1]    = shipto.notes[1]
     oe-bolh.ship-i[2]    = shipto.notes[2]
     oe-bolh.ship-i[3]    = shipto.notes[3]
     oe-bolh.ship-i[4]    = shipto.notes[4].

    /** If the Ship To ID is a billable ID then set the bill of lading
        customer ID = to the Ship To ID  **/
    if shipto.bill then oe-bolh.cust-no = shipto.ship-id.
  end.

  if oe-relh.ship-i[1] ne "" or oe-relh.ship-i[2] ne "" or
     oe-relh.ship-i[3] ne "" or oe-relh.ship-i[4] ne "" then do:

/*     find first sys-ctrl                                             */
/*         where sys-ctrl.company eq cocode                            */
/*           and sys-ctrl.name    eq "BOLCERT"                         */
/*         no-lock no-error.                                           */
/*     if not avail sys-ctrl then do:                                  */
/*       create sys-ctrl.                                              */
/*       assign                                                        */
/*        sys-ctrl.company  = cocode                                   */
/*        sys-ctrl.name     = "BOLCERT"                                */
/*        sys-ctrl.descrip  = "Print Certificate of Compliance forms?" */
/*        sys-ctrl.log-fld  = no.                                      */
/*       MESSAGE sys-ctrl.descrip                                      */
/*           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                  */
/*           UPDATE sys-ctrl.log-fld.                                  */
/*     end.                                                            */
/*     if sys-ctrl.log-fld and sys-ctrl.char-fld eq "BRICK" then       */
/*         assign                                                      */
/*          oe-bolh.ship-i[3] = oe-relh.ship-i[1]                      */
/*          oe-bolh.ship-i[4] = oe-relh.ship-i[2].                     */
/*     else                                                            */
        assign
         oe-bolh.ship-i[1] = oe-relh.ship-i[1]
         oe-bolh.ship-i[2] = oe-relh.ship-i[2]
         oe-bolh.ship-i[3] = oe-relh.ship-i[3]
         oe-bolh.ship-i[4] = oe-relh.ship-i[4].
  end.
  /*if v-royal then oe-bolh.trailer = "HOLD".*/
