&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME probe

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF BUFFER xeb FOR eb.
DEF BUFFER b-sys-ctrl FOR sys-ctrl.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll-round AS LOG NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR vmclean2 AS LOG NO-UNDO.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-LOCK NO-ERROR.

IF AVAIL est THEN
DO:
   IF est.est-type LT 5 THEN
      find first b-sys-ctrl
          where b-sys-ctrl.company eq {&TABLENAME}.company
            and b-sys-ctrl.name    eq "CERUNF"
          no-lock no-error.
   ELSE
      find first b-sys-ctrl
          where b-sys-ctrl.company eq {&TABLENAME}.company
            and b-sys-ctrl.name    eq "CERUNC"
          no-lock no-error.

   IF AVAIL b-sys-ctrl AND b-sys-ctrl.char-fld EQ "Fibre" THEN
      RUN est/usemargin.p (ROWID(est), OUTPUT ll-use-margin).
END.

IF ll-use-margin = NO THEN
DO:
   find first b-sys-ctrl WHERE
        b-sys-ctrl.company eq {&TABLENAME}.company AND
        b-sys-ctrl.name    eq "SETPRINT"
        no-lock no-error.
  
   IF AVAIL b-sys-ctrl THEN
      vmclean2 = b-sys-ctrl.char-fld eq "McLean".

   IF est.est-type EQ 6 AND {&TABLENAME}.set-chg NE 0 AND vmclean2 THEN
      ASSIGN
         {&TABLENAME}.net-profit   = ((1 - ({&TABLENAME}.full-cost / {&TABLENAME}.sell-price)) * 100) - {&TABLENAME}.set-chg
         {&TABLENAME}.gross-profit = ((1 - ({&TABLENAME}.fact-cost / {&TABLENAME}.sell-price)) * 100) - {&TABLENAME}.set-chg.
   ELSE
      ASSIGN
         {&TABLENAME}.net-profit   = (1 - ({&TABLENAME}.full-cost / {&TABLENAME}.sell-price)) * 100
         {&TABLENAME}.gross-profit = (1 - ({&TABLENAME}.fact-cost / {&TABLENAME}.sell-price)) * 100.

   {&TABLENAME}.market-price = {&TABLENAME}.net-profit + {&TABLENAME}.comm.
END.

IF AVAIL est THEN DO:
  ASSIGN
   cocode = est.company
   locode = est.loc.

  /*IF old-{&TABLENAME}.company NE "" AND
     old-{&TABLENAME}.est-no  NE "" THEN
    ASSIGN
     est.gsa-mat      = {&TABLENAME}.gsa-mat
     est.gsa-lab      = {&TABLENAME}.gsa-lab
     est.gsa-war      = {&TABLENAME}.gsa-war
     est.gsa-war-amt  = {&TABLENAME}.gsa-war-amt
     est.gsa-war-hdl  = {&TABLENAME}.gsa-war-hdl
     est.gsa-war-tot  = {&TABLENAME}.gsa-war-tot
     est.gsa-war-u-c  = {&TABLENAME}.gsa-war-u-c
     est.gsa-war-cnt  = {&TABLENAME}.gsa-war-cnt
     est.gsa-war-uni  = {&TABLENAME}.gsa-war-uni
     est.gsa-war-pal  = {&TABLENAME}.gsa-war-pal
     est.gsa-war-per  = {&TABLENAME}.gsa-war-per
     est.gsa-war-u-p  = {&TABLENAME}.gsa-war-u-p.*/

  /*IF est.est-type LE 4                      AND
     {&TABLENAME}.gross-profit LT {&TABLENAME}.net-profit THEN DO:

    FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

    FIND FIRST xeb
        WHERE xeb.company EQ {&TABLENAME}.company
          AND xeb.est-no  EQ {&TABLENAME}.est-no
          AND xeb.form-no NE 0
          AND xeb.comm    NE 0
        NO-LOCK NO-ERROR.

    {ce/markup.i}

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ {&TABLENAME}.company
           AND sys-ctrl.name    EQ "CEROUND"
         NO-LOCK NO-ERROR.
    ll-round = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "Dollar".

    {&TABLENAME}.sell-price = {&TABLENAME}.full-cost /
                              (1 - (({&TABLENAME}.net-profit + v-com) / 100)).
          
    IF ll-round THEN {&TABLENAME}.sell-price = ROUND({&TABLENAME}.sell-price,0).

    {&TABLENAME}.gross-profit = ((1 - ({&TABLENAME}.fact-cost / {&TABLENAME}.sell-price)) * 100) - v-com.

    RUN ce/uprobeit.p (ROWID({&TABLENAME})).
  END.*/
END.

IF old-{&TABLENAME}.company  NE ""                      AND
   old-{&TABLENAME}.est-no   NE ""                      AND
   (old-{&TABLENAME}.company NE {&TABLENAME}.company OR
    old-{&TABLENAME}.est-no  NE {&TABLENAME}.est-no  OR
    old-{&TABLENAME}.line    NE {&TABLENAME}.line)      THEN
FOR EACH reftable
    WHERE reftable.reftable EQ "probe.per-msf"
      AND reftable.company  EQ old-{&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ old-{&TABLENAME}.est-no
      AND reftable.code2    EQ STRING(old-{&TABLENAME}.line,"9999999999"):
  ASSIGN
   reftable.company  = {&TABLENAME}.company
   reftable.code     = {&TABLENAME}.est-no
   reftable.code2    = STRING({&TABLENAME}.line,"9999999999").
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
