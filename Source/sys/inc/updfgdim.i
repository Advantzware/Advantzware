/* updfgdim.i   using est-no instead e-num in GUI */
DEF VAR formule AS de EXTENT 12 NO-UNDO.
DEF VAR op AS ch EXTENT 100 NO-UNDO.
DEF VAR nextop AS INT NO-UNDO.
DEF VAR num AS de EXTENT 100 NO-UNDO.
DEF VAR curnum AS CHAR NO-UNDO.
DEF VAR kar AS ch FORMAT "x" NO-UNDO.  /* style formula kalk variables */
DEF VAR v-dim-fit LIKE style.dim-fit NO-UNDO.
DEF VAR lv-o-w{1} LIKE ef.n-out NO-UNDO.
DEF VAR lv-o-l{1} LIKE ef.n-out-l NO-UNDO.
DEF VAR lv-u-w{1} LIKE eb.num-wid NO-UNDO.
DEF VAR lv-u-l{1} LIKE eb.num-len NO-UNDO.

DEF BUFFER updfgdim-{1} FOR eb.

DEF VAR fgitemsf-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR fgitemsf-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR fgitemsf-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR fgitemsf-dec LIKE sys-ctrl.dec-fld NO-UNDO.
DEF VAR v-chr-val    AS   CHAR             NO-UNDO.
DEF VAR v-fnd        AS   LOG              NO-UNDO.

IF AVAIL {1} THEN
RUN sys/ref/nk1look.p (INPUT {1}.company, INPUT "FGITEMSF", INPUT "C", 
                       INPUT NO, INPUT NO, INPUT "", INPUT "", 
                       OUTPUT fgitemsf-cha,
                       OUTPUT v-fnd).
/*
DO TRANSACTION:
  {sys/inc/fgitemsf.i}
END. */

formule = 0.

IF AVAIL {1} THEN
IF {1}.form-no EQ 0                          AND
   CAN-FIND(updfgdim-{1}
            WHERE updfgdim-{1}.company EQ {1}.company
              AND updfgdim-{1}.est-no  EQ {1}.est-no
              AND updfgdim-{1}.form-no NE 0) THEN  /* 2pc Box */
  FIND FIRST updfgdim-{1}
      WHERE updfgdim-{1}.company EQ {1}.company
        AND updfgdim-{1}.est-no  EQ {1}.est-no
        AND updfgdim-{1}.form-no NE 0
      NO-LOCK NO-ERROR.
ELSE
  FIND updfgdim-{1} WHERE ROWID(updfgdim-{1}) EQ ROWID({1}) NO-LOCK NO-ERROR.

IF AVAIL updfgdim-{1} THEN DO:
  FIND FIRST ef NO-LOCK
      WHERE ef.company EQ updfgdim-{1}.company
        AND ef.est-no  EQ updfgdim-{1}.est-no
        AND ef.form-no EQ updfgdim-{1}.form-no
      NO-ERROR.

  FIND FIRST style NO-LOCK
      {sys/ref/styleW.i}
        AND style.style EQ updfgdim-{1}.style
      NO-ERROR.

  IF AVAIL style THEN DO:
    {ce/updfgdim.i updfgdim-{1}}
  END.

  ASSIGN
   {2}itemfg.est-no      = {1}.est-no
   {2}itemfg.w-score[50] = {1}.wid
   {2}itemfg.l-score[50] = {1}.len
   {2}itemfg.d-score[50] = {1}.dep.

  IF fgitemsf-cha EQ "GrossSH"  AND
     updfgdim-{1}.est-type GE 5 AND
     AVAIL ef                   THEN DO:

    ASSIGN
     lv-o-w{1} = IF ef.n-out             EQ 0 THEN 1 ELSE ef.n-out
     lv-o-l{1} = IF ef.n-out-l           EQ 0 THEN 1 ELSE ef.n-out-l
     lv-u-w{1} = IF updfgdim-{1}.num-len EQ 0 THEN 1 ELSE updfgdim-{1}.num-len
     lv-u-l{1} = IF updfgdim-{1}.num-wid EQ 0 THEN 1 ELSE updfgdim-{1}.num-wid.

    IF ef.xgrain EQ "B" THEN
      ASSIGN
       {2}itemfg.t-wid = ef.gsh-len / lv-o-l{1} / lv-u-l{1}
       {2}itemfg.t-len = ef.gsh-wid / lv-o-w{1} / lv-u-w{1}.

    ELSE
    IF ef.xgrain EQ "S" THEN
      ASSIGN
       {2}itemfg.t-wid = ef.gsh-len / lv-o-l{1} / lv-u-w{1}
       {2}itemfg.t-len = ef.gsh-wid / lv-o-w{1} / lv-u-l{1}.

    ELSE
      ASSIGN
       {2}itemfg.t-wid = ef.gsh-wid / lv-o-w{1} / lv-u-w{1}
       {2}itemfg.t-len = ef.gsh-len / lv-o-l{1} / lv-u-l{1}.
  END.

  ELSE DO:
    ASSIGN
     {2}itemfg.t-wid = updfgdim-{1}.t-wid
     {2}itemfg.t-len = updfgdim-{1}.t-len.
   
    IF (formule[1] EQ updfgdim-{1}.t-wid AND formule[2] EQ updfgdim-{1}.t-len) OR
       (formule[1] EQ 0                  AND formule[2] EQ 0)                  THEN DO:
      IF formule[7] NE 0 THEN {2}itemfg.t-wid = formule[7].
      IF formule[8] NE 0 THEN {2}itemfg.t-len = formule[8].
    END.
  END.

  IF ROWID(updfgdim-{1}) NE ROWID({1}) THEN
    {2}itemfg.t-len = {2}itemfg.t-len * 2.

  IF NOT {2}itemfg.spare-int-2 EQ 1 THEN 
      ASSIGN /*take windowing out before weight calc*/
       {2}itemfg.t-sqin = ({2}itemfg.t-wid * {2}itemfg.t-len) - updfgdim-{1}.t-win
       {2}itemfg.t-sqft = IF v-corr THEN {2}itemfg.t-sqin * .007
                                    ELSE {2}itemfg.t-sqin / 144.
    
  IF AVAIL ef AND ef.board NE "" AND {2}itemfg.spare-int-1 NE 1 THEN
       {2}itemfg.weight-100 = {2}itemfg.t-sqft * .1 * ef.weight.
  
  IF NOT {2}itemfg.spare-int-2 EQ 1 THEN   
      ASSIGN
       {2}itemfg.t-sqin = ({2}itemfg.t-wid * {2}itemfg.t-len)
       {2}itemfg.t-sqft = IF v-corr THEN {2}itemfg.t-sqin * .007
                                    ELSE {2}itemfg.t-sqin / 144.
END.
