/*------------------------------------------------------------------------
    File      : Estvendor.p
    Purpose   :  Corrugated Customer Vendor
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 7 july 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEstVendorGet NO-UNDO 
        FIELD key-01 AS CHAR
        FIELD key-02 AS CHAR
        FIELD rec-id  AS CHAR  .
        
    
DEFINE DATASET dsEstVendorGet FOR ttEstVendorGet .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate        AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstVendorGet.

IF prmAction      = ? THEN ASSIGN prmAction      = "Select".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmEstimate   = ? THEN ASSIGN prmEstimate   = "".


DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF VAR vprint AS LOG INIT YES NO-UNDO.
def var cevendor like sys-ctrl.log-fld no-undo.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-valid AS LOG INIT YES NO-UNDO.
DEF VAR v-cebrowse-secur-group AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exc-char AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exclude-cat AS CHAR NO-UNDO.
DEF VAR v-cebrowse-exclude-vend AS CHAR NO-UNDO.

ASSIGN
    cocode = prmComp
    vprint = YES    .


find first sys-ctrl where sys-ctrl.company eq prmComp
                        and sys-ctrl.name    eq "CEVENDOR"
             no-lock no-error.


cevendor = sys-ctrl.log-fld.



if prmAction = "Select" then do:

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
         /*RUN cec/get-exclude-vend.p(INPUT ROWID(xest),
                                    INPUT v-cebrowse-exclude-cat,
                                    INPUT v-cebrowse-exclude-vend,
                                    OUTPUT v-vend-no).*/
          RUN get-exclude-vend .
   END.
   
   IF v-valid THEN DO:
  
      run build-ttEstVendorGet .

   END. /* end of v-valid yes */ 
   
END.

END.  /*if prmAction <> "Select" then do*/ 


PROCEDURE build-ttEstVendorGet :

DEF VAR v-forms AS INT EXTENT 2 NO-UNDO.
DEF VAR li AS INT NO-UNDO.

/*FIND est WHERE RECID(est) EQ v-recid NO-LOCK NO-ERROR.*/
FIND FIRST est WHERE est.company = prmComp AND est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.



IF NOT AVAIL est                        OR
   NOT CAN-FIND(FIRST eb OF est
                WHERE eb.form-no GT 0
                  AND eb.pur-man EQ NO) THEN LEAVE.

FOR EACH ef
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no
      AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man )
    NO-LOCK:

  v-forms[1] = v-forms[1] + 1.

/*  IF ef.cost-msh NE 0 THEN
     v-board-cost-not-zero = YES.*/
  

  FIND FIRST e-item-vend
      WHERE e-item-vend.company EQ ef.company
        AND e-item-vend.i-no    EQ ef.board
        AND e-item-vend.vend-no NE ""         
        AND ef.gsh-wid          GE e-item-vend.roll-w[27]
        AND ef.gsh-wid          LE e-item-vend.roll-w[28]
        AND ef.gsh-len          GE e-item-vend.roll-w[29]
        AND ef.gsh-len          LE e-item-vend.roll-w[30]
      NO-LOCK NO-ERROR .
  IF AVAIL e-item-vend THEN DO:
    CREATE ttEstVendorGet .
      ASSIGN
          ttEstVendorGet.key-01  = ""
          ttEstVendorGet.key-02  =  ""
          ttEstVendorGet.rec-id  =  "" .
  END.

  FOR EACH e-item-vend
      WHERE e-item-vend.company EQ ef.company
        AND e-item-vend.i-no    EQ ef.board
        AND e-item-vend.vend-no NE ""         
        AND ef.gsh-wid          GE e-item-vend.roll-w[27]
        AND ef.gsh-wid          LE e-item-vend.roll-w[28]
        AND ef.gsh-len          GE e-item-vend.roll-w[29]
        AND ef.gsh-len          LE e-item-vend.roll-w[30]
      NO-LOCK :
              
    FIND FIRST vend
        WHERE vend.company EQ e-item-vend.company
          AND vend.vend-no EQ e-item-vend.vend-no
        NO-LOCK NO-ERROR.
    
    CREATE ttEstVendorGet.
    ASSIGN
     ttEstVendorGet.key-01  = e-item-vend.vend-no
     ttEstVendorGet.key-02  = IF AVAIL vend THEN vend.name ELSE ""
     ttEstVendorGet.rec-id  = STRING(RECID(e-item-vend)).
  END.
END.

FOR EACH ttEstVendorGet BREAK BY ttEstVendorGet.key-01:

  IF FIRST-OF(ttEstVendorGet.key-01) THEN v-forms[2] = 0.

  v-forms[2] = v-forms[2] + 1.

  IF NOT LAST-OF(ttEstVendorGet.key-01) OR v-forms[1] NE v-forms[2] AND ttEstVendorGet.key-01 <> ""  THEN
    DELETE ttEstVendorGet.
END.

adder-blok:
FOR EACH ttEstVendorGet.

  FOR EACH ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
      NO-LOCK:
    DO li = 1 TO 6:
      IF ef.adder[li] NE ""                                          AND
         NOT CAN-FIND(FIRST e-item-vend
                      WHERE e-item-vend.company EQ prmComp
                        AND e-item-vend.i-no    EQ ef.adder[li]
                        AND e-item-vend.vend-no EQ ttEstVendorGet.key-01) 
                        AND ttEstVendorGet.key-01 <> ""  THEN DO:
        DELETE ttEstVendorGet.
        NEXT adder-blok.
      END.
    END.
  END.
END.

/* end ---------------------------------- copr. 2001  advanced software, inc. */
END PROCEDURE.

PROCEDURE get-exclude-vend:

    DO TRANSACTION:
   {sys\inc\ceboard.i}
END.

DEF VAR v-forms AS INT EXTENT 2 NO-UNDO.
DEF VAR li AS INT NO-UNDO.

/*FIND est WHERE ROWID(est) EQ v-rowid NO-LOCK NO-ERROR.*/
FIND FIRST est WHERE est.company = prmComp AND est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.

IF NOT AVAIL est OR
   NOT CAN-FIND(FIRST eb OF est
                WHERE eb.form-no GT 0
                  AND eb.pur-man EQ NO) THEN LEAVE.

FOR EACH ef 
    WHERE ef.company EQ est.company
      AND ef.est-no  EQ est.est-no
      AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
    NO-LOCK:

  v-forms[1] = v-forms[1] + 1.

  /*for combos, category for each form has to be v-cebrowse-exclude-cat*/
  
  IF NOT CAN-FIND(FIRST eb OF ef WHERE
     eb.procat EQ v-cebrowse-exclude-cat) THEN NEXT.

  FOR EACH e-item-vend FIELDS(vend-no)
      WHERE e-item-vend.company EQ ef.company
        AND e-item-vend.i-no    EQ ef.board
        AND e-item-vend.vend-no EQ v-cebrowse-exclude-vend        
        AND ef.gsh-wid          GE e-item-vend.roll-w[27]
        AND ef.gsh-wid          LE e-item-vend.roll-w[28]
        AND ef.gsh-len          GE e-item-vend.roll-w[29]
        AND ef.gsh-len          LE e-item-vend.roll-w[30]
      NO-LOCK:

    CREATE ttEstVendorGet.
    ASSIGN
     ttEstVendorGet.key-01  = e-item-vend.vend-no
     ttEstVendorGet.rec-id  = STRING(RECID(e-item-vend)).
  END.
END.

FOR EACH ttEstVendorGet BREAK BY ttEstVendorGet.key-01:

  IF FIRST-OF(ttEstVendorGet.key-01) THEN v-forms[2] = 0.

  v-forms[2] = v-forms[2] + 1.

  IF NOT LAST-OF(ttEstVendorGet.key-01) OR v-forms[1] NE v-forms[2] AND ttEstVendorGet.key-01 <> ""  THEN
     DELETE ttEstVendorGet.
END.

adder-blok:
FOR EACH ttEstVendorGet.

  FOR EACH ef
      WHERE ef.company EQ est.company
        AND ef.est-no  EQ est.est-no
        AND CAN-FIND(FIRST eb OF ef WHERE NOT eb.pur-man)
      NO-LOCK:
    DO li = 1 TO 6:
      IF ef.adder[li] NE "" AND
         NOT CAN-FIND(FIRST e-item-vend
                      WHERE e-item-vend.company EQ cocode
                        AND e-item-vend.i-no    EQ ef.adder[li]
                        AND e-item-vend.vend-no EQ ttEstVendorGet.key-01) 
                        AND ttEstVendorGet.key-01 <> ""   THEN DO:
          
        DELETE ttEstVendorGet.
        NEXT adder-blok.
      END.
    END.
  END.
END.

END PROCEDURE.


