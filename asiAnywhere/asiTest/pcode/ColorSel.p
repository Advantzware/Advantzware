/*------------------------------------------------------------------------
    File        : ColSel.p
    Purpose     : ItemCOlSel

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ColorSel.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmRawNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsColSel.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmItemNum  = ? THEN ASSIGN prmItemNum = "".
IF prmRawNum = ? THEN ASSIGN prmRawNum = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".

DEF VAR lv-pr-types AS CHAR INIT "FGLO" NO-UNDO.
DEF VAR lv-pr-list AS CHAR INIT ",Flexo,Gravure,Letterpress,Offset" NO-UNDO.
DEF VAR lv-cover% AS INT NO-UNDO.
DEFINE VARIABLE fi_press-type AS CHARACTER FORMAT "X(15)" no-undo.
/* ********************  Preprocessor Definitions  ******************** */
/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "select" THEN DO:
   FIND FIRST itemfg-ink where
           itemfg-ink.company EQ prmComp AND
           itemfg-ink.i-no = prmItemNum   AND itemfg-ink.rm-i-no = prmRawNum
           NO-LOCK NO-ERROR.

      IF AVAIL itemfg-ink THEN
      DO:
         create ttColSel.
         assign  
         ttColSel.rm-no     = itemfg-ink.rm-i-no
         ttColSel.Dscr        = itemfg-ink.dscr               
         ttColSel.pass        = itemfg-ink.pass
         ttColSel.cover       = itemfg-ink.cover%.
         IF itemfg-ink.in-out  = TRUE THEN
            ttColSel.in-out        = "In".
         ELSE 
            ttColSel.in-out       = "Out".
            
            IF AVAIL itemfg-ink THEN DO:
                FIND FIRST reftable WHERE reftable.rec_key  = itemfg-ink.rec_key AND
                    reftable.reftable = "itemfg-ink.occurs" AND
                    reftable.company  = itemfg-ink.company NO-ERROR.
                IF NOT AVAIL reftable THEN DO:
                    CREATE reftable.
                    ASSIGN
                        reftable.rec_key  = itemfg-ink.rec_key
                        reftable.reftable = "itemfg-ink.occurs"
                        reftable.company  = itemfg-ink.company
                        reftable.val[1]   = 1.
                END.
                IF  AVAIL reftable THEN
                    ASSIGN
                    ttColSel.Occurs= reftable.val[1].
                END.
                
                FIND FIRST item
                    WHERE item.company EQ itemfg-ink.company
                    AND item.i-no    EQ itemfg-ink.rm-i-no
                    NO-LOCK NO-ERROR.
                fi_press-type = IF AVAIL item THEN
                      ENTRY(INDEX(lv-pr-types,item.press-type) + 1,lv-pr-list)
                    ELSE "".
                         
                        ASSIGN  ttColSel.press  = fi_press-type.




      END.
   END.
