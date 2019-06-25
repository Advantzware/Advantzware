
/*------------------------------------------------------------------------
    File        : ItemColor.p
    Purpose     : ItemColors

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemColor.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsColor.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEFINE STREAM s1.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum  = ? THEN ASSIGN prmItemNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR lv-pr-types AS CHAR INIT "FGLO" NO-UNDO.
DEF VAR lv-pr-list AS CHAR INIT ",Flexo,Gravure,Letterpress,Offset" NO-UNDO.
DEF VAR lv-cover% AS INT NO-UNDO.
DEFINE VARIABLE fi_press-type AS CHARACTER FORMAT "X(15)" no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
IF prmAction = "Select" THEN DO:

FOR EACH itemfg-ink where itemfg-ink.company EQ prmComp AND
     itemfg-ink.i-no = prmItemNum no-lock:

      create ttcolor.
      assign  
         
          ttcolor.rm-i-no     = itemfg-ink.rm-i-no
          ttcolor.Dscr        = itemfg-ink.dscr               
          ttcolor.pass        = itemfg-ink.pass
          ttcolor.cover       = itemfg-ink.cover%
         .
      IF  itemfg-ink.in-out  = TRUE THEN
          ASSIGN
             ttcolor.in-out        = "In".
      ELSE 
          ASSIGN
             ttcolor.in-out       = "Out".
  MESSAGE "tt" ttcolor.rm-i-no. 
  IF AVAIL itemfg-ink THEN DO:
    FIND FIRST reftable WHERE reftable.rec_key  = itemfg-ink.rec_key AND
       reftable.reftable = "itemfg-ink.occurs" AND
       reftable.company  = itemfg-ink.company NO-ERROR.
    /*IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
       reftable.rec_key  = itemfg-ink.rec_key
       reftable.reftable = "itemfg-ink.occurs"
       reftable.company  = itemfg-ink.company
       reftable.val[1]   = 1.
    END.*/
          
    
    IF  AVAIL reftable THEN
        ASSIGN
            ttcolor.Occurs = reftable.val[1].
  END.

 FIND FIRST item
          WHERE item.company EQ itemfg-ink.company
            AND item.i-no    EQ itemfg-ink.rm-i-no
          NO-LOCK NO-ERROR.
    fi_press-type = IF AVAIL item THEN
                      ENTRY(INDEX(lv-pr-types,item.press-type) + 1,lv-pr-list)
                    ELSE "".

     
      
          IF fi_press-type = "Flexo" THEN ASSIGN ttcolor.press-type  = "F".
          ELSE IF fi_press-type = "Gravure" THEN ASSIGN ttcolor.press-type  = "G".
          ELSE IF fi_press-type = "Letterpress" THEN ASSIGN ttcolor.press-type  = "L".
          ELSE IF fi_press-type = "Offset" THEN ASSIGN ttcolor.press-type  = "O".
           
END.

/*need to fix below, this can't be right here...*/

FOR EACH  eb Where eb.company = itemfg-ink.company no-lock :
    assign         
        /*ttcolor.unit2    = eb.unit2*/
        ttcolor.i-ps2    = eb.i-ps2[1]
        ttcolor.i-code2  = eb.i-code2[1]
        ttcolor.i-dscr2  = eb.i-dscr2[1]
        ttcolor.iper2    = eb.i-%2[1] 
        ttcolor.unit     = eb.i-col
        ttcolor.i-ps     = eb.i-ps[1]
        ttcolor.i-code   = eb.i-code[1]
        ttcolor.i-dscr   = eb.i-dscr[1]
        ttcolor.i-per    = eb.i-%[1]
        .
END. /*IF available eb*/
       
  
END.
/**/

