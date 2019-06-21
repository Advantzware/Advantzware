
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
{ItemColor2.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmRaw    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDscr   AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsColor.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.


IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum  = ? THEN ASSIGN prmItemNum  = "".
IF prmRaw      = ? THEN ASSIGN prmRaw      = "".
IF prmDscr     = ? THEN ASSIGN prmDscr     = "".
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    when "search" then do:
  FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  no-lock: 
for each itemfg where itemfg.i-no = oe-ordl.i-no AND itemfg.i-no = prmItemNum NO-LOCK :
FOR EACH itemfg-ink where itemfg-ink.i-no = itemfg.i-no AND (itemfg-ink.rm-i-no begins prmRaw or prmRaw = "" )
                    and (itemfg-ink.dscr begins prmDscr or prmDscr = "" ) no-lock:
                    create ttcolor .
                     BUFFER-COPY itemfg-ink TO ttcolor .
 End.
End. 
  
WHEN "delete" THEN DO:
    OUTPUT STREAM s1 TO color.txt.
    FOR EACH beforecolor TRANSACTION:
        ASSIGN v-return-value = BUFFER beforecolor:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
    END.
    IF NOT v-return-value THEN 
        DO:
        EXPORT STREAM s1 ERROR-STATUS:GET-NUMBER(1) ":" ERROR-STATUS:GET-MESSAGE(1).
        END.
        ELSE DO:
            EXPORT STREAM s1 "Deleted.".    
        END.
        OUTPUT STREAM s1 CLOSE.
    END.
    WHEN "update" THEN DO:
        FOR EACH beforecolor TRANSACTION:
            ASSIGN v-return-value = BUFFER beforecolor:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforecolor TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforecolor:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  no-lock: 
for each itemfg where itemfg.i-no = oe-ordl.i-no AND itemfg.i-no = prmItemNum NO-LOCK :
FOR EACH itemfg-ink where itemfg-ink.i-no = itemfg.i-no no-lock:

      create ttcolor.
      assign  
         
          ttcolor.rm-i-no     = itemfg-ink.rm-i-no
          ttcolor.Dscr        = itemfg-ink.dscr               
          ttcolor.pass        = itemfg-ink.pass
          /*ttcolor.in-out      = itemfg-ink.in-out*/
          ttcolor.cover       = itemfg-ink.cover%
         .
 IF  itemfg-ink.in-out  = TRUE THEN DO:
            ASSIGN
                ttcolor.in-out        = "In".

        END.
        ELSE DO:
            ASSIGN
                ttcolor.in-out       = "Out".


  
  END.
END.
FIND FIRST reftable WHERE reftable.rec_key  = itemfg-ink.rec_key AND
       reftable.reftable = "itemfg-ink.occurs" AND
       reftable.company  = itemfg-ink.company NO-ERROR.
    IF  AVAIL reftable THEN DO:
        ASSIGN
            ttcolor.Occurs = reftable.val[1].
    END.

FOR EACH ITEM NO-LOCK :
     
      assign  
          ttcolor.press-type  = ITEM.press-type.
END.
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
       
   
 
END.   /*FOR EACH Itemfg*/
END.   /*FOR EACH oe-ordl*/

END PROCEDURE.
