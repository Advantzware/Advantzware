    

/*------------------------------------------------------------------------
    File        : viewqrygl.p
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewQueryGeneralLedgerhist NO-UNDO
        FIELD trnum     AS INT
        FIELD actnum    AS CHAR
        FIELD jrnl      AS CHAR
        FIELD trdscr    AS CHAR
        FIELD tramt     AS DEC
        FIELD trdate    AS CHAR
        FIELD invoice   AS CHAR
        .

DEFINE DATASET dsViewQueryGeneralLedgerhist FOR ttViewQueryGeneralLedgerhist .

DEFINE INPUT PARAMETER prmUser            AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmAction          AS CHAR         NO-UNDO.
DEFINE INPUT PARAMETER prmtrnum           AS INT           NO-UNDO.
DEFINE INPUT PARAMETER prmactnum          AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER prmjrnl            AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER prmtrdscr          AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER prmtramt           AS DEC           NO-UNDO.
DEFINE INPUT PARAMETER prmtrdate          AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey          AS CHAR         NO-UNDO.
DEF OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewQueryGeneralLedgerhist.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR  g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE addersText AS CHARACTER NO-UNDO.

IF prmUser         = ? THEN ASSIGN prmUser             = "".
IF prmAction       = ? THEN ASSIGN prmAction           = "View".
IF prmtrnum        = ? THEN ASSIGN prmtrnum            = 0.
IF prmactnum       = ? THEN ASSIGN prmactnum           = "".
IF prmjrnl         = ? THEN ASSIGN prmjrnl             = "".  
IF prmtrdscr       = ? THEN ASSIGN prmtrdscr           = "".
IF prmtramt        = ? THEN ASSIGN prmtramt            = 0.
IF prmtrdate       = ? THEN ASSIGN prmtrdate           = "".
IF prmRecKey       = ? THEN ASSIGN prmRecKey           = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp 
    g_company = prmComp
    g_loc    = "Main" .

DEF TEMP-TABLE tt-glhist NO-UNDO LIKE glhist.

{sa/sa-sls01.i}
  
    MESSAGE "tesdtttt " prmAction prmtrnum .
 RUN build-inquiry.



 IF prmAction = "Search" THEN DO:

     
    FOR EACH tt-glhist NO-LOCK BY tt-glhist.actnum: 
        
            create ttViewQueryGeneralLedgerhist.
            assign
                ttViewQueryGeneralLedgerhist.trnum     = tt-glhist.tr-num
                ttViewQueryGeneralLedgerhist.actnum    = tt-glhist.actnum
                ttViewQueryGeneralLedgerhist.jrnl      = tt-glhist.jrnl
                ttViewQueryGeneralLedgerhist.trdscr    = tt-glhist.tr-dscr
                ttViewQueryGeneralLedgerhist.tramt     = tt-glhist.tr-amt
                ttViewQueryGeneralLedgerhist.trdate    = string(tt-glhist.tr-date) .
                    
              FIND  FIRST report  WHERE report.rec_key EQ tt-glhist.rec_key
                            AND report.term-id EQ v-term   
                            AND report.rec-id  EQ RECID(tt-glhist) NO-LOCK NO-ERROR. 
              IF AVAIL report  THEN DO:
                ttViewQueryGeneralLedgerhist.invoice   = report.key-01 . 
              END.

     END.
             
   
 END. /* end search */

 PROCEDURE build-inquiry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-company LIKE glhist.company NO-UNDO.
  DEF VAR lv-tr-num  LIKE glhist.tr-num  NO-UNDO.
  DEF VAR lv-inv-no AS CHAR NO-UNDO.
  DEF VAR li-inv-no AS INT NO-UNDO.


/*  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'run-no-source':U,OUTPUT char-hdl).*/
  /*DO li = 1 TO NUM-ENTRIES(char-hdl):
    IF VALID-HANDLE(WIDGET-HANDLE(ENTRY(li,char-hdl))) THEN
      RUN get-fields IN WIDGET-HANDLE(ENTRY(li,char-hdl)) (OUTPUT lv-company,
                                                           OUTPUT lv-tr-num). 
  END.*/

  EMPTY TEMP-TABLE tt-glhist.

  FOR EACH report WHERE report.term-id EQ v-term:
    DELETE report.
  END.
  MESSAGE "prmtrnum " prmtrnum v-term .
  FOR EACH glhist NO-LOCK
      WHERE glhist.company EQ cocode
        AND glhist.tr-num  EQ prmtrnum:
    CREATE tt-glhist.
    BUFFER-COPY glhist TO tt-glhist.
    
  END.

  FOR EACH gltrans NO-LOCK
      WHERE gltrans.company EQ cocode
        AND gltrans.trnum   EQ prmtrnum:
    CREATE tt-glhist.
    BUFFER-COPY gltrans TO tt-glhist
    ASSIGN
     tt-glhist.tr-num = gltrans.trnum.
    
  END.

  FOR EACH tt-glhist WHERE tt-glhist.tr-dscr MATCHES "*Inv# *":
    lv-inv-no = TRIM(SUBSTR(tt-glhist.tr-dscr,INDEX(tt-glhist.tr-dscr,"Inv# ") + 5,8)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN lv-inv-no = "".

    li-inv-no = INT(lv-inv-no) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN li-inv-no = 0.

    IF li-inv-no NE 0                              AND
       CAN-FIND(FIRST ar-inv
                WHERE ar-inv.company EQ cocode
                  AND ar-inv.inv-no  EQ li-inv-no) THEN DO:
      CREATE report.
      ASSIGN
       report.term-id = v-term
       report.rec-id  = RECID(tt-glhist)
       report.rec_key = tt-glhist.rec_key
       report.key-01  = lv-inv-no.
    END.
  END.
 
END PROCEDURE.

