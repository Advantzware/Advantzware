

/*------------------------------------------------------------------------
    File        : RfqItemDscr.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : mon Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttRfqItemDscr NO-UNDO
    FIELD aRfqNo LIKE rfq.rfq-no
    FIELD aCustNo LIKE rfq.cust-no 
    FIELD vRfqDt  LIKE rfq.req-date 
    FIELD vRfqPart LIKE rfqitem.part-no
    FIELD vRfqstyle LIKE rfqitem.style
    FIELD vRfqstyleDscr LIKE style.dscr
    .
DEFINE DATASET dsRfqItemDscr FOR ttRfqItemDscr.

DEFINE INPUT PARAMETER prmUser    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo   AS INT  NO-UNDO.
DEFINE INPUT PARAMETER PrmPartNo  AS CHAR  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqItemDscr.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmUser = ? THEN prmUser = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

    FIND FIRST rfq WHERE
         rfq.company EQ prmComp AND
         rfq.loc EQ prmLoc AND
         rfq.rfq-no = PrmRfqNo 
         NO-LOCK.

    FIND FIRST rfqitem WHERE
         rfqitem.company EQ prmComp AND
         rfqitem.loc EQ prmLoc AND
         rfqitem.rfq-no = rfq.rfq-no AND
         rfqitem.part-no = PrmPartNo
         NO-LOCK NO-ERROR.

    IF AVAILABLE rfq THEN DO:
        CREATE ttRfqItemDscr.
        ASSIGN
            ttRfqItemDscr.aRfqNo = rfq.rfq-no
            ttRfqItemDscr.aCustNo = rfq.cust-no 
            ttRfqItemDscr.vRfqDt = rfq.req-date  
            ttRfqItemDscr.vRfqPart =  rfqitem.part-no
            ttRfqItemDscr.vRfqstyle = rfqitem.style.

        find style where
             style.company = rfqitem.company and
             style.style = rfqitem.style
             no-lock no-error.
  
        if avail style then
           ttRfqItemDscr.vRfqstyleDscr = style.dscr.
    END.
    

