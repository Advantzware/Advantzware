
/*------------------------------------------------------------------------
    File        : CostTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Tue Jan 22 00:06:51 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE            VARIABLE hdCostProcs AS HANDLE.
DEFINE NEW SHARED VARIABLE cocode      AS CHARACTER NO-UNDO INIT '001'.

DEFINE STREAM sOutput.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

DEFINE VARIABLE dResultNew      AS DECIMAL.
DEFINE VARIABLE dResultOld      AS DECIMAL.

DEFINE VARIABLE cFromUOM        AS CHARACTER INIT "LF".
DEFINE VARIABLE cToUOM          AS CHARACTER INIT "MSF".
DEFINE VARIABLE dBasis          AS DECIMAL   INIT .24.
DEFINE VARIABLE dLen            AS DECIMAL   INIT 12.
DEFINE VARIABLE dWid            AS DECIMAL   INIT 22.
DEFINE VARIABLE dDep            AS DECIMAL   INIT 2.
DEFINE VARIABLE dValueToConvert AS DECIMAL   INIT 100.
DEFINE VARIABLE dCostPerUOM     AS DECIMAL.
DEFINE VARIABLE cCostUOM        AS CHARACTER.
DEFINE VARIABLE dCostFreight    AS DECIMAL.
DEFINE VARIABLE dCost           AS DECIMAL   EXTENT 6.
DEFINE VARIABLE cCostSource     AS CHARACTER.
DEFINE VARIABLE cOutputFile     AS CHARACTER INIT "C:\Temp\CostTesterOutput.csv".
DEFINE VARIABLE iTimer          AS INTEGER.
DEFINE VARIABLE iCountTotal     AS INTEGER.
DEFINE VARIABLE iCountDiff      AS INTEGER.
DEFINE VARIABLE lFound          AS LOGICAL.

OUTPUT STREAM sOutput TO VALUE(cOutputFile).
/*dResultNew = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs,cFromUOM,cToUOM,dBasis,dLen,dWid,dDep,dValueToConvert).*/
/*RUN rm/convcuom.p(cFromUOM, cToUOM, dBasis, dLen, dWid, dDep, dValueToConvert, OUTPUT dResultOld).            */
/*MESSAGE "New: " dResultNew SKIP                                                                               */
/*"Old: " dResultOld                                                                                            */
/*VIEW-AS ALERT-BOX.                                                                                            */

/*RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 1, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight, OUTPUT lFound).*/
/*                                                                                                                  */
/*MESSAGE 1 "Cost: " dCostPerUOM SKIP                                                                               */
/*    "UOM: " cCostUOM SKIP                                                                                         */
/*    "Cost Per UOM Freight:" dCostFreight                                                                          */
/*    VIEW-AS ALERT-BOX .                                                                                           */
/*                                                                                                                  */
/*RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 2, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight, OUTPUT lFound).*/
/*                                                                                                                  */
/*MESSAGE 2 "Cost: " dCostPerUOM SKIP                                                                               */
/*    "UOM: " cCostUOM SKIP                                                                                         */
/*    "Cost Per UOM Freight:" dCostFreight                                                                          */
/*    VIEW-AS ALERT-BOX .                                                                                           */
/*                                                                                                                  */
/*RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 3, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight, OUTPUT lFound).*/
/*                                                                                                                  */
/*MESSAGE 3 "Cost: " dCostPerUOM SKIP                                                                               */
/*    "UOM: " cCostUOM SKIP                                                                                         */
/*    "Cost Per UOM Freight:" dCostFreight                                                                          */
/*    VIEW-AS ALERT-BOX .                                                                                           */
/*                                                                                                                  */
/*FIND FIRST inv-head NO-LOCK                                                                                       */
/*    WHERE inv-head.company EQ '001'                                                                               */
/*    AND inv-head.inv-no EQ 8418                                                                                   */
/*    NO-ERROR.                                                                                                     */
/*                                                                                                                  */
/*FIND FIRST inv-line NO-LOCK                                                                                       */
/*    WHERE inv-line.company EQ inv-head.company                                                                    */
/*    AND inv-line.r-no EQ inv-head.r-no                                                                            */
/*    NO-ERROR.                                                                                                     */
/*                                                                                                                  */
/*RUN oe/GetCostInvl.p (ROWID(inv-line),                                                                            */
/*    OUTPUT dCost[1], OUTPUT dCost[2],                                                                             */
/*    OUTPUT dCost[3], OUTPUT dCost[4],                                                                             */
/*    OUTPUT dCost[5], OUTPUT cCostUOM,                                                                             */
/*    OUTPUT dCost[6], OUTPUT cCostSource).                                                                         */
/*                                                                                                                  */
/*MESSAGE 4 "Invoice Line Costs: " inv-head.inv-no SKIP                                                             */
/*    1 dCost[1] SKIP                                                                                               */
/*    2 dCost[2] SKIP                                                                                               */
/*    3 dCost[3] SKIP                                                                                               */
/*    4 dCost[4] SKIP                                                                                               */
/*    5 dCost[5] SKIP                                                                                               */
/*    6 dCost[6] SKIP                                                                                               */
/*    "UOM: " cCostUOM SKIP                                                                                         */
/*    "Source:" cCostSource                                                                                         */
/*    VIEW-AS ALERT-BOX .                                                                                           */

EXPORT STREAM sOutput DELIMITER "," 
    "ItemID" 
    "Rita"
    "Tag"
    "Whse"
    "Bin"
    "Job"
    "Job2"
    "PO"
    "Po line"
    "Qty"
    "Old Cost"
    "Old Cost UOM"
    "New Cost"
    "New Cost UOM"
    "New Cost Source"
    .
iTimer = TIME.
FOR EACH itemfg NO-LOCK
    WHERE itemfg.company EQ '001'
    AND itemfg.i-no    GE ''
    AND itemfg.i-no    LE 'ZZZZZZZZZZZ'
    AND itemfg.stat EQ 'A' :

    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company EQ itemfg.company
        AND fg-rcpth.i-no    EQ itemfg.i-no
        USE-INDEX i-no,
        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND INDEX("A",fg-rdtlh.rita-code) EQ 0
        USE-INDEX rm-rdtl

        BREAK BY INT(fg-rcpth.rita-code NE "R")
        BY INT(fg-rcpth.rita-code NE "C")
        BY fg-rcpth.trans-date
        BY fg-rdtlh.trans-time
        BY fg-rcpth.r-no
        BY fg-rdtlh.rec_key

        TRANSACTION:
        ASSIGN 
            iCountTotal = iCountTotal + 1
            dCost[1] = 0
            dCost[2] = 0
            dCost[3] = 0
            dCost[4] = 0
            dCost[5] = 0
            dCost[6] = 0
            .
        
        IF fg-rcpth.rita-code NE "R" OR fg-rcpth.job-no NE "" OR fg-rcpth.po-no NE "" THEN DO: 
            RUN GetCostForFGItemHist IN hdCostProcs (fg-rcpth.company, fg-rcpth.i-no, fg-rcpth.job-no, fg-rcpth.job-no2, fg-rcpth.po-no, fg-rcpth.po-line, fg-rdtlh.tag, fg-rcpth.rita-code,
                OUTPUT dCost[1], OUTPUT dCost[4], OUTPUT dCost[3], OUTPUT dCost[2], OUTPUT dCost[5], OUTPUT dCost[6], OUTPUT cCostUOM, OUTPUT cCostSource, OUTPUT lFound).
    
            IF ROUND(dCost[5],2) NE ROUND(fg-rdtlh.cost,2) THEN 
            DO:
                iCountDiff = iCountDiff + 1.
                EXPORT STREAM sOutput DELIMITER ","
                    fg-rcpth.i-no
                    fg-rcpth.rita-code
                    fg-rdtlh.tag
                    fg-rdtlh.loc
                    fg-rdtlh.loc-bin
                    fg-rcpth.job-no
                    fg-rcpth.job-no2
                    fg-rcpth.po-no
                    fg-rcpth.po-line
                    fg-rdtlh.qty
                    fg-rdtlh.cost
                    fg-rcpth.pur-uom
                    dCost[5]
                    cCostUOM
                    cCostSource.
            END.
        END.
    /*        MESSAGE 5 "History Costs: " itemfg.i-no SKIP*/
    /*            1 dCost[1] SKIP                         */
    /*            2 dCost[2] SKIP                         */
    /*            3 dCost[3] SKIP                         */
    /*            4 dCost[4] SKIP                         */
    /*            5 dCost[5] SKIP                         */
    /*            6 dCost[6] SKIP                         */
    /*            "UOM: " cCostUOM SKIP                   */
    /*            "Source:" cCostSource                   */
    /*            VIEW-AS ALERT-BOX .                     */
    END.
END.
MESSAGE "Records: " iCountTotal SKIP 
    "Differences: " iCountDiff iCountDiff / iCountTotal * 100 "%" SKIP 
    "Total time" TIME - iTimer 
    VIEW-AS ALERT-BOX.
