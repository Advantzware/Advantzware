/*------------------------------------------------------------------------
    File        : orderenq.p
    Purpose     : Order Enquiry Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Jyoti Bajaj
    Created     : Sat August 25 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{dsOrdEnq1.i}
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPonum     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartno    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPartdesc  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmFrmdate   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmTodate    AS CHARACTER  NO-UNDO.
/*DEFINE INPUT PARAMETER prmPartDisp  AS CHARACTER  NO-UNDO.*/


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR onhandqty AS INTEGER.
DEFINE VAR quotedate AS DATE.
DEFINE VAR showadd AS LOGICAL INIT FALSE.
DEFINE VAR custx AS CHARACTER.
DEFINE VAR rowout AS CHARACTER.
DEFINE BUFFER bcust FOR cust.

FIND FIRST cust where cust.active = "x" NO-LOCK NO-ERROR.
IF AVAILABLE cust THEN ASSIGN custx = cust.cust-no.

FOR EACH oe-ordl where oe-ordl.po-no = string(prmPonum) and 
                       oe-ordl.part-no = string(prmPartnum) and
                       oe-ordl.req-date = date(prmTodate)
                       oe-ordl.part-dscr1 =  prmPartdesc and no-lock:

FIND FIRST oe-rel WHERE oe-rel.company = oe-ordl.company AND 
                        oe-rel.ord-no = oe-ordl.ord-no  and
                        oe-rel.i-no = oe-ordl.i-no no-lock no-error.
FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN DO:
FIND FIRST oe-ord WHERE oe-ord.company = oe-ordl.company AND oe-ord.cust-no = oe-ordl.cust-no AND oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.
          FOR EACH fg-bin WHERE fg-bin.company = oe-ordl.company AND fg-bin.i-no = oe-ordl.i-no  AND fg-bin.job-no = string(oe-ordl.job-no)  NO-LOCK:
              ASSIGN onhandqty = onhandqty + fg-bin.qty.
          END.
FOR EACH quoteitm WHERE quoteitm.part-no = oe-ordl.part-no NO-LOCK:
            FIND FIRST quotehd WHERE quotehd.company = cust.company
                                 AND quotehd.loc = quoteitm.loc
                                 AND quotehd.q-no = quoteitm.q-no
                                 AND quotehd.quo-date > quotedate  NO-LOCK NO-ERROR.
            END.
        
    END.
    IF AVAILABLE oe-rel THEN DO:
  FIND FIRST oe-relh
      where oe-relh.company  eq oe-rel.company
        and oe-relh.ord-no   eq oe-rel.ord-no
        and oe-relh.rel-no   eq oe-rel.rel-no
        and oe-relh.b-ord-no eq oe-rel.b-ord-no
        and oe-relh.cust-no  eq oe-rel.cust-no
        and oe-relh.posted   eq no
        and oe-relh.deleted  eq no
      use-index order NO-LOCK NO-ERROR.
          IF NOT AVAILABLE oe-relh THEN DO:
                  IF (TODAY + (5 * 365)) < oe-rel.rel-date THEN DO:
                            rowout = "Not Scheduled".
                  END.
                  ELSE DO:
                    rowout = string(oe-rel.rel-date).
                  END.
          END.
          ELSE DO:
                  IF (TODAY + (5 * 365)) < oe-relh.rel-date THEN DO:
                            rowout = "Not Scheduled".
                  END.
                  ELSE DO:
                             rowout = string(oe-relh.rel-date).
                  END.
          END.
          create ttOrder
                      BUFFER-COPY oe-ord TO ttOrder
                                        
                      ASSIGN
                      

      END.
END.

