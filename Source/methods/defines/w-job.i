/* w-job.i - rstark - 10.22.2021 */

DEFINE NEW SHARED TEMP-TABLE w-job NO-UNDO
    FIELD job-no-disp AS CHARACTER
    FIELD job-no      LIKE job-hdr.job-no
    FIELD job-no2     LIKE job-hdr.job-no2
    FIELD po-no       LIKE fg-bin.po-no
    FIELD i-no        LIKE job-hdr.i-no
    FIELD j-no        LIKE job-hdr.j-no
    FIELD loc         LIKE fg-bin.loc
    FIELD loc-desc      AS CHARACTER FORMAT "x(20)"
    FIELD loc-bin     LIKE fg-bin.loc-bin
    FIELD tag         LIKE fg-bin.tag
    FIELD lead-days   LIKE itemfg-loc.lead-days FORMAT ">>>"
    FIELD ord-level   LIKE itemfg-loc.ord-level FORMAT ">>>,>>>,>>>.<<<"
    FIELD ord-max     LIKE itemfg-loc.ord-max   FORMAT ">>>,>>>,>>>.<<"
    FIELD ord-min     LIKE itemfg-loc.ord-min   FORMAT ">>>,>>>,>>>.<<<"
    FIELD onHand        AS INTEGER FORMAT "->>,>>>,>>9" LABEL "OnHand"
    FIELD onHoldQty     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "OnHold"
    FIELD onOrder       AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Jobs/POs"
    FIELD allocated     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Allocated"
    FIELD backOrder     AS INTEGER FORMAT "->>,>>>,>>9" LABEL "BackOrder"
    FIELD qtyAvailable  AS INTEGER FORMAT "->>,>>>,>>9" LABEL "Available"
    FIELD beg-date      AS DATE    FORMAT "99/99/9999"  LABEL "BeginDate"
        INDEX w-job
            job-no
            job-no2
            loc
            loc-bin
            tag
            .
