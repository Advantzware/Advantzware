/*------------------------------------------------------------------------
    File        : w-Jobs.i
    Purpose     : 

    Syntax      :

    Description : Temp-table defintion for w-jobs to display data
                  in I-F-1 bin/jobs browse

    Author(s)   : Rahul Rawat
    Created     : Tue Apr 07 01:26:25 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {1} TEMP-TABLE w-job NO-UNDO
  FIELD job-no-disp          AS CHARACTER
  FIELD job-no               LIKE job-hdr.job-no
  FIELD job-no2              LIKE job-hdr.job-no2
  FIELD po-no                LIKE fg-bin.po-no
  FIELD i-no                 LIKE job-hdr.i-no
  FIELD j-no                 LIKE job-hdr.j-no
  FIELD loc                  LIKE fg-bin.loc
  FIELD loc-bin              LIKE fg-bin.loc-bin
  FIELD tag                  LIKE fg-bin.tag
  FIELD cust-no              LIKE fg-bin.cust-no
  FIELD cases                AS INTEGER
  FIELD case-count           LIKE fg-bin.case-count
  FIELD cases-unit           LIKE fg-bin.cases-unit
  FIELD qty                  AS INTEGER FORMAT "->>>,>>9"
  FIELD std-tot-cost         LIKE job-hdr.std-tot-cost
  FIELD std-mat-cost         LIKE job-hdr.std-mat-cost
  FIELD std-lab-cost         LIKE job-hdr.std-lab-cost
  FIELD std-var-cost         LIKE job-hdr.std-var-cost
  FIELD std-fix-cost         LIKE job-hdr.std-fix-cost
  FIELD last-cost            LIKE fg-bin.last-cost
  FIELD sell-uom             LIKE itemfg.sell-uom
  FIELD partial-count        LIKE fg-bin.partial-count
  FIELD rel-qty AS           INTEGER FORMAT "->>>,>>9"
  FIELD bol-qty AS           INTEGER FORMAT "->>>,>>9"
  FIELD avl-qty AS           INTEGER FORMAT "->>>,>>9"
  FIELD tot-wt               LIKE fg-bin.tot-wt
  FIELD tagStatusID          AS CHARACTER FORMAT "X(32)"
  FIELD tagStatusDescription AS CHARACTER FORMAT "X(4)"
  FIELD onHold               AS LOGICAL 
  INDEX w-job job-no job-no2 loc loc-bin tag.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
