
/*------------------------------------------------------------------------
    File        BinItem.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ItemJoB Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE w-job NO-UNDO 
  BEFORE-TABLE beforeItemJob
  field job-no-disp as char
  field job-no like job-hdr.job-no
  field job-no2 like job-hdr.job-no2
  field i-no like job-hdr.i-no
  field j-no like job-hdr.j-no
  field loc like fg-bin.loc
  field loc-bin like fg-bin.loc-bin
  field tag like fg-bin.tag
  FIELD cust-no LIKE fg-bin.cust-no
  FIELD cases AS INT
  field case-count like fg-bin.case-count
  field cases-unit like fg-bin.cases-unit
  field qty as int format "->>>,>>9"
  field std-tot-cost like  job-hdr.std-tot-cost
  field std-mat-cost like  job-hdr.std-mat-cost
  field std-lab-cost like  job-hdr.std-lab-cost
  field std-var-cost like  job-hdr.std-var-cost
  field std-fix-cost like  job-hdr.std-fix-cost
  field last-cost like fg-bin.last-cost
  field sell-uom like itemfg.sell-uom
  FIELD partial-count LIKE fg-bin.partial-count
  INDEX w-jobs job-no job-no2 loc loc-bin tag.

DEFINE TEMP-TABLE w-jobs NO-UNDO 
 
  field job-no-disp as char
  field job-no like job-hdr.job-no
  field job-no2 like job-hdr.job-no2
  field i-no like job-hdr.i-no
  field j-no like job-hdr.j-no
  field loc like fg-bin.loc
  field loc-bin like fg-bin.loc-bin
  field tag like fg-bin.tag
  FIELD cust-no LIKE fg-bin.cust-no
  FIELD cases AS INT
  field case-count like fg-bin.case-count
  field cases-unit like fg-bin.cases-unit
  field qty as int format "->>>,>>9"
  field std-tot-cost like  job-hdr.std-tot-cost
  field std-mat-cost like  job-hdr.std-mat-cost
  field std-lab-cost like  job-hdr.std-lab-cost
  field std-var-cost like  job-hdr.std-var-cost
  field std-fix-cost like  job-hdr.std-fix-cost
  field last-cost like fg-bin.last-cost
  field sell-uom like itemfg.sell-uom
  FIELD partial-count LIKE fg-bin.partial-count
  INDEX w-jobs job-no job-no2 loc loc-bin tag.

DEFINE DATASET dsItemJob FOR w-job .

DEFINE QUERY q-ItemJobQuery FOR w-job.

DEFINE DATA-SOURCE src-ItemJob  FOR QUERY q-ItemJobQuery.

BUFFER w-job :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemJob  :HANDLE).
