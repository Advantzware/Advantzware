
/*------------------------------------------------------------------------
    File        : ttFGBins.i
    Purpose     : 

    Syntax      :

    Description : Holds temp-table definition for ttFGBins

    Author(s)   : BV
    Created     : Thu Feb 14 13:55:21 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttFGBins NO-UNDO
    FIELD company      LIKE fg-bin.company
    FIELD job-no       LIKE fg-bin.job-no
    FIELD job-no2      LIKE fg-bin.job-no2
    FIELD loc          LIKE fg-bin.loc
    FIELD loc-bin      LIKE fg-bin.loc-bin
    FIELD tag          LIKE fg-bin.tag
    FIELD cust-no      LIKE fg-bin.cust-no
    FIELD i-no         LIKE fg-bin.i-no
    FIELD po-no        LIKE fg-bin.po-no
    FIELD aging-date   LIKE fg-bin.aging-date
    FIELD pur-uom      LIKE fg-bin.pur-uom
    FIELD std-tot-cost LIKE fg-bin.std-tot-cost
    FIELD std-mat-cost LIKE fg-bin.std-mat-cost
    FIELD std-lab-cost LIKE fg-bin.std-lab-cost
    FIELD std-var-cost LIKE fg-bin.std-var-cost
    FIELD std-fix-cost LIKE fg-bin.std-fix-cost
    FIELD case-count   LIKE fg-bin.case-count
    FIELD units-pallet LIKE fg-bin.units-pallet
    FIELD cases-unit   LIKE fg-bin.cases-unit
    FIELD qty          LIKE fg-bin.qty
    FIELD rita-code    LIKE fg-rcpth.rita-code
    FIELD itemFGCustNo LIKE itemfg.cust-no
    FIELD itemFGINo    LIKE itemfg.i-no
    FIELD itemFGPartNO LIKE itemfg.part-no
    FIELD itemfgIName  LIKE itemfg.i-name
    FIELD uomMult      LIKE uom.mult
        INDEX tt-fg-bin IS PRIMARY
            company
            i-no
            job-no
            job-no2
            loc
            loc-bin
            tag
            cust-no
        INDEX tagRitaCode
            tag
            rita-code
            .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
