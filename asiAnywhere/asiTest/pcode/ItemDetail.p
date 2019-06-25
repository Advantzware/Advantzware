
/*------------------------------------------------------------------------
    File        : ItemDetail.p
    Purpose     : ItemDetail

    Syntax      :

    Description : Return a Dataset of all Item Inquiry

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemDetail.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Part-no AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemDetail.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
       RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
        
        ASSIGN
            v-qry-handle = QUERY q-itemfgQuery:HANDLE.
                
        v-qry-handle:QUERY-PREPARE(v-qry-string).
        
        DATASET dsItemDetail:FILL().
   

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    
        ASSIGN
            prm-query = "FOR EACH itemfg where itemfg.company eq " + QUOTER(prmComp)
                      + " AND itemfg.part-no = " + QUOTER(Part-no) + " NO-LOCK ".

                                               
END PROCEDURE.
