
/*------------------------------------------------------------------------
    File        : ListCorrPrep.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttListCorrPrep NO-UNDO
    
    FIELD vSnum       AS INT
    FIELD vBnum       AS INT
    FIELD vCode       AS CHARACTER
    FIELD vQty        AS DECIMAL
    FIELD vDesc       AS CHAR
    FIELD vCost       AS DECIMAL
    FIELD vMl         AS CHAR
    FIELD vSimon      AS CHAR
    FIELD vMark       AS DECIMAL
    FIELD vAmort      AS DECIMAL
    FIELD vLine       AS INT
    FIELD hfjfgd      AS CHAR 
        .
DEFINE DATASET dsListCorrPrep FOR ttListCorrPrep.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActSelect   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsListCorrPrep.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.



{est/d-machex.i NEW}

DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.
 DEFINE VAR vEstimate AS CHAR NO-UNDO.
 def buffer bf-prep for est-prep.
 def var li-next as int no-undo.

 
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  

IF prmActSelect = "Select" THEN DO:
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST eb WHERE eb.est-no =  vEstimate AND eb.company = prmComp AND eb.form-no = 1 NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ vEstimate
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  
  FOR EACH est-prep WHERE est-prep.company = est.company AND est-prep.est-no = est.est-no NO-LOCK :
      IF AVAIL est-prep THEN DO:
           CREATE ttListCorrPrep.
           ASSIGN
              ttListCorrPrep.vSnum         = est-prep.s-num
              ttListCorrPrep.vBnum         = est-prep.b-num
              ttListCorrPrep.vCode         = est-prep.CODE
              ttListCorrPrep.vQty          = est-prep.qty
              ttListCorrPrep.vDesc         = est-prep.dscr
              ttListCorrPrep.vCost         = est-prep.cost
              ttListCorrPrep.vMl           = IF  est-prep.ml = TRUE THEN "M" ELSE "L"
              ttListCorrPrep.vSimon        = est-prep.simon
              ttListCorrPrep.vMark         = est-prep.mkup
              ttListCorrPrep.vAmort        = est-prep.amtz 
              ttListCorrPrep.vLine         = est-prep.LINE .
             
                END. 

  END. /* end of for each*/
 END. /* end of select*/
