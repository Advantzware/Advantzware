


/*------------------------------------------------------------------------
    File        : GetQty1.p
    Purpose     : Get Default Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttGetDefqty NO-UNDO
        FIELD lv-qty-1  AS INTEGER
        FIELD lv-qty-2  AS INTEGER
        FIELD lv-qty-3  AS INTEGER
        FIELD lv-qty-4  AS INTEGER
        FIELD lv-qty-5  AS INTEGER
        FIELD lv-qty-6  AS INTEGER
        FIELD lv-qty-7  AS INTEGER
        FIELD lv-qty-8  AS INTEGER
        FIELD lv-qty-9  AS INTEGER
        FIELD lv-qty-10 AS INTEGER
        FIELD lv-qty-11 AS INTEGER
        FIELD lv-qty-12 AS INTEGER
        FIELD lv-qty-13 AS INTEGER
        FIELD lv-qty-14 AS INTEGER
        FIELD lv-qty-15 AS INTEGER
        FIELD lv-qty-16 AS INTEGER
        FIELD lv-qty-17 AS INTEGER
        FIELD lv-qty-18 AS INTEGER
        FIELD lv-qty-19 AS INTEGER
        FIELD lv-qty-20 AS INTEGER
        FIELD lv-qty-21 AS INTEGER
        FIELD lv-qty-22 AS INTEGER
        FIELD lv-qty-23 AS INTEGER
        FIELD lv-qty-24 AS INTEGER
        FIELD lv-qty-25 AS INTEGER
        FIELD lv-qty-26 AS INTEGER
        FIELD lv-qty-27 AS INTEGER
        FIELD lv-qty-28 AS INTEGER        
        FIELD lv-rels-1 AS INTEGER
        FIELD lv-rels-2 AS INTEGER
        FIELD lv-rels-3 AS INTEGER
        FIELD lv-rels-4 AS INTEGER
        FIELD lv-rels-5 AS INTEGER
        FIELD lv-rels-6 AS INTEGER
        FIELD lv-rels-7 AS INTEGER
        FIELD lv-rels-8 AS INTEGER
        FIELD lv-rels-9 AS INTEGER
        FIELD lv-rels-10 AS INTEGER
        FIELD lv-rels-11 AS INTEGER
        FIELD lv-rels-12 AS INTEGER
        FIELD lv-rels-13 AS INTEGER
        FIELD lv-rels-14 AS INTEGER
        FIELD lv-rels-15 AS INTEGER
        FIELD lv-rels-16 AS INTEGER
        FIELD lv-rels-17 AS INTEGER
        FIELD lv-rels-18 AS INTEGER
        FIELD lv-rels-19 AS INTEGER
        FIELD lv-rels-20 AS INTEGER
        FIELD lv-rels-21 AS INTEGER
        FIELD lv-rels-22 AS INTEGER
        FIELD lv-rels-23 AS INTEGER
        FIELD lv-rels-24 AS INTEGER
        FIELD lv-rels-25 AS INTEGER
        FIELD lv-rels-26 AS INTEGER
        FIELD lv-rels-27 AS INTEGER
        FIELD lv-rels-28 AS INTEGER
        
        FIELD lv-match-up AS DECIMAL
        FIELD v-do-gsa AS LOGICAL
        FIELD v-do-mr AS LOGICAL
        FIELD v-do-speed AS LOGICAL
        FIELD v-drop-rc AS LOGICAL
        FIELD v-ink-all-forms AS LOGICAL               
        .
   
       
       


    DEFINE DATASET dsGetDefqty FOR ttGetDefqty .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.  

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetDefqty.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?  THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".    
    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
                 

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".



IF prmAction = "GetDefaultQty" THEN DO:
    /*find est-qty where recid(est-qty) = ip-recid no-lock no-error.*/
   find est-qty where est-qty.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est-qty.company = prmComp no-lock no-error.
   if avail est-qty then do:    
      find est where est.company = est-qty.company and
                     est.est-no = est-qty.est-no
                     no-lock no-error.

      CREATE ttGetDefqty.            

      ASSIGN ttGetDefqty.lv-qty-1  = est-qty.qty[1]
             ttGetDefqty.lv-qty-2  = est-qty.qty[2]
             ttGetDefqty.lv-qty-3  = est-qty.qty[3]
             ttGetDefqty.lv-qty-4  = est-qty.qty[4]
             ttGetDefqty.lv-qty-5  = est-qty.qty[5]
             ttGetDefqty.lv-qty-6  = est-qty.qty[6]
             ttGetDefqty.lv-qty-7  = est-qty.qty[7]
             ttGetDefqty.lv-qty-8  = est-qty.qty[8]
             ttGetDefqty.lv-qty-9  = est-qty.qty[9]
             ttGetDefqty.lv-qty-10 = est-qty.qty[10]
             ttGetDefqty.lv-qty-11 = est-qty.qty[11]
             ttGetDefqty.lv-qty-12 = est-qty.qty[12]
             ttGetDefqty.lv-qty-13 = est-qty.qty[13]
             ttGetDefqty.lv-qty-14 = est-qty.qty[14]
             ttGetDefqty.lv-qty-15 = est-qty.qty[15]
             ttGetDefqty.lv-qty-16 = est-qty.qty[16]
             ttGetDefqty.lv-qty-17 = est-qty.qty[17]
             ttGetDefqty.lv-qty-18 = est-qty.qty[18]
             ttGetDefqty.lv-qty-19 = est-qty.qty[19]
             ttGetDefqty.lv-qty-20 = est-qty.qty[20].
                     
      ASSIGN ttGetDefqty.lv-rels-1  = est-qty.qty[21]
             ttGetDefqty.lv-rels-2  = est-qty.qty[22]
             ttGetDefqty.lv-rels-3  = est-qty.qty[23]
             ttGetDefqty.lv-rels-4  = est-qty.qty[24]
             ttGetDefqty.lv-rels-5  = est-qty.qty[25]
             ttGetDefqty.lv-rels-6  = est-qty.qty[26]
             ttGetDefqty.lv-rels-7  = est-qty.qty[27]
             ttGetDefqty.lv-rels-8  = est-qty.qty[28]
             ttGetDefqty.lv-rels-9  = est-qty.qty[29]
             ttGetDefqty.lv-rels-10 = est-qty.qty[30]
             ttGetDefqty.lv-rels-11 = est-qty.qty[31]
             ttGetDefqty.lv-rels-12 = est-qty.qty[32]
             ttGetDefqty.lv-rels-13 = est-qty.qty[33]
             ttGetDefqty.lv-rels-14 = est-qty.qty[34]
             ttGetDefqty.lv-rels-15 = est-qty.qty[35]
             ttGetDefqty.lv-rels-16 = est-qty.qty[36]
             ttGetDefqty.lv-rels-17 = est-qty.qty[37]
             ttGetDefqty.lv-rels-18 = est-qty.qty[38]
             ttGetDefqty.lv-rels-19 = est-qty.qty[39]
             ttGetDefqty.lv-rels-20 = est-qty.qty[40].                                   
   end.
END.

