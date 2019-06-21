
/*------------------------------------------------------------------------
    File        : payterms.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE TEMP-TABLE ttPaymentTerms NO-UNDO
      FIELD   tcode         AS CHAR        
      FIELD   dscr          AS CHAR
      FIELD   disc_rate     AS DEC
      FIELD   disc_dys      AS INT
      FIELD   net_dys       AS INT
      FIELD   CUT_date      AS INT
      FIELD   vtype         AS CHAR
      FIELD   cod           AS CHAR
      FIELD   extra         AS CHAR
      FIELD   vreckey       AS CHAR
      . 

DEFINE DATASET dsPaymentTerms FOR ttPaymentTerms.

DEFINE INPUT PARAMETER prmAction           AS CHAR NO-UNDO.    
DEFINE INPUT PARAMETER prmUser             AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReckey           AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmtcode            AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmdscr             AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmdisc_rate        AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmdisc_dys         AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmnet_dys          AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmCUT_date         AS INT    NO-UNDO.
DEFINE INPUT PARAMETER prmvtype            AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcod              AS CHAR   NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPaymentTerms .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
 DEF VAR prmComp AS CHAR NO-UNDO.

 
     IF prmAction             = ? THEN ASSIGN prmAction            = "Select".
     IF prmUser               = ? THEN ASSIGN prmUser              = "".
     IF prmReckey             = ? THEN ASSIGN prmReckey            = "".
     IF prmtcode              = ? THEN ASSIGN prmtcode             = "".
     IF prmdscr               = ? THEN ASSIGN prmdscr              = "".
     IF prmdisc_rate          = ? THEN ASSIGN prmdisc_rate         = 0.
     IF prmdisc_dys           = ? THEN ASSIGN prmdisc_dys          = 0.
     IF prmnet_dys            = ? THEN ASSIGN prmnet_dys           = 0.
     IF prmCUT_date           = ? THEN ASSIGN prmCUT_date          = 0.
     IF prmvtype              = ? THEN ASSIGN prmvtype             = "".
     IF prmcod                = ? THEN ASSIGN prmcod               = "".
     
     
   DEFINE VARIABLE termsCOD AS LOGICAL INITIAL no  NO-UNDO.
     DEFINE VARIABLE saveTermsCOD AS LOGICAL NO-UNDO.

     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  g_company = prmComp.



   IF prmAction = "GridSelect"  THEN DO:

       FOR EACH terms WHERE terms.company = prmComp NO-LOCK:


           CREATE  ttPaymentTerms.
               ASSIGN 
                    ttPaymentTerms.tcode            = terms.t-code 
                    ttPaymentTerms.dscr             = terms.dscr
                    ttPaymentTerms.disc_rate        = terms.disc-rate         
                    ttPaymentTerms.disc_dys         = terms.disc-days        
                    ttPaymentTerms.net_dys          = terms.net-days         
                    ttPaymentTerms.CUT_date         = terms.cut-date         
                    ttPaymentTerms.vtype            = terms.type 
                   ttPaymentTerms.vreckey          = terms.rec_key       .
                
       END.



   END. /*IF prmAction = "View" THEN DO:*/


   IF prmAction = "Search"  THEN DO:

    FOR EACH terms WHERE terms.company = prmComp 
        AND (terms.t-code BEGINS prmtcode OR prmtcode = "")
        AND (terms.dscr BEGINS prmdscr OR prmdscr = "" ) NO-LOCK :
    

        CREATE  ttPaymentTerms.
            ASSIGN 
                 ttPaymentTerms.tcode            = terms.t-code 
                 ttPaymentTerms.dscr             = terms.dscr
                 ttPaymentTerms.disc_rate        = terms.disc-rate         
                 ttPaymentTerms.disc_dys         = terms.disc-days        
                 ttPaymentTerms.net_dys          = terms.net-days         
                 ttPaymentTerms.CUT_date         = terms.cut-date         
                 ttPaymentTerms.vtype            = terms.type    
                 ttPaymentTerms.vreckey          = terms.rec_key     .
                          
    END.

    

END. /*IF prmAction = "View" THEN DO:*/

IF prmAction = "Add" THEN DO:
 prmvtype = CAPS(prmvtype).
IF LOOKUP(TRIM(prmvtype),",P,S") LE 0 THEN DO:
      cError =  "Type" + " " + "may be  P, or S..." .
      RETURN .
    END.    
END.


IF prmAction = "Add" THEN DO:

    
    CREATE  terms.
            ASSIGN 
                terms.company           = prmComp
                 terms.t-code           = prmtcode      
                 terms.dscr             = prmdscr       
                 terms.disc-rate        = prmdisc_rate      
                 terms.disc-days        = prmdisc_dys      
                 terms.net-days         = prmnet_dys       
                 terms.cut-date         = prmCUT_date      
                 terms.type             = prmvtype  . 

            ASSIGN termsCOD = IF prmcod = "1"  THEN YES ELSE NO.

             /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST reftable EXCLUSIVE-LOCK
       WHERE reftable.reftable EQ 'terms.cod'
         AND reftable.company EQ terms.company
         AND reftable.loc EQ ''
         AND reftable.code EQ terms.t-code NO-ERROR.
  IF NOT AVAILABLE reftable THEN DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'terms.cod'
      reftable.company = terms.company
      reftable.code = terms.t-code.
  END.
  ASSIGN
    reftable.val[1] = INT(termsCOD).
  FIND CURRENT reftable NO-LOCK.

  ASSIGN
      prmAction = "View"
      prmReckey = terms.rec_key .

END.

IF prmAction = "Update" THEN DO:
 prmvtype = CAPS(prmvtype).
IF LOOKUP(TRIM(prmvtype),",P,S") LE 0 THEN DO:
      cError =  "Type" + " " + "may be  P, or S..." .
      RETURN .
    END.    
END.
    
IF prmAction = "Update" THEN DO:

    FIND FIRST terms WHERE terms.company = prmComp AND terms.rec_key = prmReckey
                                        EXCLUSIVE-LOCK NO-ERROR.
    

        CREATE  ttPaymentTerms.
            ASSIGN 
                terms.dscr             = prmdscr      
                terms.disc-rate        = prmdisc_rate         
                terms.disc-days        = prmdisc_dys         
                terms.net-days         = prmnet_dys          
                terms.cut-date         = prmCUT_date         
                terms.type             = prmvtype  .    
                  
            ASSIGN termsCOD = IF prmcod = "1"  THEN YES ELSE NO.
            
  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST reftable EXCLUSIVE-LOCK
       WHERE reftable.reftable EQ 'terms.cod'
         AND reftable.company EQ terms.company
         AND reftable.loc EQ ''
         AND reftable.code EQ terms.t-code NO-ERROR.
  IF NOT AVAILABLE reftable THEN DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'terms.cod'
      reftable.company = terms.company
      reftable.code = terms.t-code.
  END.
  ASSIGN
    reftable.val[1] = INT(termsCOD).
  FIND CURRENT reftable NO-LOCK.

  ASSIGN
      prmAction = "View " .
                
  
END. /*IF prmAction = "Update" THEN DO:*/

IF prmAction = "Delete" THEN DO:
    FIND FIRST terms WHERE terms.company = prmComp AND terms.rec_key = prmReckey
                                        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL terms THEN 
        DELETE terms.

    FIND LAST terms WHERE terms.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL terms THEN ASSIGN
        prmAction = "View"
        prmReckey = terms.rec_key .
END. /*IF prmAction = "Delete" THEN DO:*/



IF prmAction = "View"  THEN DO:

    FIND FIRST terms WHERE terms.company = prmComp AND terms.rec_key = prmReckey
        NO-LOCK NO-ERROR.
    

        CREATE  ttPaymentTerms.
            ASSIGN 
                 ttPaymentTerms.tcode            = terms.t-code 
                 ttPaymentTerms.dscr             = terms.dscr
                 ttPaymentTerms.disc_rate        = terms.disc-rate         
                 ttPaymentTerms.disc_dys         = terms.disc-days        
                 ttPaymentTerms.net_dys          = terms.net-days         
                 ttPaymentTerms.CUT_date         = terms.cut-date         
                 ttPaymentTerms.vtype            = terms.type 
                 ttPaymentTerms.vreckey          = terms.rec_key       .

            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ 'terms.cod'
                AND reftable.company EQ terms.company
                AND reftable.loc EQ ''
                AND reftable.code EQ terms.t-code NO-ERROR.
            IF NOT AVAILABLE reftable THEN DO:
                CREATE reftable.
                ASSIGN
                    reftable.reftable = 'terms.cod'
                    reftable.company = terms.company
                    reftable.code = terms.t-code.
            END.

          
          /*  termsCOD = AVAIL reftable AND reftable.val[1] EQ 1.*/
            ASSIGN ttPaymentTerms.cod = string(reftable.val[1]) .
               
END. /*IF prmAction = "View" THEN DO:*/   
