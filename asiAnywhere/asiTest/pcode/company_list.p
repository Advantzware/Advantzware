
/*------------------------------------------------------------------------
    File        : company_list.p
    Purpose     : Customer

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttcompanylistview NO-UNDO
    FIELD   company     AS CHAR
    FIELD   fid         AS CHAR 
    FIELD   vname       AS CHAR
    FIELD   sid         AS CHAR 
    FIELD   addr1       AS CHAR
    FIELD   addr2       AS CHAR
    FIELD   city        AS CHAR 
    FIELD   state       AS CHAR 
    FIELD   zip         AS CHAR 
    FIELD   co-acc      AS CHAR 
    FIELD   num-per     AS CHAR 
    FIELD   acc-level   AS CHAR 
    FIELD   acc-dig1    AS CHAR 
    FIELD   acc-dig2    AS CHAR 
    FIELD   acc-dig3    AS CHAR 
    FIELD   acc-dig4    AS CHAR 
    FIELD   acc-dig5    AS CHAR 
    FIELD   yend-off    AS CHAR 
    FIELD   curr-code   AS CHAR 
    FIELD   yend-per    AS CHAR 
    FIELD   firstyear   AS CHAR 
    FIELD   prdnum      AS CHAR 
    FIELD   prddt1      AS CHAR 
    FIELD   prddt2      AS CHAR 
    FIELD   cdesc       AS CHAR 
    FIELD   reckey     AS CHAR .
                     
  

DEFINE DATASET dscompanylistview FOR ttcompanylistview.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCompany  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmfid      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvname    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmsid      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaddr1    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaddr2    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcity     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmstate    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmzip      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcoacc    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmnumper   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmacclevel AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaccdig1  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaccdig2  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaccdig3  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaccdig4  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmaccdig5  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmyendoff  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcurrcode AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmyendper  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmfirstyear AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmprdnum    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmprddt1    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmprddt2    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcdesc     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscompanylistview.
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.

     FOR EACH ttcompanylistview:
        DELETE ttcompanylistview .
    END.

     IF prmAction       = ?  THEN ASSIGN prmAction      = "Select".
     IF prmCompany      = ?  THEN ASSIGN prmCompany     = "".
     IF prmComp         = ?  THEN ASSIGN prmComp        = "".
     IF prmUser         = ?  THEN ASSIGN prmUser        = "".
     IF prmfid          = ?  THEN ASSIGN prmfid         = "".
     IF prmvname        = ?  THEN ASSIGN prmvname       = "".
     IF prmsid          = ?  THEN ASSIGN prmsid         = "".
     IF prmaddr1        = ?  THEN ASSIGN prmaddr1       = "".
     IF prmaddr2        = ?  THEN ASSIGN prmaddr2       = "".
     IF prmcity         = ?  THEN ASSIGN prmcity        = "".
     IF prmstate        = ?  THEN ASSIGN prmstate       = "".
     IF prmzip          = ?  THEN ASSIGN prmzip         = "".
     IF prmcoacc        = ?  THEN ASSIGN prmcoacc       = "".
     IF prmnumper       = ?  THEN ASSIGN prmnumper      = "".
     IF prmacclevel     = ?  THEN ASSIGN prmacclevel    = "".
     IF prmaccdig1      = ?  THEN ASSIGN prmaccdig1     = "".
     IF prmaccdig2      = ?  THEN ASSIGN prmaccdig2     = "".
     IF prmaccdig3      = ?  THEN ASSIGN prmaccdig3     = "".
     IF prmaccdig4      = ?  THEN ASSIGN prmaccdig4     = "".
     IF prmaccdig5      = ?  THEN ASSIGN prmaccdig5     = "".
     IF prmyendoff      = ?  THEN ASSIGN prmyendoff     = "".
     IF prmcurrcode     = ?  THEN ASSIGN prmcurrcode    = "".
     IF prmyendper      = ?  THEN ASSIGN prmyendper     = "".
     IF prmfirstyear    = ?  THEN ASSIGN prmfirstyear   = "".
     IF prmprdnum       = ?  THEN ASSIGN prmprdnum      = "".
     IF prmprddt1       = ?  THEN ASSIGN prmprddt1      = "".
     IF prmprddt2       = ?  THEN ASSIGN prmprddt2      = "".
     IF prmcdesc        = ?  THEN ASSIGN prmcdesc       = "".
                                                    

       DEF BUFFER bf-account FOR account.
        DEF BUFFER bf-company FOR company.
    
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

       



     IF prmAction = "Search" THEN DO:
         FOR EACH company WHERE (company.company = prmCompany OR prmCompany = "")
                            AND (company.name  =  prmvname OR prmvname = "") 
                        
                        NO-LOCK:
             CREATE ttcompanylistview.
             ASSIGN 
                 ttcompanylistview.company   = company.company 
                 ttcompanylistview.vNAME      = company.name 
                 ttcompanylistview.fid       = company.fid 
                 ttcompanylistview.sid       = company.sid   
                 ttcompanylistview.reckey    = company.rec_key .
            

      END. /*FOR EACH buff-cust  */
END. /*IF prmAction = "Search" THEN DO:*/

IF prmAction = "updaterec" THEN DO:
 FIND FIRST company WHERE company.rec_key = prmReckey NO-LOCK NO-ERROR.
    prmcurrcode = CAPS(prmcurrcode).

    FIND FIRST currency
                    WHERE currency.company EQ company.company
        AND currency.c-code  EQ prmcurrcode NO-LOCK NO-ERROR.
      IF NOT AVAIL currency THEN do:
      cError = "Currency code is invalid, try help..." .
        RETURN .
      END.

   IF prmcoacc NE ""  AND
       prmcoacc NE prmCompany THEN DO:
      FIND FIRST bf-company
          WHERE bf-company.company EQ prmcoacc NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-company THEN DO:
        cError =  "Invalid Account, try help..." .
        RETURN .
      END.
   END.

END. /* end of validate update */

IF prmAction = "updaterec" THEN DO:
    FIND FIRST company WHERE company.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
          company.fid           =  prmfid      
          company.name          =  prmvname    
          company.sid           =  prmsid      
          company.addr[1]       =  prmaddr1    
          company.addr[2]       =  prmaddr2    
          company.city          =  prmcity     
          company.state         =  prmstate    
          company.zip           =  prmzip      
          /*company.co-acc        =  prmcoacc    */
          company.num-per       =  int(prmnumper)   
          company.acc-level     =  INT(prmacclevel)
          company.acc-dig[1]    =  int(prmaccdig1)
          company.acc-dig[2]    =  int(prmaccdig2)
          company.acc-dig[3]    =  int(prmaccdig3)  
          company.acc-dig[4]    =  int(prmaccdig4) 
          company.acc-dig[5]    =  int(prmaccdig5)
          company.yend-off      =  int(prmyendoff)
          company.curr-code     =  prmcurrcode   .

        FIND FIRST account WHERE account.company EQ company.company NO-LOCK NO-ERROR.

  IF NOT AVAIL account AND company.co-acc NE "" THEN
  FOR EACH account WHERE account.company EQ company.co-acc NO-LOCK:
    CREATE bf-account.
    BUFFER-COPY account TO bf-account
      ASSIGN
       bf-account.company = company.company.
  END.

  ASSIGN
    prmAction = "View" .

END. /* end of update*/

MESSAGE "testinghhh " prmCompany prmAction  .
IF prmAction = "addnewrec" THEN DO:

    FIND FIRST company WHERE  company.company = prmCompany NO-LOCK NO-ERROR.
    IF AVAIL  company  THEN DO:
        cError = "Company already exists with Company " + prmCompany + "...." .
        RETURN.
    END.

     prmcurrcode = CAPS(prmcurrcode).

    FIND FIRST currency
                    WHERE /*currency.company EQ company.company:SCREEN-VALUE
                      AND*/ currency.c-code  EQ prmcurrcode NO-LOCK NO-ERROR.
      IF NOT AVAIL currency THEN do:
      cError = "Currency code is invalid, try help..." .
        RETURN .
      END.

   IF prmcoacc NE ""  AND
       prmcoacc NE prmCompany THEN DO:
      FIND FIRST bf-company
          WHERE bf-company.company EQ prmcoacc NO-LOCK NO-ERROR.
      IF NOT AVAIL bf-company THEN DO:
        cError =  "Invalid Account, try help..." .
        RETURN .
      END.
   END.

 END.  /* end of validate add*/

IF prmAction = "addnewrec" THEN DO:

    CREATE company .
    ASSIGN
          company.company       =  prmCompany          
          company.fid           =  prmfid      
          company.name          =  prmvname    
          company.sid           =  prmsid      
          company.addr[1]       =  prmaddr1    
          company.addr[2]       =  prmaddr2    
          company.city          =  prmcity     
          company.state         =  prmstate    
          company.zip           =  prmzip      
          company.co-acc        =  prmcoacc    
          company.num-per       =  int(prmnumper)
          company.acc-level     =  INT(prmacclevel)
          company.acc-dig[1]    =  int(prmaccdig1)
          company.acc-dig[2]    =  int(prmaccdig2)
          company.acc-dig[3]    =  int(prmaccdig3)  
          company.acc-dig[4]    =  int(prmaccdig4) 
          company.acc-dig[5]    =  int(prmaccdig5)
          company.yend-off      =  int(prmyendoff)
          company.curr-code     =  prmcurrcode  .
          /*company.yend-per      =  log(prmyendper  ) .*/
          /*      =  prmfirstyear
                                =  prmprdnum   
                                =  prmprddt1   
                                  prmprddt2   
                                  prmcdesc    
                                  prmReckey   */

    RUN create-controls.

  FIND FIRST account WHERE account.company EQ company.company NO-LOCK NO-ERROR.

  IF NOT AVAIL account AND company.co-acc NE "" THEN
  FOR EACH account WHERE account.company EQ company.co-acc NO-LOCK:
    CREATE bf-account.
    BUFFER-COPY account TO bf-account
      ASSIGN
       bf-account.company = company.company.
  END.

  ASSIGN prmAction = "View" 
         prmReckey = company.rec_key .

END.  /* end of add new rec*/


IF prmAction ="Delete" THEN DO:
       FIND FIRST company WHERE company.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL company  THEN 
           DELETE company .
       
       FIND LAST company .
       IF AVAIL company THEN 
           ASSIGN prmAction = "View"
                  prmReckey = company.rec_key .

END.  /* delete record */

IF prmAction = "View" THEN DO:
    FIND FIRST company WHERE company.rec_key = prmReckey  NO-LOCK NO-ERROR.
        CREATE ttcompanylistview.
           
              ASSIGN 
                 ttcompanylistview.company       = company.company 
                 ttcompanylistview.fid           = company.fid
                 ttcompanylistview.vname         = company.name
                 ttcompanylistview.sid           = company.sid  
                 ttcompanylistview.addr1         = company.addr[1]
                 ttcompanylistview.addr2         = company.addr[2]  
                 ttcompanylistview.city          = company.city
                 ttcompanylistview.state         = company.state 
                 ttcompanylistview.zip           = company.zip
                 ttcompanylistview.co-acc        =   company.co-acc
                 ttcompanylistview.num-per       = string(company.num-per) 
                 ttcompanylistview.acc-level     = string(company.acc-level)
                 ttcompanylistview.acc-dig1      = string(company.acc-dig[1]) 
                 ttcompanylistview.acc-dig2      = string(company.acc-dig[2])
                 ttcompanylistview.acc-dig3      = string(company.acc-dig[3]) 
                 ttcompanylistview.acc-dig4      = string(company.acc-dig[4])
                 ttcompanylistview.acc-dig5      = string(company.acc-dig[5])
                 ttcompanylistview.yend-off      = string(company.yend-off )
                 ttcompanylistview.curr-code    = company.curr-code 
                 ttcompanylistview.yend-per      = string(company.yend-per)
                 ttcompanylistview.reckey        = company.rec_key   
               .

            IF AVAIL company THEN
                FIND FIRST currency WHERE currency.company = company.company
                AND currency.c-code = company.curr-code
                NO-LOCK NO-ERROR.

            IF AVAIL currency THEN ttcompanylistview.cdesc = currency.c-desc.
            
            FIND FIRST period WHERE period.company = company.company AND
                period.pstat NO-LOCK NO-ERROR.
            IF AVAIL period THEN ASSIGN ttcompanylistview.firstyear = STRING(period.yr)
                ttcompanylistview.prdnum = STRING(period.pnum)
                ttcompanylistview.prddt1 = STRING(period.pst)
                ttcompanylistview.prddt2 = STRING(period.pend).
                
    
    
  
END. /*IF prmAction = "View" THEN DO:*/




PROCEDURE create-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF NOT AVAIL company  THEN RETURN.
 
 find first gl-ctrl where gl-ctrl.company = company.company no-lock no-error.
 if not available gl-ctrl then do:
                create gl-ctrl. gl-ctrl.company = company.company.
 end.
 find first rm-ctrl where rm-ctrl.company = company.company no-lock no-error.
 if not available rm-ctrl then do:
                        create rm-ctrl. rm-ctrl.company = company.company.
 end.
 find first fg-ctrl where fg-ctrl.company = company.company no-lock no-error.
 if not available fg-ctrl then do:
                        create fg-ctrl. fg-ctrl.company = company.company.
 end.
 find first ap-ctrl where ap-ctrl.company = company.company no-lock no-error.
 if not available ap-ctrl then do:
                        create ap-ctrl. ap-ctrl.company = company.company.
 end.
 find first ar-ctrl where ar-ctrl.company = company.company no-lock no-error.
 if not available ar-ctrl then do:
                create ar-ctrl. ar-ctrl.company = company.company.
 end.
 find first oe-ctrl where oe-ctrl.company = company.company no-lock no-error.
 if not available oe-ctrl then do:
               create oe-ctrl. oe-ctrl.company = company.company.
 end.

 create loc.
 assign loc.company = company.company
        loc.loc     = "Main"
        loc.dscr    = "Main".
 for each loc where loc.company = company.company:
         find first ce-ctrl where ce-ctrl.company = company.company and
                                  ce-ctrl.loc = loc.loc
                                                  no-lock no-error.
     if not available ce-ctrl then do:
                    create ce-ctrl.
                    assign
                    ce-ctrl.company = company.company
                    ce-ctrl.loc     = loc.loc.
         end.
 end.
 
END PROCEDURE.
