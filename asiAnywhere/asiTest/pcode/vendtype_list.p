
/*------------------------------------------------------------------------
    File        : vendtype_list.p
    Purpose     : Sales Tax Code
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendTypeList NO-UNDO
    FIELD vtype      AS CHAR
    FIELD vdscr      AS CHAR
    FIELD reckey     AS CHAR
    FIELD extra      AS CHAR 
    .

DEFINE DATASET dsVendTypeList FOR ttVendTypeList.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvtype    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvdscr    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHAR  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendTypeList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmvtype         = ?  THEN ASSIGN prmvtype     = "".
IF prmvdscr         = ?  THEN ASSIGN prmvdscr     = "".
                                                  


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.

         
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  cocode  = prmComp .



IF prmAction = "GridSelect" THEN DO:
    FOR EACH ventype WHERE ventype.company = prmComp NO-LOCK:
        CREATE ttVendTypeList.
        ASSIGN
            ttVendTypeList.vtype = ventype.type
            ttVendTypeList.vdscr = ventype.Dscr
            ttVendTypeList.reckey = ventype.rec_key  . 
    END.
                                        
                                        
     
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "GridSearch" THEN DO:
    FOR EACH ventype WHERE ventype.company = prmComp
           AND (ventype.TYPE BEGINS prmvtype OR prmvtype = "")
           AND (ventype.Dscr BEGINS prmvdscr OR prmvdscr = "") NO-LOCK:
        CREATE ttVendTypeList.
        ASSIGN
            ttVendTypeList.vtype = ventype.type
            ttVendTypeList.vdscr = ventype.Dscr
            ttVendTypeList.reckey = ventype.rec_key  . 
    END.

 END.

 

 IF prmAction = "Update" THEN DO:
     FIND FIRST ventype WHERE ventype.company = prmComp
         AND ventype.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.


    IF AVAIL ventype THEN
        ASSIGN
            ventype.Dscr    = prmvdscr . 
            ASSIGN
                prmAction = "View" .

 END.

 
 IF prmAction = "Add" THEN DO:

     CREATE ventype.

     ASSIGN
            ventype.company = prmComp
            ventype.type    = prmvtype 
            ventype.Dscr    = prmvdscr .

     ASSIGN
         prmReckey = ventype.rec_key
         prmAction = "View" .

 END.

 IF prmAction = "Delete" THEN DO:
    FIND FIRST ventype WHERE ventype.company = prmComp
         AND ventype.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL ventype THEN
         DELETE ventype.

    FIND LAST ventype WHERE ventype.company = prmComp NO-LOCK NO-ERROR.
     IF AVAIL ventype THEN
         ASSIGN
         prmAction = "View"
         prmReckey = ventype.rec_key.

 END.

 
 IF prmAction = "View" THEN DO:
     FIND FIRST ventype WHERE ventype.company = prmComp
         AND ventype.rec_key = prmReckey NO-LOCK NO-ERROR.

     CREATE ttVendTypeList.
        ASSIGN
             ttVendTypeList.vtype = ventype.type
             ttVendTypeList.vdscr = ventype.Dscr
             ttVendTypeList.reckey = ventype.rec_key  . 


 END.


