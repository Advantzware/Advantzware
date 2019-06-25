


/*------------------------------------------------------------------------
    File        : TrailerLook.p(copy of browsers/l-truck.w)
    Purpose     : trailer

    Syntax      :

    Description : Return a Dataset of all trailer

    Author(s)   : Jyoti
    Created     : Mar 06 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTrailerLook NO-UNDO 
    FIELD vTruckCode   AS CHARACTER FORMAT "X(20)"     
    FIELD vTruckDesc   AS CHARACTER FORMAT "X(25)"
    FIELD vLoc   AS CHARACTER FORMAT "x(5)"
    FIELD vCarrier   AS CHARACTER FORMAT "x(5)"
    FIELD vLengthInches   AS INT FORMAT ">>9"
    FIELD vWidthInches   AS INT FORMAT ">>9"
    FIELD vHeightInches   AS INT FORMAT ">>9"
    .

DEFINE DATASET dsTrailerLook FOR ttTrailerLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmTrailer   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRelease   AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTrailerLook .

DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
       
IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmTrailer   = ? THEN ASSIGN prmTrailer   = "".
IF prmRelease   = ? THEN ASSIGN prmRelease   = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


/*------------------------------------------------------------------------------------------------------------------*/

IF prmAction = "TrailerLeave" THEN DO:
    IF TRIM(prmTrailer) EQ "" OR NOT CAN-FIND(FIRST truck WHERE truck.company EQ prmComp AND
           truck.truck EQ prmTrailer) THEN
       DO:          
          cError = "Invalid Trailer#.".
          RETURN.
       END.

    FIND FIRST oe-relh WHERE
         oe-relh.company  EQ prmComp AND
         oe-relh.release# EQ INT(prmRelease)
         NO-LOCK NO-ERROR.

    IF AVAIL oe-relh THEN DO:
       IF TRIM(oe-relh.trailer) NE "" AND
          oe-relh.trailer NE prmTrailer THEN DO:

           cError = "Trailer # does not match Release Trailer#.".
           RETURN.

          /*ll = NO.
          IF NOT g-sharpshooter THEN
             MESSAGE "Trailer # does not match Release Trailer#.  Is this the Correct Trailer#?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.
          ELSE
          DO:
             RUN custom/d-msg.w ("Question","","Is this the Correct Trailer#?","",2,"YES,NO", OUTPUT v-msgreturn).
             IF v-msgreturn = 1 THEN ll = YES.
          END.
          IF NOT ll THEN
             RETURN NO-APPLY.
          */
       END.
    END.
END.


/*------------------------------------------------------------------------------*/
    
if prmAction <> "search" then do:
    FOR EACH truck WHERE truck.company eq prmComp  NO-LOCK :
        create ttTrailerLook.
            assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches
                .             
    END.	 /* FOR EACH truck */         
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:     
    if prmField = "truckcode"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.truck-code = prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = EQUAL */
         if prmCondition = "BEGIN" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.truck-code BEGINS prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = BEGIN */ 
     end.  /* if prmField = truckcode  */

     if prmField = "description"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.truck-desc = prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = EQUAL */
         if prmCondition = "BEGIN" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.truck-desc BEGINS prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = BEGIN */ 
     end.  /* if prmField = description  */

     if prmField = "location"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.loc = prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = EQUAL */
         if prmCondition = "BEGIN" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.loc BEGINS prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = BEGIN */ 
     end.  /* if prmField = location  */

     if prmField = "carrier"  then do:
        if prmCondition = "EQUAL" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.carrier = prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = EQUAL */
         if prmCondition = "BEGIN" then do:             
             FOR EACH truck WHERE  truck.company eq prmComp AND truck.carrier BEGINS prmText NO-LOCK:
                create ttTrailerLook.
                assign                                         
                ttTrailerLook.vTruckCode        =  truck.truck-code 
                ttTrailerLook.vTruckDesc        =  truck.truck-desc                 
                ttTrailerLook.vLoc              =  truck.loc               
                ttTrailerLook.vCarrier          =  truck.carrier
                ttTrailerLook.vLengthInches     =  truck.length-inches
                ttTrailerLook.vWidthInches      =  truck.width-inches                                           
                ttTrailerLook.vHeightInches     =  truck.height-inches  
                 .
            END. /*FOR EACH truck where*/
        END. /*if prmCondition = BEGIN */ 
     end.  /* if prmField = carrier  */
    
END.  /* IF prmAction = search then do: */

