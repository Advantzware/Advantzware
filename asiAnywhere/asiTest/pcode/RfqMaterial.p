

/*------------------------------------------------------------------------
    File        : list_rfqs.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     : tue Feb 12 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cocode LIKE oe-ordl.company.

{RfqMaterial.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRfqNo     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER PrmPartNo    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER MatRowid     AS RECID  NO-UNDO.
DEFINE INPUT PARAMETER  prmBoard    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmBrdDscr  AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmCal      AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmGshwid   AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmGshlen   AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeaf1    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafw1   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafl1   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeaf2    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafw2   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafl2   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeaf3    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER  prmvLeafw3  AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafl3   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeaf4    AS CHAR         NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafw4   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafl4   AS DECIMAL      NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr1 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr2 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr3 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr4 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr5 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecdscr6 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder1    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder2    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder3    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder4    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder5    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder6    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder7    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder8    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder9    AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder10   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder11   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmAdder12   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno1   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno2   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno3   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno4   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno5   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecno6   AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafdscr  AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafdscr2 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafdscr3 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmLeafdscr4 AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecQty   AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecQty2  AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecQty3  AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  vSpecQty4    AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecQty5  AS DECIMAL     NO-UNDO. 
DEFINE INPUT PARAMETER  prmSpecQty6  AS DECIMAL     NO-UNDO. 
                                                
DEFINE INPUT-OUTPUT PARAMETER DATASET  FOR dsRfqMaterial.
                                       
    DEFINE OUTPUT PARAMETER  cError  AS CHAR     NO-UNDO. 
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ll-is-corr-style   AS LOG  NO-UNDO.
def var K_frac as dec init 6.25 no-undo.
def buffer buff-rfqitem for rfqitem.
DEF VAR prmLoc AS CHAR NO-UNDO.
                 
IF prmComp = ?   THEN ASSIGN prmComp = "".
IF prmCust = ?   THEN ASSIGN prmCust = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmRfqNo = ?  THEN ASSIGN prmRfqNo = 0.
IF PrmPartNo = ?  THEN ASSIGN PrmPartNo = "".
IF prmLeaf1=?  THEN ASSIGN prmLeaf1 = "".
IF prmLeaf2=?  THEN ASSIGN prmLeaf2 = "".
IF prmLeaf3=?  THEN ASSIGN prmLeaf3 = "".
IF prmLeaf4=?  THEN ASSIGN prmLeaf4 = "".
IF prmSpecno1=?  THEN ASSIGN prmSpecno1 = "".
IF prmSpecno2=?  THEN ASSIGN prmSpecno2 = "".
IF prmSpecno3=?  THEN ASSIGN prmSpecno3 = "".
IF prmSpecno4=?  THEN ASSIGN prmSpecno4 = "".
IF prmSpecno5=?  THEN ASSIGN prmSpecno5 = "".
IF prmSpecno6=?  THEN ASSIGN prmSpecno6 = "".
IF prmAdder1=?  THEN ASSIGN prmAdder1 = "".
IF prmAdder2=?  THEN ASSIGN prmAdder2 = "".
IF prmAdder3=?  THEN ASSIGN prmAdder3 = "".
IF prmAdder4=?  THEN ASSIGN prmAdder4 = "".
IF prmAdder5=?  THEN ASSIGN prmAdder5 = "".
IF prmAdder6=?  THEN ASSIGN prmAdder6 = "".

IF prmAction = "" THEN ASSIGN prmAction = "Select".
IF  prmBoard = ""  THEN ASSIGN prmBrdDscr = ""
                               prmCal  = 0 .
IF prmLeaf1 = ""  THEN ASSIGN prmLeafdscr = "".
IF prmLeaf2 = ""  THEN ASSIGN prmLeafdscr2 = "".
IF prmLeaf3 = ""  THEN ASSIGN prmLeafdscr3 = "".
IF prmLeaf4 = ""  THEN ASSIGN prmLeafdscr4 = "".
IF prmSpecno1 = ""  THEN ASSIGN prmSpecdscr1 = "".
IF prmSpecno2 = ""  THEN ASSIGN prmSpecdscr2 = "".
IF prmSpecno3 = ""  THEN ASSIGN prmSpecdscr3 = "".
IF prmSpecno4 = ""  THEN ASSIGN prmSpecdscr4 = "".
IF prmSpecno5 = ""  THEN ASSIGN prmSpecdscr5 = "".
IF prmSpecno6 = ""  THEN ASSIGN prmSpecdscr6 = "".
IF prmAdder1 = ""  THEN ASSIGN prmAdder7 = "".
IF prmAdder2 = ""  THEN ASSIGN prmAdder8 = "".
IF prmAdder3 = ""  THEN ASSIGN prmAdder9 = "".
IF prmAdder4 = ""  THEN ASSIGN prmAdder10 = "".
IF prmAdder5 = ""  THEN ASSIGN prmAdder11 = "".
IF prmAdder6 = ""  THEN ASSIGN prmAdder12 = "".
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

IF prmCust = "" THEN DO:
    FIND FIRST usercust WHERE usercust.user_id = prmUser 
        AND usercust.cust_default
        AND usercust.company EQ prmComp
        NO-LOCK NO-ERROR.
    IF AVAILABLE usercust THEN ASSIGN prmCust = usercust.cust-no.
    
END. /*IF prmCust = ""*/
  
IF prmAction = "UpdateRfqMaterial" THEN DO:

FIND FIRST item WHERE item.company = prmComp and item.i-no = prmBoard  AND
        (item.mat-type = "B" or item.mat-type = "P" or item.mat-type = "1" 
         OR item.mat-type = "2" or item.mat-type = "3" or item.mat-type = "4") NO-LOCK NO-ERROR.
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Board!!!!".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        ASSIGN prmCal = item.cal
            prmBrdDscr  = ITEM.i-name.
               /*rfqitem.gsh-wid = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
               rfqitem.gsh-len = string(item.s-len) 
               rfqitem.test = item.reg-no
               rfqitem.flute = item.flute.*/
                                       
    END.
    MESSAGE "test" prmLeaf1 prmLeaf2.
    IF  prmLeaf1 <> "" THEN DO:
    FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "W" and item.i-no = prmLeaf1    NO-LOCK NO-ERROR.
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Window/Wax!!!!".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        ASSIGN prmLeafdscr = item.i-name.
              /* rfqitem.leaf-w[1] = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
               rfqitem.leaf-l[1] = string(item.s-len) .*/
                                       
    END.
    END.
    IF prmLeaf2 <>"" THEN DO:
    
    FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "F" and item.i-no = prmLeaf2  NO-LOCK NO-ERROR.
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Foil!!!!".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        ASSIGN prmLeafdscr2 = item.i-name.
        /*
    rfqitem.leaf-w = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[2]:screen-value = string(item.s-len) 
                                        .                                                   */
    END.
    END.
    IF prmLeaf3 <> "" THEN DO:
    FIND FIRST item WHERE item.company = prmComp AND item.mat-type = "F" and item.i-no = prmLeaf3  NO-LOCK NO-ERROR.
    if not avail item then do:
        ASSIGN 
            cError = "Invalid Stamp!!!!".
            RETURN. 
    end. /*if not avail item then do:*/ 
    ELSE DO:
        ASSIGN prmLeafdscr3 = item.i-name.
        /*
     rfqitem.leaf-w[3]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[3]:screen-value = string(item.s-len) 
                                        
                                                                                           */
    END.
    END.
   IF prmLeaf4 <> "" THEN DO:
    FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "L" and item.i-no = prmLeaf4  NO-LOCK NO-ERROR.
    if not avail item then do:
       ASSIGN 
           cError = "Invalid Leminate!!!!".
           RETURN. 
   end. /*if not avail item then do:*/ 
   ELSE DO:
       ASSIGN prmLeafdscr4 = item.i-name.
       /*
     if avail item then assign rfqitem.leaf-w[4]:screen-value = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                                        rfqitem.leaf-l[4]:screen-value = string(item.s-len) 
                                                                                          */
   END.
   END.


  IF  prmSpecno1 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno1  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 1!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr1 = item.i-name.
  END.
  END.
  IF prmSpecno2 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno2  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 2!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr2 = item.i-name.
  END.
  END.
  IF prmSpecno3 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno3  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 3!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr3 = item.i-name.
  END.
  END.

  IF prmSpecno4 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno4  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 4!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr4 = item.i-name.
  END.
  END.

  IF prmSpecno5 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno5  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 5!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr5 = item.i-name.
  END.
  END.

  IF prmSpecno6 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp AND item.mat-type = "M" and item.i-no = prmSpecno6  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Specification 6!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmSpecdscr6 = item.i-name.
  END.
  END.
  IF prmAdder1 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder1  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder1!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder7 = item.i-name.
  END.
  END.

  IF prmAdder2 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder2   NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder2!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder8 = item.i-name.
  END.
  END.
  IF prmAdder3 <> "" THEN DO:
  FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder3  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder3!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder9 = item.i-name.
  END.
  END.
  IF prmAdder4 <> "" THEN DO:
   FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder4  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder4!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder10 = item.i-name.
  END.
  END.

  IF prmAdder5 <> "" THEN DO:
   FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder5  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder5!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder11 = item.i-name.
  END.
  END.
  IF prmAdder6 <> "" THEN DO:
   FIND FIRST item WHERE  item.company = prmComp  and item.i-no = prmAdder6  NO-LOCK NO-ERROR.
  if not avail item then do:
      ASSIGN 
          cError = "Invalid Item Adder6!!!!".
          RETURN. 
  end. /*if not avail item then do:*/ 
  ELSE DO:
      ASSIGN prmAdder12 = item.i-name.
  END.
  END.

END.  /*IF prmAction = "UpdateRfqMaterial" THEN DO:*/

IF prmAction = "UpdateRfqMaterial" THEN DO:
    
    /*FIND rfqitem where recid(rfqitem) = MatRowid   no-error.*/
    
    FIND buff-rfqitem WHERE
         buff-rfqitem.company EQ prmComp AND
         buff-rfqitem.loc EQ prmLoc AND
         buff-rfqitem.rfq-no = prmRfqNo AND
         buff-rfqitem.seq = int(PrmPartNo)
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL buff-rfqitem THEN DO:
        assign
            buff-rfqitem.board       = prmBoard    
            buff-rfqitem.brd-dscr     = prmBrdDscr  
            buff-rfqitem.cal          = prmCal      
            buff-rfqitem.gsh-wid      = prmGshwid   
            buff-rfqitem.gsh-len      = prmGshlen   
            buff-rfqitem.leaf[1]      = prmLeaf1                   
            buff-rfqitem.leaf-w[1]    = prmLeafw1        
            buff-rfqitem.leaf-l[1]    = prmLeafl1        
            buff-rfqitem.leaf[2]      = prmLeaf2         
            buff-rfqitem.leaf-w[2]    = prmLeafw2        
            buff-rfqitem.leaf-l[2]    = prmLeafl2        
            buff-rfqitem.leaf[3]      = prmLeaf3         
            buff-rfqitem.leaf-w[3]    = prmvLeafw3       
            buff-rfqitem.leaf-l[3]    = prmLeafl3        
            buff-rfqitem.leaf[4]      = prmLeaf4         
            buff-rfqitem.leaf-w[4]    = prmLeafw4        
            buff-rfqitem.leaf-l[4]    = prmLeafl4        
            buff-rfqitem.spec-dscr[1] = prmSpecdscr1     
            buff-rfqitem.spec-dscr[2] = prmSpecdscr2     
            buff-rfqitem.spec-dscr[3] = prmSpecdscr3     
            buff-rfqitem.spec-dscr[4] = prmSpecdscr4     
            buff-rfqitem.spec-dscr[5] = prmSpecdscr5     
            buff-rfqitem.spec-dscr[6] = prmSpecdscr6     
            buff-rfqitem.adder[1]     = prmAdder1        
            buff-rfqitem.adder[2]     = prmAdder2        
            buff-rfqitem.adder[3]     = prmAdder3        
            buff-rfqitem.adder[4]     = prmAdder4        
            buff-rfqitem.adder[5]     = prmAdder5        
            buff-rfqitem.adder[6]     = prmAdder6        
            buff-rfqitem.adder[7]     = prmAdder7        
            buff-rfqitem.adder[8]     = prmAdder8        
            buff-rfqitem.adder[9]     = prmAdder9        
            buff-rfqitem.adder[10]    = prmAdder10       
            buff-rfqitem.adder[11]    = prmAdder11       
            buff-rfqitem.adder[12]    = prmAdder12       
            buff-rfqitem.spec-no[1]   = prmSpecno1       
            buff-rfqitem.spec-no[2]   = prmSpecno2       
            buff-rfqitem.spec-no[3]   = prmSpecno3       
            buff-rfqitem.spec-no[4]   = prmSpecno4       
            buff-rfqitem.spec-no[5]  =  prmSpecno5       
            buff-rfqitem.spec-no[6]  =  prmSpecno6       
            buff-rfqitem.leaf-dscr[1] = prmLeafdscr      
            buff-rfqitem.leaf-dscr[2] = prmLeafdscr2     
            buff-rfqitem.leaf-dscr[3] = prmLeafdscr3     
            buff-rfqitem.leaf-dscr[4] = prmLeafdscr4     
            buff-rfqitem.spec-qty[1] =  prmSpecQty 
            buff-rfqitem.spec-qty[2] =  prmSpecQty2      
            buff-rfqitem.spec-qty[3] =  prmSpecQty3      
            buff-rfqitem.spec-qty[4] =  vSpecQty4       
            buff-rfqitem.spec-qty[5] =  prmSpecQty5      
            buff-rfqitem.spec-qty[6] =  prmSpecQty6      
           .                    
        RELEASE buff-rfqitem.  
    END.   /*if avail buff-rfqitem*/                                                                               
    ASSIGN prmAction = "Select". 
END.  /*if prmActiob*/

IF prmAction = "Select" THEN DO:
    FIND FIRST rfqitem WHERE rfqitem.company = prmComp
                         AND rfqitem.loc = prmLoc
                         AND rfqitem.rfq-no = PrmRfqNo 
                         AND rfqitem.seq = int(PrmPartNo)
                         AND rfqitem.seq < 999  NO-LOCK NO-ERROR.
    RUN CreateMaterial.
END.  /*if prmAction = select*/

PROCEDURE CreateMaterial:
    CREATE ttRfqMaterial.
       ASSIGN
           ttRfqMaterial.vBoard       = rfqitem.board
           ttRfqMaterial.vBrdDscr      = rfqitem.brd-dscr
           ttRfqMaterial.vCal         = rfqitem.cal
           ttRfqMaterial.vGshwid      = rfqitem.gsh-wid
           ttRfqMaterial.vGshlen      = rfqitem.gsh-len
           ttRfqMaterial.flute        = rfqitem.flute
           ttRfqMaterial.test         = rfqitem.test
           ttRfqMaterial.vLeaf1       = rfqitem.leaf[1]
           ttRfqMaterial.vLeafw1      = rfqitem.leaf-w[1]
           ttRfqMaterial.vLeafl1      = rfqitem.leaf-l[1]
           ttRfqMaterial.vLeaf2       = rfqitem.leaf[2]
           ttRfqMaterial.vLeafw2      = rfqitem.leaf-w[2]
           ttRfqMaterial.vLeafl2      = rfqitem.leaf-l[2]
           ttRfqMaterial.vLeaf3       = rfqitem.leaf[3]
           ttRfqMaterial.vLeafw3      = rfqitem.leaf-w[3]
           ttRfqMaterial.vLeafl3      = rfqitem.leaf-l[3]
           ttRfqMaterial.vLeaf4       = rfqitem.leaf[4]
           ttRfqMaterial.vLeafw4      = rfqitem.leaf-w[4]
           ttRfqMaterial.vLeafl4      = rfqitem.leaf-l[4]
           ttRfqMaterial.vSpecdscr1   = rfqitem.spec-dscr[1]
           ttRfqMaterial.vSpecdscr2   = rfqitem.spec-dscr[2]
           ttRfqMaterial.vSpecdscr3   = rfqitem.spec-dscr[3]
           ttRfqMaterial.vSpecdscr4   = rfqitem.spec-dscr[4]
           ttRfqMaterial.vSpecdscr5   = rfqitem.spec-dscr[5]
           ttRfqMaterial.vSpecdscr6   = rfqitem.spec-dscr[6]
           ttRfqMaterial.vAdder1      = rfqitem.adder[1]
           ttRfqMaterial.vAdder2      = rfqitem.adder[2]
           ttRfqMaterial.vAdder3      = rfqitem.adder[3]
           ttRfqMaterial.vAdder4      = rfqitem.adder[4]
           ttRfqMaterial.vAdder5      = rfqitem.adder[5]
           ttRfqMaterial.vAdder6      = rfqitem.adder[6]
           ttRfqMaterial.vAdder7      = rfqitem.adder[7]
           ttRfqMaterial.vAdder8      = rfqitem.adder[8]
           ttRfqMaterial.vAdder9      = rfqitem.adder[9]
           ttRfqMaterial.vAdder10     = rfqitem.adder[10]
           ttRfqMaterial.vAdder11     = rfqitem.adder[11]
           ttRfqMaterial.vAdder12     = rfqitem.adder[12]
           ttRfqMaterial.vSpecno1     = rfqitem.spec-no[1]
           ttRfqMaterial.vSpecno2     = rfqitem.spec-no[2]
           ttRfqMaterial.vSpecno3     = rfqitem.spec-no[3]
           ttRfqMaterial.vSpecno4     = rfqitem.spec-no[4]
           ttRfqMaterial.vSpecno5     = rfqitem.spec-no[5]
           ttRfqMaterial.vSpecno6     = rfqitem.spec-no[6]
           ttRfqMaterial.vLeafdscr    = rfqitem.leaf-dscr[1]
           ttRfqMaterial.vLeafdscr2   = rfqitem.leaf-dscr[2]
           ttRfqMaterial.vLeafdscr3   = rfqitem.leaf-dscr[3]
           ttRfqMaterial.vLeafdscr4   = rfqitem.leaf-dscr[4]
           ttRfqMaterial.vSpecQty     = rfqitem.spec-qty[1]
           ttRfqMaterial.vSpecQty2     = rfqitem.spec-qty[2]
           ttRfqMaterial.vSpecQty3     = rfqitem.spec-qty[3]
           ttRfqMaterial.vSpecQty4     = rfqitem.spec-qty[4]
           ttRfqMaterial.vSpecQty5     = rfqitem.spec-qty[5]
           ttRfqMaterial.vSpecQty6     = rfqitem.spec-qty[6]
           ttRfqMaterial.MatRowid      = RECID(rfqitem)   
           .
       
END PROCEDURE.
