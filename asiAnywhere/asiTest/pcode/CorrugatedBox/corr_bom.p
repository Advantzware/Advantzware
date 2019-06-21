

/*------------------------------------------------------------------------
    File        : corr_bom.p
    Purpose     : corrBom
    Syntax      :

    Description : Return a Dataset of Corrugated Layout Bom 
    Author(s)   : 
    Created     : 17 sep 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorrBom NO-UNDO
        FIELD vBomItem         AS CHAR FORMAT "x(10)" EXTENT 8
        FIELD vBomItemDscr     AS CHAR FORMAT "x(30)" EXTENT 10
        FIELD vShrink          AS DECIMAL FORMAT ">>9.9999" INITIAL 0 EXTENT 8
        FIELD vSqInch          AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 EXTENT 10
        
        FIELD vLamCode         AS CHAR    
        FIELD vAdhesive        AS CHAR    
        
        .
DEFINE DATASET dsCorrBom FOR ttCorrBom .

DEFINE INPUT PARAMETER prmUser                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction                    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmComp                      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmForm                      AS INT         NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem1                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem2                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem3                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem4                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem5                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem6                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem7                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItem8                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLamCode                   AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAdhesive                  AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr1              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr2              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr3              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr4              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr5              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr6              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr7              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr8              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr9              AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmBomItemDscr10             AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmShrink1                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink2                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink3                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink4                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink5                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink6                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink7                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmShrink8                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch1                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch2                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch3                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch4                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch5                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch6                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch7                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch8                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch9                   AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER prmSqInch10                  AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cError                      AS CHAR        NO-UNDO.



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrBom.

IF prmUser             = ?  THEN ASSIGN prmUser              = "".
IF prmAction           = ?  THEN ASSIGN prmAction            = "".
IF prmComp             = ?  THEN ASSIGN prmComp              = "".
IF prmBomItem1         = ?  THEN ASSIGN prmBomItem1          = "".
IF prmBomItem2         = ?  THEN ASSIGN prmBomItem2          = "".
IF prmBomItem3         = ?  THEN ASSIGN prmBomItem3          = "".
IF prmBomItem4         = ?  THEN ASSIGN prmBomItem4          = "".
IF prmBomItem5         = ?  THEN ASSIGN prmBomItem5          = "".
IF prmBomItem6         = ?  THEN ASSIGN prmBomItem6          = "".
IF prmBomItem7         = ?  THEN ASSIGN prmBomItem7          = "".
IF prmBomItem8         = ?  THEN ASSIGN prmBomItem8          = "".
IF prmLamCode          = ?  THEN ASSIGN prmLamCode           = "".
IF prmAdhesive         = ?  THEN ASSIGN prmAdhesive          = "".
IF prmBomItemDscr1     = ?  THEN ASSIGN prmBomItemDscr1      = "".
IF prmBomItemDscr2     = ?  THEN ASSIGN prmBomItemDscr2      = "".
IF prmBomItemDscr3     = ?  THEN ASSIGN prmBomItemDscr3      = "".
IF prmBomItemDscr4     = ?  THEN ASSIGN prmBomItemDscr4      = "".
IF prmBomItemDscr5     = ?  THEN ASSIGN prmBomItemDscr5      = "".
IF prmBomItemDscr6     = ?  THEN ASSIGN prmBomItemDscr6      = "".
IF prmBomItemDscr7     = ?  THEN ASSIGN prmBomItemDscr7      = "".
IF prmBomItemDscr8     = ?  THEN ASSIGN prmBomItemDscr8      = "".
IF prmBomItemDscr9     = ?  THEN ASSIGN prmBomItemDscr9      = "".
IF prmBomItemDscr10    = ?  THEN ASSIGN prmBomItemDscr10     = "".
IF prmShrink1          = ?  THEN ASSIGN prmShrink1           = 0.
IF prmShrink2          = ?  THEN ASSIGN prmShrink2           = 0.
IF prmShrink3          = ?  THEN ASSIGN prmShrink3           = 0.
IF prmShrink4          = ?  THEN ASSIGN prmShrink4           = 0.
IF prmShrink5          = ?  THEN ASSIGN prmShrink5           = 0.
IF prmShrink6          = ?  THEN ASSIGN prmShrink6           = 0.
IF prmShrink7          = ?  THEN ASSIGN prmShrink7           = 0.
IF prmShrink8          = ?  THEN ASSIGN prmShrink8           = 0.
IF prmSqInch1          = ?  THEN ASSIGN prmSqInch1           = 0.
IF prmSqInch2          = ?  THEN ASSIGN prmSqInch2           = 0.
IF prmSqInch3          = ?  THEN ASSIGN prmSqInch3           = 0.
IF prmSqInch4          = ?  THEN ASSIGN prmSqInch4           = 0.
IF prmSqInch5          = ?  THEN ASSIGN prmSqInch5           = 0.
IF prmSqInch6          = ?  THEN ASSIGN prmSqInch6           = 0.
IF prmSqInch7          = ?  THEN ASSIGN prmSqInch7           = 0.
IF prmSqInch8          = ?  THEN ASSIGN prmSqInch8           = 0.
IF prmSqInch9          = ?  THEN ASSIGN prmSqInch9           = 0.
IF prmSqInch10         = ?  THEN ASSIGN prmSqInch10          = 0.

DEF VAR lv-bom-item AS cha EXTENT 8 NO-UNDO.
ASSIGN
        lv-bom-item[1] = prmBomItem1
        lv-bom-item[2] = prmBomItem2
        lv-bom-item[3] = prmBomItem3
        lv-bom-item[4] = prmBomItem4
        lv-bom-item[5] = prmBomItem5
        lv-bom-item[6] = prmBomItem6
        lv-bom-item[7] = prmBomItem7
        lv-bom-item[8] = prmBomItem8
            .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

def buffer b-item for item.
def var v-bom-count as int no-undo.
DEF VAR v-lam-in AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-cur-lam-code AS CHAR NO-UNDO.
DEF VAR v-cur-adh-code AS CHAR NO-UNDO.


/**********************************OverRide**************************************/
IF prmAction = "Update" THEN DO:


    def var i as int no-undo.

    FOR EACH ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK:
    find item where item.company = prmComp and
                  item.i-no = ef.board
                  EXCLUSIVE-LOCK no-error.
    END.
    
    
    IF prmLamCode NE "" THEN
      DO:
         IF prmLamCode NE "" AND
          NOT CAN-FIND(FIRST ITEM
                      where item.company  = prmComp and
                        item.mat-type = "L"
                        AND item.industry EQ "2"
                        AND item.i-no EQ prmLamCode) THEN DO:
             ASSIGN cError = "Invalid LamCode, try help..." .
             RETURN .
          END.
      END.

      IF prmAdhesive NE "" THEN
      DO:
         IF NOT CAN-FIND(FIRST ITEM
                         where item.company  = prmComp and
                            item.mat-type = "G"
                           AND item.industry EQ "2"
                           AND item.i-no EQ prmAdhesive) THEN DO:
           ASSIGN cError = "Invalid Adhesive, try help..." .
           RETURN .
         END.
      END.
      
      repeat v-bom-count = 1 to 8: 

          
        IF lv-bom-item[v-bom-count] = "" THEN
      DO:
         find item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count no-error.
         IF AVAIL item-bom THEN
            DELETE item-bom.
           
         NEXT.
      END.

      find item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count EXCLUSIVE-LOCK no-error.      
      if not avail item-bom then do:
          create item-bom.
          assign item-bom.company = item.company
                 item-bom.parent-i = item.i-no
                 item-bom.line# = v-bom-count.   
      end.
     item-bom.i-no = lv-bom-item[v-bom-count].

      IF NOT CAN-FIND(FIRST b-item WHERE
         b-item.company EQ item-bom.company AND
         b-item.i-no    EQ item-bom.i-no) THEN
         do:
         ASSIGN   cError = "Bill Of Material found for a Invalid Item#.  Not Creating Item Bill" .
                    RETURN.
            delete item-bom.
            NEXT.
         end.
     
      case v-bom-count :
           when 1 then assign   item-bom.i-no = prmBomItem1
                                item-bom.shrink = prmShrink1   .
           when 2 then assign   item-bom.i-no = prmBomItem2
                                item-bom.shrink = prmShrink2   .
           when 3 then assign   item-bom.i-no = prmBomItem3
                                item-bom.shrink = prmShrink3   .
           when 4 then assign   item-bom.i-no = prmBomItem4
                                item-bom.shrink = prmShrink4   .
           when 5 then assign   item-bom.i-no = prmBomItem5
                                item-bom.shrink = prmShrink5   .
           when 6 then assign   item-bom.i-no = prmBomItem6
                                item-bom.shrink = prmShrink6   .
           when 7 then assign   item-bom.i-no = prmBomItem7
                                item-bom.shrink = prmShrink7   .
           when 8 then assign   item-bom.i-no = prmBomItem8
                                item-bom.shrink = prmShrink8   .
      end. 
      END.
      
      assign item.avg-cost  = 0
         item.last-cost = 0.

  for each item-bom where  item-bom.company = item.company and
                           item-bom.parent-i = item.i-no AND
                           item-bom.line# LT 9 no-lock:
     find b-item where  b-item.company = item-bom.company and
                        b-item.i-no = item-bom.i-no no-lock no-error.
     assign item.avg-cost  = item.avg-cost  + b-item.avg-cost
            item.last-cost = item.last-cost + b-item.last-cost.
  end.

  FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm EXCLUSIVE-LOCK NO-ERROR.
  /*FIND CURRENT ef EXCLUSIVE NO-ERROR.*/

  IF AVAIL ef THEN DO:

     ASSIGN
        ef.lam-code = prmLamCode
        ef.adh-code = prmAdhesive
        ef.adh-sqin = prmSqInch1. + prmSqInch2 + prmSqInch3 +
                      prmSqInch4 + prmSqInch5 + prmSqInch6 +
                      prmSqInch7 + prmSqInch8.
  END.
    ASSIGN
            prmAction = "Select" .
 END.

/*********************************End OverRide**********************************/

/**********************************OverRide**************************************/
IF prmAction = "Select" THEN DO:
    
    def var lv-item-recid as recid no-undo.
   /* DEF VAR lv-bom-item AS cha EXTENT 8 NO-UNDO.
    def var i as int no-undo. */

    FOR EACH ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK:
    find item where item.company = prmComp and
                  item.i-no = ef.board
                  no-lock no-error.
    END.
    

CREATE ttCorrBom.
    repeat v-bom-count = 1 to 8:   
     
         FIND item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count NO-LOCK no-error.
         if not avail item-bom then next. 
      find b-item where b-item.company = item-bom.company and
                        b-item.i-no    = item-bom.i-no no-lock no-error.    
      
       IF AVAIL b-item THEN DO:
           
        case v-bom-count :
            
            when 1 then assign  ttCorrBom.vBomItem[1]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[1] = b-item.i-name
                                ttCorrBom.vShrink[1]      = item-bom.shrink.
            when 2 then assign  ttCorrBom.vBomItem[2]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[2] = b-item.i-name
                                ttCorrBom.vShrink[2]      = item-bom.shrink.
            when 3 then assign  ttCorrBom.vBomItem[3]      = item-bom.i-no
                                ttCorrBom.vBomItemDscr[3]  = b-item.i-name
                                ttCorrBom.vShrink[3]       = item-bom.shrink.
            when 4 then assign  ttCorrBom.vBomItem[4]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[4] = b-item.i-name
                                ttCorrBom.vShrink[4]      = item-bom.shrink.
            when 5 then assign  ttCorrBom.vBomItem[5]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[5] = b-item.i-name
                                ttCorrBom.vShrink[5]      = item-bom.shrink.
            when 6 then assign  ttCorrBom.vBomItem[6]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[6] = b-item.i-name
                                ttCorrBom.vShrink[6]      = item-bom.shrink.
            when 7 then assign  ttCorrBom.vBomItem[7]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[7] = b-item.i-name
                                ttCorrBom.vShrink[7]      = item-bom.shrink.
            when 8 then assign  ttCorrBom.vBomItem[8]     = item-bom.i-no
                                ttCorrBom.vBomItemDscr[8] = b-item.i-name
                                ttCorrBom.vShrink[8]      = item-bom.shrink.
        end.        
   end.
   
   END.
  /* Reset master item avg/last costs */
  

  FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.

  IF AVAIL ef THEN DO :

     ASSIGN
      ttCorrBom.vLamCode =  ef.lam-code
      ttCorrBom.vAdhesive =  ef.adh-code

       ttCorrBom.vSqInch[1]   = IF ttCorrBom.vBomItem[1] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[1] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[1] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
      
      ttCorrBom.vSqInch[2]    = IF ttCorrBom.vBomItem[2] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[2] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[2] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
      
      ttCorrBom.vSqInch[3]    = IF ttCorrBom.vBomItem[3] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[3] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[3] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
                               
      ttCorrBom.vSqInch[4]   = IF ttCorrBom.vBomItem[4] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[4] / 100)))
                               ELSE 0

      /*ttCorrBom.vSqInch[4] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
      
      ttCorrBom.vSqInch[5]   = IF ttCorrBom.vBomItem[5] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[5] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[5] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
      
      ttCorrBom.vSqInch[6]   = IF ttCorrBom.vBomItem[6] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[6] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[6] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/
      
      ttCorrBom.vSqInch[7]  = IF ttCorrBom.vBomItem[7] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[7] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[7] = {round(trunc({ttCorrBom.vSqInch[1]},0) + (({ttCorrBom.vSqInch[1]} - trunc({ttCorrBom.vSqInch[1]},0)) / K_FRAC),2)}*/

      ttCorrBom.vSqInch[8]  = IF ttCorrBom.vBomItem[8] NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (ttCorrBom.vShrink[8] / 100)))
                               ELSE 0
      /*ttCorrBom.vSqInch[8] = {round(trunc({ttCorrBom.vSqInch[8]},0) + (({ttCorrBom.vSqInch[8]} - trunc({ttCorrBom.vSqInch[8]},0)) / K_FRAC),2)}*/

      v-lam-in = ef.gsh-len * ef.gsh-wid.

   IF ef.lam-code NE "" THEN
       ttCorrBom.vSqInch[9] = v-lam-in .
      /*ttCorrBom.vSqInch[9] = {round(trunc({v-lam-in},0) + (({v-lam-in} - trunc({v-lam-in},0)) / K_FRAC),2) } .*/
                                   
   RUN adh-sq-in-tot-proc.                                      

   IF ef.lam-code NE "" THEN
   DO:
      v-cur-lam-code = ef.lam-code.

      FIND FIRST b-item
           where b-item.company  = prmComp and
       b-item.mat-type = "L" 
           AND b-item.industry EQ "2"
           AND b-item.i-no EQ ef.lam-code
           NO-LOCK NO-ERROR.

      IF AVAIL b-item THEN
         ttCorrBom.vBomItemDscr[9] = b-item.i-name.
   END.

   IF ef.adh-code NE "" THEN
   DO:
      v-cur-adh-code = ef.adh-code.

      FIND FIRST b-item
           where b-item.company  = prmComp and
        b-item.mat-type = "G" 
           AND b-item.industry EQ "2"
           AND b-item.i-no EQ ef.adh-code
           NO-LOCK NO-ERROR.

      IF AVAIL b-item THEN
         ttCorrBom.vBomItemDscr[10] = b-item.i-name.
   END.
  END.
END.  /*end case*/
  DO i = 1 TO 8:
    FIND FIRST ttCorrBom  WHERE ttCorrBom.vBomItem[i] = "" NO-LOCK NO-ERROR.
    IF AVAIL ttCorrBom THEN
    ASSIGN ttCorrBom.vBomItemDscr[i] = ""
           ttCorrBom.vShrink[i] = 0
           ttCorrBom.vSqInch[i] = 0
                .
    
  END.    /* end override*/ 


/*********************************End OverRide**********************************/

  /*------------------------------------------------------------------------------*/
  PROCEDURE adh-sq-in-tot-proc :

   DEF VAR v-total AS DEC DECIMALS 4 NO-UNDO.

   ASSIGN
      v-total = ttCorrBom.vSqInch[1] + ttCorrBom.vSqInch[2] + ttCorrBom.vSqInch[3] + ttCorrBom.vSqInch[4] +
                ttCorrBom.vSqInch[5] + ttCorrBom.vSqInch[6] + ttCorrBom.vSqInch[7] + ttCorrBom.vSqInch[8]
    ttCorrBom.vSqInch[10] = v-total .
     /* v-sq-in-10:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
                                       STRING({sys/inc/k16.i v-total}).*/
END PROCEDURE.
/*------------------------------------------------------------------------------*/
