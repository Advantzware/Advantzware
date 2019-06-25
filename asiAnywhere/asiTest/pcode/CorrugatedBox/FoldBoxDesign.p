/*------------------------------------------------------------------------
    File        : FoldBoxDesign 
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Folding Box Design

    Author(s)   : 
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */       
                           
DEFINE TEMP-TABLE ttFoldBoxDesign NO-UNDO 
     FIELD vdesign_no   LIKE  box-design-hdr.design-no        /* int */
     FIELD vdesign_dscr LIKE  box-design-hdr.DESCRIPTION    /* char */
     FIELD vbox_image   LIKE  box-design-hdr.box-image       /* char */
     FIELD vbox_3d_image LIKE box-design-hdr.box-3d-image   /* char */ 
     FIELD vDieImage AS CHAR 
        .

DEFINE DATASET dsFoldBoxDesign FOR ttFoldBoxDesign.

    DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstNo         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFormNo        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmDesignNo      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmDesignDscr    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoxImage      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBox3DImage    AS CHAR NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldBoxDesign.

    IF prmUser       = ? THEN ASSIGN prmUser        = "" .
    IF prmAction     = ? THEN ASSIGN prmAction      = "" .
    IF prmComp       = ? THEN ASSIGN prmComp        = "" .
    IF prmEstNo      = ? THEN ASSIGN prmEstNo       = "".
    IF prmFormNo     = ? THEN ASSIGN prmFormNo      = 0.
    IF prmDesignNo   = ? THEN ASSIGN prmDesignNo    = 0 .
    IF prmDesignDscr = ? THEN ASSIGN prmDesignDscr  = "" .
    IF prmBoxImage   = ? THEN ASSIGN prmBoxImage    = "" .
    IF prmBox3DImage = ? THEN ASSIGN prmBox3DImage  = "" .

  FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


 DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.
 def new shared var cocode as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
{cec/descalc.i new}
def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO like box-design-line.
ASSIGN
    cocode = prmComp.

 /*******************************Update*********************************/
IF prmAction = "Update" THEN DO:
    prmBoxImage = REPLACE(prmBoxImage,"/","\").
    prmBox3DImage = REPLACE(prmBox3DImage,"/","\") .
    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    

  FOR EACH eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo NO-LOCK:
/*FIND FIRST eb WHERE eb.company EQ prmComp AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
            AND eb.form-no EQ prmFormNo NO-LOCK NO-ERROR.*/

FIND FIRST box-design-hdr where /*box-design-hdr.design-no = 0*/
                                box-design-hdr.company = eb.company 
                            and box-design-hdr.est-no = eb.est-no   
                            and box-design-hdr.form-no   eq eb.form-no
                            and box-design-hdr.blank-no  eq eb.blank-no
    EXCLUSIVE-LOCK NO-ERROR.

  
  
  /*find first style where style.company eq eb.company
                   and style.style   eq eb.style
                 EXCLUSIVE-LOCK no-error.
if avail style then
  find first box-design-hdr where box-design-hdr.design-no eq style.design-no
                               
             no-lock no-error.*/
        
        ASSIGN
            
            box-design-hdr.description   = prmDesignDscr
            box-design-hdr.box-image     = prmBoxImage
            box-design-hdr.box-3d-image  = prmBox3DImage
                .
END.
END.

/*****************************End Update******************************/


/*******************************Select*********************************/
IF prmAction = "Select" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo) AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    

  FOR EACH eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo NO-LOCK:
/*FIND FIRST eb WHERE eb.company EQ prmComp AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
            AND eb.form-no EQ prmFormNo NO-LOCK NO-ERROR.*/
      

FIND FIRST box-design-hdr where /*box-design-hdr.design-no = 0*/
                                box-design-hdr.company = eb.company 
                            and box-design-hdr.est-no = eb.est-no   
                            and box-design-hdr.form-no   eq eb.form-no
                            and box-design-hdr.blank-no  eq eb.blank-no
    NO-LOCK NO-ERROR.

            if not avail box-design-hdr and avail eb then do:
            run build-box ("B") .
             end.

  
  
  find first style where style.company eq eb.company
                   and style.style   eq eb.style
                 no-lock no-error.
if avail style then
  /*find first box-design-hdr where box-design-hdr.design-no eq style.design-no
                               
             no-lock no-error.*/

        CREATE ttFoldBoxDesign.
        ASSIGN
            ttFoldBoxDesign.vdesign_no     = style.design-no 
            ttFoldBoxDesign.vdesign_dscr   = box-design-hdr.description
            ttFoldBoxDesign.vbox_image     = box-design-hdr.box-image
            ttFoldBoxDesign.vbox_3d_image  = box-design-hdr.box-3d-image
            ttFoldBoxDesign.vDieImage      = ef.cad-image
                .       
END.
END.

/*****************************End Select******************************/

/******************************procuder**************************************/

PROCEDURE build-box :

def input parameter v-rebuild as char.

def buffer xbox-design-hdr  for box-design-hdr.
def buffer xbox-design-line for box-design-line.

cocode = eb.company.
find xeb where recid(xeb) = recid(eb) no-lock.               

for each box-design-hdr where box-design-hdr.design-no = 0 and
                              box-design-hdr.company = xeb.company 
                          and box-design-hdr.est-no = xeb.est-no
    /*{cec/est-6.w box-design-hdr}*/
      and box-design-hdr.form-no   eq xeb.form-no
      and box-design-hdr.blank-no  eq xeb.blank-no
    no-lock:
        
  for each box-design-line of box-design-hdr:
    create w-box-l.
    buffer-copy box-design-line to w-box-l.
  end.
  

  create w-box-h.
  buffer-copy box-design-hdr to w-box-h.
end.

{cec/est-6.del}

find first xest where xest.company = xeb.company and
                      xest.est-no = xeb.est-no
                      no-lock.
find first xef where xef.company = xeb.company 
                 and xef.est-no   eq xeb.est-no
                 and xef.form-no eq xeb.form-no  no-lock.

find first style where style.company eq xeb.company
                   and style.style   eq xeb.style
                 no-lock no-error.
if avail style then
  find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                               and xbox-design-hdr.est-no    eq ""
             no-lock no-error.

if avail xbox-design-hdr then do:
   run cec/descalc.p (recid(xest), recid(xeb)).
   create box-design-hdr.
   assign  box-design-hdr.design-no   = 0
           box-design-hdr.company = xeb.company
           box-design-hdr.est-no      = xeb.est-no
           box-design-hdr.form-no     = xeb.form-no
           box-design-hdr.blank-no    = xeb.blank-no
           box-design-hdr.description = if avail xbox-design-hdr then
                                          xbox-design-hdr.description else ""
           box-design-hdr.lscore      = v-lscore-c
           box-design-hdr.lcum-score  = v-lcum-score-c
/*           fil_id                     = recid(box-design-hdr). */
           box-design-hdr.wscore = xbox-design-hdr.wscore
           box-design-hdr.wcum-score = xbox-design-hdr.wcum-score
           box-design-hdr.box-text = xbox-design-hdr.box-text
           box-design-hdr.box-image = xbox-design-hdr.box-image
           box-design-hdr.box-3d-image = xbox-design-hdr.box-3d-image
           .

   for each xbox-design-line of xbox-design-hdr no-lock:
      create box-design-line.
      assign box-design-line.design-no  = box-design-hdr.design-no
             box-design-line.company = box-design-hdr.company
             box-design-line.est-no      = box-design-hdr.est-no
             box-design-line.form-no    = box-design-hdr.form-no
             box-design-line.blank-no   = box-design-hdr.blank-no
             box-design-line.line-no    = xbox-design-line.line-no
             box-design-line.line-text  = xbox-design-line.line-text.

      find first w-box-design-line
           where w-box-design-line.line-no eq box-design-line.line-no   no-error.
      if avail w-box-design-line then
         assign  box-design-line.wscore     = w-box-design-line.wscore-c
                 box-design-line.wcum-score = w-box-design-line.wcum-score-c.
   end.
 
   if v-rebuild ne "B" then do:
      if v-rebuild eq "S" then
         box-design-hdr.description = w-box-h.description.
      else  assign box-design-hdr.lscore      = w-box-h.lscore
                   box-design-hdr.lcum-score  = w-box-h.lcum-score
                   box-design-hdr.wscore      = w-box-h.wscore
                   box-design-hdr.wcum-score  = w-box-h.wcum-score.

      for each w-box-l of box-design-hdr no-lock,
          first box-design-line of w-box-l:
      
          if v-rebuild eq "S" then
             assign box-design-line.line-no    = w-box-l.line-no
                     box-design-line.line-text  = w-box-l.line-text.
          else do:
             find first w-box-design-line
                  where w-box-design-line.line-no eq w-box-l.line-no   no-error.
             if avail w-box-design-line then
                assign box-design-line.wscore     = w-box-l.wscore
                       box-design-line.wcum-score = w-box-l.wcum-score.
          end.     
      end.
   end.
end.



END PROCEDURE.

