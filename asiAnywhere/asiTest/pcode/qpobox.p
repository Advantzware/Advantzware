/*------------------------------------------------------------------------
    File        : qpobox.p 
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Corrugated Box Design

    Author(s)   : 
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */       
                         
DEFINE TEMP-TABLE ttQpoBoxDesign NO-UNDO 
     FIELD design_no   LIKE  box-design-hdr.design-no        /* int */
     FIELD design_dscr LIKE  box-design-hdr.DESCRIPTION    /* char */
     FIELD lscore      LIKE  box-design-hdr.lscore              /* char */
     FIELD lcum_score  LIKE  box-design-hdr.lcum-score       /* char */
     FIELD enum        LIKE  box-design-hdr.e-num                 /* int */
     FIELD est_no      LIKE  box-design-hdr.est-no              /* char */
     FIELD eqty        LIKE  box-design-hdr.eqty                  /* decimal */
     FIELD box_image   LIKE  box-design-hdr.box-image       /* char */
     FIELD wscore      LIKE  box-design-hdr.wscore              /* char */
     FIELD wcum_score  LIKE  box-design-hdr.wcum-score     /* char */
     FIELD box_text    LIKE  box-design-hdr.box-text        /* char */
     FIELD box_3d_image LIKE box-design-hdr.box-3d-image   /* char */ 
     FIELD  DieImage   AS CHAR
        .

DEFINE DATASET dsQpoBoxDesign FOR ttQpoBoxDesign.

    DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPoNo          AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmReckey        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLineno        AS INT NO-UNDO.
    
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQpoBoxDesign.

    IF prmUser     = ? THEN ASSIGN prmUser      = "" .
    IF prmAction   = ? THEN ASSIGN prmAction    = "Select" .
    IF prmComp     = ? THEN ASSIGN prmComp      = "" .
    IF prmPoNo     = ? THEN ASSIGN prmPoNo      = 0 .
    IF prmReckey   = ? THEN ASSIGN prmReckey    = "" .
    IF prmLineno   = ? THEN ASSIGN prmLineno    = 0 .
    


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


  FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp .

 def var lv-wscore like box-design-hdr.wscore no-undo.
def var lv-wcum-score like box-design-hdr.wcum-score no-undo.
def var li-cnt as int no-undo.
def var li-line-no as int extent 99 no-undo.
 DEF VAR v-sc-fmt AS CHAR NO-UNDO.
 def var ls-prev-wscore as cha no-undo.
  def var ls-prev-wcum as cha no-undo.
  def var ls-ws-value as cha no-undo.
  def var i as int no-undo.
  def var li-pos as int no-undo.
  def var li-pos-nxt as int no-undo.
  def var li-ln as int no-undo.
  def var ls-wscore as cha no-undo.
  def var ls-wcum as cha no-undo.
  def var ls-key as cha no-undo.


/*******************************Select*********************************/

  
IF prmAction = "Select" THEN DO:

     FIND FIRST po-ordl WHERE 
       po-ordl.company eq cocode and 
           po-ordl.po-no EQ INT(prmPoNo)  and
           po-ordl.LINE EQ INT(prmLineno)  NO-LOCK NO-ERROR.

     
    FOR EACH job-mat WHERE job-mat.company    eq po-ordl.company 
        and job-mat.rm-i-no    eq po-ordl.i-no 
        and job-mat.job-no     eq string(fill(" ",6 - 
                                              length(trim(po-ordl.job-no)))) + 
                                              trim(po-ordl.job-no) 
        and job-mat.job-no2    eq po-ordl.job-no2 
        and job-mat.i-no       eq po-ordl.i-no 
        and ((job-mat.frm      eq po-ordl.s-num and po-ordl.s-num ne 0) or 
             po-ordl.s-num     eq 0) 
        and ((job-mat.blank-no eq po-ordl.b-num and po-ordl.b-num ne 0) or 
             po-ordl.b-num     eq 0) NO-LOCK, 
        EACH job WHERE job.company eq job-mat.company 
        and job.job     eq job-mat.job 
        and job.job-no  eq job-mat.job-no 
        and job.job-no2 eq job-mat.job-no2 NO-LOCK, 
        EACH est WHERE est.company eq job.company 
        AND est.est-no  EQ job.est-no NO-LOCK, 
        EACH ef WHERE ef.company eq est.company 
        AND ef.est-no  EQ est.est-no 
        and ef.form-no eq job-mat.frm NO-LOCK, 
        EACH eb OF ef NO-LOCK, 
        EACH box-design-hdr WHERE box-design-hdr.design-no = 0  
        AND box-design-hdr.company = eb.company  
        AND box-design-hdr.est-no = eb.est-no  
        AND box-design-hdr.form-no = eb.form-no  
        AND box-design-hdr.blank-no = eb.blank-no NO-LOCK :
    

    assign lv-wscore = ""
         lv-wcum-score = ""
         li-cnt = 0
         li-line-no = 0.
  
  for each box-design-line of box-design-hdr no-lock by box-design-line.line-no:
      assign lv-wscore = lv-wscore + box-design-line.wscore  + chr(13) 
             /*lv-wcum-score = lv-wcum-score + box-design-line.wcum-score + chr(13)*/
             lv-wcum-score = lv-wcum-score + box-design-line.wcum-score + " "
             li-cnt = li-cnt + 1
             li-line-no[li-cnt] = box-design-line.line-no
             .   
      END.  

  find first style where style.company eq eb.company
                   and style.style   eq eb.style
                 no-lock no-error.
    IF AVAIL style  THEN
        CREATE ttQpoBoxDesign.
        ASSIGN
           
            ttQpoBoxDesign.design_no     = style.design-no 
            ttQpoBoxDesign.design_dscr   = box-design-hdr.description
            ttQpoBoxDesign.lscore        = box-design-hdr.lscore
            ttQpoBoxDesign.lcum_score    = box-design-hdr.lcum-score
            ttQpoBoxDesign.enum          = box-design-hdr.e-num
            ttQpoBoxDesign.est_no        = box-design-hdr.est-no
            ttQpoBoxDesign.eqty          = box-design-hdr.eqty
            ttQpoBoxDesign.box_image     = box-design-hdr.box-image
            ttQpoBoxDesign.box_text      = box-design-hdr.box-text
            ttQpoBoxDesign.box_3d_image  = box-design-hdr.box-3d-image
            ttQpoBoxDesign.DieImage      = ef.cad-image 
            ttQpoBoxDesign.wscore  = lv-wscore
            ttQpoBoxDesign.wcum_score      = lv-wcum-score

                .
        
    END.
END.

/*****************************End Select******************************/
