/*------------------------------------------------------------------------
    File        : CorrBoxDesign 
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Corrugated Box Design

    Author(s)   : 
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */       
                           
DEFINE TEMP-TABLE ttCorrBoxDesign NO-UNDO 
     FIELD vdesign_no   LIKE  box-design-hdr.design-no        /* int */
     FIELD vdesign_dscr LIKE  box-design-hdr.DESCRIPTION    /* char */
     FIELD vlscore      LIKE  box-design-hdr.lscore              /* char */
     FIELD vlcum_score  LIKE  box-design-hdr.lcum-score       /* char */
     FIELD venum        LIKE  box-design-hdr.e-num                 /* int */
     FIELD vform_no     LIKE  box-design-hdr.form-no            /* int */
     FIELD vblank_no    LIKE  box-design-hdr.blank-no         /* int */
     FIELD vest_no      LIKE  box-design-hdr.est-no              /* char */
     FIELD veqty        LIKE  box-design-hdr.eqty                  /* decimal */
     FIELD vbox_image   LIKE  box-design-hdr.box-image       /* char */
     FIELD vwscore      LIKE  box-design-hdr.wscore              /* char */
     FIELD vwcum_score  LIKE  box-design-hdr.wcum-score     /* char */
     FIELD vbox_text    LIKE  box-design-hdr.box-text        /* char */
     FIELD vbox_3d_image LIKE box-design-hdr.box-3d-image   /* char */ 

     FIELD  vBlankQty   AS INT
    FIELD  vFormQty    AS INT
    FIELD  vCustPart   AS CHAR
    FIELD  vEstDate    AS DATETIME 
    FIELD  vDieImage   AS CHAR
        .

DEFINE DATASET dsCorrBoxDesign FOR ttCorrBoxDesign.

    DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDesignNo      AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmDesignDscr    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLScore        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLCumScore     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEnum          AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmFormNo        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBlankNo       AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmEstNo         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEqty          AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmBoxImage      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmWScore        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmWCumScore     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoxText       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBox3DImage    AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrBoxDesign.

    IF prmUser       = ? THEN ASSIGN prmUser        = "" .
    IF prmAction     = ? THEN ASSIGN prmAction      = "" .
    IF prmComp       = ? THEN ASSIGN prmComp        = "" .
    IF prmDesignNo   = ? THEN ASSIGN prmDesignNo    = 0 .
    IF prmDesignDscr = ? THEN ASSIGN prmDesignDscr  = "" .
    IF prmLScore     = ? THEN ASSIGN prmLScore      = "" .
    IF prmLCumScore  = ? THEN ASSIGN prmLCumScore   = "" .
    IF prmEnum       = ? THEN ASSIGN prmEnum        = 0 .
    IF prmFormNo     = ? THEN ASSIGN prmFormNo      = 0 .
    IF prmBlankNo    = ? THEN ASSIGN prmBlankNo     = 0 .
    IF prmEstNo      = ? THEN ASSIGN prmEstNo       = "" .
    IF prmEqty       = ? THEN ASSIGN prmEqty        = 0 .
    IF prmBoxImage   = ? THEN ASSIGN prmBoxImage    = "" .
    IF prmWScore     = ? THEN ASSIGN prmWScore      = "" .
    IF prmWCumScore  = ? THEN ASSIGN prmWCumScore   = "" .
    IF prmBoxText    = ? THEN ASSIGN prmBoxText     = "" .
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


/*******************************Update*********************************/
IF prmAction = "Update" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
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

    prmBoxImage = REPLACE(prmBoxImage,"/","\").
    prmBox3DImage = REPLACE(prmBox3DImage,"/","\") .

  FOR EACH eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo NO-LOCK:
/*FIND FIRST eb WHERE eb.company EQ prmComp AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
            AND eb.form-no EQ prmFormNo NO-LOCK NO-ERROR.*/

FIND FIRST box-design-hdr where box-design-hdr.design-no = 0
                           AND   box-design-hdr.company = eb.company 
                            and box-design-hdr.est-no = eb.est-no   
                            and box-design-hdr.form-no   eq eb.form-no
                            and box-design-hdr.blank-no  eq eb.blank-no
    EXCLUSIVE-LOCK NO-ERROR.


        ASSIGN
            
            box-design-hdr.description   = prmDesignDscr
            box-design-hdr.lscore        = prmLScore
           
            box-design-hdr.box-image     = prmBoxImage
           /* box-design-hdr.wscore        = prmWCumScore
            box-design-hdr.wcum-score    = prmWCumScore*/
            box-design-hdr.box-3d-image  = prmBox3DImage
                .

        find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "BOXDESUM" no-lock no-error.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign v-sc-fmt  = if AVAIL sys-ctrl AND (
                        sys-ctrl.char-fld eq "MM" or
                        (sys-ctrl.char-fld eq "Both" and AVAIL est AND est.metric)) then "->>>>9" else "->9.99"
         ls-prev-wscore = box-design-hdr.wscore
         ls-prev-wcum = box-design-hdr.wcum-score . 

  
  /* width build-screen  assign box-design-line for character */
  assign ls-ws-value = prmWCumScore
         li-pos = 1
         li-pos-nxt = 1
         li-ln = 1
         ls-wscore = "". 
  
         
  FOR EACH box-design-line OF box-design-hdr:
      DELETE box-design-line.
  END.

  do i = 1 to length(ls-ws-value):
     ls-key = substring(ls-ws-value,i,1).
 
 
     if asc(ls-key) < 33 then do:  /* control key chr(13) = return key but says ctrl-j */
        
        create box-design-line.
        assign box-design-line.company = box-design-hdr.company
               box-design-line.design-no = box-design-hdr.design-no
               box-design-line.line-no = li-ln
               box-design-line.est-no     = box-design-hdr.est-no
               box-design-line.form-no   = box-design-hdr.form-no
               box-design-line.blank-no  = box-design-hdr.blank-no
               box-design-line.wscore = IF ls-wscore <> "" THEN (ls-wscore + " ") ELSE ""
               ls-wscore = ""
               li-ln = li-ln + 1.       
        next.       
     end.
     ELSE
        assign ls-wscore = ls-wscore + ls-key.
         
  end.
  /* == Width total assignment ========*/
  def var ld-wcum as dec no-undo.
  def var ls-new-wcum as cha no-undo.
  def var ld-wcum-prev as dec no-undo.
  assign li-ln = 1
         ls-wscore = ""
         ld-wcum = 0
         ls-new-wcum = "".
  do i = 1 to length(ls-ws-value):         
     ls-key = substring(ls-ws-value,i,1).
     if asc(ls-key) < 33 then do:  /* control key */
        find box-design-line of box-design-hdr where box-design-line.line-no = li-ln.
        ld-wcum = ld-wcum + dec(ls-wscore).
        if ld-wcum - trunc(ld-wcum,0) >= 0.16 then assign ld-wcum = ld-wcum + 1 - 0.16. 
        
        assign box-design-line.wcum-score =  if ld-wcum <> 0 and ld-wcum <> ld-wcum-prev
                                             then string(ld-wcum,v-sc-fmt)
                                             else ""
        
               ls-wscore = ""
               li-ln = li-ln + 1
               ld-wcum-prev = ld-wcum.       
        next.       
     end.
     else
        assign ls-wscore = ls-wscore + ls-key.
        
  end.
  
  for each box-design-line of box-design-hdr EXCLUSIVE-LOCK:
      ls-new-wcum = ls-new-wcum + 
                    if box-design-line.wcum-score <> "" then box-design-line.wcum-score
                    else CHR(13).

    /*  box-design-line.wscore =  if box-design-line.wscore <> "" then box-design-line.wcum-score
                                        else CHR(13).*/
                     
  end.
  box-design-hdr.wcum-score = ls-new-wcum.
  

   /*==== lscore assignment */
  def var ls-lscore as cha no-undo.
  def var ld-ls-val as dec no-undo.
  def var li-start as int no-undo.
  def var ls-char as cha no-undo.
  
  assign ls-lscore = ""
         ld-ls-val = 0
         li-start = 0
         ls-char = ""
         box-design-hdr.lcum-score = "".

  do i = 1 to length(prmLScore) :
     ls-char = substring(box-design-hdr.lscore,i,1).
     if ls-char <> "" then do:
        ls-lscore = ls-lscore + ls-char.
        if li-start = 0 then li-start = i.
     end.
     else if ls-lscore <> "" then do:         
          ld-ls-val = ld-ls-val + dec(ls-lscore).

          if ld-ls-val - trunc(ld-ls-val,0) >= 0.16 then
             assign ld-ls-val = ld-ls-val + 1 - 0.16.   

          if length(string(ld-ls-val)) = length(ls-lscore) then                      /*string(ld-ls-val*/
             substring(box-design-hdr.lcum-score, li-start, i - li-start + 1) = trim(string(ld-ls-val,">>>.99")).
          else do:       
              CASE INDEX(ls-lscore,".") :
                 WHEN 1 then
                     substring(box-design-hdr.lcum-score,li-start - 2,(i - li-start + 1)) = string(ld-ls-val,"z.99").        
                 WHEN 2 THEN
                       substring(box-design-hdr.lcum-score,li-start - 2,(i - li-start + 1)) = (string(ld-ls-val,"zzz.99")).        
                 WHEN 3 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = (string(ld-ls-val,">>9.99")).        
                 WHEN 4 THEN                                                                  
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = (string(ld-ls-val,">>9.99")).        

              END CASE.
          END.
          assign ls-lscore = ""
                 li-start = 0.
     end.
  end.

  if ls-lscore <> "" then do:
     ld-ls-val = ld-ls-val + dec(ls-lscore).
     if ld-ls-val - trunc(ld-ls-val,0) >= 0.16 then
             assign ld-ls-val = ld-ls-val + 1 - 0.16.

     if length(string(ld-ls-val)) = length(ls-lscore) then                           /*string(ld-ls-val*/
             substring(box-design-hdr.lcum-score, li-start, i - li-start + 1) = trim(string(ld-ls-val,"zz9.99")).     
     else do:
              CASE INDEX(ls-lscore,".") :
                 WHEN 1 then
                     substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = string(ld-ls-val,"z.99").        
                 WHEN 2 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,"zzz.99")).        
                 WHEN 3 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,">>9.99")).        
                 WHEN 4 THEN
                       substring(box-design-hdr.lcum-score,li-start - 1,(i - li-start + 1)) = trim(string(ld-ls-val,">>9.99")).        

              END CASE.
     END.

     assign ls-lscore = ""
            li-start = 0.    
  end.
 



  

             
  
END.
END.

/*****************************End Update******************************/


/*******************************Select*********************************/
IF prmAction = "Select" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
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
  
  
  FOR EACH eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo AND eb.blank-no = prmBlankNo  NO-LOCK:
/*FIND FIRST eb WHERE eb.company EQ prmComp AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstNo))) + TRIM(prmEstNo)
            AND eb.form-no EQ prmFormNo NO-LOCK NO-ERROR.*/

FIND FIRST box-design-hdr where box-design-hdr.design-no = 0
                            AND    box-design-hdr.company = eb.company 
                            and box-design-hdr.est-no = eb.est-no   
                            and box-design-hdr.form-no   eq eb.form-no
                            and box-design-hdr.blank-no  eq eb.blank-no
    NO-LOCK NO-ERROR.

            IF NOT AVAIL box-design-hdr THEN DO:
                find xeb where recid(xeb) = recid(eb) no-lock.
                find xest where recid(xest) = recid(est) no-lock.
                RUN build-box1 ("B").
            END.

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
/*if avail style then*/
  /*find first box-design-hdr where box-design-hdr.design-no eq style.design-no
                               
             no-lock no-error.*/

        CREATE ttCorrBoxDesign.
        ASSIGN
            ttCorrBoxDesign.vCustPart      = eb.part-no 
            ttCorrBoxDesign.vEstDate       = est.est-date
            /*ttCorrBoxDesign.vdesign_no     = IF AVAIL style THEN style.design-no ELSE ""  
            ttCorrBoxDesign.vdesign_dscr   = box-design-hdr.description
            ttCorrBoxDesign.vlscore        = box-design-hdr.lscore
            ttCorrBoxDesign.vlcum_score    = box-design-hdr.lcum-score
            ttCorrBoxDesign.venum          = box-design-hdr.e-num
            ttCorrBoxDesign.vform_no       = eb.form-no
            ttCorrBoxDesign.vFormQty       = est.form-qty
            ttCorrBoxDesign.vblank_no      = eb.blank-no
            ttCorrBoxDesign.vBlankQty      = bi
            ttCorrBoxDesign.vest_no        = box-design-hdr.est-no
            ttCorrBoxDesign.veqty          = box-design-hdr.eqty
            ttCorrBoxDesign.vbox_image     = box-design-hdr.box-image
            ttCorrBoxDesign.vwscore        = lv-wscore
            ttCorrBoxDesign.vwcum_score    = lv-wcum-score
            ttCorrBoxDesign.vbox_text      = box-design-hdr.box-text
            ttCorrBoxDesign.vbox_3d_image  = box-design-hdr.box-3d-image
            ttCorrBoxDesign.vDieImage      = ef.cad-image */
                .
        
         FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp
                           AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
         ASSIGN
          ttCorrBoxDesign.vDieImage = (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") + ef.cad-image + ".jpg".
END.
END.

/*****************************End Select******************************/
/***********************procuder*************/

PROCEDURE build-box1 :

def input parameter v-rebuild as char.
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR lv-eb-form-no AS INT NO-UNDO.
DEF VAR lv-eb-blank-no AS INT NO-UNDO.

IF NOT AVAIL xeb THEN RETURN.


def buffer xbox-design-hdr  for box-design-hdr.
def buffer xbox-design-line for box-design-line.

cocode = xeb.company.

EMPTY TEMP-TABLE w-box-h.
EMPTY TEMP-TABLE w-box-l.

for each box-design-hdr where box-design-hdr.design-no = 0 and
                              box-design-hdr.company = xeb.company 
                          and box-design-hdr.est-no = xeb.est-no
    /*{cec/est-6.w box-design-hdr}*/
      and box-design-hdr.form-no   eq xeb.form-no
      and box-design-hdr.blank-no  eq xeb.blank-no
    no-lock:

    create w-box-h.
    buffer-copy box-design-hdr to w-box-h.

    FOR EACH box-design-line OF box-design-hdr NO-LOCK:
        CREATE w-box-l.
        BUFFER-COPY box-design-line TO w-box-l.
    END.
end.

IF v-rebuild NE "N" THEN
DO:
   {cec/est-6.del}
END.

find first style where style.company eq xeb.company
                   and style.style   eq xeb.style
                 no-lock no-error.
if avail style then
  find first xbox-design-hdr where xbox-design-hdr.design-no eq style.design-no
                               and xbox-design-hdr.est-no    eq ""
             no-lock no-error.


if avail xbox-design-hdr then do:

   IF v-rebuild NE "N" THEN
   DO:
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
   END. /*if v-rebuild ne "N"*/

   if v-rebuild ne "B" AND v-rebuild NE "N" then do:
      FIND FIRST w-box-h NO-ERROR.

      IF AVAIL w-box-h THEN
      DO:
         if v-rebuild eq "S" then
            ASSIGN box-design-hdr.description = w-box-h.description
                   box-design-hdr.box-image = w-box-h.box-image
                   box-design-hdr.box-3d-image = w-box-h.box-3d-image.
         ELSE
            assign box-design-hdr.lscore      = w-box-h.lscore
                   box-design-hdr.lcum-score  = w-box-h.lcum-score
                   box-design-hdr.wscore      = w-box-h.wscore
                   box-design-hdr.wcum-score  = w-box-h.wcum-score.
        
         for each w-box-l of box-design-hdr,
             first box-design-line of w-box-l:
         
             if v-rebuild eq "S" then
                assign box-design-line.line-no    = w-box-l.line-no
                       box-design-line.line-text  = w-box-l.line-text.
             ELSE
                assign box-design-line.wscore     = w-box-l.wscore
                       box-design-line.wcum-score = w-box-l.wcum-score.
         end.
      END.
   end.
end.

END PROCEDURE.
