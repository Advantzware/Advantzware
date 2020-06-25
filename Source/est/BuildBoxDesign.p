
/*------------------------------------------------------------------------
    File        : BuildBoxDesign.p
    Purpose     : Processes Build Box design

    Syntax      :

    Description : Estimate Builder Procedure.

    Author(s)   : Sewa.Singh
    Created     : Thursday Jun 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcRebuild AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.

DEFINE BUFFER xbox-design-hdr  FOR box-design-hdr.
DEFINE BUFFER xbox-design-line FOR box-design-line.


/*Refactor - From B-estitm.w*/
DEFINE NEW SHARED VARIABLE cocode     AS cha     NO-UNDO.
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE NEW SHARED BUFFER xeb  FOR eb.
{cec/descalc.i new}
DEFINE TEMP-TABLE w-box-h NO-UNDO LIKE box-design-hdr.
DEFINE TEMP-TABLE w-box-l NO-UNDO LIKE box-design-line.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND xeb WHERE ROWID(xeb) = ipriRowid NO-LOCK.               
 cocode = xeb.company.
FOR EACH box-design-hdr WHERE box-design-hdr.design-no = 0 AND
                              box-design-hdr.company = xeb.company 
                          AND box-design-hdr.est-no = xeb.est-no      
      AND box-design-hdr.form-no   EQ xeb.form-no
      AND box-design-hdr.blank-no  EQ xeb.blank-no
    NO-LOCK:  

  CREATE w-box-h.
  BUFFER-COPY box-design-hdr TO w-box-h.
END.

{cec/est-6del.i}

FIND FIRST xest NO-LOCK WHERE xest.company = xeb.company AND
                      xest.est-no = xeb.est-no
                      .
FIND FIRST xef NO-LOCK WHERE xef.company = xeb.company 
                 AND xef.est-no   EQ xeb.est-no
                 AND xef.form-no EQ xeb.form-no  .

FIND FIRST style NO-LOCK WHERE style.company EQ xeb.company
                   AND style.style   EQ xeb.style
                 NO-ERROR.
IF AVAILABLE style THEN
  FIND FIRST xbox-design-hdr NO-LOCK WHERE xbox-design-hdr.design-no EQ style.design-no
     			               AND xbox-design-hdr.company   eq style.company       
                               AND xbox-design-hdr.est-no    EQ ""
             NO-ERROR.

IF AVAILABLE xbox-design-hdr THEN DO:
   RUN cec/descalc.p (RECID(xest), RECID(xeb)).
   CREATE box-design-hdr.
   ASSIGN  box-design-hdr.design-no   = 0
           box-design-hdr.company     = xeb.company
           box-design-hdr.est-no      = xeb.est-no
           box-design-hdr.form-no     = xeb.form-no
           box-design-hdr.blank-no    = xeb.blank-no
           box-design-hdr.description = IF AVAILABLE xbox-design-hdr THEN
                                          xbox-design-hdr.description ELSE ""
           box-design-hdr.lscore      = v-lscore-c
           box-design-hdr.lcum-score  = v-lcum-score-c
           box-design-hdr.wscore      = xbox-design-hdr.wscore
           box-design-hdr.wcum-score  = xbox-design-hdr.wcum-score
           box-design-hdr.box-text    = xbox-design-hdr.box-text
           box-design-hdr.box-image   = xbox-design-hdr.box-image
           box-design-hdr.box-3d-image = xbox-design-hdr.box-3d-image
           .

   FOR EACH xbox-design-line OF xbox-design-hdr NO-LOCK:
      CREATE box-design-line.
      ASSIGN box-design-line.design-no = box-design-hdr.design-no
             box-design-line.company   = box-design-hdr.company
             box-design-line.est-no    = box-design-hdr.est-no
             box-design-line.form-no   = box-design-hdr.form-no
             box-design-line.blank-no  = box-design-hdr.blank-no
             box-design-line.line-no   = xbox-design-line.line-no
             box-design-line.line-text = xbox-design-line.line-text.

      FIND FIRST w-box-design-line
           WHERE w-box-design-line.line-no EQ box-design-line.line-no   NO-ERROR.
      IF AVAILABLE w-box-design-line THEN
         ASSIGN  box-design-line.wscore     = w-box-design-line.wscore-c
                 box-design-line.wcum-score = w-box-design-line.wcum-score-c.
   END.

   IF ipcRebuild NE "B" THEN DO:
      IF ipcRebuild EQ "S" THEN
         box-design-hdr.description = w-box-h.description.
      ELSE  ASSIGN box-design-hdr.lscore     = w-box-h.lscore
                   box-design-hdr.lcum-score = w-box-h.lcum-score
                   box-design-hdr.wscore     = w-box-h.wscore
                   box-design-hdr.wcum-score = w-box-h.wcum-score.

      FOR EACH w-box-l OF box-design-hdr NO-LOCK,
          FIRST box-design-line OF w-box-l:

          IF ipcRebuild EQ "S" THEN
             ASSIGN box-design-line.line-no   = w-box-l.line-no
                     box-design-line.line-text = w-box-l.line-text.
          ELSE DO:
             FIND FIRST w-box-design-line
                  WHERE w-box-design-line.line-no EQ w-box-l.line-no   NO-ERROR.
             IF AVAILABLE w-box-design-line THEN
                ASSIGN box-design-line.wscore     = w-box-l.wscore
                       box-design-line.wcum-score = w-box-l.wcum-score.
          END.     
      END.
   END.
END.


FIND CURRENT box-design-line NO-LOCK NO-ERROR.
FIND CURRENT box-design-hdr NO-LOCK NO-ERROR. 

/* **********************  Internal Procedures  *********************** */



