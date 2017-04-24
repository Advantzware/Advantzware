/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*----------------------------------------------------------------------------

File: get-relations.p
From File: _treldat.p (Progress ADE Source)

Description:
   Display table relations to the currently set output device (e.g., a file,
   the printer).

Input Parameters:
   p_DbId    - Id of the _Db record for this database.
   p_Tbl     - The table to show relations for or "ALL"

Author: Tony Lavinio, Laura Stern

Date Created: 10/12/92

Modified on 05/31/95 gfs Allow display of hidden tables (not meta-schema).
            06/14/94 gfs Added NO-LOCKs to file accesses.
            07/10/98 DLM Added DBVERSION and _Owner check
----------------------------------------------------------------------------*/

USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

DEFINE NEW GLOBAL SHARED VARIABLE fhidden AS LOGICAL NO-UNDO INITIAL NO. /*{ prodict/fhidden.i } */

{ Consultingwerk/SmartFramework/System/dsMissingRelations.i &BY-REFERENCE=BY-REFERENCE}


DEFINE INPUT PARAMETER p_DbId  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl   AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER DATASET FOR dsMissingRelations .


DEFINE WORKFILE g_relate NO-UNDO
  FIELD g_owner  LIKE _File._File-name
  FIELD g_member LIKE _File._File-name
  FIELD g_idx    LIKE _Index._Index-name.

DEFINE BUFFER g_mfile  FOR dictdb._File.
DEFINE BUFFER g_xfield FOR dictdb._Field.
DEFINE BUFFER g_xfile  FOR dictdb._File.

DEFINE BUFFER _db FOR dictdb._db .
DEFINE BUFFER _file FOR dictdb._file.
DEFINE BUFFER _field FOR dictdb._field.
DEFINE BUFFER _index FOR dictdb._index.
DEFINE BUFFER _Index-field FOR dictdb._Index-field.

DEFINE VARIABLE noway    AS LOGICAL NO-UNDO.
/*DEFINE VARIABLE line     AS CHAR    NO-UNDO.*/

/*FORM                                                                 */
/*  line FORMAT "x(77)" NO-LABEL                                       */
/*  WITH FRAME rptline DOWN NO-BOX USE-TEXT STREAM-IO.                 */
/*                                                                     */
/*FORM                                                                 */
/*  SKIP(1)                                                            */
/*  SPACE(3) g_mfile._File-name LABEL "Working on" FORMAT "x(32)" SPACE*/
/*  SKIP(1)                                                            */
/*  WITH FRAME working_on SIDE-LABELS VIEW-AS DIALOG-BOX               */
/*  TITLE "Generating Report".                                         */

/*-----------------------------Mainline code---------------------------*/

FIND _DB WHERE RECID(_Db) = p_DbId NO-LOCK.
IF INTEGER(DBVERSION("DICTDB":U)) > 8 THEN DO:
  FIND _File WHERE  _File._File-name = "_File":U
               AND _File._Owner = "PUB":U NO-LOCK.
  noway = CAN-DO(_Can-read,USERID("DICTDB":U)).
  FIND _File WHERE _File._File-name = "_Field":U
               AND _File._Owner = "PUB":U NO-LOCK.
  noway = noway AND CAN-DO(_Can-read,USERID("DICTDB":U)).
  FIND _File WHERE  _File._File-name = "_Index":U
               AND _File._Owner = "PUB":U NO-LOCK.
  noway = noway AND CAN-DO(_Can-read,USERID("DICTDB":U)).
  FIND _File WHERE _File._File-name = "_Index-field":U
               AND _File._Owner = "PUB":U NO-LOCK.
  noway = noway AND CAN-DO(_Can-read,USERID("DICTDB":U)).
END.
ELSE DO:
  FIND _File "_File":U NO-LOCK.
  noway = CAN-DO(_Can-read,USERID("DICTDB":U)).
  FIND _File "_Index":U NO-LOCK.
  noway = noway AND CAN-DO(_Can-read,USERID("DICTDB":U)).
  FIND _File "_Field":U NO-LOCK.
  noway = noway AND CAN-DO(_Can-read,USERID("DICTDB":U)).
END.

IF NOT noway THEN DO:
  UNDO, THROW NEW AppError ("You do not have permission to use this option":U, 0) .
END.

IF p_Tbl = "ALL":U THEN
  SESSION:IMMEDIATE-DISPLAY = YES.

FOR EACH g_mfile NO-LOCK
  WHERE g_mfile._Db-recid = p_DbId
  AND (IF p_Tbl = "ALL":U THEN (IF NOT fHidden THEN NOT _Hidden ELSE g_mfile._File-Number > 0)
                        ELSE g_mfile._File-name = p_Tbl) ON ERROR UNDO, THROW:
  IF INTEGER(DBVERSION("DICTDB":U)) > 8 AND
    (g_mfile._Owner <> "PUB":U AND g_mfile._Owner <> "_FOREIGN":U) THEN
        NEXT.

/*  IF p_Tbl = "ALL" THEN                              */
/*    DISPLAY g_mfile._File-name WITH FRAME working_on.*/

  /* Clear work file */
  FOR EACH g_relate NO-LOCK:
      DELETE g_relate.
  END.

  FOR
    EACH _Index       WHERE _Unique NO-LOCK,
    EACH _File        OF _Index WHERE _File._File-number > 0 NO-LOCK,
    EACH _Index-field OF _Index NO-LOCK,
    EACH _Field       OF _Index-field NO-LOCK:

    IF _Index-seq = 1 THEN
      FOR EACH g_xfield WHERE g_xfield._Field-name = _Field._Field-name
        AND RECID(g_xfield) <> RECID(_Field) NO-LOCK,
        EACH g_xfile OF g_xfield NO-LOCK ON ERROR UNDO, THROW:
        IF g_mfile._File-name <> _File._File-name
          AND g_mfile._File-name <> g_xfile._File-name THEN
            NEXT.
        CREATE g_relate.
        ASSIGN
          g_relate.g_owner  = _File._File-name
          g_relate.g_member = g_xfile._File-name
          g_relate.g_idx    = _Index._Index-name.
      END.
    ELSE
      FOR EACH g_relate
        WHERE g_idx = _Index._Index-name AND g_owner = _File._File-name,
        EACH g_xfile WHERE g_xfile._File-name = g_member NO-LOCK ON ERROR UNDO, THROW:
        IF NOT CAN-FIND(g_xfield OF g_xfile
          WHERE g_xfield._Field-name = _Field._Field-name)
          AND AVAILABLE g_relate
          THEN DELETE g_relate.
      END.
  END.

  FOR EACH g_relate NO-LOCK BREAK BY g_owner BY g_member ON ERROR UNDO, THROW:
    IF NOT (FIRST-OF(g_member) AND LAST-OF(g_member)) THEN
        DELETE g_relate.
  END.


/*  DISPLAY STREAM rpt g_mfile._File-nam + ":" @ line WITH FRAME rptline.*/
/*  DOWN STREAM rpt WITH FRAME rptline.                                  */
/*  line= ?.                                                             */

  FOR EACH g_relate NO-LOCK WHERE g_owner = g_mfile._File-name BY g_owner:

    CREATE ttRelation .
    ASSIGN ttRelation.ParentTable = g_relate.g_owner
           ttRelation.ChildTable  = g_relate.g_member
           ttRelation.IndexName   = g_relate.g_idx.


/*    line = "  " + g_member + " OF " + g_owner + " ".                                   */
    FIND g_xfile NO-LOCK WHERE g_xfile._db-recid = p_DbId
                           AND g_xfile._File-name = g_owner
                           AND (g_xfile._Owner = "PUB":U OR g_xfile._Owner = "_FOREIGN":U).
    FIND _Index OF g_xfile NO-LOCK WHERE _Index-name = g_idx.
    FOR EACH _Index-field OF _Index NO-LOCK, EACH _Field OF _Index-field NO-LOCK:
        ttRelation.IndexFields = ttRelation.IndexFields + _Field-name + ",":U .
/*      line = line + STRING(_Index-seq > 1,",/(") + _Field-name.                        */
    END.

    ttRelation.IndexFields = TRIM (ttRelation.IndexFields, ",":U) .

/*    line = line + ")".                                                                 */
/*    DISPLAY STREAM rpt line WITH FRAME rptline.                                        */
/*    DOWN STREAM rpt WITH FRAME rptline.                                                */
  END.

/*  FOR EACH g_relate NO-LOCK                                                            */
/*        WHERE g_member = g_mfile._File-name BY g_member:                               */
/*    line = "  " + g_member + " OF " + g_owner + " ".                                   */
/*    FIND g_xfile NO-LOCK WHERE g_xfile._db-recid = p_DbId                              */
/*                           AND g_xfile._File-name = g_owner                            */
/*                           AND (g_xfile._Owner = "PUB" OR g_xfile._Owner = "_FOREIGN").*/
/*    FIND _Index OF g_xfile NO-LOCK WHERE _Index-name = g_idx.                          */
/*    FOR EACH _Index-field OF _Index,EACH _Field OF _Index-field NO-LOCK:               */
/*      line = line + STRING(_Index-seq > 1,",/(") + _Field-name.                        */
/*    END.                                                                               */
/*    line = line + ")".                                                                 */
/*    DISPLAY STREAM rpt line WITH FRAME rptline.                                        */
/*    DOWN STREAM rpt WITH FRAME rptline.                                                */
/*  END.                                                                                 */

/*  IF line = ? THEN DO:                                       */
/*    DISPLAY STREAM rpt "   No relations found for this file."*/
/*      @ line WITH FRAME rptline.                             */
/*    DOWN STREAM rpt 2 WITH FRAME rptline.                    */
/*  END.                                                       */
/*  ELSE                                                       */
/*    DOWN STREAM rpt 1 WITH FRAME rptline.                    */
END.



/*IF p_Tbl = "ALL" THEN DO:
  HIDE FRAME working_on NO-PAUSE.
  SESSION:IMMEDIATE-DISPLAY = no.
END.*/
/* _treldat.p - end of file */



