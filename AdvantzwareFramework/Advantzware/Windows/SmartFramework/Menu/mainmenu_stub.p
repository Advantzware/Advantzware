/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : mainmenu_stub.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : Mon Feb 08 15:50:55 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{methods/defines/mainmenu.i}

/* ***************************  Main Block  *************************** */

PROCEDURE Set-comp_loc :
/*------------------------------------------------------------------------------
  Purpose:     Set Global and Screen Company/Location Values.
  Parameters:  INPUT company & location values
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-company_name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-loc AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-loc_dscr AS CHARACTER NO-UNDO.

  DEFINE BUFFER sys-ctrl FOR sys-ctrl . 

    ASSIGN
      g_company = ip-company
      g_loc = ip-loc
      .

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
     sys-ctrl.NAME = "bitmap" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = g_company
         sys-ctrl.name     = "bitmap"
         sys-ctrl.descrip  = "images\bigboxes".
   END.

END PROCEDURE.
