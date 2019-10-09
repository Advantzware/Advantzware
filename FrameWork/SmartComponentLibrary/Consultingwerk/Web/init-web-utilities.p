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
    File        : init-web-utilities.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Oct 31 22:50:13 GMT+01:00 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Web.* FROM PROPATH.

DEFINE VARIABLE hWebStart AS HANDLE NO-UNDO.

{ src/web/method/cgidefs.i  NEW } /* standard WS cgidefs.i: functions,vars */
{ src/web/method/cgiarray.i NEW } /* standard WS cgiarray.i: vars          */ 
{ src/web/method/tagmap.i   NEW } /* standard WS tagmap.i: TT tagmap       */
{ src/web/method/webutils.i NEW }

/* Also defined in web/objects/web-util.p and adeuib/_semain.w.  This needs to
   be centralized. */
DEFINE NEW SHARED VARIABLE server-connection AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE transaction-state AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

/* Load standard and user-defined super procedures.  web/objects/web-util.p and 
   init-session runs within.  This program is set as web-utilities-hdl. */
RUN webutil/webstart.p PERSISTENT SET hWebStart.

RUN Consultingwerk/Web/Support/refresh-web-context.p .
