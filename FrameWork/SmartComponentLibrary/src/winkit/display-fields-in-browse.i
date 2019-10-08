/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : display-fields-in-browse.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 09 10:06:40 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE WinKitDisplayFieldsIn{1} :
/*------------------------------------------------------------------------------
    Purpose: Callback to enable color coding and calculated fields for the
             RenderedBrowseControl
    Notes:
------------------------------------------------------------------------------*/

    DISPLAY {&FIELDS-IN-QUERY-{1}} WITH BROWSE {1} NO-ERROR .

END PROCEDURE.

/* Mike Fechner, Consultingwerk Ltd. 20.08.2013
   Optionally set a column read-only - in case it was made enabled in the
   DEFINE BROWSE statement to facilitate the calculated field behaviour. */
&IF "{2}" NE "" &THEN
{2}:READ-ONLY IN BROWSE {1} = FALSE .
&ENDIF

