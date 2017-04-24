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
    File        : row-entry-handler.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Aug 10 06:49:04 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .

DEFINE INPUT  PARAMETER phBrowse AS HANDLE NO-UNDO.

/* ***************************  Main Block  *************************** */

{Consultingwerk/Assertion/HandleAssert/WidgetType.i phBrowse ""BROWSE"":U} .

ON "ROW-ENTRY":U OF phBrowse
DO:
    /* disabled temporarily */
END.
