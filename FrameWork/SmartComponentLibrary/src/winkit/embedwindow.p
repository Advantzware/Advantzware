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
    File        : embedwindow.p
    Purpose     : 

    Syntax      : phWindow: The handle to the ABL window widget that
                            will be embedded
                  pcFormType: The name of the .NET Form (implementing
                              IEmbeddedWindowForm) that will be used 
                              as Container for the window widget
                  poForm: The reference to the new instance of the 
                          IEmbeddedWindowForm                                 

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 03 16:53:12 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Forms.* .

DEFINE INPUT  PARAMETER phWindow   AS HANDLE              NO-UNDO.
DEFINE INPUT  PARAMETER pcFormType AS CHARACTER           NO-UNDO.
DEFINE OUTPUT PARAMETER poForm     AS IEmbeddedWindowForm NO-UNDO .

/* ***************************  Main Block  *************************** */

IF pcFormType = "":U THEN 
    poForm = NEW EmbeddedWindowForm () .
ELSE 
    poForm = DYNAMIC-NEW (pcFormType) () .

poForm:EmbedWindow(phWindow) .
