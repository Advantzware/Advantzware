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
    File        : create-event-source.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Apr 22 12:37:15 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cEventSource AS CHARACTER NO-UNDO.

{Consultingwerk/products.i}

/* ***************************  Main Block  *************************** */

&IF DEFINED (DotNetAccessible) NE 0 &THEN
ASSIGN cEventSource = SESSION:PARAMETER . 

System.Diagnostics.EventLog:CreateEventSource (cEventSource, "Application":U) .

System.Environment:Exit (100) .   

CATCH err AS Progress.Lang.Error:
    MESSAGE err:GetMessage (1)
        VIEW-AS ALERT-BOX.
    
    /* On error set exist code */
    System.Environment:Exit (1) .        
END CATCH.
&ENDIF


    