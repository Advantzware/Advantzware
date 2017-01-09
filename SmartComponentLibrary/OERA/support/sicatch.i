/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : sicatch.i
    Purpose     : Standard catch block include file for service interface
                  procedures

    Syntax      :

    Description : Responsible for returning runtime errors to the client

    Author(s)   : 
    Created     : Thu Oct 09 17:59:38 CEST 2014
    Notes       : Handles pre OpenEdge 11.4 in traditional way with 
                  RETURN ERROR <error message> . 
                  In OpenEdge 11.4 the ability to throw an error from the 
                  server to the client was introduced
  ----------------------------------------------------------------------*/

DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
DEFINE VARIABLE iError  AS INTEGER   NO-UNDO .

&IF PROVERSION BEGINS "10" OR PROVERSION BEGINS "11.0" OR PROVERSION BEGINS "11.1" OR PROVERSION BEGINS "11.2" OR PROVERSION BEGINS "11.3" &THEN
/* traditional return of error */
&SCOPED-DEFINE CanThrowErrors no
&ELSE
/* rethrow errors as we are above 11.4 */
&SCOPED-DEFINE CanThrowErrors yes
&ENDIF

CATCH err AS Progress.Lang.Error:
     
    IF NOT SESSION:REMOTE THEN 
        UNDO, THROW err . 
     
    &IF "{&CanThrowErrors}" EQ "yes" &THEN
    /* Mike Fechner, Consultingwerk Ltd. 09.10.2014
       Throw Error to the client, when client is OpenEdge 11.4 and client is an ABL Client (WebClient, 4GLClient) */
    IF err:GetClass():IsSerializable() AND 
       VALID-HANDLE (Consultingwerk.Framework.Session.SessionManager:ContextDataset) AND 
       
       Consultingwerk.ProVersionEnum:ToDecimal (Consultingwerk.OERA.ContextWrapper:ClientProVersion) >= 11.4 AND 
       Consultingwerk.ClientType:IsKnownClientType (Consultingwerk.OERA.ContextWrapper:ClientType) THEN DO:
    
        Consultingwerk.Util.LogManager:WriteFormattedMessage ("Throwing error:  &1":U, err:GetClass():TypeName) . 
       
        UNDO, THROW err . 
    END.        
    &ENDIF
    
    ASSIGN cReturn = "":U .
    
    DO iError = 1 TO err:NumMessages:
        cReturn = cReturn + err:GetMessage(iError) + "~n":U . 
    END.
    
    IF SESSION:ERROR-STACK-TRACE = TRUE THEN 
        RETURN ERROR cReturn + "~n":U + 
                     "Backend-Stacktrace:":U + "~n":U + 
                     err:CallStack.
    ELSE
        RETURN ERROR TRIM (cReturn, "~n":U) .    
END CATCH.
