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
    File        : NetworkHelper
    Purpose     : Provides access to .NET network routines
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 23 09:49:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Exceptions.* FROM PROPATH . 
USING Consultingwerk.Util.*       FROM PROPATH .  
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Util.NetworkHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED NetworkHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Downloads a file from a specified web address to a given file 
        Notes:   Currently supported on Windows only   
        @param pcFileAddress The address (URL) to download from
        @param pcTargetFile The file name (and path) to download to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DownloadFile (pcFileAddress AS CHARACTER,
                                            pcTargetFile AS CHARACTER):
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN        
        DEFINE VARIABLE oWebClient AS System.Net.WebClient.
        
        oWebClient = NEW System.Net.WebClient().
        oWebClient:DownloadFile (pcFileAddress,
                                 pcTargetFile).
        
        oWebClient:Dispose() .
        
        DELETE OBJECT oWebClient.
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadFile":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Downloads a string from a specified web address to a given file 
        Notes:   Currently supported on Windows only   
        @param pcFileAddress The address (URL) to download from
        @return A LONGCHAR value with the data from the URL 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LONGCHAR DownloadString (pcFileAddress AS CHARACTER):
        
        DEFINE VARIABLE lcString AS LONGCHAR NO-UNDO . 
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN       
        DEFINE VARIABLE oWebClient AS System.Net.WebClient NO-UNDO .
        
        oWebClient = NEW System.Net.WebClient().
        lcString = oWebClient:DownloadString (pcFileAddress).
                
        RETURN lcString . 

        FINALLY:
            oWebClient:Dispose() .
            
            DELETE OBJECT oWebClient.
        END FINALLY.                
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadString":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .

&IF NOT PROVERSION BEGINS "10.2" &THEN
    /*------------------------------------------------------------------------------
        Purpose: Downloads a string from a specified web address to a given file 
        Notes:   Currently supported on Windows only, not supported on OpenEdge 10.2B
                 due to .NET 4.0 dependency   
        @param pcFileAddress The address (URL) to download from
        @param poStatus The HttpStatus instance with the http status code and description of the request
        @return A LONGCHAR value with the data from the URL 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LONGCHAR DownloadString (pcFileAddress AS CHARACTER,
                                                  OUTPUT poStatus AS HttpStatus):
        
        DEFINE VARIABLE lcString AS LONGCHAR NO-UNDO . 
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN       
        DEFINE VARIABLE oWebClient AS System.Net.WebClient        NO-UNDO .
        DEFINE VARIABLE oFieldInfo AS System.Reflection.FieldInfo NO-UNDO .
        DEFINE VARIABLE oResponse  AS System.Net.HttpWebResponse  NO-UNDO .
        
        poStatus = NEW HttpStatus () . 
        
        oWebClient = NEW System.Net.WebClient().

        oFieldInfo = oWebClient:GetType():GetField("m_WebResponse":U, 
                                                   CAST (Progress.Util.EnumHelper:Or (System.Reflection.BindingFlags:Instance,
                                                                                      System.Reflection.BindingFlags:NonPublic),
                                                         System.Reflection.BindingFlags)) .

        DO ON ERROR UNDO, THROW:
            lcString = oWebClient:DownloadString (pcFileAddress).

            CATCH webex AS System.Net.WebException:
                IF TYPE-OF (webex:Response, System.Net.HttpWebResponse) THEN DO:
                    ASSIGN poStatus:StatusCode        = CAST (webex:Response, System.Net.HttpWebResponse):StatusCode:value__ 
                           poStatus:StatusDescription = CAST (webex:Response, System.Net.HttpWebResponse):StatusDescription . 
                END.
                ELSE 
                    UNDO, THROW webex . 
            END CATCH.

            FINALLY:
                IF VALID-OBJECT (oFieldInfo) THEN DO:
                    oResponse = CAST (oFieldInfo:GetValue (oWebClient), System.Net.HttpWebResponse) .
        
                    IF VALID-OBJECT (oResponse) THEN 
                        ASSIGN poStatus:StatusCode        = oResponse:StatusCode:value__ 
                               poStatus:StatusDescription = oResponse:StatusDescription .  
                END . 
            END FINALLY.
        END.
               
        RETURN lcString . 
        
        FINALLY:
            oWebClient:Dispose() .
            
            DELETE OBJECT oWebClient.            		
        END FINALLY.        
&ELSE
        UNDO, THROW NEW NotSupportedOnThisPlatform ("DownloadString":U,
                                                    "Consultingwerk.Util.NetworkHelper":U) .
&ENDIF
    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Extracts the Host portion from an URL 
        Notes:   
        @param pcUrl The URL that contains the host
        @return The hostname
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER HostFromUrl (pcUrl AS CHARACTER):
		
		IF NUM-ENTRIES (pcUrl, "/":U) < 3 THEN 
		    UNDO, THROW NEW InvalidParameterValueException ("pcUrl":U, pcUrl,
		                                                    "Consultingwerk.Util.NetworkHelper":U) .

        IF ENTRY (2, pcUrl, "/":U) <> "":U THEN 
            UNDO, THROW NEW InvalidParameterValueException ("pcUrl":U, pcUrl,
                                                            "Consultingwerk.Util.NetworkHelper":U) .

        RETURN ENTRY (1, ENTRY (3, pcUrl, "/":U), ":":U) .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Extracts the Resource portion from an http or https URL 
        Notes:   
        @param pcUrl The URL that contains the resource
        @return The resource name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER ResourceFromUrl (pcUrl AS CHARACTER):
        
        DEFINE VARIABLE i    AS INTEGER NO-UNDO.
        DEFINE VARIABLE iPos AS INTEGER NO-UNDO.

        DO i = 1 TO 2:
            iPos = INDEX (pcUrl, "/":U, iPos + 1) .
        END.

        IF NUM-ENTRIES (pcUrl, "/":U) > 3 THEN  
            RETURN SUBSTRING (pcUrl, INDEX (pcUrl, "/":U, iPos + 1)) .     

        IF INDEX (pcUrl, "?":U) > 0 THEN 
            RETURN "/":U + SUBSTRING (pcUrl, INDEX (pcUrl, "?":U)) .
            
        RETURN "/":U .

    END METHOD .
    
END CLASS.
