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
    File        : RollbaseAdapter
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 03 09:51:43 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                               FROM PROPATH .
USING Consultingwerk.Assertion.*                     FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.*               FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.Data.*          FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.Login.*         FROM PROPATH .
USING Consultingwerk.RollbaseAdapter.MetaData.*      FROM PROPATH .
USING Consultingwerk.Util.*                          FROM PROPATH .
USING Consultingwerk.Web.*                           FROM PROPATH .
USING Progress.Json.ObjectModel.*                    FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.RollbaseAdapter.RollbaseAdapter
    IMPLEMENTS IRollbaseAdapter:

    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDef.i }

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Credentials to be used for Rollbase
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Credentials AS IRollbaseCredentials NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the URL to be used for communication with rollbase
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RollbaseRestApiUrl AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Session ID used by the Rollbase Adapter
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SessionId AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RollbaseAdapter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RollbaseAdapter ():

        DEFINE VARIABLE oConfiguration AS Consultingwerk.Framework.IConfigurationProvider NO-UNDO .

        SUPER ().

        oConfiguration = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        THIS-OBJECT:Credentials = {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseCredentials}  .

        IF VALID-OBJECT (oConfiguration) THEN
            THIS-OBJECT:RollbaseRestApiUrl = oConfiguration:GetValue ("rollbaseRestApiUrl":U,
                                                                      "https://www.rollbase.com/rest/api/":U) .
        ELSE
            THIS-OBJECT:RollbaseRestApiUrl = "https://www.rollbase.com/rest/api/":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs an create2 request to create a record with given field
                 values for a new Rollbase object instance
        Notes:
        @param pcObjectName The Integration Name of the obejct to create
        @param poValues The List of Name Value Pairs for of field names and values
        @return The Character reponse from Rollbase
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Create2 (pcObjectName AS CHARACTER,
                                     poValues AS Consultingwerk.ListNameValuePair):

        DEFINE VARIABLE cRequestUrl     AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR                     NO-UNDO .
        DEFINE VARIABLE lLogoff         AS LOGICAL                      NO-UNDO INIT FALSE .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cRequestUrl = SUBSTITUTE ("&1create2?sessionId=&2&&objName=&3":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId,
                                         pcObjectName) .

        {Consultingwerk/foreachABL.i NameValuePair oPair in poValues}

            ASSIGN cRequestUrl = cRequestUrl + SUBSTITUTE ("&&&1=&2":U,
                                                           StringHelper:UrlEncode (oPair:Name, "QUERY":U),
                                                           StringHelper:UrlEncode (oPair:Value, "QUERY":U)).
        END.

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        RETURN STRING (lcResponse) .

        FINALLY:
            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a DATE from a Rollbase Returned Date String
        Notes:
        @param pcDateString The String containing a DATE
        @return The DATE value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATE DateValue (pcDateString AS CHARACTER):

        /* Remove weekday */

        ASSIGN pcDateString = REPLACE (pcDateString, "Mon ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Tue ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Wed ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Thu ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Fri ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Sat ":U, "":U)
               pcDateString = REPLACE (pcDateString, "Sun ":U, "":U)
               .

        /* Replace month */
        ASSIGN pcDateString = REPLACE (pcDateString, "Jan ":U, "01 ":U)
               pcDateString = REPLACE (pcDateString, "Feb ":U, "02 ":U)
               pcDateString = REPLACE (pcDateString, "Mar ":U, "03 ":U)
               pcDateString = REPLACE (pcDateString, "Apr ":U, "04 ":U)
               pcDateString = REPLACE (pcDateString, "May ":U, "05 ":U)
               pcDateString = REPLACE (pcDateString, "Jun ":U, "06 ":U)
               pcDateString = REPLACE (pcDateString, "Jul ":U, "07 ":U)
               pcDateString = REPLACE (pcDateString, "Aug ":U, "08 ":U)
               pcDateString = REPLACE (pcDateString, "Sep ":U, "09 ":U)
               pcDateString = REPLACE (pcDateString, "Oct ":U, "10 ":U)
               pcDateString = REPLACE (pcDateString, "Nov ":U, "11 ":U)
               pcDateString = REPLACE (pcDateString, "Dec ":U, "12 ":U)
               .

        RETURN DATE (INTEGER (ENTRY (1, pcDateString, " ":U)),
                     INTEGER (ENTRY (2, pcDateString, " ":U)),
                     INTEGER (ENTRY (3, pcDateString, " ":U))) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a DATETIME from a Rollbase Returned DateTime String
        Notes:
        @param pcDateTimeString The String containing a DATE
        @return The DATETIME value
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC DATETIME DateTimeValue (pcDateTimeString AS CHARACTER):

        /* Remove weekday */

        ASSIGN pcDateTimeString = REPLACE (pcDateTimeString, "Mon ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Tue ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Wed ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Thu ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Fri ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Sat ":U, "":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Sun ":U, "":U)
               .

        /* Replace month */
        ASSIGN pcDateTimeString = REPLACE (pcDateTimeString, "Jan ":U, "01 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Feb ":U, "02 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Mar ":U, "03 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Apr ":U, "04 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "May ":U, "05 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Jun ":U, "06 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Jul ":U, "07 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Aug ":U, "08 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Sep ":U, "09 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Oct ":U, "10 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Nov ":U, "11 ":U)
               pcDateTimeString = REPLACE (pcDateTimeString, "Dec ":U, "12 ":U)
               .

        RETURN DATETIME (INTEGER (ENTRY (1, pcDateTimeString, " ":U)),
                         INTEGER (ENTRY (2, pcDateTimeString, " ":U)),
                         INTEGER (ENTRY (3, pcDateTimeString, " ":U)),
                         INTEGER (ENTRY (1, ENTRY (4, pcDateTimeString, " ":U), ":":U)),
                         INTEGER (ENTRY (2, ENTRY (4, pcDateTimeString, " ":U), ":":U)),
                         INTEGER (ENTRY (3, ENTRY (4, pcDateTimeString, " ":U), ":":U))) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deletes a record
        Notes:
        @param piId The Id of the record to delete
        @return Logical value indicating if the record was deleted
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Delete (piId AS INT64):

        DEFINE VARIABLE cRequestUrl AS CHARACTER         NO-UNDO .
        DEFINE VARIABLE lcResponse  AS LONGCHAR          NO-UNDO .
        DEFINE VARIABLE lLogoff     AS LOGICAL           NO-UNDO INIT FALSE .

        DEFINE VARIABLE oParser     AS ObjectModelParser NO-UNDO .
        DEFINE VARIABLE oResponse   AS JsonObject        NO-UNDO .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cRequestUrl = SUBSTITUTE ("&1delete?sessionId=&2&&id=&3&&output=json":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId,
                                         piId) .

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        ASSIGN oParser   = NEW Progress.Json.ObjectModel.ObjectModelParser ()
               oResponse = CAST (oParser:Parse (lcResponse), JsonObject) .

        IF oResponse:Has ("status":U) AND oResponse:GetCharacter ("status":U) = "ok":U THEN
            RETURN TRUE .

        RETURN FALSE .

        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        @SuppressUnusedWarnings.
        CATCH err AS System.Net.WebException:
            RETURN FALSE .
        END CATCH.
        &ENDIF

        FINALLY:

            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Retrieves Rollbase Object Definition at a dsObjectDef Dataset
        Notes:
        @param pcObjectName The name of the object (class) to retrieve the definition for
        @param dsObjectDef OUTPUT Dataset The dataset with the object defintion
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetObjectDef (pcObjectName AS CHARACTER,
                                     OUTPUT DATASET dsObjectDef):

        DEFINE VARIABLE cRequestUrl     AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR                     NO-UNDO .
        DEFINE VARIABLE lLogoff         AS LOGICAL                      NO-UNDO INIT FALSE .

        DATASET dsObjectDef:EMPTY-DATASET () .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cRequestUrl = SUBSTITUTE ("&1getObjectDef?sessionId=&2&&objName=&3":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId,
                                         pcObjectName) .

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        DATASET dsObjectDef:XML-NODE-NAME = "resp":U .

        DATASET dsObjectDef:READ-XML ("LONGCHAR":U, lcResponse, ? , ?, ?) .

        FINALLY:
            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns an Character Array of available object definitions
        Notes:
        @param poPermission The optional PermissionEnum value controlling if the output should be limited to certain objects
        @return The Character Array of available object definitions
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER EXTENT GetObjectDefNames (poPermission AS PermissionEnum):

        DEFINE VARIABLE cRequestUrl     AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR                     NO-UNDO .
        DEFINE VARIABLE lLogoff         AS LOGICAL                      NO-UNDO INIT FALSE .
        DEFINE VARIABLE oResponseParser AS ObjectDefNamesResponseParser NO-UNDO .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cRequestUrl = SUBSTITUTE ("&1getObjectDefNames?sessionId=&2&&output=json":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId) .

        IF VALID-OBJECT (poPermission) THEN
            ASSIGN cRequestUrl = SUBSTITUTE ("&1&&permission?&2":U,
                                             cRequestUrl,
                                             StringHelper:UrlEncode (LC(poPermission:ToString()),
                                                                     "QUERY":U)) .

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        oResponseParser = NEW ObjectDefNamesResponseParser () .

        RETURN oResponseParser:ParseResponse (lcResponse) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oResponseParser) .

            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Authenticates with Rollbase
        Notes:   Uses the Credentials property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Login ():

        DEFINE VARIABLE cRequestUrl     AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR            NO-UNDO .
        DEFINE VARIABLE oResponseParser AS LoginResponseParser NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:Credentials) THEN
            UNDO, THROW NEW RollbaseLoginException ("Need to set Credentials before Login."{&TRAN}, 0) .

        ASSIGN cRequestUrl = SUBSTITUTE ("&1login?loginName=&2&&password=&3":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         StringHelper:UrlEncode (THIS-OBJECT:Credentials:UserName,
                                                                 "QUERY":U),
                                         StringHelper:UrlEncode (THIS-OBJECT:Credentials:Password,
                                                                 "QUERY":U)) .

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        oResponseParser = NEW LoginResponseParser () .
        oResponseParser:ParseResponse (lcResponse) .

        IF oResponseParser:ErrorMessage > "":U THEN
            UNDO, THROW NEW RollbaseLoginException (oResponseParser:ErrorMessage, 0) .

        IF CharacterType:IsNullOrEmpty (oResponseParser:SessionId) THEN
            UNDO, THROW NEW RollbaseLoginException ("No session id received from Rollbase host."{&TRAN}, 0) .

        THIS-OBJECT:SessionId = oResponseParser:SessionId .

        CATCH rberr AS RollbaseException:
            UNDO, THROW rberr .
        END CATCH.

        CATCH err AS Progress.Lang.Error:
            LogManager:WriteError (err) .

            UNDO, THROW NEW RollbaseException (err, "Error while rollbase login call."{&TRAN}, 0) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oResponseParser) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Closes a Rollbase session
        Notes:   Automatically executed when the Service Interface Deactivates
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Logoff ():

        DEFINE VARIABLE cRequestUrl     AS CHARACTER           NO-UNDO .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN
            RETURN .

        ObjectAssert:IsValid (THIS-OBJECT:Credentials,
                              "Credentials":U) .

        ASSIGN cRequestUrl = SUBSTITUTE ("&1logout?sessionId=&2":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         StringHelper:UrlEncode (THIS-OBJECT:SessionId,
                                                                 "QUERY":U))  .

        THIS-OBJECT:PerformRequest (cRequestUrl) .

        THIS-OBJECT:SessionId = "":U .

        CATCH rberr AS RollbaseException:
            UNDO, THROW rberr .
        END CATCH.

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW RollbaseException (err, "Error while rollbase login call."{&TRAN}, 0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the Rollbase Request using an HTTP GET
        Notes:
        @param pcRequestUrl The Url to request from
        @return The LONGCHAR Request response
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR PerformRequest (pcRequestUrl AS CHARACTER):

        DEFINE VARIABLE oHttpClient     AS HttpClient          NO-UNDO .
        DEFINE VARIABLE cRollbaseHost   AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR            NO-UNDO .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        cRollbaseHost = Consultingwerk.Util.NetworkHelper:HostFromUrl (pcRequestUrl) .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF SESSION:WINDOW-SYSTEM BEGINS "MS-WIN":U THEN DO:
            lcResponse = Consultingwerk.Util.NetworkHelper:DownloadString (pcRequestUrl) .
        END.
        ELSE DO:
&ENDIF
            oHttpClient = NEW HttpClient () .

            lcResponse = oHttpClient:GetLongchar (cRollbaseHost,
                                                  pcRequestUrl) .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        END.
&ENDIF
        RETURN lcResponse .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oHttpClient) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL Data Type Name for a Rollbase Field Type
        Notes:   Returns ? for unknown/unmappable field types
        @param pcRollbaseType The Rollbase DataType
        @return The ABL Data Type
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER RollbaseDataTypeToAbl (pcRollbaseType AS CHARACTER):

        CASE pcRollbaseType:
            WHEN "FieldBoolean":U       THEN
                RETURN "LOGICAL":U .
/*            WHEN "FieldComment":U       THEN RETURN "CLOB":U .*/
            WHEN "FieldDate":U          THEN
                RETURN "DATE":U .
            WHEN "FieldDateTime":U      THEN
                RETURN "DATETIME":U .
            WHEN "FieldDouble":U        THEN
                RETURN "DECIMAL":U .
            WHEN "FieldInt":U           THEN
                RETURN "INT":U .
            WHEN "FieldLong":U          THEN
                RETURN "INT64":U .
            WHEN "FieldString":U        THEN
                RETURN "CHARACTER":U .

            OTHERWISE
                RETURN ? .
        END CASE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs an selectQuery Request and populates the results to an
                 ABL temp-table
        Notes:
        @param pcObjectName The name of the object to retrieve data from
        @param pcQuery The rollbase QueryString to use, include WHERE when providing a filter
        @param piStartRow The number of the row to start output with (0 based)
        @param piNumRows The number of rows to return
        @param phTable INPUT-OUTPUT TABLE-HANLDLE The name of the temp-table to select into
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SelectToTempTable (pcObjectName AS CHARACTER,
                                          pcQuery AS CHARACTER,
                                          piStartRow AS INTEGER,
                                          piNumRows AS INTEGER,
                                          INPUT-OUTPUT TABLE-HANDLE phTable):

        DEFINE VARIABLE hColumn     AS HANDLE                    NO-UNDO .
        DEFINE VARIABLE cColumns    AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cRequestUrl AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE lcResponse  AS LONGCHAR                  NO-UNDO .
        DEFINE VARIABLE lLogoff     AS LOGICAL                   NO-UNDO INIT FALSE .
        DEFINE VARIABLE oParser     AS SelectQueryResponseParser NO-UNDO .

        DEFINE VARIABLE cQuery      AS CHARACTER                 NO-UNDO .

        DO i = 1 TO phTable:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:

            ASSIGN hColumn  = phTable:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD (i).

            /* Mike Liewehr, AKIOMA KG / 01.05.2014
               Allow for providing field subsets instead of only all fields
               corresponds to same check in SelectQueryResponseParser */
            IF NOT (hColumn:XML-NODE-TYPE = "HIDDEN":U) THEN
            DO:
                   cColumns = cColumns +
                              (IF i > 1 THEN ", ":U ELSE "":U) +
                              (IF hColumn:XML-NODE-NAME > "":U THEN hColumn:XML-NODE-NAME ELSE hColumn:NAME) .
            END.
        END.

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cQuery = SUBSTITUTE ("select &1 from &2 &3":U,
                                    cColumns,
                                    pcObjectName,
                                    pcQuery) .

        ASSIGN cRequestUrl = SUBSTITUTE ("&1selectQuery?sessionId=&2&&output=json&&startRow=&3&&maxRows=&4&&query=&5":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId,
                                         piStartRow,
                                         piNumRows,
                                         StringHelper:UrlEncode(cQuery, "QUERY":U)) .

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        oParser = NEW SelectQueryResponseParser () .

        oParser:ParseJsonResponse (lcResponse,
                                   phTable:DEFAULT-BUFFER-HANDLE) .

        FINALLY:
            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .

            IF phTable:NUM-REFERENCES = 1 THEN
                DELETE OBJECT phTable .

            GarbageCollectorHelper:DeleteObject (oParser) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs an update2 request to update field values of a Rollbase
                 object instance
        Notes:
        @param piObjectId The Id of the object to update
        @param poValues The List of Name Value Pairs for of field names and values
        @return The Character reponse from Rollbase
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER Update2 (piObjectId AS INT64,
                                     poValues AS Consultingwerk.ListNameValuePair):

        DEFINE VARIABLE cRequestUrl     AS CHARACTER                    NO-UNDO .
        DEFINE VARIABLE lcResponse      AS LONGCHAR                     NO-UNDO .
        DEFINE VARIABLE lLogoff         AS LOGICAL                      NO-UNDO INIT FALSE .
        DEFINE VARIABLE oResponseParser AS ObjectDefNamesResponseParser NO-UNDO .

        FIX-CODEPAGE (lcResponse) = "utf-8":U .

        IF CharacterType:IsNullOrEmpty (THIS-OBJECT:SessionId) THEN DO:
            ASSIGN lLogoff = TRUE .
            THIS-OBJECT:Login() .
        END.

        ASSIGN cRequestUrl = SUBSTITUTE ("&1update2?sessionId=&2&&id=&3":U,
                                         THIS-OBJECT:RollbaseRestApiUrl,
                                         THIS-OBJECT:SessionId,
                                         piObjectId) .

        {Consultingwerk/foreachABL.i NameValuePair oPair in poValues}

            ASSIGN cRequestUrl = cRequestUrl + SUBSTITUTE ("&&&1=&2":U,
                                                           StringHelper:UrlEncode (oPair:Name, "QUERY":U),
                                                           StringHelper:UrlEncode (oPair:Value, "QUERY":U)).
        END.

        lcResponse = THIS-OBJECT:PerformRequest (cRequestUrl) .

        RETURN STRING (lcResponse) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oResponseParser) .

            IF lLogoff = TRUE THEN
                THIS-OBJECT:Logoff() .
        END FINALLY.

    END METHOD .

END CLASS.
