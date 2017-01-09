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
    File        : UserGroupDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : 11.10.2012 00:37:56
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                              FROM PROPATH .
USING Consultingwerk.OERA.*                         FROM PROPATH .
USING Consultingwerk.SmartFramework.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .
USING Progress.Lang.*                               FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.UserGroupDataAccess
    INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/Authorization/dsUserGroup.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE QUERY qSmartUserGroupSmartUserSmartGroup FOR
        SmartUserGroup, SmartUser, SmartGroup SCROLLING.
    DEFINE PRIVATE DATA-SOURCE src_SmartUserGroup FOR QUERY qSmartUserGroupSmartUserSmartGroup .

    DEFINE PRIVATE VARIABLE cCurrentGroupGuid        AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cCurrentUserGuid         AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE cCurrentLoginCompanyGuid AS CHARACTER NO-UNDO.
    DEFINE PRIVATE VARIABLE lGroupAssignment         AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE lUseOuterJoin AS LOGICAL NO-UNDO INIT TRUE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserGroupDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserGroupDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUserGroup:HANDLE, FALSE) .

        BUFFER eSmartUserGroup:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartUserGroup:HANDLE, "UserGroupGuid,SmartUserGroup.UserGroupGuid,GroupGuid,SmartUserGroup.GroupGuid,UserGuid,SmartUserGroup.UserGuid":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsUserGroup DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsUserGroup BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

        BUFFER eSmartUserGroup:SET-CALLBACK ("AFTER-FILL":U, "eSmartUserGroupAfterFill":U, THIS-OBJECT).
        DATASET dsUserGroup:SET-CALLBACK ("BEFORE-FILL":U, "dsUserGroupBeforeFill":U, THIS-OBJECT).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUserGroup:HANDLE, FALSE) .

        BUFFER eSmartUserGroup:DETACH-DATA-SOURCE () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Call-Back method for the BEFORE-FILL event for the dsUserGroup dataset
        Notes:   Set the FILL-MODE for the ePossibleAssignments temp-table to NO-FILL
        @param dsUserGroup The dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID dsUserGroupBeforeFill (DATASET dsUserGroup):

        BUFFER ePossibleAssignments:FILL-MODE = "NO-FILL":U .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate calculated fields
        Notes:
        @param dsUserGroup The dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartUserGroupAfterFill (DATASET dsUserGroup):

        IF NOT lGroupAssignment THEN DO:
            /* Display all users of the current LoginCompany and company independent ones where LoginCompanyGuid = "" */
            FOR EACH SmartUser WHERE SmartUser.LoginCompanyGuid = cCurrentLoginCompanyGuid OR SmartUser.LoginCompanyGuid = "":U NO-LOCK:
                IF CAN-FIND (FIRST eSmartUserGroup WHERE eSmartUserGroup.UserGuid = SmartUser.UserGuid) THEN
                    NEXT.

                CREATE ePossibleAssignments.
                BUFFER-COPY SmartUser TO ePossibleAssignments.

                ASSIGN
                    ePossibleAssignments.GroupGuid     = cCurrentGroupGuid
                    ePossibleAssignments.UserGroupGuid = GUID
                    .
            END.

            /* Marko Rüterbories, Consultingwerk Ltd. 16.01.2013
               If the selected group is company independent then load all users */
            IF cCurrentLoginCompanyGuid = "":U THEN DO:
                FOR EACH SmartUser WHERE SmartUser.LoginCompanyGuid <> "":U NO-LOCK ON ERROR UNDO, THROW:

                    IF CAN-FIND (FIRST eSmartUserGroup WHERE eSmartUserGroup.UserGuid = SmartUser.UserGuid) THEN
                        NEXT.

                    CREATE ePossibleAssignments.
                    BUFFER-COPY SmartUser TO ePossibleAssignments.

                    ASSIGN
                        ePossibleAssignments.GroupGuid     = cCurrentGroupGuid
                        ePossibleAssignments.UserGroupGuid = GUID
                        .
                    END.
            END.
        END.
        ELSE DO:
            /* Display all users of the current LoginCompany and company independent ones where LoginCompanyGuid = "" */
            FOR EACH SmartGroup WHERE SmartGroup.LoginCompanyGuid = cCurrentLoginCompanyGuid OR SmartGroup.LoginCompanyGuid = "":U NO-LOCK ON ERROR UNDO, THROW:

                IF CAN-FIND (FIRST eSmartUserGroup WHERE eSmartUserGroup.GroupGuid = SmartGroup.GroupGuid) THEN
                    NEXT.

                CREATE ePossibleAssignments.
                BUFFER-COPY SmartGroup TO ePossibleAssignments.

                ASSIGN
                    ePossibleAssignments.UserGuid     = cCurrentUserGuid
                    ePossibleAssignments.UserGroupGuid = GUID
                    .
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Extracts LoginCompanyGuid and GroupGuid from the FetchDataRequest
                 data to be able to fill the ePossibleAssignments table.
        Notes:
        @param poFetchDataRequest RequestData for the current FetchData process
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID FetchData (poFetchDataRequest AS Consultingwerk.OERA.IFetchDataRequest):

        DEFINE VARIABLE cContextPart AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE i            AS INTEGER          NO-UNDO.

        DEFINE VARIABLE oParser      AS QueryParser      NO-UNDO .
        DEFINE VARIABLE oQuery       AS IQueryExpression NO-UNDO .

        ASSIGN
            cCurrentLoginCompanyGuid = "":U
            lGroupAssignment         = FALSE
            .
        /* Marko Rüterbories, Consultingwerk Ltd. 04.02.2013
           Get all settings from the CustomContext */
        IF NUM-ENTRIES (poFetchDataRequest:CustomContext, "|":U) > 0 THEN
            DO i = 1 TO NUM-ENTRIES (poFetchDataRequest:CustomContext, "|":U) ON ERROR UNDO, THROW:

                cContextPart = ENTRY (i, poFetchDataRequest:CustomContext, "|":U).

                IF cContextPart BEGINS "LoginCompany=":U THEN
                    cCurrentLoginCompanyGuid = ENTRY (2, cContextPart, "=":U).

                IF cContextPart BEGINS "GroupAssignment=":U THEN
                    lGroupAssignment = LOGICAL (ENTRY (2, cContextPart, "=":U)).
            END.

        /* Marko Rüterbories, Consultingwerk Ltd. 04.02.2013
           Get Group or UserGuid */
        IF LOOKUP ("eSmartUserGroup.GroupGuid":U, poFetchDataRequest:Queries, " ":U) > 0 THEN
            cCurrentGroupGuid = ENTRY (LOOKUP ("eSmartUserGroup.GroupGuid":U,
                                               poFetchDataRequest:Queries,
                                               " ":U) + 2,
                                       poFetchDataRequest:Queries,
                                       " ":U).
        ELSE
            cCurrentGroupGuid = "":U.

        cCurrentGroupGuid = TRIM (cCurrentGroupGuid, '"':U).

        IF LOOKUP ("eSmartUserGroup.UserGuid":U, poFetchDataRequest:Queries, " ":U) > 0 THEN
            cCurrentUserGuid = ENTRY (LOOKUP ("eSmartUserGroup.UserGuid":U,
                                               poFetchDataRequest:Queries,
                                               " ":U) + 2,
                                       poFetchDataRequest:Queries,
                                       " ":U).
        ELSE
            cCurrentUserGuid = "":U.

        cCurrentUserGuid = TRIM (cCurrentUserGuid, '"':U).

        /* Mike Fechner, Consultingwerk Ltd. 13.05.2015
           SCL-808 When filtering by the Grous's LoginCompanyGuid, we need
           to use inner-join within SourceDefaultQuery */
        DO ON ERROR UNDO, THROW:
            oParser = NEW QueryParser () .
            oQuery = oParser:ParseQueryString (poFetchDataRequest:Queries) .

            IF VALID-OBJECT (QueryExpression:FindQueryExpressionForBufferField (NEW BufferFieldName ("eSmartUserGroup.GroupLoginCompanyGuid":U),
                                                                                oQuery)) THEN
                ASSIGN lUseOuterJoin = FALSE .
            ELSE
                ASSIGN lUseOuterJoin = TRUE .

            CATCH err AS Progress.Lang.Error :
                /* When the above errors (parsing error), use the default query,
                   don't process error any further */
                ASSIGN lUseOuterJoin = TRUE .
            END CATCH.
        END.

        SUPER:FetchData (poFetchDataRequest).

        FINALLY:
            ASSIGN lUseOuterJoin = TRUE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Values from ePossibleAssignments will not be stored in the DB
        Notes:
        @param phBuffer HANDLE of the BUFFER to be stored
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID SaveBuffer (INPUT phBuffer AS HANDLE):

        IF phBuffer:NAME = "ePossibleAssignments":U THEN
            RETURN.

        SUPER:SaveBuffer(INPUT phBuffer).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.

        IF pcTable = "eSmartUserGroup":U AND pcColumn = "eSmartUserGroup.GroupLoginCompanyGuid":U THEN
            RETURN "SmartGroup.LoginCompanyGuid":U .

        CASE pcTable:
            WHEN "eSmartUserGroup":U THEN
                cMap = DATA-SOURCE src_SmartUserGroup:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        RETURN ENTRY(LOOKUP(pccolumn,cMap) + 1,cMap).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        CASE pcTable:
            WHEN "eSmartUserGroup":U THEN
                IF lUseOuterJoin THEN
                    RETURN "FOR EACH SmartUserGroup, FIRST SmartUser WHERE SmartUser.UserGuid = SmartUserGroup.UserGuid OUTER-JOIN, FIRST SmartGroup WHERE SmartGroup.GroupGuid = SmartUserGroup.GroupGuid OUTER-JOIN":U.
                ELSE
                    RETURN "FOR EACH SmartUserGroup, FIRST SmartUser WHERE SmartUser.UserGuid = SmartUserGroup.UserGuid, FIRST SmartGroup WHERE SmartGroup.GroupGuid = SmartUserGroup.GroupGuid":U.
        END CASE .

    END METHOD.

END CLASS.
