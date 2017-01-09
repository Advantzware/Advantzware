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
    File        : UserDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories
    Created     : 12.10.2012 09:37:46
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.OERA.*                           FROM PROPATH .
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Authentication.*  FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authentication.UserDataAccess
    INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/Authentication/dsUser.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE QUERY qUserLoginCompany FOR
        SmartUser, SmartLoginCompany SCROLLING.
    DEFINE PRIVATE DATA-SOURCE src_SmartUser FOR QUERY qUserLoginCompany .


    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UserDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UserDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN
            THIS-OBJECT:AssignSkipList = "UserGuid":U.

    END CONSTRUCTOR.

    /**
     * Purpose: Executes SAVE-ROW-CHANGES on the before buffer
     * Notes:   Overrides the DataAccess:SaveRowChanges method; resets the
     *          AssignSkipList property for new records that already contain a
     *          GUID value, this allows to provide the GUID value for new
     *          records optionally from the client
     * @param phBeforeBuffer The handle of the BEFORE-BUFFER
     */
    METHOD PROTECTED OVERRIDE VOID SaveRowChanges (phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO INIT FALSE .

        IF phBeforeBuffer:ROW-STATE = ROW-CREATED THEN DO:

            IF NOT phBeforeBuffer:AFTER-BUFFER:AVAILABLE THEN
                BufferHelper:FindAfterBuffer (phBeforeBuffer) .

            IF phBeforeBuffer:AFTER-BUFFER::UserGuid > "":U THEN
                ASSIGN THIS-OBJECT:AssignSkipList = "":U
                       lChanged                   = TRUE .
        END.

        SUPER:SaveRowChanges (phBeforeBuffer) .

        IF lChanged THEN
            ASSIGN THIS-OBJECT:AssignSkipList = "UserGuid":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUser:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        THIS-OBJECT:AttachDataSource (BUFFER eSmartUser:HANDLE,
                                      DATA-SOURCE src_SmartUser:HANDLE, "
                                      UserGuid,SmartUser.UserGuid,
                                      UserName,SmartUser.UserName,
                                      LanguageGuid,SmartUser.LanguageGuid,
                                      UserFullName,SmartUser.UserFullName,
                                      UserEmail,SmartUser.UserEmail,
                                      UserPassword,SmartUser.UserPassword,
                                      UserPasswordChangedDate,SmartUser.UserPasswordChangedDate,
                                      UserLastLogin,SmartUser.UserLastLogin,
                                      LoginCompanyGuid,SmartUser.LoginCompanyGuid,
                                      ManagerUserGuid,SmartUser.ManagerUserGuid,
                                      UserSecurityId,SmartUser.UserSecurityId":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsUser DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsUser BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

        BUFFER eSmartUser:SET-CALLBACK ("AFTER-FILL":U, "eSmartUserAfterFill":U, THIS-OBJECT).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsUser:HANDLE, FALSE) .

        BUFFER eSmartUser:DETACH-DATA-SOURCE () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the calculated fields of the eSmartUser TEMP-TABLE
        Notes:
        @param dsUser The User Dataset used in the Dataset Fill Event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartUserAfterFill (DATASET dsUser):

        DEFINE BUFFER bufSmartUser FOR eSmartUser.

        FOR EACH eSmartUser EXCLUSIVE-LOCK:

            /* Set Language fields */
            FIND FIRST SmartLanguage WHERE SmartLanguage.LanguageGuid = eSmartUser.LanguageGuid NO-LOCK NO-ERROR.

            IF AVAILABLE (SmartLanguage) THEN
                ASSIGN
                    eSmartUser.LanguageIsoCode = SmartLanguage.LanguageIsoCode
                    eSmartUser.LanguageName    = SmartLanguage.LanguageName
                    .
            ELSE
                ASSIGN ERROR-STATUS:ERROR = FALSE NO-ERROR .

            RELEASE SmartLanguage.

            /* Set Manager fields */
            FIND FIRST bufSmartUser WHERE bufSmartUser.UserGuid = eSmartUser.ManagerUserGuid NO-LOCK NO-ERROR.

            IF AVAILABLE (bufSmartUser) THEN
                ASSIGN
                    eSmartUser.ManagerUserName     = bufSmartUser.UserName
                    eSmartUser.ManagerUserFullName = bufSmartUser.UserFullName
                    .
            ELSE
                ASSIGN ERROR-STATUS:ERROR = FALSE NO-ERROR .

            RELEASE bufSmartUser.

        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Validates the Username when adding or modifying a SmartUser row
        Notes:
        @param e An EventArgs that contains the Event specific data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnBeforeSavingRowChanges (e AS Consultingwerk.OERA.SaveRowChangesEventArgs):

        SUPER:OnBeforeSavingRowChanges(INPUT e).

        IF e:RowState = Consultingwerk.OERA.Enum.RowStateEnum:RowCreated OR
           e:RowState = Consultingwerk.OERA.Enum.RowStateEnum:RowModified THEN DO:

            e:BufferHandle:FIND-BY-ROWID (e:BeforeBufferHandle:AFTER-ROWID).

            IF e:BufferHandle::UserName <> e:BeforeBufferHandle::UserName THEN DO:

                IF e:BufferHandle::LoginCompanyGuid = "":U THEN
                    FIND FIRST SmartUser WHERE SmartUser.UserName = e:BufferHandle::UserName NO-LOCK NO-ERROR.

                ELSE
                    FIND FIRST SmartUser WHERE SmartUser.UserName = e:BufferHandle::UserName AND
                                               (SmartUser.LoginCompanyGuid = e:BufferHandle::LoginCompanyGuid OR
                                                SmartUser.LoginCompanyGuid = "":U) NO-LOCK NO-ERROR.

                IF AVAILABLE (SmartUser) THEN
                    UNDO, THROW NEW AppError ("A User with that Name already exists!"{&TRAN}, 0).

            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code for alternative mapping
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.

        CASE pcTable:
            WHEN "eSmartUser":U THEN
                cMap = DATA-SOURCE src_SmartUser:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

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
            WHEN "eSmartUser":U THEN
                RETURN "FOR EACH SmartUser, FIRST SmartLoginCompany WHERE SmartLoginCompany.LoginCompanyGuid = SmartUser.LoginCompanyGuid OUTER-JOIN":U.

        END CASE .
    END METHOD.

END CLASS.
