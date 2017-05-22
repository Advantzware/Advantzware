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
    File        : AttachmentDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 13.01.2013 11:21:55
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AttachmentDataAccess
    INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/System/dsAttachment.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE PRIVATE QUERY qAttachmentAttachmentRole FOR
        SmartAttachment, SmartAttachmentRole SCROLLING .
    DEFINE PRIVATE DATA-SOURCE src_SmartAttachment FOR QUERY qAttachmentAttachmentRole .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the AttachmentDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN
            ASSIGN THIS-OBJECT:AssignSkipList = "AttachmentGUID":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttachment:HANDLE, FALSE) .

        IF VALID-OBJECT (THIS-OBJECT:FetchDataRequest) AND THIS-OBJECT:FetchDataRequest:CustomContext = "CompleteBinding":U THEN
            BUFFER eSmartAttachment:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartAttachment:HANDLE,
                                                        "AttachmentGUID,SmartAttachment.AttachmentGUID,TableGUID,SmartAttachment.TableGUID,RecordKeyValue,SmartAttachment.RecordKeyValue,AttachmentRoleGUID,SmartAttachment.AttachmentRoleGUID,CommentRoleGUID,SmartAttachment.CommentRoleGUID,AttachmentName,SmartAttachment.AttachmentName,AttachmentFolder,SmartAttachment.AttachmentFolder,LanguageGuid,SmartAttachment.LanguageGuid,AttachmentThumbnail,SmartAttachment.AttachmentThumbnail":U,
                                                        "":U) .
        ELSE
            BUFFER eSmartAttachment:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartAttachment:HANDLE,
                                                        "AttachmentGUID,SmartAttachment.AttachmentGUID,TableGUID,SmartAttachment.TableGUID,RecordKeyValue,SmartAttachment.RecordKeyValue,AttachmentRoleGUID,SmartAttachment.AttachmentRoleGUID,CommentRoleGUID,SmartAttachment.CommentRoleGUID,AttachmentName,SmartAttachment.AttachmentName,AttachmentFolder,SmartAttachment.AttachmentFolder,LanguageGuid,SmartAttachment.LanguageGuid,AttachmentThumbnail,SmartAttachment.AttachmentThumbnail":U,
                                                        "AttachmentData,CommentData":U) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsAttachment DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsAttachment BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

        BUFFER eSmartAttachment:SET-CALLBACK ("AFTER-ROW-FILL":U, "eSmartAttachmentAfterRowFill":U, THIS-OBJECT).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsAttachment:HANDLE, FALSE) .

        BUFFER eSmartAttachment:DETACH-DATA-SOURCE () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Callback method for the AFTER-ROW-FILL event of eSmartAttachment
        Notes:   Errors thrown from or raised in the FILL call-back methods of the
                 DataAccess class (SET-CALLBACK) will stop the Dataset from getting
                 filled / filled further. However errors thrown from the call-back
                 methods are not thrown from the FILL() method further up into a
                 catch block or to the client.
                 See http://knowledgebase.progress.com/articles/Article/P188375
        @param dsAttachment The Dataset
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID eSmartAttachmentAfterRowFill (DATASET dsAttachment):

        ASSIGN eSmartAttachment.AttachmentSize = LENGTH (SmartAttachment.AttachmentData) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs validation for new eSmartAttachment rows
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID eSmartAttachmentCreateBeginTrans ():

        DEFINE VARIABLE oEntity      AS IBusinessEntity NO-UNDO .
        DEFINE VARIABLE cEntityName  AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cEntityTable AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE cSourceTable AS CHARACTER       NO-UNDO .

        FIND eSmartAttachment WHERE ROWID (eSmartAttachment) = BUFFER eSmartAttachmentBefore:AFTER-ROWID .

        FIND SmartTable WHERE SmartTable.TableGUID = eSmartAttachment.TableGUID NO-LOCK NO-ERROR .

        IF NOT AVAILABLE SmartTable THEN DO:
            ASSIGN cEntityName  = ENTRY (1, eSmartAttachment.TableGUID)
                   cEntityTable = ENTRY (2, eSmartAttachment.TableGUID) .

            /* Launch the Business Entity */
            oEntity = ServiceInterface:GetBusinessEntity (cEntityName) .

            Consultingwerk.Assertion.ObjectAssert:IsValid (oEntity, "Business Entity for Attachement":U) .

            IF NOT TYPE-OF (oEntity, SmartBusinessEntity) THEN
                UNDO, THROW NEW AppError ("Only SmartBusinessEntities support attachments."{&TRAN}, 0) .

            ASSIGN cSourceTable = CAST (oEntity, SmartBusinessEntity):SourceTable (cEntityTable) .

            FIND SmartTable WHERE SmartTable.DatabaseName = ENTRY (1, cSourceTable, ".":U)
                              AND SmartTable.TableName    = ENTRY (2, cSourceTable, ".":U) NO-LOCK NO-ERROR .

            IF AVAILABLE eSmartAttachment THEN
                ASSIGN eSmartAttachment.TableGUID = SmartTable.TableGUID .
        END.

        IF NOT AVAILABLE SmartTable THEN DO:
            DatasetHelper:SetErrorString (BUFFER eSmartAttachmentBefore:HANDLE,
                                          "Unable to verify TableGUID"{&TRAN}) .
        END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Saves the current row
        Notes:   Stores LOB field values, as they are not mapped by the DATA-SOURCE
        @param phBeforeBuffer The handle of the ProDataset before buffer
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID SaveRowChanges (phBeforeBuffer AS HANDLE):

        DEFINE BUFFER b_SmartAttachment  FOR SmartAttachment .

        SUPER:SaveRowChanges (phBeforeBuffer).

        IF NOT phBeforeBuffer:ROW-STATE = ROW-DELETED THEN DO:

            BufferHelper:FindAfterBuffer (phBeforeBuffer) .

            FIND b_SmartAttachment WHERE b_SmartAttachment.AttachmentGuid = eSmartAttachment.AttachmentGUID
                EXCLUSIVE-LOCK NO-ERROR .

            IF AVAILABLE b_SmartAttachment THEN DO:

                IF LENGTH (eSmartAttachment.AttachmentData) > 0 THEN
                    COPY-LOB FROM eSmartAttachment.AttachmentData TO b_SmartAttachment.AttachmentData .
                IF LENGTH (eSmartAttachment.CommentData) > 0 THEN
                    COPY-LOB FROM eSmartAttachment.CommentData TO b_SmartAttachment.CommentData .
            END.
        END.

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

        CASE pcTable:
            WHEN "eSmartAttachment":U THEN
                cMap = DATA-SOURCE src_SmartAttachment:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

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
            WHEN "eSmartAttachment":U THEN
                RETURN "FOR EACH SmartAttachment, FIRST SmartAttachmentRole WHERE SmartAttachmentRole.AttachmentRoleGuid = SmartAttachment.AttachmentRoleGuid OUTER-JOIN":U.

        END CASE .

    END METHOD.

END CLASS.
