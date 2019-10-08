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
    File        : AttachmentDatasetController
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 13.01.2013 11:21:55
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.Framework.*                      FROM PROPATH .
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.SmartFramework.System.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*       FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinListView.*             FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.AttachmentDatasetController
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF :

    { Consultingwerk/SmartFramework/System/dsAttachment.i }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE VARIABLE oImageProvider AS IImageProvider NO-UNDO .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller
        Notes:   Interface IDatasetController member
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    PRIVATE SET.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the UltraListView control
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListView AS UltraListView NO-UNDO
    GET.
    SET.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AttachmentDatasetController class
        Notes:   Assigns the DatasetHandle property
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AttachmentDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsAttachment:HANDLE .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
&ENDIF
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Clears the List View
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearListView ():

        THIS-OBJECT:ListView:Items:Clear () .

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Creates an attachment from the file name
        Notes:
        @param pcFileName The file name to import as Attachmens
        @param pcPath The pathname to add those files to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateAttachment (pcFileName AS CHARACTER,
                                         pcPath AS CHARACTER):

        THIS-OBJECT:CreateAttachments (ArrayHelper:Array (pcFileName),
                                       pcPath) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates attachments from file names (typically after a drag and
                 drop operation of files)
        Notes:
        @param pcFileNames And Array of file names to import as Attachmens
        @param pcPath The pathname to add those files to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CreateAttachments (pcFileNames AS CHARACTER EXTENT,
                                          pcPath AS CHARACTER):

&IF DEFINED (SmartComponentLibrary) NE 0 AND PROVERSION NE "10.2B":U &THEN
        DEFINE VARIABLE i                       AS INTEGER                           NO-UNDO .
        DEFINE VARIABLE oAdapter                AS SmartBusinessEntityAdapter        NO-UNDO .
        DEFINE VARIABLE cTempTableReference     AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cAttachmentRoleGuid     AS CHARACTER                         NO-UNDO .

        DEFINE VARIABLE oForm                   AS AttachmentRoleSelectForm          NO-UNDO .
        DEFINE VARIABLE oDialogResult           AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oAttachmentRoleProvider AS IAttachmentRoleProvider           NO-UNDO .

        oAttachmentRoleProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IAttachmentRoleProvider} .

        Consultingwerk.Assertion.ObjectAssert:IsValid (oAttachmentRoleProvider, "Attachment Role Provider":U) .

        CASE oAttachmentRoleProvider:GetCount():

            WHEN 0 THEN
                UNDO, THROW NEW AppError ("Unable to create Attachment without Attachment role provider or when no attachment role is defined."{&TRAN}, 0) .
            WHEN 1 THEN DO:
                cAttachmentRoleGuid = UNBOX (ValueLists:GetAttachmentRoleValueList():ValueListItems:Item [0]:DataValue) .
            END.
            OTHERWISE DO:
                oForm = NEW AttachmentRoleSelectForm () .
                WAIT-FOR oForm:ShowDialog () SET oDialogResult .

                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

                cAttachmentRoleGuid = oForm:SelectedValue .
            END.
        END CASE .

        oAdapter = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) .

        IF NOT TYPE-OF (oAdapter:SmartDataSource,
                        ISmartBusinessEntityAdapter) THEN
            UNDO, THROW NEW AppError ("Only ISmartBusinessEntityAdapter imlpementations supported for attachments"{&TRAN}, 0) .

        ASSIGN cTempTableReference =
            SUBSTITUTE ("&1,&2":U, CAST (oAdapter:SmartDataSource,
                                       ISmartBusinessEntityAdapter):EntityName,
                                 CAST (oAdapter:SmartDataSource,
                                       ISmartBusinessEntityAdapter):EntityTable) .

        DO i = 1 TO EXTENT (pcFileNames):

            oAdapter:CreateRecord () .

            ASSIGN eSmartAttachment.TableGUID          = cTempTableReference
                   eSmartAttachment.AttachmentRoleGUID = cAttachmentRoleGuid
                   eSmartAttachment.AttachmentName     = ENTRY (NUM-ENTRIES (pcFileNames[i], "~\":U),
                                                                pcFileNames[i],
                                                                "~\":U).

            COPY-LOB FROM FILE pcFileNames[i] TO eSmartAttachment.AttachmentData .

            oAdapter:UpdateRow (FALSE) .
        END.

        oAdapter:RetrieveData () .
&ENDIF

        FINALLY:
            /* In case of errors, reject pending changes from this method */
            DATASET dsAttachment:REJECT-CHANGES () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object detaching from the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Starts Drag and Drop of the Attachment
        Notes:
        @param pcAttachmentKey The key of the Attachment record for the drag and drop operation
        @param poControl The Control to start the Drag and Drop operation from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DragAttachment (pcAttachmentKey AS CHARACTER,
                                       poControl AS System.Windows.Forms.Control):

        DEFINE VARIABLE mptrAttachment AS MEMPTR                          NO-UNDO .
        DEFINE VARIABLE oData          AS System.Windows.Forms.DataObject NO-UNDO .
        DEFINE VARIABLE hDataset       AS HANDLE                          NO-UNDO .

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState() .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.System.AttachmentBusinessEntity":U,
                                                       "GetLargeObjects":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       NEW CharacterHolder (pcAttachmentKey)) .

        hDataset::eSmartAttachment:FIND-FIRST () .

        IF NOT hDataset::eSmartAttachment:AVAILABLE THEN
            RETURN .

        COPY-LOB FROM hDataset:GET-BUFFER-HANDLE ("eSmartAttachment":U):BUFFER-FIELD ("AttachmentData":U):BUFFER-VALUE
                 TO mptrAttachment .

        oData = Consultingwerk.Util.DragAndDropHelper:GetDragDropDataObject (hDataset::eSmartAttachment::AttachmentName,
                                                                             mptrAttachment) .

        poControl:DoDragDrop (oData,
                              System.Windows.Forms.DragDropEffects:Link) .

        FINALLY:
            SET-SIZE (mptrAttachment) = 0 .

            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .

            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState() .

            PROCESS EVENTS .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the ListView with the current attachments
        Notes:
        @param pcFolder The folder selected in the navigation bar
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateListView (pcFolder AS CHARACTER):

        DEFINE BUFFER b_eSmartAttachment FOR eSmartAttachment .

        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO .

        THIS-OBJECT:ListView:Items:Clear () .

        FOR EACH b_eSmartAttachment BY b_eSmartAttachment.AttachmentName ON ERROR UNDO, THROW:
            oListViewItem = THIS-OBJECT:ListView:Items:Add (b_eSmartAttachment.AttachmentGUID,
                                                            b_eSmartAttachment.AttachmentName) .

            oListViewItem:SubItems["AttachmentRole":U]:Value = b_eSmartAttachment.AttachmentRoleName .
            oListViewItem:SubItems["AttachmentSize":U]:Value = FileHelper:HumanReadableFileSize (b_eSmartAttachment.AttachmentSize) .

            IF VALID-OBJECT (oImageProvider) THEN
                oListViewItem:Appearance:Image = oImageProvider:ImageForKey ("Consultingwerk/Windows/Framework/Images/SmartFramework/document_32.png":U) .
        END.

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.
        Notes:   Interface IDatasetController member
        @param poConsumer The reference of the Object attaching to the Dataset Controller
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Saves an aAttachment to the file
        Notes:
        @param pcAttachmentKey The attachment to save
        @param pcFileName The name of the file to save to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveAttachment (pcAttachmentKey AS CHARACTER,
                                       pcFileName AS CHARACTER):

        DEFINE VARIABLE hDataset       AS HANDLE                          NO-UNDO .

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState() .

        FrameworkSettings:ServiceAdapter:InvokeMethod ("":U,
                                                       "Consultingwerk.SmartFramework.System.AttachmentBusinessEntity":U,
                                                       "GetLargeObjects":U,
                                                       INPUT-OUTPUT DATASET-HANDLE hDataset,
                                                       NEW CharacterHolder (pcAttachmentKey)) .

        hDataset::eSmartAttachment:FIND-FIRST () .

        IF NOT hDataset::eSmartAttachment:AVAILABLE THEN
            RETURN .

        COPY-LOB FROM hDataset:GET-BUFFER-HANDLE ("eSmartAttachment":U):BUFFER-FIELD ("AttachmentData":U):BUFFER-VALUE
                 TO FILE pcFileName .

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .

            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState() .
        END FINALLY.

    END METHOD .

END CLASS.
