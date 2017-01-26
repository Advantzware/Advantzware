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
    File        : DockManagerHelper
    Purpose     : Helper class that supports custom serialization/
                  deserialization of the Infragistics UltraDockManager
                  component
    Syntax      : Static methods only, private constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jul 05 20:22:52 CEST 2011
    Notes       : When serializing/deserializing the settings we do assume
                  that the key of a Pane
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Util.* .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.UltraWinDock.* .
&ENDIF
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Util.DockManagerHelper:

     {Consultingwerk/Util/TempTables/dsDockManagerSettings.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED DockManagerHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Ensures that all DockablePanes / DockableGroupPanes in the collection
                 have a Key. If no Key is present, a GUID will be used
        Notes:   Recursive function. The purpose of this method is to work around
                 the fact that the Dockable Panes InternalId is not accessible. When
                 serializing we assume that the Keys are GUID to serve as InternalId's
                 when deserializing (constructor parameters)
        @param poPanes The DockablePanesCollection to ensure that all panes have a key
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID EnsureKeys (poPanes AS DockablePanesCollection):

        {Consultingwerk/foreach.i DockablePaneBase oPane in poPanes}

            IF oPane:Key > "":U THEN .
            ELSE
                oPane:Key = System.Guid:NewGuid ():ToString() .

            IF TYPE-OF (oPane, DockableGroupPane) THEN
                IF CAST (oPane, DockableGroupPane):Panes:Count > 0 THEN

                    DockManagerHelper:EnsureKeys (CAST (oPane, DockableGroupPane):Panes) .
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Ensures that all DockAreaPanes in the UltraDockManager have a Key.
                 If no Key is present, a GUID will be used
        Notes:   Recursive function. The purpose of this method is to work around
                 the fact that the DockAreaPanes InternalId is not accessible. When
                 serializing we assume that the Keys are GUID to serve as InternalId's
                 when deserializing (constructor parameters)
        @param poDockManager The reference to the UltraDockManager instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID EnsureKeys (poDockManager AS UltraDockManager):

        {Consultingwerk/foreach.i DockAreaPane oArea in poDockManager:DockAreas}

            IF oArea:Key > "":U THEN .
            ELSE
                oArea:Key = System.Guid:NewGuid ():ToString() .

            IF oArea:Panes:Count > 0 THEN
                DockManagerHelper:EnsureKeys (oArea:Panes) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the child DockableControlPanes/DockableGroupPanes for the
                 given paren (DockableGroupPane).
        Notes:   Since the DockAreaPanes inherit from the DockableGroupPanes this
                 method is recursively used for DockableControlPanes/DockableGroupPanes
                 at any given level
        @param poPane The reference to the DockableGroupPane
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID RestoreDockablePaneSettings (poPane AS DockableGroupPane):

        DEFINE VARIABLE oDockAreaPane AS DockablePaneBase NO-UNDO .
        DEFINE VARIABLE oParentId     AS System.Guid      NO-UNDO.

        ASSIGN oParentId = NEW System.Guid (poPane:Key).

        FOR EACH ttDockAreas WHERE ttDockAreas.ParentInternalId = poPane:Key
                                BY ttDockAreas.Order ON ERROR UNDO, THROW:

            CASE ttDockAreas.PaneType:
                WHEN "DockableControlPane":U THEN DO:
                    oDockAreaPane = NEW DockableControlPane (NEW System.Guid (ttDockAreas.InternalId),
                                                             NEW System.Guid ("00000000-0000-0000-0000-000000000000":U),
                                                             -1,
                                                             oParentId,
                                                             ttDockAreas.PaneIndex) .
                    ASSIGN
                        oDockAreaPane:Key  = ttDockAreas.InternalId
                        oDockAreaPane:Text = ttDockAreas.AreaText
                        oDockAreaPane:TextTab = IF ttDockAreas.AreaTextTab > "":U
                                                    THEN ttDockAreas.AreaTextTab
                                                    ELSE ttDockAreas.AreaText
                        .

                    IF NUM-ENTRIES (ttDockAreas.Size, ",":U) > 1 THEN
                        oDockAreaPane:Size = NEW System.Drawing.Size (INTEGER (ENTRY (1, ttDockAreas.Size)),
                                                                      INTEGER (ENTRY (2, ttDockAreas.Size))) .

                    CAST (oDockAreaPane, DockableControlPane):Control = NEW System.Windows.Forms.Panel () .
                    CAST (oDockAreaPane, DockableControlPane):Control:Text = ttDockAreas.AreaText .

                END.
                WHEN "DockableGroupPane":U THEN DO:
                    oDockAreaPane = NEW DockableGroupPane (NEW System.Guid (ttDockAreas.InternalId),
                                                           NEW System.Guid ("00000000-0000-0000-0000-000000000000":U),
                                                           -1,
                                                           oParentId,
                                                           ttDockAreas.PaneIndex) .
                    ASSIGN
                        oDockAreaPane:Key  = ttDockAreas.InternalId
                        oDockAreaPane:Text = ttDockAreas.AreaText
                        oDockAreaPane:TextTab = IF ttDockAreas.AreaTextTab > "":U
                                                    THEN ttDockAreas.AreaTextTab
                                                    ELSE ttDockAreas.AreaText
                        CAST (oDockAreaPane, DockableGroupPane):ChildPaneStyle = {Consultingwerk/parse-enum.i
                                                                                    Infragistics.Win.UltraWinDock.ChildPaneStyle
                                                                                    ttDockAreas.ChildPaneStyle} .
                        .
                    IF NUM-ENTRIES (ttDockAreas.Size, ",":U) > 1 THEN
                        oDockAreaPane:Size = NEW System.Drawing.Size (INTEGER (ENTRY (1, ttDockAreas.Size)),
                                                                      INTEGER (ENTRY (2, ttDockAreas.Size))) .
                END.
                OTHERWISE DO:
                    UNDO, THROW NEW AppError
                        (SUBSTITUTE ("Unexpected Pane type &1."{&TRAN}, ttDockAreas.PaneType), 0) .
                END .

            END CASE .

            CAST (poPane, DockableGroupPane):Panes:Add (oDockAreaPane) .
        END.

        {Consultingwerk/foreach.i DockablePaneBase oGroupPane in CAST(poPane,DockableGroupPane):Panes}

             IF TYPE-OF (oGroupPane, DockableGroupPane) THEN
                 RestoreDockablePaneSettings (CAST (oGroupPane, DockableGroupPane)) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the DockAreas for the given DockAreasCollection (direct
                 childs of the UltraDockManager)
        Notes:   Calls RestoreDockablePaneSettings (poPane AS DockableGroupPane) to
                 restore the Panes or the DockAreaPanes
        @param poAreas The reference to the DockAreasCollection
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID RestoreDockAreaSettings (poAreas AS DockAreasCollection):

        DEFINE VARIABLE oDockAreaPane AS DockAreaPane NO-UNDO .

        FOR EACH ttDockAreas WHERE ttDockAreas.ParentInternalId = "":U
                                BY ttDockAreas.Order ON ERROR UNDO, THROW:

            oDockAreaPane = NEW DockAreaPane ({Consultingwerk/parse-enum.i
                                                Infragistics.Win.UltraWinDock.DockedLocation
                                                ttDockAreas.DockedLocation},
                                               NEW System.Guid (ttDockAreas.InternalId)) .

            oDockAreaPane:Key = ttDockAreas.InternalId .

            oDockAreaPane:ChildPaneStyle = {Consultingwerk/parse-enum.i
                                                Infragistics.Win.UltraWinDock.ChildPaneStyle
                                                ttDockAreas.ChildPaneStyle} .

            IF NUM-ENTRIES (ttDockAreas.Size, ",":U) > 1 THEN
                oDockAreaPane:Size = NEW System.Drawing.Size (INTEGER (ENTRY (1, ttDockAreas.Size)),
                                                              INTEGER (ENTRY (2, ttDockAreas.Size))) .

            IF ttDockAreas.DockedBefore > "":U THEN
                oDockAreaPane:DockedBefore = NEW System.GUID (ttDockAreas.DockedBefore) .

            poAreas:Add (oDockAreaPane) .
        END.

        {Consultingwerk/foreach.i DockAreaPane oDockAreaPane2 in poAreas}
            RestoreDockablePaneSettings (oDockAreaPane2) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the settings of the UltraDockManager from the given
                 dsDockManagerSettins ProDataset
        Notes:
        @param poDockManager The reference to the UltraDockManager instance
        @param dsDockManagerSettings DATASET with the Dockmanager Settings, from Consultingwerk/Util/TempTables/dsDockManagerSettings.i
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RestoreDockManagerSettings (poDockManager AS UltraDockManager,
                                                          DATASET FOR dsDockManagerSettings):

        CAST(poDockManager, System.ComponentModel.ISupportInitialize):BeginInit().

        /* Clear DockAreas */
        poDockManager:DockAreas:Clear() .

        FIND FIRST ttDockManager .

        ASSIGN poDockManager:LayoutStyle = {Consultingwerk/parse-enum.i
                                                Infragistics.Win.UltraWinDock.DockAreaLayoutStyle
                                                ttDockManager.LayoutStyle}
               poDockManager:WindowStyle = {Consultingwerk/parse-enum.i
                                                Infragistics.Win.UltraWinDock.WindowStyle
                                                ttDockManager.WindowStyle}
                                                .

        RestoreDockAreaSettings (poDockManager:DockAreas) .

        FINALLY:
            CAST(poDockManager, System.ComponentModel.ISupportInitialize):EndInit().
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the Settings of the UltraDockManager from the ISettingsService
        Notes:   Uses the Key "DockManagerSettings", typically used inside the
                 OnLoad method of a Form
        @param poDockManager The UltraDockManager instance to restore the settings for
        @param pcSettingsKey The Settings Key (Form Identifier) to load the settings from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RestoreFromSettingsService (poDockManager AS UltraDockManager,
                                                          pcSettingsKey AS CHARACTER):

        DEFINE VARIABLE oSettingsServiceLong AS Consultingwerk.Framework.ISettingsServiceLong NO-UNDO .
        DEFINE VARIABLE oSettingsService     AS Consultingwerk.Framework.ISettingsService     NO-UNDO .
        DEFINE VARIABLE cSettings            AS LONGCHAR                                      NO-UNDO .
        DEFINE VARIABLE oMemoryStream        AS System.IO.MemoryStream                        NO-UNDO .
        DEFINE VARIABLE oBytes               AS "System.Byte[]":U                             NO-UNDO .
        DEFINE VARIABLE mptr                 AS MEMPTR                                        NO-UNDO .
        DEFINE VARIABLE iChunks              AS INTEGER                                       NO-UNDO .
        DEFINE VARIABLE i                    AS INTEGER                                       NO-UNDO .

        FIX-CODEPAGE (cSettings) = "utf-8":U .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poDockManager} .
        Consultingwerk.Assertion.Assert:NotNullOrEmpty (pcSettingsKey, "Settings Key":U) .

        oSettingsServiceLong = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsServiceLong} .

        IF NOT VALID-OBJECT (oSettingsServiceLong) THEN DO:

            oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

            IF NOT VALID-OBJECT (oSettingsService) THEN
                RETURN .
        END.

        IF VALID-OBJECT (oSettingsServiceLong) THEN
            cSettings = oSettingsServiceLong:GetSettingLong (pcSettingsKey,
                                                             "DockManagerSettings":U) .
        ELSE
            cSettings = oSettingsService:GetSetting (pcSettingsKey,
                                                     "DockManagerSettings":U) .

        ASSIGN iChunks = INTEGER (cSettings) NO-ERROR .

        IF iChunks > 0 THEN .
        ELSE RETURN .

        ASSIGN cSettings = "":U .

        DO i = 1 TO iChunks:

            IF VALID-OBJECT (oSettingsServiceLong) THEN
                ASSIGN cSettings = cSettings + oSettingsServiceLong:GetSettingLong (pcSettingsKey,
                                                                                    SUBSTITUTE ("DockManagerSettings.&1":U, i)) .
            ELSE
                ASSIGN cSettings = cSettings + oSettingsService:GetSetting (pcSettingsKey,
                                                                            SUBSTITUTE ("DockManagerSettings.&1":U, i)) .
        END.

        IF LENGTH (cSettings) > 0 THEN .
        ELSE RETURN .

        mptr = BASE64-DECODE (cSettings) .

        oBytes = Consultingwerk.Util.DataTypeHelper:MemptrToByteArray (mptr) .

        IF oBytes:Length > 0 THEN .
        ELSE
            RETURN .

        oMemoryStream = NEW System.IO.MemoryStream (oBytes) .
        oMemoryStream:Position = 0 .

        DO ON ERROR UNDO, THROW:

            poDockManager:LoadFromBinary (oMemoryStream) .

            @SuppressUnusedWarnings.
            CATCH ex AS System.Exception:
                /* noop - Ignore .NET Exception from statement above
                   when the stored settings are using a different set of Infragistics
                   Controls we may be getting errors about incompatible types etc. */
            END CATCH.

        END.

        FINALLY:
            SET-SIZE (mptr) = 0 .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the settings of the DockAreaPanes
        Notes:   Recursively calls StoreDockablePaneSettings (oArea:Panes), uses the
                 Key as the InternalId (.NET GUID compatibility required)
        @param poAreas The reference to the DockAreasCollection
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID StoreDockAreaSettings (poAreas AS DockAreasCollection):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/foreach.i DockAreaPane oArea in poAreas}

            CREATE ttDockAreas .
            ASSIGN i = i + 1
                   ttDockAreas.PaneType = "DockAreaPane":U
                   ttDockAreas.InternalId = oArea:KEY
                   ttDockAreas.Order = i
                   ttDockAreas.DockedLocation = oArea:DockedLocation:ToString ()
                   ttDockAreas.ChildPaneStyle = oArea:ChildPaneStyle:ToString ()
                   ttDockAreas.DockedBefore = oArea:DockedBefore:ToString ()
                   ttDockAreas.Size = SUBSTITUTE ("&1,&2":U, oArea:Size:Width, oArea:Size:Height) .

            IF oArea:Panes:Count > 0 THEN
                StoreDockablePaneSettings (oArea:Panes) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the settings of the Panes in the given DockablePanesCollection
        Notes:   Uses the Key as the InternalId (.NET GUID compatibility required)
        @param poPanes The reference to the DockablePanesCollection
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC VOID StoreDockablePaneSettings (poPanes AS DockablePanesCollection):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/foreach.i DockablePaneBase oPane in poPanes}

            CREATE ttDockAreas .
            ASSIGN i = i + 1
                   ttDockAreas.PaneType = oPane:GetType():Name
                   ttDockAreas.InternalId = oPane:Key
                   ttDockAreas.ParentInternalId = oPane:Parent:Key
                   ttDockAreas.Order = i
                   ttDockAreas.AreaText = oPane:Text
                   ttDockAreas.AreaTextTab = oPane:TextTab
                   ttDockAreas.PaneIndex = oPane:Index
                   ttDockAreas.Size = SUBSTITUTE ("&1,&2":U, oPane:ActualSize:Width, oPane:ActualSize:Height) .

            /* Special handling for DockableGroupPanes, i.e. recursion */
            IF TYPE-OF (oPane, DockableGroupPane) THEN DO:
                   ttDockAreas.ChildPaneStyle = CAST (oPane, DockableGroupPane):ChildPaneStyle:ToString () .

                IF CAST (oPane, DockableGroupPane):Panes:Count > 0 THEN
                    StoreDockablePaneSettings (CAST (oPane, DockableGroupPane):Panes) .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the settings of the UltraDockManager and returns them in the
                 dsDockManagerSettings Dataset.
        Notes:
        @param poDockManager The reference to the UltraDockManager instance
        @param dsDockManagerSettings OUTPUT DATASET with the dockmanager settings, from Consultingwerk/Util/TempTables/dsDockManagerSettings.i
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StoreDockManagerSettings (poDockManager AS UltraDockManager,
                                                        OUTPUT DATASET FOR dsDockManagerSettings):

        DockManagerHelper:EnsureKeys (poDockManager) .

        DATASET dsDockManagerSettings:EMPTY-DATASET () .

        CREATE ttDockManager .
        ASSIGN ttDockManager.LayoutStyle = poDockManager:LayoutStyle:ToString ()
               ttDockManager.WindowStyle = poDockManager:WindowStyle:ToString ()
               .

        StoreDockAreaSettings (poDockManager:DockAreas) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the current Settings of the UltraDockManager to the
                 ISettingsService
        Notes:   Uses the Key "DockManagerSettings", typically used inside the
                 OnFormClosing method of a Form
        @param poDockManager The UltraDockManager instance to store the settings from
        @param pcSettingsKey The Settings Key (Form Identifier) to save the settings under
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StoreToSettingsService (poDockManager AS UltraDockManager,
                                                      pcSettingsKey AS CHARACTER):

        DEFINE VARIABLE oSettingsServiceLong AS Consultingwerk.Framework.ISettingsServiceLong NO-UNDO .
        DEFINE VARIABLE oSettingsService     AS Consultingwerk.Framework.ISettingsService     NO-UNDO .
        DEFINE VARIABLE oMemoryStream        AS System.IO.MemoryStream                        NO-UNDO .
        DEFINE VARIABLE cSettings            AS LONGCHAR                                      NO-UNDO .
        DEFINE VARIABLE mptr                 AS MEMPTR                                        NO-UNDO .
        DEFINE VARIABLE iChunks              AS INTEGER                                       NO-UNDO .
        DEFINE VARIABLE i                    AS INTEGER                                       NO-UNDO .

        FIX-CODEPAGE (cSettings) = "utf-8":U .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poDockManager} .
        Consultingwerk.Assertion.Assert:NotNullOrEmpty (pcSettingsKey, "Settings Key":U) .

        oSettingsServiceLong = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsServiceLong} .

        IF NOT VALID-OBJECT (oSettingsServiceLong) THEN DO:

            oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

            IF NOT VALID-OBJECT (oSettingsService) THEN
                RETURN .
        END.

        oMemoryStream = NEW System.IO.MemoryStream () .

        poDockManager:SaveAsBinary (oMemoryStream) .

        oMemoryStream:Position = 0 .

        mptr = DataTypeHelper:ByteArrayToMemptr (oMemoryStream:GetBuffer()) .

        cSettings = BASE64-ENCODE (mptr) .

        ASSIGN iChunks = MathHelper:Roundup (LENGTH (cSettings) / 8192) .

        IF VALID-OBJECT (oSettingsServiceLong) THEN
            oSettingsServiceLong:StoreSettingLong (pcSettingsKey,
                                                   "DockManagerSettings":U,
                                                   STRING (iChunks)) .
        ELSE
            oSettingsService:StoreSetting (pcSettingsKey,
                                           "DockManagerSettings":U,
                                           STRING (iChunks)) .

        DO i = 1 TO iChunks:

            IF VALID-OBJECT (oSettingsServiceLong) THEN
                oSettingsServiceLong:StoreSettingLong (pcSettingsKey,
                                                       SUBSTITUTE ("DockManagerSettings.&1":U, i),
                                                       STRING (SUBSTRING (cSettings,
                                                                          (i - 1) * 8192 + 1,
                                                                          8192))) .
            ELSE
                oSettingsService:StoreSetting (pcSettingsKey,
                                               SUBSTITUTE ("DockManagerSettings.&1":U, i),
                                               STRING (SUBSTRING (cSettings,
                                                                  (i - 1) * 8192 + 1,
                                                                  8192))) .
        END.

        FINALLY:
            SET-SIZE (mptr) = 0  .
        END FINALLY.

    END METHOD.

&ENDIF
END CLASS.