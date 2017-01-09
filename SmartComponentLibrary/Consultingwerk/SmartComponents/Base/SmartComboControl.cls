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
    File        : SmartComboControl
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Feb 24 16:42:23 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* . /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartComboControl
    INHERITS UltraCombo
    IMPLEMENTS ISmartDataTarget, ISmartDataTargetWithBindingSource  :

    DEFINE PRIVATE VARIABLE oBindingSource    AS Progress.Data.BindingSource NO-UNDO .
    DEFINE PRIVATE VARIABLE oSmartDataSource  AS ISmartDataSource            NO-UNDO .

    DEFINE         VARIABLE oColumnsList     AS "System.Collections.Generic.List<UltraGridColumn>" NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Get the BindingSource of the current Class.
                 This Property is used to get a possibly existing DesignTime
                 BindingSource from the SmartDataBrowser Instance. A SmartDataAdapter
                 derived Class gets registered as the SmartDataSource for the
                 SmartDataBrowser and RegisterSmartDataTarget () is called to do the
                 handshake between those two Classes.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO
    GET:
        IF VALID-OBJECT(THIS-OBJECT:DataSource) THEN
            IF TYPE-OF(THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN
                RETURN CAST(THIS-OBJECT:DataSource, Progress.Data.BindingSource).
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.
        Notes:   Set during constructor using a check at the
                 System.ComponentModel.LicenseManager
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
       Purpose: Indicated that the SmartDataAdapter is currrently being
                Initialized
       Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Initializing AS LOGICAL NO-UNDO INIT FALSE
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartDataSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkDataSource.i directly into class file
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) .
    END GET .
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) THEN
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource
                 can be set to any class implementing the Interface ISmartDataSource.
                 This Property is set by the developer when initializing a Control
                 implementing the Interface ISmartDataTarget.
                 Enforced by Interface ISmartDataTarget.
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO
    GET():
        RETURN oSmartDataSource .
    END GET.
    SET (INPUT arg AS ISmartDataSource):
        DEFINE VARIABLE lAttachToAdapterBindingSource AS LOGICAL NO-UNDO INIT FALSE .

        IF VALID-OBJECT (arg) THEN DO:
            oSmartDataSource = arg.

            IF VALID-OBJECT(THIS-OBJECT:DataSource) THEN DO:
                IF TYPE-OF(THIS-OBJECT:DataSource, Progress.Data.BindingSource) THEN
                    oBindingSource = CAST(THIS-OBJECT:DataSource, Progress.Data.BindingSource).
            END.
            ELSE
                /* set flag that we will be using the BindingSource of the DataAdapter (if present) */
                ASSIGN lAttachToAdapterBindingSource = TRUE .

            oSmartDataSource:RegisterSmartDataTarget(THIS-OBJECT).

            oSmartDataSource:BeginUpdateState:Subscribe (BeginUpdateStateHandler) .
            oSmartDataSource:EndUpdateState:Subscribe (EndUpdateStateHandler) .

            /* use the bindingsource of the DataAdapter if there was no design time bindingsource */
            IF lAttachToAdapterBindingSource = TRUE THEN
                THIS-OBJECT:DataSource = oSmartDataSource:BindingSource .
        END.
        ELSE DO:
            IF VALID-OBJECT (oSmartDataSource) THEN DO:
                oSmartDataSource:DeregisterSmartDataTarget (THIS-OBJECT) .

                oSmartDataSource:BeginUpdateState:Unsubscribe (BeginUpdateStateHandler) .
                oSmartDataSource:EndUpdateState:Unsubscribe (EndUpdateStateHandler) .
            END.

            oSmartDataSource = ? .
        END.
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: The Constructor initializes a new SmartComboControl and sets default
                 values. Set's the  DesignTime property using the LicenseManager.
        Notes:   According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartComboControl (  ):

        SUPER().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        /* Marko Rüterbories, Consultingwerk Ltd. 25.02.2009
           Fix default value. */
        THIS-OBJECT:SyncWithCurrencyManager = TRUE .

        /* Mike Fechner, Consultingwerk Ltd. 24.01.2010
           Only subscribe to Disposed event when not in Visual Designer
           to avoid conflicts when re-opening layouts. */
        IF NOT THIS-OBJECT:DesignTime THEN
            THIS-OBJECT:Disposed:Subscribe (DisposedHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: This Method attaches a new BindingSource given as the Parameter
                 poBindingSource to the Objects of the current Class. The new
                 BindingSource may be created by a DataAdapter or borrowed from a
                 Browser.
                 It is called by the SmartDataAdapter Class when creating a new
                 BindingSource or when a new SmartDataSource is registered to the
                 Class implementing the Interface ISmartDataTarget.
        Notes:   Enforced by Interface ISmartDataTarget.
        @param poBindingSource The BindingSource to be attached to the Grids DataSource Property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource):

        /* Mike Fechner, Consultingwerk Ltd. 05.01.2010
           Don't change BindingSource in Visual Designer */
        IF THIS-OBJECT:DesignTime THEN
            RETURN .

        oBindingSource = poBindingSource .

        /* Replace databindings with bindings to the binding source of the ISmartDataSource */
        THIS-OBJECT:DataSource = oBindingSource .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the BeginUpdateState event of the SmartDataSource
        Notes:
        @param sender The reference to the object that raised this event
        @param e The DataSourceUpdateStateEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BeginUpdateStateHandler (sender AS System.Object,
                                                   e AS DataSourceUpdateStateEventArgs):

        DisableForUpdate (e:UpdatingDataTarget) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disable the Controls during an Update.
                 This Method is called by the SmartDataAdapter by a call to
                 BeginUpdateState () which is defined in the Interface ISmartDataSource.
                 Enforced by Interface ISmartDataTarget.
        Notes:
        @param poSmartDataTarget The reference to the ISmartDataTarget instance that is in update state now
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisableForUpdate (poSmartDataTarget AS ISmartDataTarget):

        THIS-OBJECT:ENABLED = FALSE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Disposed event of this instance
        Notes:
        @param sender The reference to the object that raised the Disposed event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DisposedHandler (sender AS System.Object,
                                           e AS System.EventArgs  ):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF VALID-OBJECT (oColumnsList) THEN
        DO:
            DO i = oColumnsList:COUNT - 1 TO 0 BY -1:
                IF VALID-OBJECT (oColumnsList[i]) THEN
                    DELETE OBJECT oColumnsList[i] .
            END.

            DELETE OBJECT oColumnsList .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enable the Controls after an Update.
                 This Method is called by the SmartDataAdapter when setting the
                 SmartDataSourceState Property to Navigatable.
                 Enforced by Interface ISmartDataTarget.
        Notes:   Use this only in a Browser!
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EnableAfterUpdate():

        THIS-OBJECT:ENABLED = TRUE.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EndUpdateState event of the SmartDataSource
        Notes:
        @param sender The reference to the object that raised this event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID EndUpdateStateHandler (sender AS System.Object,
                                                 e AS DataSourceUpdateStateEventArgs):

        EnableAfterUpdate () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the beginning of a Containers InitializeComponents
                 method
        Notes:   Enforced by System.ComponentModel.ISupportInitialize
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.begininit(VS.80).aspx
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnBeginInit ():

        ASSIGN THIS-OBJECT:Initializing = TRUE  .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invoked at the end of a Containers InitializeComponents method
        Notes:   Enforced by System.ComponentModel.ISupportInitialize
                 http://msdn.microsoft.com/de-de/library/system.componentmodel.isupportinitialize.endinit(VS.80).aspx
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnEndInit(  ):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE j AS INTEGER NO-UNDO.

        oColumnsList = NEW "System.Collections.Generic.List<UltraGridColumn>" () .

        DO i = 0 TO THIS-OBJECT:DisplayLayout:BandsSerializer:Count - 1:
            DO j = 0 TO THIS-OBJECT:DisplayLayout:BandsSerializer[i]:Columns:Count - 1:
                oColumnsList:Add (THIS-OBJECT:DisplayLayout:BandsSerializer[i]:Columns[j]) .
            END.
        END.

        ASSIGN
            THIS-OBJECT:Initializing = FALSE .

    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: This Method is not needed here but enforced by the Interface
                 ISmartDataTarget.
        Notes:   Required only for visual DataTagets (Viewer)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EvaluateTableIOState ():
        /* NOOP */
    END METHOD.

END CLASS.
