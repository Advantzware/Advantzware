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
    File        : StringResourceFilter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 11 17:25:11 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartFramework.Localization.*    FROM PROPATH .  
USING Consultingwerk.Windows.Framework.Localization.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Localization.StringResourceFilter 
    IMPLEMENTS IStringResourceFilter: 

    DEFINE VARIABLE oTranslationService AS IUserInterfaceTranslationService NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the StringResourceFilter class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC StringResourceFilter ():
		SUPER ().
		
        oTranslationService = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.Localization.IUserInterfaceTranslationService
                                                            "NEW Consultingwerk.SmartFramework.Localization.UserInterfaceTranslationService()"} .  

        oTranslationService:FilterStringResource:Subscribe (FilterStringResourceHandler) .		
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FilterStringResource event of the 
                 IUserInterfaceTranslationService service
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The FilterStringResourceEventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID FilterStringResourceHandler (sender AS Progress.Lang.Object,
	                                                   e AS FilterStringResourceEventArgs):
		
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
		DEFINE VARIABLE cPatterns AS CHARACTER NO-UNDO INIT 
		    ">>*,*.TextHAlignAsString,*.TextVAlignAsString,$this.WindowPositionRegistryKey,~
*.AppServerPartition,*.BindingSourceExcludeFields,*.BindingSourceIncludeFields,*.QuerySort,~
*.QueryString,*.DisabledColumnsForAdd,*.DisabledColumnsForUpdate,*.SecurityKey,*.SettingsKey,~
*.SmartTableIOState,*.VisibleGridColumns,*.BindTo,*.EntityJoin,*.EntityName,*.EntityTable,~
*.EntityView,*.ContextID,*.ContextString,*.CustomContext,*.DatasetControllerType,*.ForeignFields,~
*.NonUpdatableColumns,*.LookupBrowserExcludeFields,*.LookupBrowserFields,*.LookupControls,~
*.LookupDialogClassName,*.LookupDialogFilterFields,*.LookupDialogFilterOperators,*.LookupFields,~
*.LookupKeyField,*.LookupKeyValue,*.LookupKeyValueColumn,*.ViewerLogicClass,*.CursorPositionFormat,~
*.DateTimeFormat":U.
		
        DO i = 1 TO NUM-ENTRIES (cPatterns) ON ERROR UNDO, THROW:
    		IF e:ResourceKey MATCHES ENTRY (i, cPatterns) THEN DO:
    		    e:Filtered = TRUE . 
    		    RETURN . 
    		END.
        END.

	END METHOD .

END CLASS.
