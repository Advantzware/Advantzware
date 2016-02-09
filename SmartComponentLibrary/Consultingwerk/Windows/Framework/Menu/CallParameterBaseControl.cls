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
    File        : CallParameterBaseControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Fri Dec 07 21:18:59 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.SmartUserControl FROM PROPATH .
USING Consultingwerk.Windows.Framework.Menu.*              FROM PROPATH .  
USING Infragistics.Win.Misc.*                              FROM ASSEMBLY .
USING Progress.Lang.*                                      FROM PROPATH .
USING System.Windows.Forms.*                               FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Menu.CallParameterBaseControl 
    INHERITS SmartUserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: represent the current mode. The Databinding has to be disabled when
                 adding a new record.
        Notes:   Disable not needed data synchronization to the text property. (SCL-119)
           
                 Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to 
                 Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869)
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY AddingRecord AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current CallParameterObject instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY CallParameterObject AS Consultingwerk.ISerializable
    GET.
    SET.
	
	DEFINE VARIABLE lUpdatingUiData AS LOGICAL NO-UNDO.
	DEFINE VARIABLE lWritebackData  AS LOGICAL NO-UNDO.
	
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CallParameterBaseControl class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC CallParameterBaseControl ():
        SUPER ().
        InitializeComponent ().
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the InternalClearDataFields Method and makes sure that no
                 Writeback happends
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ClearDataFields ():

        lUpdatingUiData = TRUE.
        
        THIS-OBJECT:Text = "":U.
        
        InternalClearDataFields ().
        
        FINALLY:
            lUpdatingUiData = FALSE.
        END FINALLY.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID InitializeComponent ():
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:SuspendLayout().
        THIS-OBJECT:Name = "CallParameterBaseControl":U.
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Clear all editor fields
        Notes:   Needs to be implemented in a derived class
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID InternalClearDataFields ():
		
		UNDO, THROW NEW AppError ("Method not implemented!":U, 0).

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the ControlEventHandler after the initialization of the
                 UserControl
        Notes:   
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
	METHOD OVERRIDE PROTECTED VOID OnLoad (e AS System.EventArgs):
		
		SUPER:OnLoad(INPUT e).

        RecurseInitializeControlEventHandler (THIS-OBJECT).
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: The Text property is used to synchronize the JSON serialized 
                 Objects between the UserControl and the Viewer in which it is used.
        Notes:   
        @param e An EventArgs instance which has no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnTextChanged (e AS System.EventArgs):
        
        DEFINE VARIABLE lcSource      AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE lcDestination AS LONGCHAR NO-UNDO.

        SUPER:OnTextChanged (e).

        /* Marko Rüterbories, Consultingwerk Ltd. 18.09.2013
           Disable not needed data synchronization to the text property . (SCL-119)
           
           Invalid cast from Consultingwerk.Windows.Framework.LaunchFormCallParameter to 
           Consultingwerk.SmartFramework.InvokeServiceMethodCallParameter. (12869) */
        IF THIS-OBJECT:AddingRecord THEN RETURN.

        IF lWritebackData OR 
           lUpdatingUiData OR 
           NOT (THIS-OBJECT:Text <> ? AND 
                THIS-OBJECT:Text <> "":U) THEN RETURN.

        lcSource = THIS-OBJECT:Text .
        
        COPY-LOB FROM lcSource 
                 TO lcDestination
                 CONVERT TARGET CODEPAGE "utf-8":U .

        IF VALID-OBJECT (CallParameterObject) THEN 
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (CallParameterObject).

        CallParameterObject = Consultingwerk.Serializable:DeserializeInstance (lcDestination).
        
        lUpdatingUiData = TRUE.

        THIS-OBJECT:UpdateUiData ().

        FINALLY:
            lUpdatingUiData = FALSE.
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Subscribe the EventHandler(s) to events of all editor controls added 
                to the CallParameterBaseControl. 
       Notes:   
       @param poParentControl The The Parent Control (THIS-OBJECT), nested Container
   ------------------------------------------------------------------------------*/    
    METHOD PROTECTED VOID RecurseInitializeControlEventHandler (poParentControl AS Control):
        
        {Consultingwerk/foreach.i Control oControl in poParentControl:Controls}

            /* Skip certain Controls */
            IF TYPE-OF (oControl, System.Windows.Forms.Label) OR 
                TYPE-OF (oControl, UltraLabel) THEN NEXT .
    
            /* Mike Fechner, Consultingwerk Ltd. 06.07.2009
               Bug 1887: CheckedChanged instead of TextChanged for CheckBoxes, Infragistics and Microsoft */
            IF TYPE-OF (oControl, Infragistics.Win.UltraWinEditors.UltraCheckEditor) THEN
                CAST (oControl, Infragistics.Win.UltraWinEditors.UltraCheckEditor):CheckedChanged:Subscribe (TextChangedEventHandler) .
            ELSE IF TYPE-OF (oControl, System.Windows.Forms.CheckBox) THEN
                CAST (oControl, System.Windows.Forms.CheckBox):CheckedChanged:Subscribe (TextChangedEventHandler) .
            ELSE
                oControl:TextChanged:Subscribe (TextChangedEventHandler) .

            /* Recursion */    
            IF oControl:Controls:Count > 0 THEN 
                RecurseInitializeControlEventHandler (oControl) .             
        END.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChangedEvent event of Editor Controls on 
                 the CallParameterBaseControl to write back the current value of the 
                 JSON Object to the Text property of the UserControl. 
        Notes:   
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID TextChangedEventHandler (sender AS System.Object, 
                                                e      AS System.EventArgs):
        DEFINE VARIABLE lcSource      AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE lcDestination AS LONGCHAR NO-UNDO.
        
        IF NOT lUpdatingUiData THEN DO:
            lWritebackData = TRUE.
            
            WritebackCallParameterToTextProperty ().
            
            lcSource = CallParameterObject:Serialize ().
            
            COPY-LOB FROM lcSource 
                     TO lcDestination
                     CONVERT TARGET CODEPAGE "utf-8":U .
            
            THIS-OBJECT:Text = lcDestination.
            
        END.
        
        FINALLY:
            lWritebackData = FALSE.
        END FINALLY.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Updates the data shown in the UI
        Notes:   Needs to be implemented in a derived class
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID UpdateUiData ():
	    
	    UNDO, THROW NEW AppError ("Method not implemented!":U, 0).
	    
	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the data of the CallParameter object after the user has
                 entered data to the UI
        Notes:   Needs to be implemented in a derived class
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID WritebackCallParameterToTextProperty ():
		
        UNDO, THROW NEW AppError ("Method not implemented!":U, 0).
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the CallParameterBaseControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC CallParameterBaseControl ():

    END DESTRUCTOR .

END CLASS.
