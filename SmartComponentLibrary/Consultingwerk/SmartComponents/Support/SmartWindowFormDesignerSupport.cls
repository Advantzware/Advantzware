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
    File        : SmartWindowFormDesignerSupport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 07 11:54:42 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Progress.Lang.*                FROM PROPATH .
USING System.ComponentModel.*        FROM ASSEMBLY .
USING System.ComponentModel.Design.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Support.SmartWindowFormDesignerSupport: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Inserts a UserControl to a Form in the Visual Designer                                                                        
        Notes:                                                         
        @param poControl The reference to the design instance of the Form               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID InsertUserControl (poControl AS System.Windows.Forms.Control):
        
        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .
        DEFINE VARIABLE cClassName    AS CHARACTER                                                       NO-UNDO .
        DEFINE VARIABLE oType         AS System.Type                                                     NO-UNDO .
        DEFINE VARIABLE oInstance     AS Progress.Windows.UserControl                                    NO-UNDO .  
        DEFINE VARIABLE oContainer    AS System.Windows.Forms.Control                                    NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartComponent Library":U,
                                                                                "Scanning Project Folder(s) for UserControls."{&TRAN}) .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Progress.Windows.UserControl":U, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () . 

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        oForm:Text = "Select UserControl class"{&TRAN} .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog (oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN DO:
                                    
            ASSIGN cClassName = oForm:GetSelectedBusinessEntityName () .
        END.
        ELSE 
            ASSIGN cClassName = ? .                                                

        IF cClassName > "":U THEN DO: 
            oType = Progress.Util.TypeHelper:GetType(cClassName) .
            
            /* Mike Fechner, Consultingwerk Ltd. 07.08.2010
               When ABL derived user control has not been instantiated the
               .NET Type information is not yet available */
            IF NOT VALID-OBJECT (oType) THEN DO ON ERROR UNDO, THROW:
                oInstance = DYNAMIC-NEW (cClassName) () .    
                oType = Progress.Util.TypeHelper:GetType(cClassName) .
                DELETE OBJECT oInstance .             
                
                CATCH err AS Progress.Lang.Error:
                	MESSAGE err:GetMessage (1) SKIP 
                	        err:CallStack
                        VIEW-AS ALERT-BOX ERROR TITLE "Error initializing design time instance of "{&TRAN} + QUOTER (cClassName) .	
                
                    RETURN . 
                END CATCH.
            END.
                        
            DO ON ERROR UNDO, THROW:
                oInstance = CAST(CAST(poControl:Container, IDesignerHost):CreateComponent (oType) ,
                                      Progress.Windows.UserControl) .      
                
                CATCH ex AS System.Exception:
                    MESSAGE ex:Message SKIP 
                            ex:CallStack
                        VIEW-AS ALERT-BOX ERROR TITLE "Error creating design time instance using Designer Host"{&TRAN} .
                END CATCH.
            
            END.
            
            oInstance:Visible = TRUE .

            oContainer = CAST (CAST(poControl:Container, IDesignerHost):RootComponent, System.Windows.Forms.Control) .
             
            oContainer:Controls:Add (oInstance) .
            
            oInstance:BringToFront() .

/*            MESSAGE "UserControl has been created on the design canvas." SKIP       */
/*                    "If you can't see it, please use the Outline view to locate it!"*/
/*                VIEW-AS ALERT-BOX.                                                  */
        END.
        
        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

END CLASS.
