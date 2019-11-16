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
    File        : CreateVerbs.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 16 15:17:05 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

    DEFINE PRIVATE VARIABLE oDesignerVerbHelper AS Consultingwerk.SmartComponents.DesignerVerbHelper NO-UNDO . 


/* ***************************  Main Block  *************************** */

    /*------------------------------------------------------------------------------
        Purpose: Creates Designer Verbs based on the DesignerVerbs property                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    METHOD PROTECTED VOID CreateVerbs ():    

        DEFINE VARIABLE oHost     AS System.ComponentModel.Design.IDesignerHost NO-UNDO .
        DEFINE VARIABLE oDesigner AS System.ComponentModel.Design.IDesigner     NO-UNDO . 

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF THIS-OBJECT:DesignTime AND THIS-OBJECT:DesignerVerbs > "":U THEN DO:

            IF VALID-OBJECT (THIS-OBJECT:Site) THEN DO:
                
                IF NOT VALID-OBJECT (oDesignerVerbHelper) THEN 
                    oDesignerVerbHelper = NEW Consultingwerk.SmartComponents.DesignerVerbHelper (THIS-OBJECT) .
                    
                ASSIGN oHost = CAST (THIS-OBJECT:Site:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IDesignerHost":U)),
                                    System.ComponentModel.Design.IDesignerHost).
                
                IF VALID-OBJECT (oHost) THEN DO:
                    
                    oDesigner = oHost:GetDesigner (THIS-OBJECT) . 
                    
                    IF VALID-OBJECT (oDesigner) AND VALID-OBJECT (oDesigner:Verbs) THEN DO:
                        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:DesignerVerbs): 
                            oDesigner:Verbs:Add (oDesignerVerbHelper:CreateDesignerVerb (ENTRY(i, THIS-OBJECT:DesignerVerbs))) .
                        END.
                    END.    
                END.
            END .    
        END. 

    END METHOD .   