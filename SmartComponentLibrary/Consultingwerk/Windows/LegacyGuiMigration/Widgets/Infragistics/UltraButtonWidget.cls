/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : UltraButtonWidget
    Purpose     : Represents a button
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:22:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.Misc.*                                          FROM ASSEMBLY .
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH.

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraButtonWidget 
    INHERITS InfragisticsControlWidget: 

    DEFINE VARIABLE oControl AS UltraButton NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field 
                 associated with the widget 
        Notes:   For example, the DATA-TYPE attribute of a slider widget always 
                 returns the value "INTEGER" because slider widgets can only 
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY DATA-TYPE AS CHARACTER NO-UNDO INIT ?  
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: The type of a handle
        Notes:   FILL-IN
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY TYPE AS CHARACTER NO-UNDO INIT "BUTTON":U 
    GET .
    PROTECTED SET . 

    /*------------------------------------------------------------------------------
       Purpose: Constructor for the UltraButtonWidget class
       Notes:
       @param poEditor The reference to the UltraTextEditor this instance should represent
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraButtonWidget (poEditor AS UltraButton):
        SUPER (poEditor).

        ASSIGN oControl = poEditor .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Reads the image contained in a specified file.                                                                       
        Notes:                   
        @param pcImageFileName A character-string expression that specifies a full or relative pathname for a file that contains an image 
        @return If the read is successful, the method returns TRUE                                                      
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC LOGICAL LOAD-IMAGE(pcImageFileName AS CHARACTER):
        
        DEFINE VARIABLE oImage AS System.Drawing.Bitmap NO-UNDO . 
        
        FILE-INFORMATION:FILE-NAME = pcImageFileName .
        
        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN DO:
            IF VALID-OBJECT(oControl:Appearance) THEN 
                oControl:Appearance:Image = ? .
                
            RETURN TRUE .              
        END.

        /* Mike Fechner, Consultingwerk Ltd. 21.02.2009
           Bug 1574: Support for icons, icons do not need to be made transparent */        
        IF pcImageFileName MATCHES "*.ico":U THEN 
            oImage = ImageHelper:LoadIcon (FILE-INFORMATION:FULL-PATHNAME):ToBitmap() NO-ERROR .
        ELSE DO:
            oImage = ImageHelper:Load (FILE-INFORMATION:FULL-PATHNAME) .    

            IF VALID-OBJECT(oImage) THEN 
                oImage:MakeTransparent() .
        END.
       
        IF NOT VALID-OBJECT (oControl:Appearance) THEN 
            oControl:Appearance = NEW Infragistics.Win.Appearance () .         
                
        oControl:Appearance:Image = oImage . 
        
        RETURN TRUE . 

    END METHOD.

END CLASS.
