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
    File        : DialogResultEnum
    Purpose     : A platform neutral Enumeration for DialogResults
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 20 07:40:51 CEST 2011
    Notes       : Used by MessageFormHelper / IMessageForm
                  Enum members prefixed with DialogResult (in comparison 
                  to System.Windows.Forms.DialogResult members) because
                  Yes and No wouldn't be valid property names
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.Framework.Enum.*        FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING System.Windows.Forms.*                 FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Framework.Enum.DialogResultEnum 
    INHERITS Consultingwerk.Enum: 

    /* Static members */  
    {Consultingwerk/EnumMember.i DialogResultAbort 1 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultCancel 2 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultIgnore 3 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultNo 4 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultNone 5 DialogResultEnum}  
    {Consultingwerk/EnumMember.i DialogResultOK 6 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultRetry 7 DialogResultEnum}
    {Consultingwerk/EnumMember.i DialogResultYes 8 DialogResultEnum}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DialogResultEnum class                                                                       
        Notes:          
        @param piValue The internal (numeric) representation of the Enumeration member
        @param pcLabel The text label of the Enumaration member
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE DialogResultEnum (piValue AS INTEGER, pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Value = piValue
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList with the names of the Enum members 
        Notes:   
        @return The CharacterList with the names of the Enum members
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetNames ():
        
        DEFINE VARIABLE oReturn AS CharacterList NO-UNDO . 
        
        RETURN NEW CharacterList ('{&EnumMembers}':U).

    END METHOD .
    
&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the Consultingwerk DialogResultEnum member that matches a 
                 System.Windows.Forms.DialogResult member                                                                        
        Notes:     
        @param poDialogResult The .NET DialogResult member 
        @return The DialogResultEnum member matching the parameter value                                                                    
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResultEnum FromSystemWindowsForms (poDialogResult AS DialogResult):

        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:Abort) THEN 
            RETURN DialogResultEnum:DialogResultAbort . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:Cancel) THEN 
            RETURN DialogResultEnum:DialogResultCancel . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:Ignore) THEN 
            RETURN DialogResultEnum:DialogResultIgnore . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:No) THEN 
            RETURN DialogResultEnum:DialogResultNo . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:None) THEN 
            RETURN DialogResultEnum:DialogResultNone . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:OK) THEN 
            RETURN DialogResultEnum:DialogResultOK . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:Retry) THEN 
            RETURN DialogResultEnum:DialogResultRetry . 
        IF Progress.Util.EnumHelper:AreEqual(poDialogResult, DialogResult:Yes) THEN 
            RETURN DialogResultEnum:DialogResultYes . 

        RETURN ? . 

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Converts a DialogResultEnum member to a default LOGICAL representation                                                                         
        Notes:   Converts Abort, Cancel, No, None to FALSE, 
                 Converts OK, Yes, Retry to TRUE
                 Converts Ignore to ?         
        @return The logical value matching the Enum's value                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ToLogical ():
        
        CASE THIS-OBJECT:Value:
            WHEN 1 OR WHEN 2 OR WHEN 4 OR WHEN 5 THEN RETURN FALSE .
            WHEN 6 OR WHEN 8 OR WHEN 7 THEN RETURN TRUE . 
            OTHERWISE RETURN ? .
        END CASE . 

        RETURN ? . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Converts a DialogResultEnum member to a default LOGICAL representation                                                                      
        Notes:   Converts Cancel to ? in a yes-no-cancel dialog
                 Converts Abort, Cancel, No, None to FALSE, 
                 Converts OK, Yes, Retry to TRUE
                 Converts Ignore to ?         
        @param poDialogResults The array of DialogResultEnum enum values as returned by MessageFormDefaults:GetDialogResult....
        @return The logical value matching the Enum's value                                                            
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ToLogical (poDialogResults AS DialogResultEnum EXTENT):
        
        IF EXTENT (poDialogResults) = 3 AND 
           poDialogResults[3] = DialogResultEnum:DialogResultCancel  AND 
           poDialogResults[1] = DialogResultEnum:DialogResultYes  AND 
           poDialogResults[2] = DialogResultEnum:DialogResultNo THEN 
           
            CASE THIS-OBJECT:Value:  
                WHEN {&DialogResultNo} THEN RETURN FALSE .
                WHEN {&DialogResultYes} THEN RETURN TRUE .
                OTHERWISE RETURN ?. 
            END CASE.

        RETURN THIS-OBJECT:ToLogical () . 

    END METHOD.

    {Consultingwerk/EnumFromString.i Consultingwerk.Framework.Enum.DialogResultEnum}    

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the System.Windows.Forms.DialogResult member that matches a 
                 Consultingwerk DialogResultEnum member                                                                               
        Notes:     
        @param poDialogResultEnum A DialogResultEnum to return the .NET DialogResult enum member for
        @return The DialogResult Enum member matching the DialogResultEnum member                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC DialogResult ToSystemWindowsForms (poDialogResultEnum AS DialogResultEnum):
    
        CASE poDialogResultEnum:Value:
            WHEN 1 THEN RETURN DialogResult:Abort .
            WHEN 2 THEN RETURN DialogResult:Cancel .
            WHEN 3 THEN RETURN DialogResult:Ignore .
            WHEN 4 THEN RETURN DialogResult:No .
            WHEN 5 THEN RETURN DialogResult:None .
            WHEN 6 THEN RETURN DialogResult:OK .
            WHEN 7 THEN RETURN DialogResult:Retry .
            WHEN 8 THEN RETURN DialogResult:Yes .
        END CASE . 
    
        RETURN ? . 
        
    END METHOD .
&ENDIF
END CLASS.
