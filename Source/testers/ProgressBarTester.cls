 
 /*------------------------------------------------------------------------
    File        : ProgressBar
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : seema.rani
    Created     : Thu Jan 28 14:52:25 IST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS testers.ProgressBarTester INHERITS Form: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer        NO-UNDO.
   DEFINE PRIVATE VARIABLE progressBar1 AS System.Windows.Forms.ProgressBar NO-UNDO.      
       
    CONSTRUCTOR PUBLIC ProgressBarTester ( ):       
       
      SUPER().    
     
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

    END CONSTRUCTOR.

    METHOD PRIVATE VOID InitializeComponent(  ):       

      THIS-OBJECT:progressBar1 = NEW System.Windows.Forms.ProgressBar().
      THIS-OBJECT:SuspendLayout().

      THIS-OBJECT:progressBar1:Location = NEW System.Drawing.Point(13, 13).
      THIS-OBJECT:progressBar1:Name = "progressBar1".
      THIS-OBJECT:progressBar1:Size = NEW System.Drawing.Size(267, 20).
      THIS-OBJECT:progressBar1:TabIndex = 0.
      THIS-OBJECT:progressBar1:Value = 50. /* <--- this indicates % */

      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 50).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:progressBar1).
      THIS-OBJECT:Name = "progressbar".
      THIS-OBJECT:Text = "ProgressBar".
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC ProgressBarTester ( ):

    END DESTRUCTOR.
END CLASS.