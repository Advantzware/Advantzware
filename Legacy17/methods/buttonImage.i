/* buttonImage.i - rstark - 2.22.2017 */
/* generated 07.26.2017 @ 10:09:40 am */

&GLOBAL-DEFINE imageName
&GLOBAL-DEFINE imageFolder Graphics/32x32/
&IF "{3}" NE "" &THEN
&GLOBAL-DEFINE imageFolder Graphics/{3}x{3}/
&ENDIF

    &IF "{2}" EQ " " &THEN &GLOBAL-DEFINE imageName inactive
&ELSEIF "{2}" EQ "+FG#" &THEN &GLOBAL-DEFINE imageName memory
&ELSEIF "{2}" EQ "3D Image" &THEN &GLOBAL-DEFINE imageName 3d_glasses
&ELSEIF "{2}" EQ "Add" &THEN &GLOBAL-DEFINE imageName document_empty
&ELSEIF "{2}" EQ "Add Job" &THEN &GLOBAL-DEFINE imageName add
&ELSEIF "{2}" EQ "Add Periods" &THEN &GLOBAL-DEFINE imageName add
&ELSEIF "{2}" EQ "Add Tandem" &THEN &GLOBAL-DEFINE imageName add
&ELSEIF "{2}" EQ "Allocation" &THEN &GLOBAL-DEFINE imageName graph_fork
&ELSEIF "{2}" EQ "Auto-Calc" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Auto-Calculate" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "B.O.M." &THEN &GLOBAL-DEFINE imageName document_text
&ELSEIF "{2}" EQ "Bol/Inv" &THEN &GLOBAL-DEFINE imageName document_notebook
&ELSEIF "{2}" EQ "Bom" &THEN &GLOBAL-DEFINE imageName document_text
&ELSEIF "{2}" EQ "Build Menu" &THEN &GLOBAL-DEFINE imageName engineer
&ELSEIF "{2}" EQ "Cad/2D" &THEN &GLOBAL-DEFINE imageName document_center_vertical
&ELSEIF "{2}" EQ "Calculate" &THEN &GLOBAL-DEFINE imageName Calculator
&ELSEIF "{2}" EQ "Caliper" &THEN &GLOBAL-DEFINE imageName Caliper
&ELSEIF "{2}" EQ "Cancel" &THEN &GLOBAL-DEFINE imageName error
&ELSEIF "{2}" EQ "Close" &THEN &GLOBAL-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Copy" &THEN &GLOBAL-DEFINE imageName copy
&ELSEIF "{2}" EQ "Copy Balances To Budgets" &THEN &GLOBAL-DEFINE imageName data_copy
&ELSEIF "{2}" EQ "Copy Menu" &THEN &GLOBAL-DEFINE imageName copy
&ELSEIF "{2}" EQ "Copy Periods" &THEN &GLOBAL-DEFINE imageName copy
&ELSEIF "{2}" EQ "Delete" &THEN &GLOBAL-DEFINE imageName delete
&ELSEIF "{2}" EQ "Delete All" &THEN &GLOBAL-DEFINE imageName erase
&ELSEIF "{2}" EQ "Depth" &THEN &GLOBAL-DEFINE imageName fit_to_width
&ELSEIF "{2}" EQ "Detail" &THEN &GLOBAL-DEFINE imageName drop_down_list
&ELSEIF "{2}" EQ "Die" &THEN &GLOBAL-DEFINE imageName painters_palette
&ELSEIF "{2}" EQ "Done" &THEN &GLOBAL-DEFINE imageName door_exit
&ELSEIF "{2}" EQ "Down" &THEN &GLOBAL-DEFINE imageName nav_down
&ELSEIF "{2}" EQ "Eff/Qty" &THEN &GLOBAL-DEFINE imageName gold_bars
&ELSEIF "{2}" EQ "Estimatebuild" &THEN &GLOBAL-DEFINE imageName keypad
&ELSEIF "{2}" EQ "First" &THEN &GLOBAL-DEFINE imageName navigate_beginning
&ELSEIF "{2}" EQ "Freight" &THEN &GLOBAL-DEFINE imageName truck_container
&ELSEIF "{2}" EQ "Goto" &THEN &GLOBAL-DEFINE imageName queue
&ELSEIF "{2}" EQ "Graphic" &THEN &GLOBAL-DEFINE imageName graphics_tablet
&ELSEIF "{2}" EQ "Hard Copy" &THEN &GLOBAL-DEFINE imageName element_copy
&ELSEIF "{2}" EQ "Help" &THEN &GLOBAL-DEFINE imageName question
&ELSEIF "{2}" EQ "History" &THEN &GLOBAL-DEFINE imageName history2
&ELSEIF "{2}" EQ "Hold" &THEN &GLOBAL-DEFINE imageName lock
&ELSEIF "{2}" EQ "Hold/Release" &THEN &GLOBAL-DEFINE imageName lock_open
&ELSEIF "{2}" EQ "Image Refresh" &THEN &GLOBAL-DEFINE imageName selection_refresh
&ELSEIF "{2}" EQ "Image Update" &THEN &GLOBAL-DEFINE imageName photos
&ELSEIF "{2}" EQ "Import" &THEN &GLOBAL-DEFINE imageName import
&ELSEIF "{2}" EQ "Import Budgets" &THEN &GLOBAL-DEFINE imageName chest_into
&ELSEIF "{2}" EQ "Import Excel" &THEN &GLOBAL-DEFINE imageName import
&ELSEIF "{2}" EQ "Import Price" &THEN &GLOBAL-DEFINE imageName import
&ELSEIF "{2}" EQ "Import-Price" &THEN &GLOBAL-DEFINE imageName import
&ELSEIF "{2}" EQ "Ink Reset" &THEN &GLOBAL-DEFINE imageName replace
&ELSEIF "{2}" EQ "Invoice" &THEN &GLOBAL-DEFINE imageName invoice
&ELSEIF "{2}" EQ "Item" &THEN &GLOBAL-DEFINE imageName coffee_bean
&ELSEIF "{2}" EQ "Job Stds" &THEN &GLOBAL-DEFINE imageName text_align_left
&ELSEIF "{2}" EQ "Last" &THEN &GLOBAL-DEFINE imageName navigate_end
&ELSEIF "{2}" EQ "Last Yr And YTD Balances" &THEN &GLOBAL-DEFINE imageName book_bookmark
&ELSEIF "{2}" EQ "Leaf/Film" &THEN &GLOBAL-DEFINE imageName film
&ELSEIF "{2}" EQ "Left" &THEN &GLOBAL-DEFINE imageName navigate_left
&ELSEIF "{2}" EQ "Load Highlighted Invoices" &THEN &GLOBAL-DEFINE imageName invoice_euro
&ELSEIF "{2}" EQ "Load Highlighted Journals" &THEN &GLOBAL-DEFINE imageName invoice
&ELSEIF "{2}" EQ "Mail?" &THEN &GLOBAL-DEFINE imageName mail
&ELSEIF "{2}" EQ "Mass Delete" &THEN &GLOBAL-DEFINE imageName selection_delete
&ELSEIF "{2}" EQ "Misc" &THEN &GLOBAL-DEFINE imageName scroll
&ELSEIF "{2}" EQ "No Note" &THEN &GLOBAL-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "No Notes" &THEN &GLOBAL-DEFINE imageName sign_warning
&ELSEIF "{2}" EQ "No UDF" &THEN &GLOBAL-DEFINE imageName sign_forbidden
&ELSEIF "{2}" EQ "No-Udf" &THEN &GLOBAL-DEFINE imageName sign_warning
&ELSEIF "{2}" EQ "Note" &THEN &GLOBAL-DEFINE imageName edit
&ELSEIF "{2}" EQ "OK" &THEN &GLOBAL-DEFINE imageName checkbox
&ELSEIF "{2}" EQ "Override" &THEN &GLOBAL-DEFINE imageName window_environment
&ELSEIF "{2}" EQ "Override Qtys" &THEN &GLOBAL-DEFINE imageName window_environment
&ELSEIF "{2}" EQ "Override Unit" &THEN &GLOBAL-DEFINE imageName window_star
&ELSEIF "{2}" EQ "Post" &THEN &GLOBAL-DEFINE imageName data_floppy_disk
&ELSEIF "{2}" EQ "Prefix Fg Item#" &THEN &GLOBAL-DEFINE imageName bookmark
&ELSEIF "{2}" EQ "Price" &THEN &GLOBAL-DEFINE imageName money_dollar
&ELSEIF "{2}" EQ "Price Change" &THEN &GLOBAL-DEFINE imageName money_bill_cut
&ELSEIF "{2}" EQ "Price Mtx" &THEN &GLOBAL-DEFINE imageName money_coins
&ELSEIF "{2}" EQ "Print" &THEN &GLOBAL-DEFINE imageName printer
&ELSEIF "{2}" EQ "Print BOL" &THEN &GLOBAL-DEFINE imageName print_layout_single
&ELSEIF "{2}" EQ "Print Box" &THEN &GLOBAL-DEFINE imageName print_layout_continous
&ELSEIF "{2}" EQ "Print Price List" &THEN &GLOBAL-DEFINE imageName print_layout_single
&ELSEIF "{2}" EQ "Prior Yr/Ytd Bal" &THEN &GLOBAL-DEFINE imageName book_bookmark
&ELSEIF "{2}" EQ "Qty" &THEN &GLOBAL-DEFINE imageName symbol_sum
&ELSEIF "{2}" EQ "Quote" &THEN &GLOBAL-DEFINE imageName document_information
&ELSEIF "{2}" EQ "Re-Sequence" &THEN &GLOBAL-DEFINE imageName sort_up_down
&ELSEIF "{2}" EQ "Rebuild" &THEN &GLOBAL-DEFINE imageName tools
&ELSEIF "{2}" EQ "Rebuild Job Qty" &THEN &GLOBAL-DEFINE imageName hammer
&ELSEIF "{2}" EQ "Rebuild Job Stds wrench" &THEN &GLOBAL-DEFINE imageName hammer
&ELSEIF "{2}" EQ "Rebuild Std" &THEN &GLOBAL-DEFINE imageName wrench
&ELSEIF "{2}" EQ "Rebuild Stds." &THEN &GLOBAL-DEFINE imageName wrench
&ELSEIF "{2}" EQ "Recalc Cost" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Recalc Costs" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Recalc Qty" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Recalc Qtys" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Recalc Totals" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Recost Board" &THEN &GLOBAL-DEFINE imageName invoice_dollar
&ELSEIF "{2}" EQ "Refresh" &THEN &GLOBAL-DEFINE imageName refresh
&ELSEIF "{2}" EQ "Release" &THEN &GLOBAL-DEFINE imageName trafficlight_green
&ELSEIF "{2}" EQ "Reprice" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Reset" &THEN &GLOBAL-DEFINE imageName undo_32
&ELSEIF "{2}" EQ "Reset Ink" &THEN &GLOBAL-DEFINE imageName paint_brush
&ELSEIF "{2}" EQ "Right" &THEN &GLOBAL-DEFINE imageName navigate_right
&ELSEIF "{2}" EQ "Sample" &THEN &GLOBAL-DEFINE imageName document_size
&ELSEIF "{2}" EQ "Save" &THEN &GLOBAL-DEFINE imageName floppy_disk
&ELSEIF "{2}" EQ "Schedule" &THEN &GLOBAL-DEFINE imageName calendar
&ELSEIF "{2}" EQ "Scores" &THEN &GLOBAL-DEFINE imageName score_board
&ELSEIF "{2}" EQ "Select Bin/Tags" &THEN &GLOBAL-DEFINE imageName elements2
&ELSEIF "{2}" EQ "Select Bins/Tag" &THEN &GLOBAL-DEFINE imageName elements_branch2
&ELSEIF "{2}" EQ "Select Bins/Tags" &THEN &GLOBAL-DEFINE imageName elements2
&ELSEIF "{2}" EQ "Set" &THEN &GLOBAL-DEFINE imageName star
&ELSEIF "{2}" EQ "Sheet Calc" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Shipped" &THEN &GLOBAL-DEFINE imageName small_truck
&ELSEIF "{2}" EQ "Spec Card" &THEN &GLOBAL-DEFINE imageName document_center
&ELSEIF "{2}" EQ "Spec Sheet" &THEN &GLOBAL-DEFINE imageName document_size
&ELSEIF "{2}" EQ "Spec/Noc" &THEN &GLOBAL-DEFINE imageName document_size
&ELSEIF "{2}" EQ "Stack Pattern" &THEN &GLOBAL-DEFINE imageName memory
&ELSEIF "{2}" EQ "Start" &THEN &GLOBAL-DEFINE imageName checkbox
&ELSEIF "{2}" EQ "Start Process" &THEN &GLOBAL-DEFINE imageName checkbox
&ELSEIF "{2}" EQ "Stat" &THEN &GLOBAL-DEFINE imageName chart_column
&ELSEIF "{2}" EQ "Transfer" &THEN &GLOBAL-DEFINE imageName arrow_mix
&ELSEIF "{2}" EQ "UDF" &THEN &GLOBAL-DEFINE imageName window_dialog
&ELSEIF "{2}" EQ "Unapprove" &THEN &GLOBAL-DEFINE imageName undo 
&ELSEIF "{2}" EQ "Unit Calc" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Unpost Actual" &THEN &GLOBAL-DEFINE imageName undo
&ELSEIF "{2}" EQ "Up" &THEN &GLOBAL-DEFINE imageName nav_up
&ELSEIF "{2}" EQ "Update" &THEN &GLOBAL-DEFINE imageName pencil
&ELSEIF "{2}" EQ "Update Base Cost" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "Update Cost/Unit/Count" &THEN &GLOBAL-DEFINE imageName redo
&ELSEIF "{2}" EQ "Update Price Matrix" &THEN &GLOBAL-DEFINE imageName calculator
&ELSEIF "{2}" EQ "View" &THEN &GLOBAL-DEFINE imageName windows
&ENDIF
