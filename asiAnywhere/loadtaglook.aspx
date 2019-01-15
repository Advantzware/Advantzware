<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="loadtaglook" Codebehind="loadtaglook.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Loadtag Creation</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmPost() {
        var retVal = makeMsgBox("Confirmation", "Post Invoice?", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
        }
    }
</script>
        
    <script language = JavaScript>    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }

    function preEnter(fieldObj, canEdit) {
        //fieldObj.style.backgroundColor = 'blue';
        //fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
    }
    
    function preLeave( fieldObj, fieldType, fieldFormat ){
    //fieldObj.style.backgroundColor='Window';
    //fieldObj.style.color='WindowText';
    fieldType = fieldType.toLowerCase();
    if ((fieldType == "") || (fieldType == "text")) {
        leaveField(fieldObj);
    }
    if (fieldType == "date") {
        if (fieldFormat == "") {
            var dateFormat = "99/99/9999";
        } else { var dateFormat = fieldFormat; }
        checkDate(dateFormat, fieldObj, '01/01/1950', '12/31/3000', 0);
    }

    if (fieldType == "number") {
        if (fieldFormat == "") {
            var numFormat = "(>>>>9)";
        } else { var numFormat = fieldFormat; }
        checkNum(numFormat, fieldObj, '?', '?', 0);
    }   
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}


    function vendtext() {
        var vend = document.getElementById("bevendTextBox");
        vend.focus();
    }
    function periodtext() {
        var vend = document.getElementById("postdateTextBox");
        var pertext = document.getElementById("perTextBox");
        pertext.value = (vend.value).substring(0, 2);

    }
    var vendlook = "";
    function vendorlook(var1) {
        vendlook = var1;
        var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1) {
        if(vendlook == 1)
            document.forms[0].bevendTextBox.value = ReturnObj1;
            else
                document.forms[0].endvendTextBox.value = ReturnObj1;
        }
        

        function calc() {
            var untct = document.getElementById("FormView1_temp8TextBox").value;
            var untplt = document.getElementById("FormView1_temp9TextBox").value;
            var partial = document.getElementById("FormView1_temp10TextBox").value;
            var total = document.getElementById("FormView1_temp11TextBox");

            total.value = (parseInt(untct * untplt)) + parseInt(partial);
        }
        
        function unitrec() {
            var untre = document.getElementById("FormView1_temp9TextBox");
        
        if (parseInt(untre.value) <= 0) {
        alert("Unit/Pallet must be entered");
        }
        }

        function CallParentWindowAlert() {
            window.opener.CallAlert();
            return false;
        }

   </script> 
  </head>    
   <body >
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
       <br />
            <asp:GridView ID="GridView1" runat="server" DataKeyNames="seq_no" DataSourceID="SqlDataSource1" 
                EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" 
                 AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="gridview_selectedindexchange"
                >
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    
                                
                    <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="temp1" HeaderText="Order#" SortExpression="temp1" />                    
                    <asp:BoundField DataField="temp3" HeaderText="Job#" SortExpression="temp3" />
                    <asp:BoundField DataField="temp4" HeaderText="" SortExpression="temp4" />
                    <asp:BoundField DataField="temp2" HeaderText="Cust#" SortExpression="temp2" />
                    <asp:BoundField DataField="temp5" HeaderText="Item#" SortExpression="temp5" />
                    <asp:BoundField DataField="temp6" HeaderText="Ord Qty" SortExpression="temp6" />
                    <asp:BoundField DataField="temp7" HeaderText="Overrun%" SortExpression="temp7" />
                    <asp:BoundField DataField="temp8" HeaderText="Unit Count" SortExpression="temp8" />                    
                    <asp:BoundField DataField="temp9" HeaderText="Units/Pallet" SortExpression="temp9" />
                    <asp:BoundField DataField="temp12" HeaderText="Partial" SortExpression="temp12" />
                    <asp:BoundField DataField="temp10" HeaderText="Total Qty Per Unit" SortExpression="temp10" />
                    <asp:BoundField DataField="temp11" HeaderText="No. of Tags" SortExpression="temp11" />
                    <asp:BoundField DataField="temp13" HeaderText="Unit Wt" SortExpression="temp13" />
                    <asp:BoundField DataField="temp14" HeaderText="Pallet Wt" SortExpression="temp14" />
                    <asp:BoundField DataField="temp15" HeaderText="FG Lot#" SortExpression="temp15" />
                    <asp:BoundField DataField="temp16" HeaderText="Item Name" SortExpression="temp16" />
                    <asp:BoundField DataField="temp17" HeaderText="Customer PO#" SortExpression="temp17" />
                    <asp:BoundField DataField="temp18" HeaderText="rowid" Visible="false" SortExpression="temp18" />
                    <asp:BoundField DataField="seq_no" HeaderText="seq_no" InsertVisible="False" Visible="false" ReadOnly="True" SortExpression="seq_no" />
                    <asp:TemplateField Visible="false">
                    <ItemTemplate>
                    <asp:Label runat="server" ID="sequenc_no" Text='<%# Bind("seq_no") %>' > </asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                                          
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
       
       
       
            <asp:SqlDataSource ID="SqlDataSource1" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"                 
                SelectCommand="SELECT temp_table.* FROM temp_table where [temp20] = @temp20" >
                
                <SelectParameters> <asp:QueryStringParameter Name="temp20" Type="String" QueryStringField="tagno" />                
                </SelectParameters>
                </asp:SqlDataSource>
                
       
              <br />
       
            <asp:FormView ID="FormView1" runat="server" OnDataBound="copyrecord_update"  DataSourceID="SqlDataSource2">
                <EditItemTemplate>
                    <asp:Panel ID="EditPanel" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Order#:</b></td><td><b>Job#</b></td><td></td><td><b>Cust# :</b></td><td><b>Item#:</b></td><td><b>Ord Qty:</b></td>
                   <td><b>Overrun%:</b></td><td><b>Unit Count:</b></td><td><b>Unit/Pallet:</b></td><td><b>Partial:</b></td><td><b>Total Qty Per Unit:</b></td>
                   <td><b>No. of Tags:</b></td><td><b>Unit Wt:</b></td><td><b>Pallet Wt:</b></td><td><b>FG Lot#:</b></td><td><b>Item Name:</b></td>
                   <td><b>Customer PO#:</b></td></tr>
                   
                   <tr><td><asp:Label ID="temp1TextBox" Width="65" runat="server" Text='<%# Bind("temp1") %>' /></td>                   
                   <td><asp:Label ID="temp3TextBox" Width="65" runat="server" Text='<%# Bind("temp3") %>' /></td>
                   <td><asp:Label ID="temp4TextBox" Width="20" runat="server" Text='<%# Bind("temp4") %>' /></td>
                   <td><asp:Label ID="temp2TextBox" Width="65" runat="server" Text='<%# Bind("temp2") %>' /></td>
                   <td><asp:Label ID="temp5TextBox" Width="85" runat="server" Text='<%# Bind("temp5") %>' /></td>
                   <td><asp:Label ID="temp6TextBox" Width="65" runat="server" Text='<%# Bind("temp6") %>' /></td>
                   <td><asp:TextBox ID="temp7TextBox" Width="65" runat="server" Text='<%# Bind("temp7") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="temp7TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp8TextBox" Width="65" onkeyup="calc()" runat="server" Text='<%# Bind("temp8") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="temp8TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>                   
                   <td><asp:TextBox ID="temp9TextBox" Width="65" onkeyup="calc();unitrec()" runat="server" Text='<%# Bind("temp9") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="temp9TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp10TextBox" Width="65" onkeyup="calc()" runat="server" Text='<%# Bind("temp12") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="temp10TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp11TextBox" Width="65" onfocus="calc()" onkeyup="calc()" runat="server" Text='<%# Bind("temp10") %>' />
                   <asp:CompareValidator ID="CompareValidator5" ControlToValidate="temp11TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp12TextBox" Width="65" runat="server" Text='<%# Bind("temp11") %>' />
                   <asp:CompareValidator ID="CompareValidator6" ControlToValidate="temp12TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:Label ID="temp13TextBox" Width="65" runat="server" Text='<%# Bind("temp13") %>' /></td>
                   <td><asp:Label ID="temp14TextBox" Width="65" runat="server" Text='<%# Bind("temp14") %>' /></td>
                   <td><asp:TextBox ID="temp15TextBox" Width="100" runat="server" Text='<%# Bind("temp15") %>' /></td>
                   <td><asp:Label ID="temp16TextBox" Width="130" runat="server" Text='<%# Bind("temp16") %>' /></td>
                   <td><asp:Label ID="temp17TextBox" Width="100" runat="server" Text='<%# Bind("temp17") %>' /></td>
                   </tr></table>
                
                   
                    <fieldset style="display:none">
                    <asp:TextBox ID="file_nameTextBox" runat="server" Text='<%# Bind("file_name") %>' />
                    <asp:Label ID="seq_noLabel1" runat="server" Text='<%# Eval("seq_no") %>' /></fieldset>
                    
                    <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="updatebutton_click" Text="Save" />
                    &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                    </fieldset>
                    </asp:Panel>
                    
                </EditItemTemplate>
                <InsertItemTemplate>
                    <asp:Panel ID="InsertPanel" Height="120px" DefaultButton="InsertButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Order#:</b></td><td><b>Job#</b></td><td></td><td><b>Cust# :</b></td><td><b>Item#:</b></td><td><b>Ord Qty:</b></td>
                   <td><b>Overrun%:</b></td><td><b>Unit Count:</b></td><td><b>Unit/Pallet:</b></td><td><b>Partial:</b></td><td><b>Total Qty Per Unit:</b></td>
                   <td><b>No. of Tags:</b></td><td><b>Unit Wt:</b></td><td><b>Pallet Wt:</b></td><td><b>FG Lot#:</b></td><td><b>Item Name:</b></td>
                   <td><b>Customer PO#:</b></td></tr>
                   
                   <tr><td><asp:Label ID="temp1TextBox" Width="65" runat="server" Text='<%# Bind("temp1") %>' /></td>                   
                   <td><asp:Label ID="temp3TextBox" Width="65" runat="server" Text='<%# Bind("temp3") %>' /></td>
                   <td><asp:Label ID="temp4TextBox" Width="20" runat="server" Text='<%# Bind("temp4") %>' /></td>
                   <td><asp:Label ID="temp2TextBox" Width="65" runat="server" Text='<%# Bind("temp2") %>' /></td>
                   <td><asp:Label ID="temp5TextBox" Width="85" runat="server" Text='<%# Bind("temp5") %>' /></td>
                   <td><asp:Label ID="temp6TextBox" Width="65" runat="server" Text='<%# Bind("temp6") %>' /></td>
                   <td><asp:TextBox ID="temp7TextBox" Width="65" runat="server" Text='<%# Bind("temp7") %>' />
                   <asp:CompareValidator ID="CompareValidator1" ControlToValidate="temp7TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp8TextBox" Width="65" onkeyup="calc()" runat="server" Text='<%# Bind("temp8") %>' />
                   <asp:CompareValidator ID="CompareValidator2" ControlToValidate="temp8TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>                   
                   <td><asp:TextBox ID="temp9TextBox" Width="65" onkeyup="calc();unitrec()" runat="server" Text='<%# Bind("temp9") %>' />
                   <asp:CompareValidator ID="CompareValidator3" ControlToValidate="temp9TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp10TextBox" Width="20" onkeyup="calc()" runat="server" Text='<%# Bind("temp12") %>' />
                   <asp:CompareValidator ID="CompareValidator4" ControlToValidate="temp10TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp11TextBox" Width="65" onfocus="calc()" onkeyup="calc()" runat="server" Text='<%# Bind("temp10") %>' />
                   <asp:CompareValidator ID="CompareValidator5" ControlToValidate="temp11TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:TextBox ID="temp12TextBox" Width="65" runat="server" Text='<%# Bind("temp11") %>' />
                   <asp:CompareValidator ID="CompareValidator6" ControlToValidate="temp12TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td>
                   <td><asp:Label ID="temp13TextBox" Width="65" runat="server" Text='<%# Bind("temp13") %>' /></td>
                   <td><asp:Label ID="temp14TextBox" Width="65" runat="server" Text='<%# Bind("temp14") %>' /></td>
                   <td><asp:TextBox ID="temp15TextBox" Width="100" runat="server" Text='<%# Bind("temp15") %>' /></td>
                   <td><asp:Label ID="temp16TextBox" Width="130" runat="server" Text='<%# Bind("temp16") %>' /></td>
                   <td><asp:Label ID="temp17TextBox" Width="100" runat="server" Text='<%# Bind("temp17") %>' /></td>
                   </tr></table>
                
                   
                    <fieldset style="display:none">
                    <asp:TextBox ID="file_nameTextBox" runat="server" Text='<%# Bind("file_name") %>' />
                    <asp:Label ID="seq_noLabel1" runat="server" Text='<%# Eval("seq_no") %>' /></fieldset>
                    
                    <asp:Button ID="InsertButton" runat="server" CausesValidation="True" OnClick="insertbutton_click" CssClass="button" Text="Save" />
                    &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                    </fieldset>
                    </asp:Panel>
                    
                </InsertItemTemplate>
                <ItemTemplate>
                <asp:Panel ID="ItemPanel" Height="120px" DefaultButton="UpdateItemButton" runat="server" >                                    
                   
                    <fieldset style="display:none">
                    <asp:Label ID="file_nameLabel" runat="server" Text='<%# Bind("file_name") %>' />                   
                    <asp:Label ID="seq_noLabel" runat="server" Text='<%# Eval("seq_no") %>' /></fieldset>
                    
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Copy" />                    
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="delete_Button_Click" Text="Delete" />
                   <asp:Button ID="Button1" runat="server" CssClass="button" OnClick="Create_tag_Click" OnClientClick="return confirm('Are you Sure you Want to Create Loadtag File?')" Text="Create Tags" />
                   
                   </asp:Panel>
                   
                </ItemTemplate>
            </asp:FormView>
     
            <asp:SqlDataSource ID="SqlDataSource2" runat="server" 
                ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>" 
                SelectCommand="SELECT * FROM [temp_table] WHERE ([seq_no] = @seq_no)">
                <SelectParameters>
                    <asp:ControlParameter ControlID="GridView1" Name="seq_no" 
                        PropertyName="SelectedValue" Type="Int32" />
                </SelectParameters>
            </asp:SqlDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


