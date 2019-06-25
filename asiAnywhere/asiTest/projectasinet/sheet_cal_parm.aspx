<%@ Page Language="C#" AutoEventWireup="true" Inherits="sheet_cal_parm" Codebehind="sheet_cal_parm.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Sheet Calculation Parameters</title>
     <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <script language="JavaScript">
         window.onload = setfocus;
         function setfocus() {
             document.forms[0].machineTextBox.focus();
         }

         function refreshParent() {
             window.opener.location.href = window.opener.location.href;

             if (window.opener.progressWindow) {
                 window.opener.progressWindow.close()
             }
             window.close();
         }

    </script> 

     <script>

         function Machinelook() {
             var NewWindow = window.open("machine_lookup.aspx", "MachineLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
         }
         function MachineLookup(ReturnObj1, ReturnObj2) {
             document.forms[0].machineTextBox.value = ReturnObj1;
             document.forms[0].machineTextBox.focus();
            
         }
        function itemlook()
        {
           var NewWindow = window.open("Item3_look.aspx","ItemLookupWindow","width=500,height=450,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLookUp(ReturnObj1, ReturnObj2, ReturnObj3)
        {

            document.forms[0].itemTextBox.value = ReturnObj3;
            document.forms[0].itemTextBox.focus();            
        }
       
       
     </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
    <br />
       <asp:Panel runat="server">
       <table class="shade">
       <tr><td align="right" style="padding-right:5px"><b>Machine:</b></td>
       <td><asp:TextBox ID="machineTextBox" runat="server"></asp:TextBox>  
       <a href="#" tabindex="1" onclick="Machinelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       </td></tr>
       <tr><td align="right" style="padding-right:5px"><b>Select RM Item Name starting with:</b></td>
       <td><asp:TextBox ID="itemTextBox" runat="server"></asp:TextBox>
       <a href="#" tabindex="1" onclick="itemlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
       </td></tr>
       <tr><td align="right" style="padding-right:5px"><b>Search By:</b></td>
       <td><asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 <asp:ListItem      Text="Item Name" />
                  <asp:ListItem     Text="Item Code" />
                                 
         </asp:RadioButtonList></td></tr>
         <tr><td colspan="2"> &nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp; <b><asp:CheckBox ID="checkBox1" runat="server" Text="Maximize Yield using All Dimensions ?" /></b> </td></tr>
         <tr><td colspan="2">
         <asp:Button ID="savebuttin" runat="server"  CssClass="button" Text="Save" OnClick="save_button_Click" />
         <input type="button" class="button" value="Cancel" onclick="self.close()" />
         </td></tr>
       
       </table>       
       </asp:Panel>
       
    </div>
    </form>
</body>
</html>
