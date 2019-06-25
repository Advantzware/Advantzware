<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="rel_ord_shipnote" Codebehind="rel_ord_shipnote.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Release Order</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language ="javascript">
    
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
    
 
function orderhelp() {
    var NewWindow = window.open("ar_inv_help.aspx", "OrderHelpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function printrep() {
    var NewWindow = window.open("topbtnorderreport.aspx", "OrderReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function printackrep() {
    var NewWindow = window.open("topprintorderack_report.aspx", "OrderAcknowledgementReport", "width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ordernotes() {
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ordernotes() {
    var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='vendor_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      
       <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr bgcolor="maroon">
                                        
                    
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                       
                        
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
          </td>
      </tr>
      <tr>
      <td>
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
          <asp:HiddenField ID="HiddenField1" runat="server" />
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>         
          <TD align=left nowrap><font size=+0><b>Release Order&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <asp:LinkButton ID="lnk_Listcustomers" OnClick="lnk_list_click" runat="server" ><img src="Images/brwsrelease0.jpg" border="0" alt="List Invoices " /></asp:LinkButton>
      <asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > <img src="Images/items0.jpg" border="0" alt="View Invoices" /></asp:LinkButton>
      <asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewcustomers_Click" > <img src="Images/shipnotes1.jpg" border="0" alt="Load Invoices" /></asp:LinkButton>
            
      </td>
      </tr></table>
     
              
      <div>
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_OnDataBound" DataSourceID="ObjectDataSource1">
               <EditItemTemplate>
                <fieldset><table class="shade">
               <tr><td>
               <table><tr>
                <td align="right" style="padding-right:5px"><b>Order#:</b></td>
               <td><asp:Label ID="orderLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("ordno") %>' /></td> 
               <td align="right" style="padding-right:5px"><b>Date Last Counted:</b></td>
               <td><asp:Label ID="dtchngLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("reldate") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Customer:</b></td>
               <td><asp:Label ID="custLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custno") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Release#:</b></td>
               <td><asp:Label ID="rellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("rellno") %>' /></td></tr></table>
               
               <table><tr><tr><tr></tr><tr></tr><tr></tr><td><asp:TextBox ID="shipnot1TextBox" Width="500px" runat="server" Text='<%# Bind("ship1") %>' /></td></tr>                
               <tr><td><asp:TextBox ID="shipnot2TextBox" Width="500px" runat="server" Text='<%# Bind("ship2") %>' /></td></tr>                             
               <tr><td><asp:TextBox ID="shipnot3TextBox" Width="500px" runat="server" Text='<%# Bind("ship3") %>' /></td></tr>                              
               <tr><td><asp:TextBox ID="shipnot4TextBox" Width="500px" runat="server" Text='<%# Bind("ship4") %>' /></td></tr>
               
               
                             
               </table>               
               </td></tr>
               </table></fieldset>
               <br /><div style="display:none">
                  <asp:Label ID="reckeyTextBox" runat="server" Text='<%# Bind("reckey") %>' />
                
                   <asp:Label ID="extraTextBox" runat="server" Text='<%# Bind("extra") %>' /></div>
               
                   <asp:Button ID="Button1" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"  Text="Save" />
                   &nbsp;<asp:Button ID="Button2" runat="server" CssClass="button"
                       CausesValidation="False" CommandName="Cancel" Text="Cancel" />               
               </EditItemTemplate>
               
               <ItemTemplate>
                   <fieldset><table class="shade">
               <tr><td>
                <table><tr>
                <td align="right" style="padding-right:5px"><b>Order#:</b></td>
               <td><asp:Label ID="orderLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("ordno") %>' /></td> 
               <td align="right" style="padding-right:5px"><b>Date Last Counted:</b></td>
               <td><asp:Label ID="dtchngLabel" Width="50px" BackColor="Turquoise" runat="server" Text='<%# Bind("reldate") %>' /></td>
               <td align="right" style="padding-right:5px"><b>Customer:</b></td>
               <td><asp:Label ID="custLabel" Width="100px" BackColor="Turquoise" runat="server" Text='<%# Bind("custno") %>' /></td>               
               <td align="right" style="padding-right:5px"><b>Release#:</b></td>
               <td><asp:Label ID="rellLabel" Width="70px" BackColor="Turquoise" runat="server" Text='<%# Bind("rellno") %>' /></td></tr></table>
               
               <table><tr><tr></tr><tr></tr><tr></tr><td><asp:Label ID="shipnot1TextBox" Width="500px" BackColor="Turquoise" runat="server" Text='<%# Bind("ship1") %>' /></td></tr>                
               <tr><td><asp:Label ID="shipnot2TextBox" Width="500px" BackColor="Turquoise" runat="server" Text='<%# Bind("ship2") %>' /></td></tr>                             
               <tr><td><asp:Label ID="shipnot3TextBox" Width="500px" BackColor="Turquoise" runat="server" Text='<%# Bind("ship3") %>' /></td></tr>                              
               <tr><td><asp:Label ID="shipnot4TextBox" Width="500px" BackColor="Turquoise" runat="server" Text='<%# Bind("ship4") %>' /></td></tr>
               
               
                             
               </table>               
               </td></tr>
               </table></fieldset> 
               
                <br />
                <div>
                  <asp:Label ID="reckeyTextBox" Visible="false" runat="server" Text='<%# Bind("reckey") %>' />
                
                   <asp:Label ID="extraTextBox" runat="server" Visible="false" Text='<%# Bind("extra") %>' /></div>                   
                   
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit" Text="Update" />
                   
               </ItemTemplate>
           </asp:FormView>
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="ReleaseOrderlist" 
                TypeName="release">
                <SelectParameters>
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                    
                   <asp:Parameter Name="prmComp"  Type="string" />
                   <asp:Parameter Name="prmUser" Type="string" />                     
                   <asp:Parameter Name="prmrellno" Type="Int32" />                                        
                   <asp:Parameter Name="prmordno" Type="Int32" />                                        
                   <asp:Parameter Name="prmpono" Type="String" />
                   <asp:Parameter Name="prmcustno" Type="String" />
                   <asp:Parameter Name="prmpartno" Type="String" />
                   <asp:Parameter Name="prmshipid" Type="String" />
                   <asp:Parameter Name="prmino" Type="String" />                   
                   <asp:Parameter Name="prmreldate"  Type="string" />
                   <asp:Parameter Name="prmjobno" Type="string" />                     
                   <asp:Parameter Name="prmjobno2" Type="Int32" />                                        
                   <asp:Parameter Name="prmcarrier" Type="String" />                                        
                   <asp:Parameter Name="prmtrailer" Type="String" />
                   <asp:Parameter Name="prmposted" Type="String" />
                   <asp:Parameter Name="prmship1" Type="String" />
                   <asp:Parameter Name="prmship2" Type="String" />
                   <asp:Parameter Name="prmship3" Type="String" />
                   <asp:Parameter Name="prmship4" Type="String" />
                   <asp:Parameter Name="prmReckey" Type="String" />
                   <asp:SessionParameter SessionField="Release_ord_r_no" Name="prmextra" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
       </div> 
          </div></td></tr></table>
      
     
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

