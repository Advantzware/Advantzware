<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="distribute_account" Codebehind="distribute_account.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>G/L Account</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script>
    <script language =javascript>
   
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

    function printgl() {
        var NewWindow = window.open("topprintcust_report.aspx", "OrderReport", "width=600,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function ordernotes() {
        var NewWindow = window.open("toporder_list_notes.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='Inv_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
           <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
         
         <table><tr><td><div> 
        <table align="left" border="1"  width="75%">
                <tr class="topheadcolor">
                                        
                    <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printgl(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
                        </td>
                        
                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table>
        </div>   </td></tr>
        <tr><td>
        
      <div>
      
         
          <asp:HiddenField ID="HiddenField_oldinv" runat="server" />
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>         
          <TD align=left nowrap><font size=+0><b>G/L Account&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp; </td>
        </TR>
      </TABLE>
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li >
      <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_listinvoice" >Brws Account</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcustomers" runat="server" OnClick="lnk_viewcustomers_Click" > View Account</asp:LinkButton></li>
      <li class="selected"><asp:LinkButton ID="load_viewcustomers" runat="server" OnClick="load_viewcustomers_Click" > Distrib. Acct </asp:LinkButton>  </li></ul></div>
      </td>
      </tr></table>
       <div>
           
           <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_ondatabound" DataSourceID="ObjectDataSource1">
               
               <ItemTemplate>
                   <asp:Panel ID="exitpanel" CssClass="shade" width="450px" runat="server" DefaultButton="">
                   <fieldset class="shade">               
               <table class="shade"><tr><td>
                   <tr><td align="right" style="padding-right: 5px"><b>Account No:</b></td>
                   <td><asp:Label ID="actTextBox" Width="100" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("act") %>' /></td>                   
                   <td><asp:Label ID="actdscrTextBox" Width="180" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("actdscr") %>' /></td></tr> 
                   </td></tr></table>
                   </fieldset></asp:Panel>
                   
               </ItemTemplate>
           </asp:FormView>
           
           
           
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="GLAccountList" 
                TypeName="ledger">
                <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="string" />  
                   <asp:Parameter Name="prmAction" DefaultValue="View" Type="String" />                                                                             
                   <asp:Parameter Name="prmact" Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" />
                   <asp:Parameter Name="prmactype" Type="String" />
                   <asp:SessionParameter SessionField="gl_account_list_reckey" Name="prmReckey" Type="String" />
                                      
                   <asp:Parameter Name="prmcry_opn" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr_opn" Type="Decimal" />
                   <asp:Parameter Name="prmbud1" Type="Decimal" />
                   <asp:Parameter Name="prmbud2" Type="Decimal" />
                   <asp:Parameter Name="prmbud3" Type="Decimal" />
                   <asp:Parameter Name="prmbud4" Type="Decimal" />
                   <asp:Parameter Name="prmbud5" Type="Decimal" />
                   <asp:Parameter Name="prmbud6" Type="Decimal" />
                   <asp:Parameter Name="prmbud7" Type="Decimal" />
                   <asp:Parameter Name="prmbud8" Type="Decimal" />                   
                   <asp:Parameter Name="prmbud9" Type="Decimal" />
                   <asp:Parameter Name="prmbud10" Type="Decimal" />
                   <asp:Parameter Name="prmbud11" Type="Decimal" />
                   <asp:Parameter Name="prmbud12" Type="Decimal" />
                   <asp:Parameter Name="prmbud13" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud1" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud2" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud3" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud4" Type="Decimal" />                   
                   <asp:Parameter Name="prmly_bud5" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud6" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud7" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud8" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud9" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud10" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud11" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud12" Type="Decimal" />
                   <asp:Parameter Name="prmly_bud13" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr1" Type="Decimal" />
                   <asp:Parameter Name="prmlyr2" Type="Decimal" />
                   <asp:Parameter Name="prmlyr3" Type="Decimal" />
                   <asp:Parameter Name="prmlyr4" Type="Decimal" />
                   <asp:Parameter Name="prmlyr5" Type="Decimal" />
                   <asp:Parameter Name="prmlyr6" Type="Decimal" />
                   <asp:Parameter Name="prmlyr7" Type="Decimal" />
                   <asp:Parameter Name="prmlyr8" Type="Decimal" />
                   <asp:Parameter Name="prmlyr9" Type="Decimal" />                   
                   <asp:Parameter Name="prmlyr10" Type="Decimal" />
                   <asp:Parameter Name="prmlyr11" Type="Decimal" />
                   <asp:Parameter Name="prmlyr12" Type="Decimal" />
                   <asp:Parameter Name="prmlyr13" Type="Decimal" />
                   <asp:Parameter Name="prmcyr1" Type="Decimal" />
                   <asp:Parameter Name="prmcyr2" Type="Decimal" />
                   <asp:Parameter Name="prmcyr3" Type="Decimal" />
                   <asp:Parameter Name="prmcyr4" Type="Decimal" />
                   <asp:Parameter Name="prmcyr5" Type="Decimal" />                   
                   <asp:Parameter Name="prmcyr6" Type="Decimal" />
                   <asp:Parameter Name="prmcyr7" Type="Decimal" />
                   <asp:Parameter Name="prmcyr8" Type="Decimal" />
                   <asp:Parameter Name="prmcyr9" Type="Decimal" />
                   <asp:Parameter Name="prmcyr10" Type="Decimal" />
                   <asp:Parameter Name="prmcyr11" Type="Decimal" />
                   <asp:Parameter Name="prmcyr12" Type="Decimal" />
                   <asp:Parameter Name="prmcyr13" Type="Decimal" />
                   <asp:Parameter Name="prmtb_not_disc" Type="String" />
                   <asp:Parameter Name="prmbtn" Type="String" />
                   
                   
                </SelectParameters>
            </asp:ObjectDataSource>
                                
       </div>    
       <div>
       <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True" OnSelectedIndexChanged="GridView1_SelectedIndex"
        AutoGenerateColumns="False" CssClass="Grid" DataSourceID="ObjectDataSource2" DataKeyNames="reckey"
        EmptyDataText="No Record Found"  Width="450px">
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <Columns>
             <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="cst_act" HeaderText="Account#"  SortExpression="cst_act" />            
            <asp:BoundField DataField="cst_dscr" HeaderText="Description" SortExpression="cst_dscr" />   
            <asp:BoundField DataField="c_rate" HeaderText="Pct"  SortExpression="c_rate" />
            <asp:BoundField DataField="actype" HeaderText="Type" SortExpression="actype" />
                      
            
            <asp:TemplateField HeaderText="Reckey" Visible="false">
            <ItemTemplate>
            <asp:Label ID="reclabel" runat="server" Text='<%# Bind("[RecKey]") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
        </Columns>
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView><br />       
           
           
           <asp:FormView ID="FormView2" runat="server" OnDataBound="FormView2_OnDataBound" DataSourceID="ObjectDataSource3">
               <EditItemTemplate>
                   <asp:Panel ID="EditPanel" Width="450px" Height="120px" DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Account#:</b></td><td><b>Description:</b></td><td><b>Pct:</b></td><td><b>Type:</b></td>
                   
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="csact_TextBox2" runat="server" Width="130px" Text='<%# Bind("cst_act") %>' /></td>
                   <td><asp:label ID="csactdscr_label" runat="server" BackColor="Turquoise" Width="150px" Text='<%# Bind("cst_dscr") %>' /></td>
                   <td><asp:TextBox ID="pct_TextBox" runat="server" MaxLength="25" Width="70px" Text='<%# Bind("c_rate") %>' /></td>
                   <td><asp:label ID="type_label" runat="server" BackColor="Turquoise" Width="70px" Text='<%# Bind("actype") %>' /></td>                   
                   <td><asp:TextBox ID="reckeyTextBox" runat="server" Visible="false" Text='<%# Bind("RecKey") %>' /></td>                  
                    </tr></table>                   
                   
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
               </EditItemTemplate>
               <InsertItemTemplate>
                  <%-- <asp:Panel ID=InsertPanel" Width="450px" Height="120px" DefaultButton="InsertButton" runat="server" >--%>
                    
                   <fieldset class="shade">
                   <table>
                   <tr>
                   <td><b>Account#:</b></td><td><b>Description:</b></td><td><b>Pct:</b></td><td><b>Type:</b></td>
                   
                   </tr>
                   <tr><td>
                   <asp:TextBox ID="csact_TextBox" runat="server" Width="130px" Text='<%# Bind("cst_act") %>' /></td>
                   <td><asp:label ID="csactdscr_label" runat="server" BackColor="Turquoise" Width="150px" Text='<%# Bind("cst_dscr") %>' /></td>
                   <td><asp:TextBox ID="pct_TextBox" runat="server" MaxLength="25" Width="70px" Text='<%# Bind("c_rate") %>' /></td>
                   <td><asp:label ID="type_label" runat="server" BackColor="Turquoise" Width="70px" Text='<%# Bind("actype") %>' /></td>                   
                   <%--<td><asp:TextBox ID="reckeyTextBox" runat="server" Text='<%# Bind("RecKey") %>' /></td>--%>                  
                    </tr></table>    
                    
                   <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="button" OnClick="AddButton_Formview2_Click" Text="Save" />
                   &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                   </fieldset><%--</asp:Panel>--%>
               </InsertItemTemplate>
               <ItemTemplate>
                  
                   <asp:Label ID="ReckeyLabel"  runat="server" Visible="false" Text='<%# Bind("[RecKey]") %>'></asp:Label>
                                      
                    <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="deleteButton_FormView2_Click" Text="Delete" />
                   
               </ItemTemplate>
           </asp:FormView>
            <%--<asp:Button ID="AddNewFormView2Button" runat="server" CssClass="button"  OnClick="AddNewFormView2Button_Click" Text="Add" />--%>
    
    
           <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="GLDistributedAccount" 
               TypeName="ledger">
               <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />                                      
                   <asp:SessionParameter SessionField="glact_dist_rec" Name="prmact" Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" />
                   <asp:Parameter Name="prmcst_act"  Type="String" />
                   <asp:Parameter Name="prmcst_dscr" Type="String" />
                   <asp:Parameter Name="prmc_rate" Type="Decimal" />
                   <asp:Parameter Name="prmactype" Type="String" />                   
                   <asp:SessionParameter SessionField="gl_account_dist_reckey" Name="prmReckey"  Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
    
    
    
           <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
               OldValuesParameterFormatString="original_{0}" SelectMethod="GLDistributedAccount" 
               TypeName="ledger">
               <SelectParameters>
                   <asp:Parameter Name="prmUser" Type="String" />
                   <asp:Parameter DefaultValue="Search" Name="prmAction" Type="String" />                                      
                   <asp:Parameter  Name="prmact" Type="String" />
                   <asp:Parameter Name="prmactdscr" Type="String" />
                   <asp:Parameter Name="prmcst_act"  Type="String" />
                   <asp:Parameter Name="prmcst_dscr" Type="String" />
                   <asp:Parameter Name="prmc_rate" Type="Decimal" />
                   <asp:Parameter Name="prmactype" Type="String" />                   
                   <asp:SessionParameter SessionField="gl_account_list_reckey" Name="prmReckey"  Type="String" />
               </SelectParameters>
           </asp:ObjectDataSource>
       </div>
       
    </div>
    </td></tr></table>    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

