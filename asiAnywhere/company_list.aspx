<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="company_list" Codebehind="company_list.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Company</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" type="text/javascript" >


//    var bSelected=false;
//    function ChSel()
//    {
//        var theForm = document.forms['frmList'];
//        if (!theForm) theForm = document.frmList;
//        bSelected = !bSelected; 
//        var i;
//        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
//    } 
//    
//    function OnKeyDown()
//    {
//        e = window.event;
//        if (e.keyCode == 13)
//        {
//            e.cancel = true;
//            var theForm = document.forms['frmList'];
//            if (!theForm) theForm = document.frmList;                
//            theForm.btnSearch.click();              
//        }
//    }


    function test() {        
        var NewWindow = window.open("company_lookup.aspx", "CompanyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

   
    function CompanyLookup(ReturnObj1) {
        document.forms[0].comp_TextBox.value = ReturnObj1;
        document.forms[0].comp_TextBox.focus();
    }
    function printcomp() {
        var NewWindow = window.open("topprintcomp_report.aspx", "CompanyReport", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }



   </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='comp_TextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
        <table><tr><td><div>
            <table align="left" border="1" width="95%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="printcomp(); return false"><asp:Image ID="Image2" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
            </table></div></td></tr>
            <tr><td>
            
                
      <div>
          <asp:ScriptManager ID="ScriptManager1" runat="server">
          </asp:ScriptManager>
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Company&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td><div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_Listcompany" runat="server" >Brws Company</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewcompany" runat="server" OnClick="lnk_viewcompany_Click" > View Company</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_periodlist" runat="server" OnClick="lnk_listperiod_Click" >Open Periods </asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_periodview" runat="server" OnClick="lnk_viewperiod_Click" >View Period</asp:LinkButton></li></ul></div>
      
      
      
      
      </td>
      </tr></table>
    
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='500px' border="0" >
        <TR >
          <TD class="shade">
            <fieldset>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"   >
              <TR>
                <TD align="center" width="50" class="shade" bgcolor="gray"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle" align="left" >&nbsp;                
                <table >
                <tr><td ><b>Company#</b></td><td align="center"><b> Name</b></td> <td><b> Records/Page</b></td></tr>
                <tr><td>
                    <asp:TextBox ID="comp_TextBox" Width="80px" runat="server"></asp:TextBox>
                    <a href="#" tabindex="1" onClick="test(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    <td><asp:TextBox ID="Name_TextBox" Width="130px" runat="server"></asp:TextBox></td>
                    <TD id="tdPageCount" runat="server" class="shade"  >
          <table><tr><td align="left">
           
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                               <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
          </td></tr></table>  
                </TD>                    
                    </tr></table>
                  
                </TD>                 
                
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </fieldset></td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td> 
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="500px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="images\sel.gif" SelectText="" >
                    <ItemStyle Width="10px" />                    
                    </asp:CommandField>
                    
                    <asp:BoundField DataField="company" HeaderText="Company" SortExpression="company" />
                    <asp:BoundField DataField="vname" HeaderText="Name" SortExpression="vname" />
                    <asp:BoundField DataField="fid" HeaderText="Federal ID" SortExpression="fid" />                    
                    <asp:BoundField DataField="sid" HeaderText="State ID" SortExpression="sid" />                    
                  
                    <asp:TemplateField Visible="false"  HeaderText="Reckey"  >
                    <ItemTemplate>      
                    <asp:Label id="reckeyLabel" runat="server" Text='<%# Bind("reckey") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                                       
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView><%--</ContentTemplate>
                        </asp:UpdatePanel>--%>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
                OldValuesParameterFormatString="original_{0}" SelectMethod="CompanyList" 
                TypeName="ledger">
                <SelectParameters>
                    <asp:Parameter DefaultValue="search" Name="prmAction" Type="String" />                    
                    <asp:SessionParameter Name="prmComp" SessionField="Customers_Company" Type="string" />
                    <asp:SessionParameter Name="prmUser" SessionField="customer_user_id" Type="string" />                     
                    <asp:SessionParameter SessionField="company_list_prmcomp" Name="prmCompany" Type="String" />
                    <asp:Parameter  Name="prmfid" Type="String" />
                    <asp:SessionParameter SessionField="company_list_prmname" Name="prmvname" Type="String" />
                    <asp:Parameter Name="prmsid" Type="String" />
                    <asp:Parameter Name="prmaddr1" Type="String" />
                    <asp:Parameter Name="prmaddr2" Type="String" />
                    <asp:Parameter Name="prmCity" Type="String" />
                    <asp:Parameter Name="prmState" Type="String" />
                    <asp:Parameter Name="prmZip" Type="String" />
                    <asp:Parameter Name="prmcoacc" Type="String" />
                    <asp:Parameter Name="prmnumper" Type="String" />
                    <asp:Parameter Name="prmacclevel" Type="String" />
                    <asp:Parameter Name="prmaccdig1" Type="String" />
                    <asp:Parameter Name="prmaccdig2" Type="String" />
                    <asp:Parameter Name="prmaccdig3" Type="String" />
                    <asp:Parameter Name="prmaccdig4" Type="String" />
                    <asp:Parameter Name="prmaccdig5" Type="String" />
                    <asp:Parameter Name="prmyendoff" Type="String" />
                    <asp:Parameter Name="prmcurrcode" Type="String" />
                    <asp:Parameter Name="prmyendper" Type="String" />
                    <asp:Parameter Name="prmfirstyear" Type="String" />
                    <asp:Parameter Name="prmprdnum" Type="String" />
                    <asp:Parameter Name="prmprddt1" Type="String" />
                    <asp:Parameter Name="prmprddt2" Type="String" />
                    <asp:Parameter Name="prmcdesc" Type="String" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    </td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

