<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="salestax_code_list" Codebehind="salestax_code_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Sales Tax Codes</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <script language =javascript>
    
    

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='tdSearch'>   
        <hd:header id="Header1" runat="server"></hd:header>
        <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                      
                 
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
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
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=left nowrap><font size=+0><b>Sales Tax Code&nbsp;</b></font></TD>
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
      <table><tr bgcolor="gray"><td> <div  id="navigation" style="width:100%">
		<ul nowrap> <li class="selected">
      <asp:LinkButton ID="lnk_Listvend" runat="server" >Browse Code</asp:LinkButton></li>
      <li><asp:LinkButton ID="lnk_viewvend" runat="server" OnClick="lnk_viewvend_Click" > View Code</asp:LinkButton></li></ul></div>
      
      
      
      
      
      </td>
      </tr></table>
      <asp:UpdatePanel id="gridviewupdatepanel" runat="server">
      <ContentTemplate>
       <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="gridviewupdatepanel"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div> 
        <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch"> 
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1"  border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick="btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick="btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle"  >&nbsp;                
                <table>
                <tr><td ><b>Sales Tax Group</b></td><td> <b>&nbsp;&nbsp;&nbsp;&nbsp; Records/Page</b> </td>
                </tr>
                    <tr><td><asp:TextBox ID="taxgr_TextBox" Width="200px" runat="server"></asp:TextBox></td>
                    <td>  <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2">
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
                                &nbsp;&nbsp;&nbsp;&nbsp;
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
                      </asp:ObjectDataSource></td>                 
                    
                    
                    </tr></table>
                  
                </TD>               
                
                            
                
               
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
        
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True" Width="1300px" AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                    
                    <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    <asp:BoundField DataField="taxgrp" HeaderText="Tax Group" 
                        SortExpression="taxgrp" />
                    <asp:BoundField DataField="code1" HeaderText="Tax Code[1]" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" SortExpression="code1" />
                    <asp:BoundField DataField="dscr1" HeaderText="Description[1]" SortExpression="dscr1" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="rate1" HeaderText="Tax Rate[1]" SortExpression="rate1" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="accl" HeaderText="Sales Tax Account[l]" SortExpression="accl" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="frt1" HeaderText="Tax frt[1]" SortExpression="frt1"  />
                    <asp:BoundField DataField="code2" HeaderText="Tax Code[2]" SortExpression="code2" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="dscr2" HeaderText="Description[2]" SortExpression="dscr2" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="rate2" HeaderText="Tax Rate[2]" SortExpression="rate2" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    
                    <asp:BoundField DataField="acc2" HeaderText="Sales Tax Account[2]" SortExpression="acc2" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="frt2" HeaderText="Tax frt[2]" SortExpression="frt2"  />
                    <asp:BoundField DataField="code3" HeaderText="Tax Code[3]" SortExpression="code3" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="dscr3" HeaderText="Description[3]" SortExpression="dscr3" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="rate3" HeaderText="Tax Rate[3]" SortExpression="rate3" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="acc3" HeaderText="Sales Tax Account[3]" SortExpression="acc3" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="frt3" HeaderText="Tax frt[3]" SortExpression="frt3"  />
                    <asp:BoundField DataField="code4" HeaderText="Tax Code[4]" SortExpression="code4" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="dscr4" HeaderText="Description[4]" SortExpression="dscr4" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="rate4" HeaderText="Tax Rate[4]" SortExpression="rate4" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="acc4" HeaderText="Sales Tax Account[4]" SortExpression="acc4" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="frt4" HeaderText="Tax frt[4]" SortExpression="frt4"  />
                    <asp:BoundField DataField="code5" HeaderText="Tax Code[5]" SortExpression="code5" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="dscr5" HeaderText="Description[5]" SortExpression="dscr5" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="rate5" HeaderText="Tax Rate[5]" SortExpression="rate5" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="acc5" HeaderText="Sales Tax Account[5]" SortExpression="acc5" ItemStyle-Wrap="false"  HeaderStyle-Wrap="false" />
                    <asp:BoundField DataField="frt5" HeaderText="Tax frt[5]" SortExpression="frt5"  />
                    
                   
                         <asp:TemplateField HeaderText="reckey" Visible="false">
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
            </asp:GridView></asp:Panel></ContentTemplate>
                        </asp:UpdatePanel>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectSalesTaxCode" 
              TypeName="voucherpay">
                <SelectParameters>
                    <%--<asp:Parameter Name="prmComp" DefaultValue="" Type="String"  />                    
                    <asp:Parameter Name="prmUser" DefaultValue="" Type="String" />--%>
                    <asp:Parameter DefaultValue="GridSearch" Name="prmAction" Type="String" />
                     
                    <asp:Parameter Name="prmComp" Type="String" />
                     
                    <asp:Parameter Name="prmUser" Type="String" DefaultValue="" />
                    <asp:SessionParameter SessionField="tax_list_prmtaxgrp"  Name="prmtaxgrp" Type="String" />
                    <asp:Parameter Name="prmcode1" Type="String" />
                    <asp:Parameter Name="prmdscr1" Type="String" />
                    <asp:Parameter Name="prmrate1" Type="Decimal" />
                    <asp:Parameter Name="prmaccl" Type="String" />
                    <asp:Parameter Name="prmfrt1" Type="String" />
                    <asp:Parameter Name="prmcode2" Type="String" />
                    <asp:Parameter Name="prmdscr2" Type="String" />
                    <asp:Parameter Name="prmrate2" Type="Decimal" />
                    <asp:Parameter Name="prmacc2" Type="String" />
                    <asp:Parameter Name="prmfrt2" Type="String" />
                    <asp:Parameter Name="prmcode3" Type="String" />
                    <asp:Parameter Name="prmdscr3" Type="String" />
                    <asp:Parameter Name="prmrate3" Type="Decimal" />
                    <asp:Parameter Name="prmacc3" Type="String" />
                    <asp:Parameter Name="prmfrt3" Type="String" />
                    <asp:Parameter Name="prmcode4" Type="String" />
                    <asp:Parameter Name="prmdscr4" Type="String" />
                    <asp:Parameter Name="prmrate4" Type="Decimal" />
                    <asp:Parameter Name="prmacc4" Type="String" />
                    <asp:Parameter Name="prmfrt4" Type="String" />
                    <asp:Parameter Name="prmcode5" Type="String" />
                    <asp:Parameter Name="prmdscr5" Type="String" />
                    <asp:Parameter Name="prmrate5" Type="Decimal" />
                    <asp:Parameter Name="prmacc5" Type="String" />
                    <asp:Parameter Name="prmfrt5" Type="String" />
                    <asp:Parameter Name="prmtax" Type="String" />
                    <asp:Parameter Name="prmReckey" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
           </div>
        </td></tr>
      </TABLE>    
   
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

