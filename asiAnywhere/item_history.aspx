<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="History_list" Codebehind="item_history.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>History</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
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
    function history()
    {
        var NewWindow = window.open("item_history_detail.aspx","HistoryDetailWindow","width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
        
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>History&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;           
            
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" ></asp:Label> &nbsp;&nbsp;
           <b>Customer</b> &nbsp;<asp:Label ID="cust_label" BackColor="turquoise" runat="server" ></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" width='400px' border="0">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="center" width="50"><nobr>
                  <%--<asp:linkbutton id="btnAdd" runat="server" OnClick="btnAdd_Click">Add new</asp:linkbutton></nobr>--%>
                <asp:button id="btnSearch" runat="server" OnClick=" btnSearch_Click" Width="40px" CssClass="button" Text="Go" ></asp:button>
                <br />
                <br />
                  <asp:button id="btnShowAll" runat="server" OnClick=" btnShowAll_Click" Width="40px" CssClass="button" Text="All" ></asp:button>
                </TD>               
                          
                <TD id="tdSearch" runat="server" class="shade" vAlign="middle"  width="400">&nbsp;                
                <table>
                <tr></tr>
                <tr>                   
                    <td><b>Item# </b>
                    <asp:TextBox ID="item_TextBox" Width="120px" runat="server"></asp:TextBox>
                    </td>
                    <td> <input type="button" id="History_Button" value="Detail" runat="server" class="buttonM" onClick="history()" /></td>
                    </tr></table>
                  
                </TD>               
                
                
                 
                
                <TD id="tdPageCount" runat="server" class="shade" >
          <table><tr><td align="center">
           <b> Records/Page</b><BR>
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
                              <%--<asp:Label ID="aLineLabel" runat="server" Text='<%# Bind("aLine") %>'></asp:Label>--%>
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
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

    
           
            
          </TD>
        </TR>
        <tr><td>
            <asp:GridView ID="GridView1" runat="server" DataSourceID="ObjectDataSource1" EmptyDataText=" Records not  Found" AllowPaging="True" AllowSorting="True"  AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" OnSelectedIndexChanged="GridView1_SelectedIndex">
                <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                        
                <Columns>
                 <asp:CommandField ButtonType="Image"  SelectImageUrl="~/Images/sel.gif"
                            SelectText="" ShowSelectButton="True">
                            <HeaderStyle ForeColor="White" />
                            <ItemStyle ForeColor="White" />
                        </asp:CommandField>
                    <asp:BoundField DataField="hisItem" HeaderText="Item#" ItemStyle-Wrap="false" SortExpression="hisItem" />
                      <asp:TemplateField HeaderText="Sales Date" SortExpression="hisDate">
                            <ItemTemplate>
                                <asp:Label ID="date_Label" runat="server" Text='<%# Bind("hisDate","{0:MM/dd/yyyy}") %>'></asp:Label>                                
                            </ItemTemplate>                            
                        </asp:TemplateField>
                    <asp:BoundField DataField="hisName" HeaderText="Name" ItemStyle-Wrap="false" SortExpression="hisName" />
                    <asp:BoundField DataField="hisDesc" HeaderText="Item Description" ItemStyle-Wrap="false" SortExpression="hisDesc" />
                    <asp:BoundField DataField="hisCost" HeaderText="Cost" ItemStyle-Wrap="false" SortExpression="hisCost" />
                    <asp:BoundField DataField="hisSell" HeaderText="Sell" ItemStyle-Wrap="false" SortExpression="hisSell" />
                    <asp:BoundField DataField="hisUom" HeaderText="Uom" ItemStyle-Wrap="false" SortExpression="hisUom" />
                    <asp:BoundField DataField="hisQuantity" HeaderText="Quantity" ItemStyle-Wrap="false" SortExpression="hisQuantity" />
                    
                    <asp:TemplateField Visible="false">
                            <ItemTemplate>
                                <asp:Label ID="cust_Label" runat="server" Text='<%# Bind("hisCust") %>'></asp:Label>
                                
                            </ItemTemplate>
                            
                        </asp:TemplateField>
                </Columns>
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    <RowStyle  CssClass="shade" />
            </asp:GridView>
            <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectHistory" TypeName="orderentry">
                <SelectParameters>
                    <asp:Parameter DefaultValue="History" Name="prmAction" Type="String" />
                    <asp:Parameter Name="prmUser" Type="String" />
                    <asp:SessionParameter Name="prmRowId" SessionField="order_rec_key" Type="String" />
                    <asp:Parameter Name="prmText" Type="String" />
                </SelectParameters>
            </asp:ObjectDataSource>
          
        </td></tr>
      </TABLE>      
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

