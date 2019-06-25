<%@ Page Language="C#" AutoEventWireup="true" Inherits="test_lookup" Title = "Test Code Information" Codebehind="test_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Test Code Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
     <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800" style="height: 32px">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="ANY">ANY</asp:ListItem>--%>
                      <asp:ListItem Value="Test">Test Code</asp:ListItem>  
                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                   <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                   
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table></asp:Panel>
  </div>
    
    <div>
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found On The Flute" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.testLookup('<%#DataBinder.Eval(Container,"DataItem.vTest")%>','<%#DataBinder.Eval(Container,"DataItem.board")%>','<%#DataBinder.Eval(Container,"DataItem.caliper")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vTest" HeaderText="Test Code" SortExpression="vTest" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="board" Visible="false" HeaderText="board" SortExpression="vTest" />
                <asp:BoundField DataField="caliper" Visible="false" HeaderText="caliper" SortExpression="vTest" />
                                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="TestCode" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:QueryStringParameter Name="prmStyle" QueryStringField="style" Type="string" />
                <asp:QueryStringParameter QueryStringField="look1" Name="prmFlute" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
