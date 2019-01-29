<%@ Page Language="C#" AutoEventWireup="true" Inherits="CorBoard_lookup" Codebehind="corboard_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Board Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" >
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="i-no">Board </asp:ListItem>  
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="Style may not be blank..." Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CorBoardLookup('<%#DataBinder.Eval(Container,"DataItem.vItemNum2")%>','<%#DataBinder.Eval(Container,"DataItem.vItemName2")%>','<%#DataBinder.Eval(Container,"DataItem.vWidth")%>','<%#DataBinder.Eval(Container,"DataItem.vLength")%>','<%#DataBinder.Eval(Container,"DataItem.vReal")%>','<%#DataBinder.Eval(Container,"DataItem.vFlute")%>','<%#DataBinder.Eval(Container,"DataItem.vTest")%>','<%#DataBinder.Eval(Container,"DataItem.vCaliper")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vItemNum" ItemStyle-Wrap="false" HeaderText="Item" SortExpression="vItemNum" />
                <asp:BoundField DataField="vItemName" ItemStyle-Wrap="false" HeaderText="Item Name" SortExpression="vItemName" />
                <asp:BoundField DataField="vER" HeaderText="ER" SortExpression="vER" />
                <asp:BoundField DataField="vCaliper" HeaderText="Caliper" SortExpression="vCaliper" />
                <asp:BoundField DataField="vWidth" HeaderText="Width" SortExpression="vWidth" />
                <asp:BoundField DataField="vLength" HeaderText="Length" SortExpression="vLength" />
                <asp:BoundField DataField="vQtyOnHand" HeaderText="QtyOnHand" SortExpression="vQtyOnHand" />
                <asp:BoundField DataField="vComm" HeaderText="Comm" SortExpression="vComm" />
                <asp:BoundField DataField="vAvail" HeaderText="Avail" SortExpression="vAvail" />
                
                <asp:BoundField DataField="vReal" HeaderText="Real" SortExpression="vReal" />
                <asp:BoundField DataField="vFlute" HeaderText="Flute" SortExpression="vFlute" />
                <asp:BoundField DataField="vTest" HeaderText="Test" SortExpression="vTest" />
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectBoard" TypeName="Corrugated">
            <SelectParameters>               
                <asp:Parameter Name="prmAction" Type="String"  />
                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmIndustry" Type="String" />
                <asp:QueryStringParameter QueryStringField="est" Name="prmEstimate" Type="String" />
                <asp:QueryStringParameter QueryStringField="style" Name="prmStyle" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


