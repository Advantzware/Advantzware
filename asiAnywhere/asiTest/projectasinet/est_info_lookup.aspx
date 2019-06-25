<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="est_info_lookup" Codebehind="est_info_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Estimate Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>

    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
     <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
    <table id="tblSearch" runat="server" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" AutoPostBack="true" OnSelectedIndexChanged="searchindexchanged"  runat="server">
                    <asp:ListItem Value="Estimate">Estimate#</asp:ListItem>  
                    <asp:ListItem Value="Customer">Customer#</asp:ListItem> 
                    <asp:ListItem Value="CustomerName">Cust. Name</asp:ListItem>             
                    <asp:ListItem Value="CustomerPart">Cust. Part</asp:ListItem> 
                                         
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation"  runat="server">
                     <asp:ListItem Value="BEGIN">BEGINS</asp:ListItem>
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="600px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                    <ItemStyle HorizontalAlign=Center Wrap="False" />
                    <ItemTemplate>       
		                <a href="#" onClick="javascript:top.opener.window.EstimateInfoLookup('<%#DataBinder.Eval(Container,"DataItem.vEstimate")%>');window.close();">Select</a>           	    		    		    
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vEstimate" HeaderText="Estimate #" 
                    SortExpression="vEstimate" HeaderStyle-Wrap="false" />         
                <asp:BoundField DataField="vCustomer" HeaderText="Customer#" 
                    SortExpression="vCustomer" HeaderStyle-Wrap="false" />                
                <asp:BoundField DataField="vshipname" HeaderText="Cust. Name" 
                    SortExpression="vshipname" HeaderStyle-Wrap="false" /> 
                <asp:BoundField DataField="vdscr" HeaderText="Item Description" SortExpression="vdscr" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vpartno" HeaderText="Cust Part #" 
                    SortExpression="vpartno" HeaderStyle-Wrap="false" />                              
                <asp:BoundField DataField="vStyle" HeaderText="Style Code" 
                    SortExpression="vStyle" HeaderStyle-Wrap="false" /> 
                <asp:BoundField DataField="vLength" HeaderText="Length" 
                    SortExpression="vLength" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vWidth" HeaderText="Width" 
                    SortExpression="vWidth" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vDepth" HeaderText="Depth" 
                    SortExpression="vDepth" HeaderStyle-Wrap="false" />
                
                <asp:BoundField DataField="zrlkkl" HeaderText="zrlkkl" Visible="false" 
                    SortExpression="zrlkkl" />
                
            </Columns>
        </asp:GridView>
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectEstimateLookup" TypeName="LookUp">
            <SelectParameters>
                                <asp:Parameter Name="prmAction" DefaultValue = "" Type="String" />
                                <asp:Parameter Name="prmUser" Type="String" />
                                <asp:Parameter Name="prmField" Type="String" />
                                <asp:Parameter Name="prmCondition" Type="String" />
                                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    
    
    <div>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
       
    </div>
    </div>
    </form>
</body>
</html>

