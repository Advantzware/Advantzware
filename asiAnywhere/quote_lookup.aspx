<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="quote_lookup" Codebehind="quote_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Order Quotes Lookup</title>
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
                  <asp:dropdownlist id="ddlSearchField" Enabled="true" OnSelectedIndexChanged="searchindexchanged" AutoPostBack="true" runat="server">
                    <asp:ListItem Value="Quote">Quote</asp:ListItem>  
                    <asp:ListItem Value="Fgnum">FG#</asp:ListItem>  
                    <asp:ListItem Value="CustPart">Customer Part#</asp:ListItem>
                                         
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" Enabled="true"  runat="server">
                     <asp:ListItem Value="BEGIN">BEGINS</asp:ListItem> 
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="115px"></asp:textbox>
                  
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
		    <a href="#" onClick="javascript:top.opener.window.quoteLookup('<%#DataBinder.Eval(Container,"DataItem.vEstimate")%>', '<%#DataBinder.Eval(Container,"DataItem.vCustomer")%>', '<%#DataBinder.Eval(Container,"DataItem.vCName")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldid")%>','<%#DataBinder.Eval(Container,"DataItem.vsman")%>','<%#DataBinder.Eval(Container,"DataItem.vCarrier")%>','<%#DataBinder.Eval(Container,"DataItem.vfreight")%>','<%#DataBinder.Eval(Container,"DataItem.vSpct")%>','<%#DataBinder.Eval(Container,"DataItem.vDueCode")%>','<%#DataBinder.Eval(Container,"DataItem.vJobNo")%>','<%#DataBinder.Eval(Container,"DataItem.vJobNo2")%>','<%#DataBinder.Eval(Container,"DataItem.vCAddr")%>','<%#DataBinder.Eval(Container,"DataItem.vCAddr2 ")%>','<%#DataBinder.Eval(Container,"DataItem.vCCity")%>','<%#DataBinder.Eval(Container,"DataItem.vCState")%>','<%#DataBinder.Eval(Container,"DataItem.vCZip")%>','<%#DataBinder.Eval(Container,"DataItem.vContact")%>','<%#DataBinder.Eval(Container,"DataItem.vTerms")%>','<%#DataBinder.Eval(Container,"DataItem.vOverPct")%>','<%#DataBinder.Eval(Container,"DataItem.vUnderPct")%>','<%#DataBinder.Eval(Container,"DataItem.vFobCode")%>','<%#DataBinder.Eval(Container,"DataItem.vTaxGr")%>','<%#DataBinder.Eval(Container,"DataItem.vCType")%>','<%#DataBinder.Eval(Container,"DataItem.vComm")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldname")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldadd1")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldadd2")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldcity")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldstat")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldzip")%>','<%#DataBinder.Eval(Container,"DataItem.vsdscr")%>','<%#DataBinder.Eval(Container,"DataItem.vTdscr")%>','<%#DataBinder.Eval(Container,"DataItem.vLstDate","{0:MM/dd/yyyy}")%>','<%#DataBinder.Eval(Container,"DataItem.vDueDate","{0:MM/dd/yyyy}")%>','<%#DataBinder.Eval(Container,"DataItem.vOrder")%>','<%#DataBinder.Eval(Container,"DataItem.vsalesman")%>','<%#DataBinder.Eval(Container,"DataItem.vprevord")%>','<%#DataBinder.Eval(Container,"DataItem.vQuoteno")%>','<%#DataBinder.Eval(Container,"DataItem.vEstType")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vEstimate" HeaderText="Estimate" SortExpression="vEstimate" />
                <asp:BoundField DataField="vquoteno" HeaderText="Quote#" SortExpression="vquoteno" />
                <asp:BoundField DataField="vCustomer" HeaderText="Customer" SortExpression="vCustomer" />
                <asp:BoundField DataField="vshipname" HeaderText="Shipname" SortExpression="vshipname" />
                <asp:BoundField DataField="vFgNo" HeaderText="FG#" SortExpression="vFgNo" />
                <asp:BoundField DataField="vdscr" HeaderText="Description" SortExpression="vdscr" />
                <asp:BoundField DataField="vpartno" HeaderText="Customer Part#" SortExpression="vpartno" />
                <asp:BoundField DataField="vrfqno" HeaderText="Rfq#" SortExpression="vrfqno" />
                
                
            </Columns>
        </asp:GridView>
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SeQuoteLookup" TypeName="LookUp">
            <SelectParameters>
                                <asp:Parameter Name="prmAction" DefaultValue = "" Type="String" />
                                <asp:Parameter Name="prmUser" Type="String" />
                                <asp:Parameter Name="prmField" Type="String" />
                                <asp:Parameter Name="prmCondition" Type="String" />
                                <asp:Parameter Name="prmText" Type="String" />
                                <asp:QueryStringParameter QueryStringField="customer" Name="prmCust" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    
    
    <div>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        
       
    </div>
    </div>
    </form>
</body>
</html>

