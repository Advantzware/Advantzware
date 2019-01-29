<%@ Page Language="C#" MasterPageFile="MasterPageReceGds.master" Debug="true" AutoEventWireup="true" Inherits="recelist_rcpt" Title="Warehouse Transactions Receipts" Codebehind="recelist_rcpt.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1"  runat="server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
<script type="text/javascript" language="javascript">

//    window.onload = setfocus;
//    function setfocus() {
//        document.forms[0].ctl00_ContentPlaceHolder1_txt_seq.focus();
//    }
    function recdate() {
        window.scroll(10, 900);
    }
    function fglotfocus() {
        window.scroll(1000, 1400);
    }
    function tagtxtbox() {
        var tagv = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vTagTextBox");

        if (tagv.value == "") {
            alert("Loadtag# Cannot Be Blank");
            tagv.focus();
        }
    }
    function coltotqty() {
        var unit = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vCasesTextBox").value;
        var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vQtyCasTextBox").value;
        var parsial = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vPartialTextBox").value;
        var totqty = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vT_QtyTextBox");
        if (parsial == "")
            parsial = "0";
        totqty.value = (parseInt(unit * count)) + parseInt(parsial);
    }
    function binlook() {
        var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLocTextBox.value;
        var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustBinLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocBinTextBox.value = ReturnObj1;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocBinTextBox.focus();
    }

    function locationlook() {
        var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1, ReturnObj2) {
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocTextBox.value = ReturnObj1;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocTextBox.focus();
    }
function rfqslook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("rfqs_lookup.aspx?customer=" + custval, "RfqsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function rfqLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_rfq.focus();
}
function open_pst() {
    var NewWindow = window.open("finish_good_rep_popup.aspx", "FGpostReport", "width=700,height=600,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
var taglookgrid = "";
function fgtaglookg() {
    taglookgrid = "0";
    var NewWindow = window.open("fgtaglookup.aspx", "fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function fgtaglook() {
    taglookgrid = "1";
    var loc1 = document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vTagTextBox.value;
    var NewWindow = window.open("fgtaglookup.aspx?binloc=" + loc1 + "", "fgtagLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function fgtaglookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13) {
    if (taglookgrid == "1") {
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTagTextBox.value = ReturnObj1;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocTextBox.value = ReturnObj2;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLocBinTextBox.value = ReturnObj3;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_noTextBox.value = ReturnObj4;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vJob_no2TextBox.value = ReturnObj5;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.value = ReturnObj6;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemNameTextBox.value = ReturnObj7;

        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasesTextBox.value = ReturnObj8;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vQtyCasTextBox.value = ReturnObj9;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasUnitTextBox.value = ReturnObj10;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vStdCostTextBox.value = ReturnObj11;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCostUomTextBox.value = ReturnObj12;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vT_QtyTextBox.value = ReturnObj13;
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTagTextBox.onchange();
    }
    else {
        document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_txt_tag.focus();
    }

}

function ondatefocus() {
    
    var dt = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vDateTextBox");
    dt.focus();    
}

function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.focus();
}

function custpartlook() {
    var custval = document.forms[0].ctl00$ContentPlaceHolder1$txt_customer.value;
    var NewWindow = window.open("customerpart_lookup.aspx?customer=" + custval, "CustPartLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_custpart.focus();
}

function customerpolook() {

    var NewWindow = window.open("lpofglook.aspx", "CustomerpoWindow", "width=650,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function lpofglLookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_po.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_txt_po.focus();
}

function fglookgrid() {
    var cust = "";
        var NewWindow = window.open("fgitem2_lookup.aspx?customer=" + cust + "", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function FGLookup(ReturnObj1) {
        document.forms[0].ctl00$ContentPlaceHolder1$txt_ino.value = ReturnObj1;
        document.forms[0].ctl00$ContentPlaceHolder1$txt_ino.focus();
    }

    function fglook() {

        var NewWindow = window.open("lfglook.aspx", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function fglLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vItemTextBox.onchange();
    }


function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$txt_job.focus();
}

function ssfgscan(val) {
    var getvalue = document.getElementById("ctl00_ContentPlaceHolder1_getvalueTextBox");
    if (getvalue.value == "no" || getvalue.value == "NO") {
        if (val == "1") {           
        var btn = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_UpdateButton");
        btn.click();
        return false; 
        }
        if (val == "2") {
            var btn = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_InsertButton");
            btn.click();
            return false; 
        }
    }
}

function clickButton(e, ctl00$ContentPlaceHolder1$btnSearch){ 

      var evt = e ? e : window.event;

      var bt = document.getElementById(ctl00$ContentPlaceHolder1$btnSearch);

      if (bt){ 

          if (evt.keyCode == 13){ 

                bt.click(); 

                return false; 

          } 

      } 

}

</script>
<div >
<TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
               <td nowrap="nowrap">
                   Seq# <br>           
			<asp:TextBox ID="txt_seq" runat="server" Width="65px" ></asp:TextBox>
			<asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="txt_seq" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
			
          	   </td>
          	   <td nowrap id="customerid" runat="server">Tag#
                        <%--<asp:Label ID="customerlabel" runat="server" Text="Customer#"></asp:Label>--%>  <br>
            <asp:TextBox ID="txt_tag"  runat="server" MaxLength="20" Width="118px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="fgtaglookg(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	
           		     
            		<td nowrap>Receipt Date# <br>
            <asp:TextBox ID="txt_date" runat="server" Width="65px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"></asp:TextBox>
			<a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_txt_date.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_txt_date); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>         
            		<td nowrap> Po# <br>
            <asp:TextBox ID="txt_po" runat="server" MaxLength="9" Width="65px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td> 
                	
                	<td nowrap> Item No# <br>
            <asp:TextBox ID="txt_ino" runat="server" MaxLength="15" Width="95px" ></asp:TextBox>
			<a href="#" tabindex="1" onClick="fglookgrid(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
                	</td> 
                	<td nowrap> Job# <br>
            <asp:TextBox ID="txt_job" runat="server" MaxLength="6" Width="45px" ></asp:TextBox>            
			<a href="#" tabindex="1" onClick="job1look(); return false"><asp:Image ID="Job1Lookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                	</td>
                	               
               		
                  <td nowrap>Rows/Page<br>
                  
                      <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
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
                             
                              <asp:TextBox ID="aLineLabel" runat="server" MaxLength="4" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
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
                  
                </td>
			<td></td>
			<td></td>
            
                  </tr>
                  </table>
               </TD></TR>  
       </table>
</div>
<div>
    <br /><fieldset style="width:1400px;">
    <asp:Panel ID="Grid_panel" runat="server" Width="1400px"  ScrollBars="Vertical" Height="180px">
    <asp:GridView ID="GridView1" runat="server" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" AutoGenerateColumns="False" DataSourceID="ObjectDataSourcegrid" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" EmptyDataText="No Record Found" OnPageIndexChanging="GridView1_PageIndexChanging">
        <Columns>
            
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
            </asp:CommandField>
                
            
            <asp:BoundField DataField="vTag" HeaderText="Tag#"  FooterStyle-Wrap="false" ItemStyle-Wrap="false" SortExpression="vTag" HeaderStyle-Wrap="false" />
            <asp:BoundField DataField="vLoc" HeaderText="Whse" SortExpression="vLoc" ItemStyle-Wrap="false" />
            <asp:BoundField DataField="vLocBin" HeaderText="Bin" ItemStyle-Wrap="false" SortExpression="vLocBin" />
            <asp:BoundField DataField="vCases" HeaderText="Units"             SortExpression="vCases" />
            <asp:BoundField DataField="vQtyCas" HeaderText="Unit Count"   SortExpression="vQtyCas" />
            <asp:BoundField DataField="vCasUnit" HeaderText="Units/Skid" SortExpression="vCasUnit" />
            <asp:BoundField DataField="vPartial" HeaderText="Partial"   SortExpression="vPartial" />
            <asp:BoundField DataField="vT_Qty" HeaderText="Total!Qty" SortExpression="vT_Qty" />
            <asp:BoundField DataField="vStackCode" HeaderText="FG Lot#" SortExpression="vStackCode" />    
            <asp:BoundField DataField="vJob_no" ItemStyle-Wrap="false" HeaderText="Job#" SortExpression="vJob_no" />
            <asp:BoundField DataField="vJob_no2" HeaderText="" SortExpression="vJob_no2" />  
                       
            <asp:BoundField DataField="vPo_no" ItemStyle-Wrap="false" HeaderText="Po#" SortExpression="vPo_no" />
            
            <asp:BoundField DataField="vItem" ItemStyle-Wrap="false" HeaderText="Item#" SortExpression="vItem" />
            <asp:BoundField DataField="vItemName" ItemStyle-Wrap="false" HeaderText="Item Name" 
                SortExpression="vItemName" />
            
            
            <asp:BoundField DataField="vStdCost" HeaderText="Cost/Uom" 
                SortExpression="vStdCost" />
            <asp:BoundField DataField="vCostUom" HeaderText="Uom" 
                SortExpression="vCostUom" />
            
            <asp:BoundField DataField="vFrtCost" HeaderText="Costs" 
                SortExpression="vFrtCost" />
            <asp:BoundField DataField="vExtCost" HeaderText="Extended Cost"  SortExpression="vExtCost" /> 
            <asp:BoundField DataField="vRno" HeaderText="Seq#" SortExpression="vRno" />            
            <asp:BoundField DataField="vCreatedBy" HeaderText="Created By " 
                SortExpression="vCreatedBy" />
            <asp:BoundField DataField="vCreate2" HeaderText="Last Updated By" 
                SortExpression="vCreate2" />
                <asp:BoundField DataField="vDate" HeaderText="Receipt Date" SortExpression="vDate" />
            <asp:BoundField Visible="false" DataField="vTransTime" HeaderText="Receipt Time" 
                SortExpression="vTransTime" />
            
            <asp:BoundField Visible="false" DataField="vTot_Wt" HeaderText="Total Wheight" 
                SortExpression="vTot_Wt" />
            <asp:BoundField Visible="false" DataField="vRecKey" HeaderText="vRecKey" 
                SortExpression="vRecKey" />
           </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" Width="100%" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <RowStyle CssClass="shade" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
    </asp:GridView>
    </asp:Panel></fieldset>
    <asp:ObjectDataSource ID="ObjectDataSourcegrid" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRece" TypeName="itemhistory">
        <SelectParameters>            
            <asp:SessionParameter Name="prmUser" SessionField="prmUser" Type="String" />            
            <asp:SessionParameter Name="prmAction" DefaultValue="GridSelect" SessionField="prmAction_rcpt_gd" Type="String"  />
            <asp:SessionParameter Name="prmFgItem" SessionField="prmIno_rcpt_gd" Type="String" />
            <asp:SessionParameter Name="prmJobno" SessionField="prmJob_rcpt_gd" Type="String" />
            <asp:SessionParameter Name="prmPono" SessionField="prmPo_rcpt_gd" Type="String" />
            <asp:SessionParameter Name="prmSeqno" SessionField="prmSeq_rcpt_gd" Type="String" />
            <asp:SessionParameter Name="prmRcptDate" SessionField="prmDate_rcpt_gd" Type="String" />
            <asp:SessionParameter Name="prmTagno" SessionField="prmTag_rcpt_gd" Type="String" />
            <asp:Parameter Name="prmTransTime" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmJob_no2" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmLocBin" Type="String" />
            <asp:Parameter Name="prmCases" Type="String" />
            <asp:Parameter Name="prmQty_Cas" Type="String" />
            <asp:Parameter Name="prmCasUnit" Type="String" />
            <asp:Parameter Name="prmPartial" Type="String" />
            <asp:Parameter Name="prmStdCost" Type="String" />
            <asp:Parameter Name="prmCost_Uom" Type="String" />
            <asp:Parameter Name="prmTQty" Type="String" />
            <asp:Parameter Name="prmFrtCost" Type="String" />
            <asp:Parameter Name="prmExtCost" Type="String" />
            <asp:Parameter Name="prmStackCode" Type="String" />
            <asp:Parameter Name="prmCreatedBy" Type="String" />
            <asp:Parameter Name="prmCreate2" Type="String" />
            <asp:Parameter Name="prmTotWt" Type="String" />
            <asp:Parameter Name="prmRecKey"  Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>

    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload" DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
        <asp:Panel ID="Edit_panel" runat="server" DefaultButton="UpdateButton">
        <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true" Width="130px" runat="server" Text='<%# Bind("vTag") %>' />
             <a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" onkeyup="tagtxtbox();ssfgscan(1);" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
             <a href="#" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" onfocus="ssfgscan(1)" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Width="45px" onfocus="ssfgscan(1)" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>            
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Width="65px" onfocus="ssfgscan(1)" onkeyup="coltotqty()"  runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units/Skid: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" onfocus="ssfgscan(1)" Width="75px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" onfocus="ssfgscan(1)" onkeyup="coltotqty()" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Lot#: </b><br>
            <asp:TextBox ID="vStackCodeTextBox" onblur="fglotfocus()" Width="65px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            <br />
            </td>            
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" onfocus="ondatefocus()" ForeColor="#848284" Width="40px" runat="server" Text='<%# Bind("vJob_no") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
                       
            </br></td>                    
            <td nowrap align="left" style="padding-right:5px;"><b>   
            Po#: </b><br>
            <asp:TextBox ID="vPo_noTextBox" MaxLength="9" Width="58px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" Text='<%# Bind("vPo_no") %>' />
                      
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" Width="95px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" Text='<%# Bind("vItem") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item Name: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" Width="110px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>           
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost/UOM: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vStdCost") %>' />                
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" onfocus="ondatefocus()" ForeColor="#848284" Width="20px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Costs: </b><br>
            <asp:TextBox ID="vFrtCostTextBox"  Width="65px" runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vFrtCost") %>' />
                
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Extended Cost: </b><br>
            <asp:TextBox ID="vExtCostTextBox" Enabled="false" Width="77px" runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vExtCost") %>' />
                <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>  
            <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoLabel" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
            </td>          
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Receipt Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );recdate()" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_FormView1_vDateTextBox.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
            
           
            </tr>
            
            <tr>
            <td colspan="2">
            <br />
            <asp:Button ID="UpdateButton" CssClass="buttonM" OnClick="UpdateButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
            </tr>
        </table>
        </fieldset>
        </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
         <asp:Panel ID="Insert_panel" runat="server" DefaultButton="InsertButton">
            <br />
        <fieldset style="background-color:#EFF3FB">
        <table class="shade">
        <tr>
       
        <td nowrap align="left" style="padding-right:5px;"><b> 
        Tag#: </b><br>
            <asp:TextBox ID="vTagTextBox" MaxLength="20" OnTextChanged="Tagtextbox_Click" AutoPostBack="true" Width="130px" runat="server" Text='<%# Bind("vTag") %>' />
            <a href="#" tabindex="1" onClick="fgtaglook(); return false"><asp:Image ID="fgtagimage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td> 
        <td nowrap align="left" style="padding-right:5px;"><b> 
            Whse: </b><br>
            <asp:TextBox ID="vLocTextBox" onkeyup="tagtxtbox();ssfgscan(2);" MaxLength="5" Width="35px" runat="server" Text='<%# Bind("vLoc") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="locationlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Bin: </b><br>
            <asp:TextBox ID="vLocBinTextBox" MaxLength="8" Width="55px" onfocus="ssfgscan(2)" runat="server" Text='<%# Bind("vLocBin") %>' />
            <a href="#" tabindex="1" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
            </td>
             <td nowrap align="left" style="padding-right:5px;"><b> 
            Units: </b><br>
            <asp:TextBox ID="vCasesTextBox" MaxLength="6" Width="45px" onfocus="ssfgscan(2)" onkeyup="coltotqty()" runat="server" Text='<%# Bind("vCases") %>' />
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCasesTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units Count: </b><br>
            <asp:TextBox ID="vQtyCasTextBox" MaxLength="6" Width="65px" onkeyup="coltotqty()" onfocus="ssfgscan(2)"  runat="server" Text='<%# Bind("vQtyCas") %>' />
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vQtyCasTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Units/Skid: </b><br>
            <asp:TextBox ID="vCasUnitTextBox" MaxLength="3" Width="75px" runat="server" onfocus="ssfgscan(2)"
                Text='<%# Bind("vCasUnit") %>' />
            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vCasUnitTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Partial: </b><br>
            <asp:TextBox ID="vPartialTextBox" MaxLength="6" onkeyup="coltotqty()" onfocus="ssfgscan(2)" Width="45px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vPartialTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
             <td nowrap align="left" style="padding-right:5px;"><b> 
            Total Qty: </b><br>
            <asp:TextBox ID="vT_QtyTextBox" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vT_Qty") %>' />
            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vT_QtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            FG Lot#: </b><br>
            <asp:TextBox ID="vStackCodeTextBox" onblur="fglotfocus()" Width="65px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b>
            Job#: </b><br>
            <asp:TextBox ID="vJob_noTextBox" MaxLength="6" Width="40px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" Text='<%# Bind("vJob_no") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;">
             <br>
            <asp:TextBox ID="vJob_no2TextBox" MaxLength="2" Width="15px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />            
            </br></td>           
                 
         <td nowrap align="left" style="padding-right:5px;"><b>   
            Po#: </b><br>
            <asp:TextBox ID="vPo_noTextBox" MaxLength="9" Width="58px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" Text='<%# Bind("vPo_no") %>' />
            
            
            <br />
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item: </b><br>
            <asp:TextBox ID="vItemTextBox" MaxLength="15" Width="95px" onfocus="ondatefocus()" ForeColor="#848284" runat="server" Text='<%# Bind("vItem") %>' />
            
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Item Name: </b><br>
            <asp:TextBox ID="vItemNameTextBox" MaxLength="30" onfocus="ondatefocus()" ForeColor="#848284" Width="110px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            <br />
            </td>           
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Cost/UOM: </b><br>
            <asp:TextBox ID="vStdCostTextBox"  Width="65px" runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vStdCost") %>' />
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vStdCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            UOM: </b><br>
            <asp:TextBox ID="vCostUomTextBox"  MaxLength="3" Width="20px" runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vCostUom") %>' />
            <br />
            </td>
           
            <td nowrap align="left" style="padding-right:5px;"><b> 
           Costs: </b><br>
            <asp:TextBox ID="vFrtCostTextBox" Width="65px"  runat="server" onfocus="ondatefocus()" ForeColor="#848284"
                Text='<%# Bind("vFrtCost") %>' />
            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vFrtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Extended Cost: </b><br>
            <asp:TextBox ID="vExtCostTextBox" Enabled="false" Width="77px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vExtCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>    
            <br />
            </td>
             <td nowrap align="left" style="padding-right:5px;"><b>
            Seq#: </b><br>
            <asp:Label ID="vRnoTextBox" Width="55px" runat="server" Text='<%# Bind("vRno") %>' />
            <br />                        
            </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Created By: </b><br>
            <asp:Label ID="vCreatedByLabel" Height="17px" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Last Update By: </b><br>
            <asp:Label ID="vCreate2Label" Width="78px" Height="17px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
            <br />
            </td>
            <td nowrap align="left" style="padding-right:5px;"><b> 
            Receipt Date: </b><br>
            <asp:TextBox ID="vDateTextBox" Width="65px" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vDate") %>' />
            <a href="#" tabindex="1" onblur="ctl00_ContentPlaceHolder1_FormView1_vDateTextBox.focus()" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView1_vDateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <br />
        </td>
        <td nowrap align="left" style="padding-right:5px;"><b> 
        </b><br>
            <asp:Label ID="vTransTimeLabel" Visible="false" Width="70px" Height="17px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vTransTime") %>' />
                <br />
        </td>
            
            <td nowrap align="left" style="padding-right:5px;"><b> 
            </b><br>
            <asp:TextBox ID="vTot_WtTextBox" Visible="false" Width="65px" Enabled="false" runat="server" Text='<%# Bind("vTot_Wt") %>' />
            <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vTot_WtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Number Only"></asp:CompareValidator>
            <br />
            </td>
            <%--<td><b> 
            vRecKey: <br>
            <asp:TextBox ID="vRecKeyTextBox" Visible="false" Width="65px" runat="server" Text='<%# Bind("vRecKey") %>' />
            <br />
            </td>--%>
            </tr>
            <tr>
            <td colspan="4">
            <br />
            <asp:Button ID="InsertButton" CssClass="buttonM" OnClick="InsertButton_Click" runat="server" CausesValidation="True" 
                CommandName="Save" Text="Save" />
            &nbsp;<asp:Button ID="InsertCancelButton" CssClass="buttonM" OnClick="InserCancelButton_Click" runat="server" 
                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                </td>
             </tr>
             </table>   
             </fieldset>
             </asp:Panel>
        </InsertItemTemplate>
        
        
        <ItemTemplate>
           
        <asp:Label ID="vRnoLabel" Visible="false" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vRno") %>' />
       
            <asp:Label ID="vTagLabel" Visible="false" MaxLength="20" Width="130px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTag") %>' />
          
            <asp:Label ID="vLocLabel" Visible="false" MaxLength="5" Width="35px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLoc") %>' />
            
            <asp:Label ID="vLocBinLabel" Visible="false" MaxLength="8" Width="55px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vLocBin") %>' />
          
            <asp:Label ID="vJob_noLabel" Visible="false" MaxLength="6" Width="40px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vJob_no") %>' />
            
            <asp:Label ID="vJob_no2Label" Visible="false" MaxLength="2" Width="15px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vJob_no2") %>' />
            
            
            <asp:Label ID="vDateLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vDate") %>' />
           
            <asp:Label ID="vPo_noLabel" Visible="false" MaxLength="9" Width="58px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vPo_no") %>' />
            
            <asp:Label ID="vItemLabel" Visible="false" MaxLength="15" Width="95px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vItem") %>' />
           
            <asp:Label ID="vItemNameLabel" Visible="false" MaxLength="30" Width="140px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vItemName") %>' />
            
            <asp:Label ID="vCasesLabel" Visible="false" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCases") %>' />
            
            <asp:Label ID="vQtyCasLabel" Visible="false" MaxLength="6" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vQtyCas") %>' />
            
            <asp:Label ID="vCasUnitLabel" Visible="false" MaxLength="3" Width="75px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCasUnit") %>' />
            
            <asp:Label ID="vPartialLabel" Visible="false" MaxLength="6" Width="45px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vPartial") %>' />
            
            <asp:Label ID="vStdCostLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vStdCost") %>' />
            
            <asp:Label ID="vCostUomLabel" Visible="false" MaxLength="3" Width="20px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCostUom") %>' />
            
            <asp:Label ID="vT_QtyLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vT_Qty") %>' />
            
            <asp:Label ID="vFrtCostLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vFrtCost") %>' />
            
            <asp:Label ID="vExtCostLabel" Visible="false" Width="77px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vExtCost") %>' />
            
            <asp:Label ID="vCreatedByLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreatedBy") %>' />
            
            <asp:Label ID="vCreate2Label" Visible="false" Width="78px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vCreate2") %>' />
           
            <asp:Label ID="vStackCodeLabel" Visible="false" Width="65px" BackColor="PaleTurquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" 
                Text='<%# Bind("vStackCode") %>' />
            
    
    <br />
        <asp:Button ID="addButton" runat="server" CommandName="new" Width="60px" CssClass="button" Text="Add"></asp:Button>&nbsp;&nbsp;&nbsp;
        <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" Width="60px" CssClass="buttonM"  Text="Update" />&nbsp;&nbsp;&nbsp;
        <%--<asp:Button ID="copybutton" runat="server" CommandName="Edit" OnClick="CopyButton_click" CssClass="buttonM" Text="Copy"  OnClick="DeleteButton_Click" />--%>
        <asp:Button ID="deleteButton" runat="server" CssClass="button" Width="60px" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>&nbsp;&nbsp;&nbsp;
        <input type="button" id="Button1" value="Post" runat="server"  style="width:60px;" class="buttonM" onClick="open_pst()" />
        
        
    
  
        </ItemTemplate>
    </asp:FormView>
    <div style="display:none;">
    <asp:TextBox ID="getvalueTextBox" runat="server"></asp:TextBox>
    </div>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRece" TypeName="itemhistory">
        <SelectParameters>            
            <asp:Parameter Name="prmUser"  Type="String" />            
            <asp:Parameter Name="prmAction" DefaultValue="Select"  Type="String"  />
            <asp:Parameter Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmJobno"  Type="String" />
            <asp:Parameter Name="prmPono"  Type="String" />
            <asp:SessionParameter Name="prmSeqno" SessionField="seqno_list" Type="String" />
            <asp:Parameter Name="prmRcptDate"  Type="String" />
            <asp:Parameter Name="prmTagno"  Type="String" />
            <asp:Parameter Name="prmTransTime" Type="String"></asp:Parameter>
            <asp:Parameter Name="prmJob_no2" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmLoc" Type="String" />
            <asp:Parameter Name="prmLocBin" Type="String" />
            <asp:Parameter Name="prmCases" Type="String" />
            <asp:Parameter Name="prmQty_Cas" Type="String" />
            <asp:Parameter Name="prmCasUnit" Type="String" />
            <asp:Parameter Name="prmPartial" Type="String" />
            <asp:Parameter Name="prmStdCost" Type="String" />
            <asp:Parameter Name="prmCost_Uom" Type="String" />
            <asp:Parameter Name="prmTQty" Type="String" />
            <asp:Parameter Name="prmFrtCost" Type="String" />
            <asp:Parameter Name="prmExtCost" Type="String" />
            <asp:Parameter Name="prmStackCode" Type="String" />
            <asp:Parameter Name="prmCreatedBy" Type="String" />
            <asp:Parameter Name="prmCreate2" Type="String" />
            <asp:Parameter Name="prmTotWt" Type="String" />
            <asp:Parameter Name="prmRecKey" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>

</asp:Content>
