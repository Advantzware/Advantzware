<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="true" AutoEventWireup="true" Inherits="fold_layout" Title="Fold Layout" Codebehind="fold_layout.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <asp:ScriptManager ID="ScriptManager1" runat="server">
    </asp:ScriptManager>
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd(vmessage) {
        var retVal = makeMsgBox("Confirmation", vmessage, 48, 4, 256, 4096);
        if (retVal == 6) {
            var NewWindow = window.open("corr_vendor_cost.aspx", "VendorCost", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

    }
</script>

<script>

window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachineTextBox"))
    {
        var machine = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachineTextBox");
        machine.focus();
    }
}

    function Leaflook1(){ 
    var typelook = "F,W";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vEstimateTextBox").innerHTML;
    var NewWindow = window.open("leaf_lookup.aspx?look1="+typelook+"&leaftype="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup1(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf1TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc1TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafS1TextBox").innerText = "1";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf1TextBox.focus();
}

function Leaflook2(){
    var typelook = "F,W";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vEstimateTextBox").innerHTML;
    var NewWindow = window.open("leaf_lookup2.aspx?look2="+typelook+"&leaf2type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup2(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf2TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc2TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafS2TextBox").innerText = "1";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf2TextBox.focus();
}

function Leaflook3(){ 
    var typelook = "F,W";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vEstimateTextBox").innerHTML;
    var NewWindow = window.open("leaf_lookup3.aspx?look3="+typelook+"&leaf3type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup3(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc3TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafS3TextBox").innerText = "1";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf3TextBox.focus();
}

function Leaflook4(){ 
    var typelook = "F,W";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vEstimateTextBox").innerHTML;
    var NewWindow = window.open("leaf_lookup4.aspx?look4="+typelook+"&leaf4type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup4(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf4TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc4TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafS4TextBox").innerText = "1";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf4TextBox.focus();
}

function Machinelook()
{
    var NewWindow = window.open("machine_lookup.aspx","MachineLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function MachineLookup(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachineTextBox.value = ReturnObj1;
  var macdesc=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachDscrTextBox");
  macdesc.innerText = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachineTextBox").onchange();
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachineTextBox.focus();
}

function Boardlook1() {
    var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vStyleLabel").innerHTML;    
       
  var NewWindow = window.open("corboard_lookup.aspx?style="+style1+"","BoardWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorBoardLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
    ReturnObj1 = ReturnObj1.replace(":", "\"");
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vBoardTextBox.value = ReturnObj1;
  //document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vBoardNameTextBox.value = ReturnObj2;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetWidTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetLenTextBox.value = ReturnObj4;
  var desc=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vBoardNameTextBox");
  desc.innerText=ReturnObj2;
    var real=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vRealTextBox");
    //var test=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFluteLabel");
    var flute=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vCaliperTextBox");  
  real.innerText=ReturnObj5;
  //test.innerText=ReturnObj6;
  flute.innerText = ReturnObj7;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vBoardTextBox").onchange();
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_FoldLayout_vBoardTextBox.focus();  
}


function frontback()
{
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox").value = frontback;
    }
}
function sideside()
{
    var side=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox").value;
    if (side.indexOf(".") != -1) {
        return;
    }
    else if (side.length > 2 && side.length < 4) {
        side = side + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox").value = side;
    }
}
function costmsf()
{
   var cost=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vCostMsfTextBox").value;
   if (cost.indexOf(".") != -1) {
       return;
   }
   else if (cost.length > 4 && cost.length < 6) {
       cost = cost + ".";
       document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vCostMsfTextBox").value = cost;
   } 
}
function freightcwt()
{
    var freight=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFreightCwtTextBox").value;
    if (freight.indexOf(".") != -1) {
        return;
    }
    else if (freight.length > 2 && freight.length < 4) {
        freight = freight + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFreightCwtTextBox").value = freight;
    }
}
function grosswid()
{
    var wid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetWidTextBox").value;
    if (wid.indexOf(".") != -1) {
        return;
    }
    else if (wid.length > 2 && wid.length < 4) {
        wid = wid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetWidTextBox").value = wid;
    }
}
function grosslen()
{
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetLenTextBox").value;
    if (len.indexOf(".") != -1) {
        return;
    }
    else if (len.length > 2 && len.length < 4) {
        len = len + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetLenTextBox").value = len;
    }
}
function machwid()
{
    var nwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachFeedWidTextBox").value;
    if (nwid.indexOf(".") != -1) {
        return;
    }
    else if (nwid.length > 2 && nwid.length < 4) {
        nwid = nwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachFeedWidTextBox").value = nwid;
    }
}
function machlen()
{
    var nlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachFeedLenTextBox").value;
    if (nlen.indexOf(".") != -1) {
        return;
    }
    else if (nlen.length > 2 && nlen.length < 4) {
        nlen = nlen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vMachFeedLenTextBox").value = nlen;
    }
}

function diewid()
{
    var dwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vDieSizeWidTextBox").value;
    if (dwid.indexOf(".") != -1) {
        return;
    }
    else if (dwid.length > 2 && dwid.length < 4) {
        dwid = dwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vDieSizeWidTextBox").value = dwid;
    }
}
function dielen()
{
    var dlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vDieSizeLenTextBox").value;
    if (dlen.indexOf(".") != -1) {
        return;
    }
    else if (dlen.length > 2 && dlen.length < 4) {
        dlen = dlen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vDieSizeLenTextBox").value = dlen;
    }
}

function leafwid1()
{
    var lwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid1TextBox").value;
    if (lwid.indexOf(".") != -1) {
        return;
    }
    else if (lwid.length > 1 && lwid.length < 3) {
        lwid = lwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid1TextBox").value = lwid;
    }
}
function leafwid2()
{
    var lwid2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid2TextBox").value;
    if (lwid2.indexOf(".") != -1) {
        return;
    }
    else if (lwid2.length > 1 && lwid2.length < 3) {
        lwid2 = lwid2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid2TextBox").value = lwid2;
    }
}
function leafwid3()
{
    var lwid3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid3TextBox").value;
    if (lwid3.indexOf(".") != -1) {
        return;
    }
    else if (lwid3.length > 1 && lwid3.length < 3) {
        lwid3 = lwid3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid3TextBox").value = lwid3;
    }
}
function leafwid4()
{
    var lwid4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid4TextBox").value;
    if (lwid4.indexOf(".") != -1) {
        return;
    }
    else if (lwid4.length > 1 && lwid4.length < 3) {
        lwid4 = lwid4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafWid4TextBox").value = lwid4;
    }
}
function leaflen1()
{
    var llen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen1TextBox").value;
    if (llen.indexOf(".") != -1) {
        return;
    }
    else if (llen.length > 1 && llen.length < 3) {
        llen = llen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen1TextBox").value = llen;
    }
}
function leaflen2()
{
    var llen2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen2TextBox").value;
    if (llen2.indexOf(".") != -1) {
        return;
    }
    else if (llen2.length > 1 && llen2.length < 3) {
        llen2 = llen2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen2TextBox").value = llen2;
    }
}
function leaflen3()
{
    var llen3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen3TextBox").value;
    if (llen3.indexOf(".") != -1) {
        return;
    }
    else if (llen3.length > 1 && llen3.length < 3) {
        llen3 = llen3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen3TextBox").value = llen3;
    }
}
function leaflen4()
{
    var llen4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen4TextBox").value;
    if (llen4.indexOf(".") != -1) {
        return;
    }
    else if (llen4.length > 1 && llen4.length < 3) {
        llen4 = llen4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafLen4TextBox").value = llen4;
    }
}
function chk_roll_click()
{
    var roll=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_chk_roll");
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox").value;
    var side=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox").value;
    var rollwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vRollWidTextBox");
    var gwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vGrosShetWidTextBox").value;
    if(roll.checked)
        {
            rollwid.innerText = gwid;
            var swap=frontback;
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox").value=side;
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox").value=swap;
        }
    else
    {
        rollwid.innerText = "0.0000";
        var swap2=frontback;
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vFrontBackTextBox").value=side;
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vSideSideTextBox").value=swap2;
    }     
}
function leafblur1()
{
    var leaf1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf1TextBox").value;       
       
    if(leaf1=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc1TextBox").value="";           
        }        
}
function leafblur2()
{
    var leaf2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf2TextBox").value;       
       
    if(leaf2=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc2TextBox").value="";           
        }        
}
function leafblur3()
{
    var leaf3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf3TextBox").value;       
       
    if(leaf3=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc3TextBox").value="";           
        }        
}
function leafblur4()
{
    var leaf4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeaf4TextBox").value;       
       
    if(leaf4=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vLeafDesc4TextBox").value="";           
        }       
}
function setValue() {
    var onlenval = 1;
    var onwidval = 1;
    var dieinval = 1; 
    var numupval = 1;
   
    if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vOnLenTextBox.value) > 0)
        onlenval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vOnLenTextBox.value);
    

    if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vOnWidTextBox.value) > 0)
        onwidval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vOnWidTextBox.value);

    if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vDieInchesTextBox.value) > 0)
        dieinval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vDieInchesTextBox.value);

    if (parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vOnTotalUpTextBox").innerHTML) > 0)
        numupval = parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vOnTotalUpTextBox").innerHTML);


    document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vDieInchesTextBox.value = dieinval / numupval;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vOnTotalUpTextBox").innerHTML = onlenval * onwidval;
    document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vDieInchesTextBox.value = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_FoldLayout$vDieInchesTextBox.value) * parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vOnTotalUpTextBox").innerHTML);

}
function open_bom()
{
    var NewWindow = window.open("fold_bom.aspx","FoldBomWindow","width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function sheet_cal() {
    var sheetcal = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_sheetLabel").innerHTML;
    if (sheetcal == "sheet") {
      var NewWindow = window.open("sheet_cal_parm.aspx", "SheetLookupWindow", "width=400,height=350,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
}

function jobbuttonconfirm() {
    var order = document.getElementById("ctl00_ContentPlaceHolder1_FormView_FoldLayout_vOrderLabel");

    if (parseInt(order.innerHTML) > 0) {
        if (confirm("Recalculate Job Standards for job# " + order.innerHTML)) {
            return true;
        }
        else {
            return false;
        }
    }
    else {
        alert("Job Standards are not available");
        return false;
    }

}

</script>
    <asp:HiddenField ID="HiddenField1" runat="server" />
    
    <asp:FormView ID="FormView_FoldLayout" runat="server" OnDataBound="FormView_FoldLayout_DataBound" DataSourceID="FoldLayout_ObjectDataSource">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
       

        <fieldset class="shade">
            <legend>Reference Information</legend>
                <table>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Estimate#:</b></td>
                        <td nowrap><b><asp:Label ID="vEstimateTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstimate") %>'> </asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Est Date:</b></td>
                        <td nowrap><b><asp:Label ID="vEstDateTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'> </asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Form:</b></td>
                        <td nowrap><b>
                        <asp:Label ID="vFormTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vForm") %>'> </asp:Label>
                        of
                        <asp:Label ID="vFormQtyTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vFormQty") %>'> </asp:Label>
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Cust Part:</b></td>
                        <td nowrap><b> <asp:Label ID="vCustPartTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vCustPart") %>'> </asp:Label></b></td>
                    </tr>
                </table>
        </fieldset>
            <asp:UpdatePanel ID="UpdatePanel1" runat="server">
             <ContentTemplate>
              <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="UpdatePanel1"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div>
        <fieldset class="shade">
            <table>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Machine:</b></td>
                    <td nowrap><b>
                        <asp:TextBox ID="vMachineTextBox" runat="server" OnTextChanged="machine_text_change" onblur="sheet_cal()"  AutoPostBack="true" Text='<%# Bind("vMachine") %>'> </asp:TextBox>
                        <a href="#" tabindex="1" onclick="Machinelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        <asp:Label ID="vMachDscrTextBox" runat="server" Text='<%# Bind("vMachDscr") %>'> </asp:Label>
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Front-Back:</b></td>
                    <td nowrap><b><asp:TextBox ID="vFrontBackTextBox" onkeyup="frontback()" MaxLength="8" runat="server" Width="60px" Text='<%# Bind("vFrontBack","{0:##0.0000}") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator27" runat="server" ControlToValidate="vFrontBackTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Side-Side:</b></td>
                    <td nowrap><b><asp:TextBox ID="vSideSideTextBox" onkeyup="sideside()" MaxLength="8" Width="60px" runat="server" Text='<%# Bind("vSideSide","{0:##0.0000}") %>'> </asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vSideSideTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Xgrain:</b></td>
                    <td nowrap><b>
                    
                        <asp:DropDownList ID="vXgrainDropDown" Width="40px" runat="server" AutoPostBack="true" OnSelectedIndexChanged="xgrain_text_change" SelectedValue='<%# Bind("vXgrain") %>'>
                                        <asp:ListItem Value=""></asp:ListItem>
                                        <asp:ListItem Value="N">N</asp:ListItem>
                                        <asp:ListItem Value="B">B</asp:ListItem>
                                        <asp:ListItem Value="S">S</asp:ListItem>
                                </asp:DropDownList>
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                    <td nowrap><b></b></td>
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Board:</b></td>
                    <td nowrap><b>
                        <asp:TextBox ID="vBoardTextBox" runat="server" AutoPostBack="true" OnTextChanged="board_text_change" Text='<%# Bind("vBoard") %>'> </asp:TextBox>
                        <a href="#" tabindex="1" onclick="Boardlook1(); return false"><asp:Image ID="img_board" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        <asp:Label ID="vBoardNameTextBox" runat="server" Text='<%# Bind("vBoardName") %>'></asp:Label>
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>&nbsp;</b></td>
                    <td nowrap><b>&nbsp;</b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Real:</b></td>
                    <td nowrap><b><asp:Label ID="vRealTextBox" runat="server" Text='<%# Bind("vReal") %>'></asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                    <td nowrap><b></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                    <td nowrap><b></b></td>
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Caliper:</b></td>
                    <td nowrap><b><asp:Label ID="vCaliperTextBox" Width="60px" runat="server" Text='<%# Bind("vCaliper","{0:##0.00000}") %>'> </asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Cost:</b></td>
                    <td nowrap><b>
                       <asp:TextBox ID="vCostUomTextBox" runat="server" Width="60px" Text='<%# Bind("vCostUom") %>'> </asp:TextBox> 
                       <asp:TextBox ID="vCostMsfTextBox" Width="60px" MaxLength="9" onkeyup="costmsf()" runat="server" Text='<%# Bind("vCostMsf","{0:####0.000}") %>'></asp:TextBox>
                       <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vCostMsfTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Wt:</b></td>
                    <td nowrap><b><asp:Label ID="vWeightTextBox" Width="60px" runat="server" Text='<%# Bind("vWeight","{0:##0.00}") %>'> </asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Freight:</b></td>
                    <td nowrap><b>                        
                        <asp:TextBox ID="vFreightUomTextBox" Width="60px" runat="server" Text='<%# Bind("vFreightUom") %>'></asp:TextBox>
                        <asp:TextBox ID="vFreightCwtTextBox" Width="60px" MaxLength="7" onkeyup="freightcwt()" runat="server" Text='<%# Bind("vFreightCwt","{0:##0.000}") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vFreightCwtTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Nc:</b></td>
                    <td nowrap><b>
                    
                        <asp:DropDownList ID="ddl_nc" Width="40px" runat="server" SelectedValue='<%# Bind("vNc") %>'>                                        
                                        <asp:ListItem>N</asp:ListItem>
                                        <asp:ListItem>C</asp:ListItem>                                        
                                </asp:DropDownList>
                    </b></td>
                </tr>
            </table>
            
            <fieldset>
                <table>
                    <tr>
                        <td nowrap>&nbsp;</td>
                        <td nowrap><b>Width</b></td>
                        <td nowrap><b>Length</b></td>
                        <td nowrap><b>SqInches</b></td>
                        <td nowrap><b>&nbsp;</b></td>
                        <td nowrap><b>&nbsp;</b></td>
                        <td nowrap><b>&nbsp;</b></td>
                        <td nowrap><b>&nbsp;</b></td>
                        <td nowrap><b>Total Up</b></td>
                        <td nowrap><b>Die Inches</b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Roll:
                         <asp:TextBox ID="vRollTextBox" Visible="false" Width="30px" runat="server" Text='<%# Bind("vRoll") %>'> </asp:TextBox> 
                         <asp:CheckBox ID="chk_roll" onclick="chk_roll_click()" runat="server" />
                         </b></td>
                        <td nowrap><b><asp:Label ID="vRollWidTextBox" runat="server" Width="60px" Text='<%# Bind("vRollWid","{0:##0.0000}") %>'></asp:Label></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Gross Sheet:</b></td>
                        <td nowrap><b><asp:TextBox ID="vGrosShetWidTextBox" onkeyup="grosswid()" MaxLength="8" Width="60px" runat="server" Text='<%# Bind("vGrosShetWid","{0:##0.0000}") %>'> </asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vGrosShetWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vGrosShetLenTextBox" onkeyup="grosslen()" MaxLength="8" Width="60px" runat="server" Text='<%# Bind("vGrosShetLen","{0:##0.0000}") %>'> </asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vGrosShetLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                        </b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>#Out:</b></td>
                        <td nowrap><b><asp:TextBox ID="vOutWidTextBox" MaxLength="3" Width="60px" AutoPostBack="true" OnTextChanged="outwid_text_change" runat="server" Text='<%# Bind("vOutWid") %>'> </asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vOutWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Cuts:</b></td>
                        <td nowrap><b><asp:TextBox ID="vOutCutTextBox" MaxLength="6" Width="60px" runat="server" Text='<%# Bind("vOutCut") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vOutCutTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b></b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Mach Feed:</b></td>
                        <td nowrap><b><asp:TextBox ID="vMachFeedWidTextBox" onkeyup="machwid()" MaxLength="8" Width="60px" runat="server" Text='<%# Bind("vMachFeedWid","{0:##0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vMachFeedWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vMachFeedLenTextBox" MaxLength="8" onkeyup="machlen()" Width="60px" runat="server" Text='<%# Bind("vMachFeedLen","{0:##0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vMachFeedLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>#Out:</b></td>
                        <td nowrap><b><asp:TextBox ID="vOutLenTextBox" MaxLength="3" Width="60px" runat="server" Text='<%# Bind("vOutLen") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vOutLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Die Size:</b></td>
                        <td nowrap><b><asp:TextBox ID="vDieSizeWidTextBox" MaxLength="8" onkeyup="diewid()" Width="60px" runat="server" Text='<%# Bind("vDieSizeWid","{0:##0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vDieSizeWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vDieSizeLenTextBox" MaxLength="8" onkeyup="dielen()" Width="60px" runat="server" Text='<%# Bind("vDieSizeLen","{0:##0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vDieSizeLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>#On:</b></td>
                        <td nowrap><b><asp:TextBox ID="vOnWidTextBox" AutoPostBack="true" OnTextChanged="onwid_text_change" onkeyup="setValue()" MaxLength="3" Width="60px" runat="server" Text='<%# Bind("vOnWid") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vOnWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vOnLenTextBox" MaxLength="2" AutoPostBack="true" OnTextChanged="onwid_text_change" onkeyup="setValue()" Width="60px" runat="server" Text='<%# Bind("vOnLen") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vOnLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b><asp:Label ID="vOnTotalUpTextBox" Width="60px" runat="server"  Text='<%# Bind("vOnTotalUp") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vDieInchesTextBox" MaxLength="5" Width="60px" runat="server" Text='<%# Bind("vDieInches") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator28" runat="server" ControlToValidate="vDieInchesTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Integer value"></asp:CompareValidator>    
                        </b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Blank:</b></td>
                        <td nowrap><b><asp:Label ID="vBlankWidTextBox" Width="60px" runat="server" Text='<%# Bind("vBlankWid","{0:##0.000}") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:Label ID="vBlankLenTextBox" Width="60px" runat="server" Text='<%# Bind("vBlankLen","{0:##0.000}") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:Label ID="vBlankSqInchTextBox" Width="60px" runat="server" Text='<%# Bind("vBlankSqInch","{0:##0.00000}") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td nowrap><b></b></td>
                        <td nowrap><b></b></td>
                    </tr>
                </table>
            </fieldset>
            <fieldset>
                <table>
                    <tr>
                        <td nowrap><b>Leaf/Film</b></td>
                        <td nowrap><b>Description</b></td>
                        <td nowrap style="width:60px;" align="center"><b>S</b></td>
                        <td nowrap><b>B</b></td>
                        <td nowrap><b>Wid</b></td>
                        <td nowrap><b>Len</b></td>
                    </tr>
                    <tr>
                        <td nowrap><b><asp:TextBox ID="vLeaf1TextBox" onblur="leafblur1()" runat="server" Text='<%# Bind("vLeaf1") %>'></asp:TextBox>
                            <a href="#" tabindex="1" onclick="Leaflook1(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafDesc1TextBox" runat="server" Text='<%# Bind("vLeafDesc1") %>'></asp:TextBox></b></td>
                        <td nowrap align="center"><b><asp:Label ID="vLeafS1TextBox" Width="60px" runat="server" Text='<%# Bind("vLeafS1") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafB1TextBox" Width="60px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB1") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vLeafB1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafWid1TextBox" Width="60px" onkeyup="leafwid1()" MaxLength="7" runat="server" Text='<%# Bind("vLeafWid1","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vLeafWid1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafLen1TextBox" Width="60px" MaxLength="7" onkeyup="leaflen1()" runat="server" Text='<%# Bind("vLeafLen1","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator20" runat="server" ControlToValidate="vLeafLen1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                    </tr>
                     <tr>
                        <td nowrap><b><asp:TextBox ID="vLeaf2TextBox" onblur="leafblur2()" runat="server" Text='<%# Bind("vLeaf2") %>'></asp:TextBox>
                            <a href="#" tabindex="1" onclick="Leaflook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafDesc2TextBox" runat="server" Text='<%# Bind("vLeafDesc2") %>'></asp:TextBox></b></td>
                        <td nowrap align="center"><b><asp:Label ID="vLeafS2TextBox" Width="60px" runat="server" Text='<%# Bind("vLeafS2") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafB2TextBox" Width="60px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB2") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vLeafB2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafWid2TextBox" Width="60px" onkeyup="leafwid2()" MaxLength="7" runat="server" Text='<%# Bind("vLeafWid2","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator21" runat="server" ControlToValidate="vLeafWid2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafLen2TextBox" Width="60px" MaxLength="7" onkeyup="leaflen2()" runat="server" Text='<%# Bind("vLeafLen2","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator22" runat="server" ControlToValidate="vLeafLen2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                    </tr>
                     <tr>
                        <td nowrap><b><asp:TextBox ID="vLeaf3TextBox" onblur="leafblur3()" runat="server" Text='<%# Bind("vLeaf3") %>'></asp:TextBox>
                            <a href="#" tabindex="1" onclick="Leaflook3(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafDesc3TextBox" runat="server" Text='<%# Bind("vLeafDesc3") %>'></asp:TextBox></b></td>
                        <td nowrap align="center"><b><asp:Label ID="vLeafS3TextBox" Width="60px" runat="server" Text='<%# Bind("vLeafS3") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafB3TextBox" Width="60px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB3") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vLeafB3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafWid3TextBox" Width="60px" onkeyup="leafwid3()" MaxLength="7" runat="server" Text='<%# Bind("vLeafWid3","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator23" runat="server" ControlToValidate="vLeafWid3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafLen3TextBox" Width="60px" MaxLength="7" onkeyup="leaflen3()" runat="server" Text='<%# Bind("vLeafLen3","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator24" runat="server" ControlToValidate="vLeafLen3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                    </tr>
                     <tr>
                        <td nowrap><b><asp:TextBox ID="vLeaf4TextBox" onblur="leafblur4()" runat="server" Text='<%# Bind("vLeaf4") %>'></asp:TextBox>
                            <a href="#" tabindex="1" onclick="Leaflook4(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafDesc4TextBox" runat="server" Text='<%# Bind("vLeafDesc4") %>'></asp:TextBox></b></td>
                        <td nowrap align="center"><b><asp:Label ID="vLeafS4TextBox" Width="60px" runat="server" Text='<%# Bind("vLeafS4") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafB4TextBox" Width="60px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB4") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vLeafB4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafWid4TextBox" Width="60px" onkeyup="leafwid4()" MaxLength="7" runat="server" Text='<%# Bind("vLeafWid4","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator25" runat="server" ControlToValidate="vLeafWid4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vLeafLen4TextBox" onblur="setfocus()" Width="60px" MaxLength="7" onkeyup="leaflen4()" runat="server" Text='<%# Bind("vLeafLen4","{0:#0.0000}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator26" runat="server" ControlToValidate="vLeafLen4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b> </td>
                    </tr>
                    <tr><td style="display:none"><asp:Label ID="vStyleLabel"  runat="server" Text= '<%# Bind("vStyle") %>' ></asp:Label>
                     <asp:Label ID="typelabel" runat="server" Text='<%# Bind("vType") %>' ></asp:Label>
                     <asp:Label ID="sheetLabel" runat="server" ></asp:Label>
                    </td></tr>
                </table>
            </fieldset>
        </fieldset>
       </ContentTemplate>
            </asp:UpdatePanel>
                      
            <%--<asp:TextBox ID="vTypeTextBox" runat="server" Text='<%# Bind("vType") %>'>
            </asp:TextBox><br />--%>
            <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM" CausesValidation="True" OnClick="UpdateButton_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" OnClick="btn_update_cancel" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </asp:Panel>
        </EditItemTemplate>
        
        <ItemTemplate>
            <fieldset class="shade">
                <legend>Reference Information</legend>
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Estimate#:</b></td>
                            <td nowrap><b><asp:Label ID="vEstimateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstimate") %>'></asp:Label>
                                            <asp:Label ID="vTypeLabel" Visible="false" runat="server" Text='<%# Bind("vType") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Est Date:</b></td>
                            <td nowrap><b><asp:Label ID="vEstDateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Form:</b></td>
                            <td nowrap><b><asp:Label ID="vFormLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vForm") %>'></asp:Label>
                                of
                                <asp:Label ID="vFormQtyLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Cust Part:</b></td>
                            <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                             <td style="display:none"><asp:Label ID="vOrderLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("Order") %>'></asp:Label></td>
                        </tr>
                    </table>
            </fieldset>
            <fieldset class="shade">
                <table>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Machine:</b></td>
                        <td nowrap><b><asp:Label ID="vMachineLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vMachine") %>'></asp:Label>
                        &nbsp;&nbsp;
                            <asp:Label ID="vMachDscrLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vMachDscr") %>'></asp:Label>
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Front-Back:</b></td>
                        <td nowrap><b><asp:Label ID="vFrontBackLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vFrontBack","{0:##0.0000}") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Side-Side:</b></td>
                        <td nowrap><b><asp:Label ID="vSideSideLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vSideSide","{0:##0.0000}") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Xgrain:</b></td>
                        <td nowrap><b><asp:Label ID="vXgrainLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vXgrain") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td><b></b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Board:</b></td>
                        <td nowrap><b><asp:Label ID="vBoardLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vBoard") %>'></asp:Label>
                        &nbsp;&nbsp;
                        <asp:Label ID="vBoardNameLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vBoardName") %>'></asp:Label>
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Real:</b></td>
                        <td nowrap><b><asp:Label ID="vRealLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vReal") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td><b></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                        <td><b></b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Caliper:</b></td>
                        <td nowrap><b><asp:Label ID="vCaliperLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vCaliper","{0:##0.00000}") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Cost:</b></td>
                        <td nowrap><b>
                        <asp:Label ID="vCostUomLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  Text='<%# Bind("vCostUom") %>'></asp:Label>
                        <asp:Label ID="vCostMsfLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vCostMsf","{0:####0.000}") %>'></asp:Label>                                                
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Wt:</b></td>
                        <td nowrap><b><asp:Label ID="vWeightLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vWeight","{0:##0.00}") %>'></asp:Label></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>Freight:</b></td>
                        <td nowrap><b>
                        <asp:Label ID="vFreightUomLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vFreightUom") %>'></asp:Label>
                        <asp:Label ID="vFreightCwtLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vFreightCwt","{0:##0.000}") %>'></asp:Label>                        
                        </b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>NC:</b></td>
                        <td nowrap><b><asp:Label ID="vNcLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vNc") %>'></asp:Label></b></td>
                    </tr>
                </table>
                <fieldset>
                    <table>
                        <tr>
                            <td>&nbsp;</td>
                            <td nowrap><b>Width:<b></td>
                            <td nowrap><b>Length:</b></td>
                            <td nowrap><b>Sq. Inches</b></td>
                            <td>&nbsp;</td>
                            <td>&nbsp;</td>
                            <td>&nbsp;</td>
                            <td>&nbsp;</td>
                            <td nowrap><b>Total Up</b></td>
                            <td nowrap><b>Die Inches</b></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Roll: 
                            <asp:Label ID="vRollLabel" Visible="false" runat="server" Text='<%# Bind("vRoll") %>'></asp:Label>
                            <asp:CheckBox ID="chk_roll" Enabled="false" runat="server" />
                            </b></td>
                            <td nowrap><b><asp:Label ID="vRollWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vRollWid","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Gross Sheet:</b></td>
                            <td nowrap><b><asp:Label ID="vGrosShetWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vGrosShetWid","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vGrosShetLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vGrosShetLen","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>#Out:</b></td>
                            <td nowrap><b><asp:Label ID="vOutWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOutWid") %>'></asp:Label></b></td>
                            <td nowrap><b><%--<asp:Label ID="vOutLenLabel" runat="server" Text='<%# Bind("vOutLen") %>'></asp:Label>--%></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Cuts:</b></td>
                            <td nowrap><b><asp:Label ID="vOutCutLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOutCut") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Mach Feed:</b></td>
                            <td nowrap><b><asp:Label ID="vMachFeedWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vMachFeedWid","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMachFeedLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vMachFeedLen","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>#Out:</b></td>
                            <td nowrap><b><asp:Label ID="vOutLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOutLen") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Die Size:</b></td>
                            <td nowrap><b><asp:Label ID="vDieSizeWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vDieSizeWid","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vDieSizeLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vDieSizeLen","{0:##0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>#On:</b></td>
                            <td nowrap><b><asp:Label ID="vOnWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOnWid") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vOnLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOnLen") %>'></asp:Label></b></td>
                            <td>&nbsp;</td>
                            
                            <td nowrap><b><asp:Label ID="vOnTotalUpLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vOnTotalUp") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vDieInchesLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vDieInches") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Blank:</b></td>
                            <td nowrap><b><asp:Label ID="vBlankWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vBlankWid","{0:##0.000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vBlankLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vBlankLen","{0:##0.000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vBlankSqInchLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vBlankSqInch","{0:##0.00000}") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                            <td nowrap><b></b></td>
                            <td nowrap><b></b></td>
                        </tr>
                    </table>
                </fieldset>
                <fieldset>
                    <table>
                        <tr>
                            <td nowrap><b>Leaf/Film</b></td>
                            <td nowrap><b>Description</b></td>
                            <td nowrap><b>S</b></td>
                            <td nowrap><b>B</b></td>                                                       
                            <td nowrap><b>Wid</b></td>
                            <td nowrap><b>Len</b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vLeaf1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vLeaf1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafDesc1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vLeafDesc1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafS1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafS1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafB1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafB1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafWid1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafWid1","{0:#0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafLen1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafLen1","{0:#0.0000}") %>'></asp:Label></b></td>
                        </tr>
                         <tr>
                            <td nowrap><b><asp:Label ID="vLeaf2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vLeaf2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafDesc2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vLeafDesc2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafS2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafS2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafB2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafB2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafWid2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafWid2","{0:#0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafLen2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafLen2","{0:#0.0000}") %>'></asp:Label></b></td>
                        </tr>
                         <tr>
                            <td nowrap><b><asp:Label ID="vLeaf3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vLeaf3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafDesc3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vLeafDesc3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafS3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafS3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafB3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafB3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafWid3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafWid3","{0:#0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafLen3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafLen3","{0:#0.0000}") %>'></asp:Label></b></td>
                        </tr>
                         <tr>
                            <td nowrap><b><asp:Label ID="vLeaf4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px"  runat="server" Text='<%# Bind("vLeaf4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafDesc4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px"  runat="server" Text='<%# Bind("vLeafDesc4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafS4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafS4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafB4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px"  runat="server" Text='<%# Bind("vLeafB4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafWid4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafWid4","{0:#0.0000}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLeafLen4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px"  runat="server" Text='<%# Bind("vLeafLen4","{0:#0.0000}") %>'></asp:Label></b></td>
                        </tr>
                    </table>
                </fieldset>
            </fieldset>
            
           
            
            <%--<asp:Label ID="vTypeLabel" runat="server" Text='<%# Bind("vType") %>'></asp:Label>--%>
            
            
            
            <asp:Button ID="btn_update" runat="server" Text="OverRide" CssClass="buttonM" OnClick="OverRide_button_click" CommandName="Edit" />
            <asp:Button ID="btn_auto_calc" runat="server" Text="Auto-Calc" CommandName="Edit" CssClass="buttonM" OnClick="btn_auto_calc_click" />
            <input type="button" class="buttonM" value="Bom" id="btn_bom" onclick="open_bom()" />
            <asp:Button ID="btn_sheet_calc" runat="server" Text="Sheet Calc" CommandName="Edit" CssClass="buttonM" OnClick="btn_sheet_calc_click" />
            <asp:Button ID="jobButton" runat="server" CausesValidation="false" CssClass="button" OnClick="Job_Button_Click" OnClientClick=" return jobbuttonconfirm()"  Text="Job Stds" >  </asp:Button>
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="FoldLayout_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldLayout" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmForm" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmMachine" Type="String" />
            <asp:Parameter Name="prmMachDscr" Type="String" />
            <asp:Parameter Name="prmFrontBack" Type="Decimal" />
            <asp:Parameter Name="prmSideSide" Type="Decimal" />
            <asp:Parameter Name="prmXgrain" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmBoardName" Type="String" />
            <asp:Parameter Name="prmReal" Type="String" />
            <asp:Parameter Name="prmCaliper" Type="Decimal" />
            <asp:Parameter Name="prmCostMsf" Type="Decimal" />
            <asp:Parameter Name="prmCostUom" Type="String" />
            <asp:Parameter Name="prmWeightt" Type="Decimal" />
            <asp:Parameter Name="prmFreightMsf" Type="Decimal" />
            <asp:Parameter Name="prmFreightUom" Type="String" />
            <asp:Parameter Name="prmNc" Type="String" />
            <asp:Parameter Name="prmRoll" Type="String" />
            <asp:Parameter Name="prmRollWid" Type="Decimal" />
            <asp:Parameter Name="prmGrosShetWid" Type="Decimal" />
            <asp:Parameter Name="prmGrosShetLen" Type="Decimal" />
            <asp:Parameter Name="prmOutWid" Type="Decimal" />
            <asp:Parameter Name="prmOutLen" Type="Decimal" />
            <asp:Parameter Name="prmOutCut" Type="Int32" />
            <asp:Parameter Name="prmDieInches" Type="Decimal" />
            <asp:Parameter Name="prmMachFeedWid" Type="Decimal" />
            <asp:Parameter Name="prmMachFeedLen" Type="Decimal" />
            <asp:Parameter Name="prmDieSizeWid" Type="Decimal" />
            <asp:Parameter Name="prmDieSizeLen" Type="Decimal" />
            <asp:Parameter Name="prmOnWid" Type="Decimal" />
            <asp:Parameter Name="prmOnLen" Type="Decimal" />
            <asp:Parameter Name="prmOnTotalUp" Type="Decimal" />
            <asp:Parameter Name="prmBlankWid" Type="Decimal" />
            <asp:Parameter Name="prmBlankLen" Type="Decimal" />
            <asp:Parameter Name="prmBlankSqInch" Type="Decimal" />
            <asp:Parameter Name="prmLeaf1" Type="String" />
            <asp:Parameter Name="prmLeaf2" Type="String" />
            <asp:Parameter Name="prmLeaf3" Type="String" />
            <asp:Parameter Name="prmLeaf4" Type="String" />
            <asp:Parameter Name="prmLeafDesc1" Type="String" />
            <asp:Parameter Name="prmLeafDesc2" Type="String" />
            <asp:Parameter Name="prmLeafDesc3" Type="String" />
            <asp:Parameter Name="prmLeafDesc4" Type="String" />
            <asp:Parameter Name="prmS1" Type="Int32" />
            <asp:Parameter Name="prmB1" Type="Int32" />
            <asp:Parameter Name="prmLeafWid1" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen1" Type="Decimal" />
            <asp:Parameter Name="prmS2" Type="Int32" />
            <asp:Parameter Name="prmB2" Type="Int32" />
            <asp:Parameter Name="prmLeafWid2" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen2" Type="Decimal" />
            <asp:Parameter Name="prmS3" Type="Int32" />
            <asp:Parameter Name="prmB3" Type="Int32" />
            <asp:Parameter Name="prmLeafWid3" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen3" Type="Decimal" />
            <asp:Parameter Name="prmS4" Type="Int32" />
            <asp:Parameter Name="prmB4" Type="Int32" />
            <asp:Parameter Name="prmLeafWid4" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen4" Type="Decimal" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_folding_blankno" Type="int32" />
            <asp:Parameter Name="prmAuto" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

