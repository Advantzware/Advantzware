<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master" AutoEventWireup="true" Debug="true" Inherits="corr_miscsub" Title="Corrugated Misc/Sub" Codebehind="corr_miscsub.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script>
window.onload=setfocus;
    function setfocus()
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vB1TextBox"))
        {
            var b=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vB1TextBox");
            b.focus();
        }
       var simon1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon1DropDownList");
       var matm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm1TextBox");
       var labm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm1TextBox");
       var simon2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon2DropDownList");
       var matm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm2TextBox");
       var labm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm2TextBox");
       var simon3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon3DropDownList");
       var matm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm3TextBox");
       var labm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm3TextBox");
       var simon4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon4DropDownList");
       var matm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm4TextBox");
       var labm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm4TextBox");
       var simon5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon5DropDownList");
       var matm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm5TextBox");
       var labm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm5TextBox");
       var simon6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon6DropDownList");
       var matm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm6TextBox");
       var labm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm6TextBox");
       
       var cost1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost1TextBox");
       var cost2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost2TextBox");
       var cost3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost3TextBox");
       var cost4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost4TextBox");
       var cost5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost5TextBox");
       var cost6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost6TextBox");
       
       if(cost1.value=="")
       {
            simon1.disabled=true;
            matm1.disabled=true;
            labm1.disabled=true;
       }
       if(cost2.value=="")
       {
            simon2.disabled=true;
            matm2.disabled=true;
            labm2.disabled=true;
       }
       if(cost3.value=="")
       {
            simon3.disabled=true;
            matm3.disabled=true;
            labm3.disabled=true;
       }
       if(cost4.value=="")
       {
            simon4.disabled=true;
            matm4.disabled=true;
            labm4.disabled=true;
       }
       if(cost5.value=="")
       {
            simon5.disabled=true;
            matm5.disabled=true;
            labm5.disabled=true;
       }
       if(cost6.value=="")
       {
            simon6.disabled=true;
            matm6.disabled=true;
            labm6.disabled=true;
       }
    }
    function cost1enable()
    {
        var cost1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost1TextBox");
        var s1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS1Label");
        var mrkup1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp1TextBox");
        var simon1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon1DropDownList");        
        var matm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm1TextBox");
        var labm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm1TextBox");
        if(cost1.value!="")
        {
            s1.innerText="1";
            mrkup1.value="2.00";
            simon1.selectedIndex=0;
            matm1.disabled=false;
            labm1.disabled=false;
            simon1.disabled=false;
        }
        else
        {
            s1.innerText="0";
            mrkup1.value="0.00";
            simon1.selectedIndex=5;
            matm1.value="0.00";
            labm1.value="0.00";;            
            matm1.disabled=true;
            labm1.disabled=true;
            simon1.disabled=true;
        }   
    }
    function cost2enable()
    {
        var cost2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost2TextBox");
        var s2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS2Label");
        var mrkup2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp2TextBox");
        var simon2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon2DropDownList");
        var matm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm2TextBox");
        var labm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm2TextBox");
        if(cost2.value!="")
        {
            s2.innerText="1";
            mrkup2.value="2.00";
            simon2.selectedIndex=0;
            matm2.disabled=false;
            labm2.disabled=false;
            simon2.disabled=false;
        }
        else
        {
            s2.innerText="0";
            mrkup2.value="0.00";
            simon2.selectedIndex=5;
            matm2.value="0.00";
            labm2.value="0.00";; 
            matm2.disabled=true;
            labm2.disabled=true;
            simon2.disabled=true;
        }   
    }
    function cost3enable()
    {
        var cost3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost3TextBox");
        var s3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS3Label");
        var mrkup3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp3TextBox");
        var simon3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon3DropDownList");
        var matm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm3TextBox");
        var labm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm3TextBox");
        if(cost3.value!="")
        {
            s3.innerText="1";
            mrkup3.value="2.00";
            simon3.selectedIndex=0;
            matm3.disabled=false;
            labm3.disabled=false;
            simon3.disabled=false;
        }
        else
        {
            s3.innerText="0";
            mrkup3.value="0.00";
            simon3.selectedIndex=5;
            matm3.value="0.00";
            labm3.value="0.00";; 
            matm3.disabled=true;
            labm3.disabled=true;
            simon3.disabled=true;
        }   
    }
    
    function cost4enable()
    {
        var cost4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost4TextBox");
        var s4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS4Label");
        var mrkup4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp4TextBox");
        var simon4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon4DropDownList");
        var matm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm4TextBox");
        var labm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm4TextBox");
        if(cost4.value!="")
        {
            s4.innerText="1";
            mrkup4.value="2.00";
            simon4.selectedIndex=0;
            matm4.disabled=false;
            labm4.disabled=false;
            simon4.disabled=false;
        }
        else
        {
            s4.innerText="0";
            mrkup4.value="0.00";
            simon4.selectedIndex=5;
            matm4.value="0.00";
            labm4.value="0.00";; 
            matm4.disabled=true;
            labm4.disabled=true;
            simon4.disabled=true;
        }   
    }
    function cost5enable()
    {
        var cost5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost5TextBox");
        var s5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS5Label");
        var mrkup5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp5TextBox");
        var simon5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon5DropDownList");
        var matm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm5TextBox");
        var labm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm5TextBox");
        if(cost5.value!="")
        {
            s5.innerText="1";
            mrkup5.value="2.00";
            simon5.selectedIndex=0;
            matm5.disabled=false;
            labm5.disabled=false;
            simon5.disabled=false;
        }
        else
        {
            s5.innerText="0";
            mrkup5.value="0.00";
            simon5.selectedIndex=5;
            matm5.value="0.00";
            labm5.value="0.00";; 
            matm5.disabled=true;
            labm5.disabled=true;
            simon5.disabled=true;
        }   
    }
    function cost6enable()
    {
        var cost6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vCost6TextBox");
        var s6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vS6Label");
        var mrkup6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp6TextBox");
        var simon6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vSimon6DropDownList");
        var matm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm6TextBox");
        var labm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm6TextBox");
        if(cost6.value!="")
        {
            s6.innerText="1";
            mrkup6.value="2.00";
            simon6.selectedIndex=0;
            matm6.disabled=false;
            labm6.disabled=false;
            simon6.disabled=false;
        }
        else
        {
            s6.innerText="0";
            mrkup6.value="0.00";
            simon6.selectedIndex=5;
            matm6.value="0.00";
            labm6.value="0.00";; 
            matm6.disabled=true;
            labm6.disabled=true;
            simon6.disabled=true;
        }   
    }
    
    function Itemlook1()
    {  
        var NewWindow = window.open("item_lookup.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup1(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem1TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr1TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem1TextBox.focus();     
    } 
    function Itemlook2()
    {  
        var NewWindow = window.open("item_lookup2.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup2(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem2TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr2TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem2TextBox.focus();       
    }  
    function Itemlook3()
    {  
        var NewWindow = window.open("item_lookup3.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup3(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem3TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr3TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem3TextBox.focus();       
    }   
    function Itemlook4()
    {  
        var NewWindow = window.open("item_lookup4.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup4(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem4TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr4TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem4TextBox.focus();       
    } 
    function Itemlook5()
    {  
        var NewWindow = window.open("item_lookup5.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup5(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem5TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr5TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem5TextBox.focus();       
    } 
    function Itemlook6()
    {  
        var NewWindow = window.open("item_lookup6.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup6(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem6TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr6TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem6TextBox.focus();       
    } 
    function Itemlook7()
    {  
        var NewWindow = window.open("item_lookup7.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup7(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem7TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr7TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem7TextBox.focus();       
    } 
    function Itemlook8()
    {  
        var NewWindow = window.open("item_lookup8.aspx","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ItemLookup8(ReturnObj1, ReturnObj2)
    { 
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem8TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr8TextBox.value = ReturnObj2;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem8TextBox.focus();      
    }
    
    function matsu1()
    {
        var mat1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf1TextBox").value;
        if(mat1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat1.length > 4 && mat1.length < 6)
        mat1=mat1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf1TextBox").value = mat1;
    } 
    function matsu2()
    {
        var mat2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf2TextBox").value;
        if(mat2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat2.length > 4 && mat2.length < 6)
        mat2=mat2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf2TextBox").value = mat2;
    }
    function matsu3()
    {
        var mat3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf3TextBox").value;
        if(mat3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat3.length > 4 && mat3.length < 6)
        mat3=mat3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf3TextBox").value = mat3;
    }
    function matsu4()
    {
        var mat4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf4TextBox").value;
        if(mat4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat4.length > 4 && mat4.length < 6)
        mat4=mat4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf4TextBox").value = mat4;
    }
    function matsu5()
    {
        var mat5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf5TextBox").value;
        if(mat5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat5.length > 4 && mat5.length < 6)
        mat5=mat5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf5TextBox").value = mat5;
    }
    function matsu6()
    {
        var mat6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf6TextBox").value;
        if(mat6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mat6.length > 4 && mat6.length < 6)
        mat6=mat6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatf6TextBox").value = mat6;
    } 
    
    function labsu1()
    {
        var lab1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf1TextBox").value;
        if(lab1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab1.length > 4 && lab1.length < 6)
        lab1=lab1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf1TextBox").value = lab1;
    } 
    function labsu2()
    {
        var lab2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf2TextBox").value;
        if(lab2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab2.length > 4 && lab2.length < 6)
        lab2=lab2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf2TextBox").value = lab2;
    } 
    function labsu3()
    {
        var lab3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf3TextBox").value;
        if(lab3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab3.length > 4 && lab3.length < 6)
        lab3=lab3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf3TextBox").value = lab3;
    } 
    function labsu4()
    {
        var lab4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf4TextBox").value;
        if(lab4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab4.length > 4 && lab4.length < 6)
        lab4=lab4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf4TextBox").value = lab4;
    } 
    function labsu5()
    {
        var lab5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf5TextBox").value;
        if(lab5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab5.length > 4 && lab5.length < 6)
        lab5=lab5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf5TextBox").value = lab5;
    } 
    function labsu6()
    {
        var lab6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf6TextBox").value;
        if(lab6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(lab6.length > 4 && lab6.length < 6)
        lab6=lab6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabf6TextBox").value = lab6;
    } 
    
    function matm1()
    {
        var matm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm1TextBox").value;
        if(matm1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm1.length > 5 && matm1.length < 7)
        matm1=matm1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm1TextBox").value = matm1;
    }
    function matm2()
    {
        var matm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm2TextBox").value;
        if(matm2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm2.length > 5 && matm2.length < 7)
        matm2=matm2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm2TextBox").value = matm2;
    }
    function matm3()
    {
        var matm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm3TextBox").value;
        if(matm3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm3.length > 5 && matm3.length < 7)
        matm3=matm3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm3TextBox").value = matm3;
    }
    function matm4()
    {
        var matm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm4TextBox").value;
        if(matm4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm4.length > 5 && matm4.length < 7)
        matm4=matm4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm4TextBox").value = matm4;
    }
    function matm5()
    {
        var matm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm5TextBox").value;
        if(matm5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm5.length > 5 && matm5.length < 7)
        matm5=matm5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm5TextBox").value = matm5;
    }
    function matm6()
    {
        var matm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm6TextBox").value;
        if(matm6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(matm6.length > 5 && matm6.length < 7)
        matm6=matm6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMatm6TextBox").value = matm6;
    }
    function labm1()
    {
        var labm1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm1TextBox").value;
        if(labm1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm1.length > 5 && labm1.length < 7)
        labm1=labm1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm1TextBox").value = labm1;
    }
    function labm2()
    {
        var labm2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm2TextBox").value;
        if(labm2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm2.length > 5 && labm2.length < 7)
        labm2=labm2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm2TextBox").value = labm2;
    }
    function labm3()
    {
        var labm3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm3TextBox").value;
        if(labm3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm3.length > 5 && labm3.length < 7)
        labm3=labm3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm3TextBox").value = labm3;
    }
    function labm4()
    {
        var labm4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm4TextBox").value;
        if(labm4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm4.length > 5 && labm4.length < 7)
        labm4=labm4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm4TextBox").value = labm4;
    }
    function labm5()
    {
        var labm5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm5TextBox").value;
        if(labm5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm5.length > 5 && labm5.length < 7)
        labm5=labm5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm5TextBox").value = labm5;
    }
    function labm6()
    {
        var labm6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm6TextBox").value;
        if(labm6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(labm6.length > 5 && labm6.length < 7)
        labm6=labm6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vLabm6TextBox").value = labm6;
    }
    
    function markup1()
    {
        var mk1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp1TextBox").value;
        if(mk1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk1.length > 2 && mk1.length < 4)
        mk1=mk1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp1TextBox").value = mk1;
    }
    function markup2()
    {
        var mk2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp2TextBox").value;
        if(mk2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk2.length > 2 && mk2.length < 4)
        mk2=mk2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp2TextBox").value = mk2;
    }
    function markup3()
    {
        var mk3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp3TextBox").value;
        if(mk3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk3.length > 2 && mk3.length < 4)
        mk3=mk3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp3TextBox").value = mk3;
    }
    function markup4()
    {
        var mk4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp4TextBox").value;
        if(mk4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk4.length > 2 && mk4.length < 4)
        mk4=mk4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp4TextBox").value = mk4;
    }
    function markup5()
    {
        var mk5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp5TextBox").value;
        if(mk5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk5.length > 2 && mk5.length < 4)
        mk5=mk5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp5TextBox").value = mk5;
    }
    function markup6()
    {
        var mk6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp6TextBox").value;
        if(mk6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(mk6.length > 2 && mk6.length < 4)
        mk6=mk6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vMarkUp6TextBox").value = mk6;
    }
    
    function qty1()
    {
        var qty1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty1TextBox").value;
        if(qty1.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty1.length > 8 && qty1.length < 10)
        qty1=qty1 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty1TextBox").value = qty1;
    }
    function qty2()
    {
        var qty2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty2TextBox").value;
        if(qty2.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty2.length > 8 && qty2.length < 10)
        qty2=qty2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty2TextBox").value = qty2;
    }
    function qty3()
    {
        var qty3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty3TextBox").value;
        if(qty3.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty3.length > 8 && qty3.length < 10)
        qty3=qty3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty3TextBox").value = qty3;
    }
    function qty4()
    {
        var qty4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty4TextBox").value;
        if(qty4.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty4.length > 8 && qty4.length < 10)
        qty4=qty4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty4TextBox").value = qty4;
    }
    function qty5()
    {
        var qty5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty5TextBox").value;
        if(qty5.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty5.length > 8 && qty5.length < 10)
        qty5=qty5 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty5TextBox").value = qty5;
    }
    function qty6()
    {
        var qty6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty6TextBox").value;
        if(qty6.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty6.length > 8 && qty6.length < 10)
        qty6=qty6 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty6TextBox").value = qty6;
    }
    function qty7()
    {
        var qty7=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty7TextBox").value;
        if(qty7.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty7.length > 8 && qty7.length < 10)
        qty7=qty7 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty7TextBox").value = qty7;
    }
    function qty8()
    {
        var qty8=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty8TextBox").value;
        if(qty8.indexOf(".") != -1)
            {        
                return;
            } 
        else if(qty8.length > 8 && qty8.length < 10)
        qty8=qty8 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vQty8TextBox").value = qty8;
    }
    function item1()
    {
        var item1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem1TextBox");
        var itemdscr1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr1TextBox");
        if(item1.value=="")
            itemdscr1.value="";
    }
    function item2()
    {
        var item2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem2TextBox");
        var itemdscr2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr2TextBox");
        if(item2.value=="")
            itemdscr2.value="";
    }
    function item3()
    {
        var item3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem3TextBox");
        var itemdscr3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr3TextBox");
        if(item3.value=="")
            itemdscr3.value="";
    }
    function item4()
    {
        var item4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem4TextBox");
        var itemdscr4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr4TextBox");
        if(item4.value=="")
            itemdscr4.value="";
    }
    function item5()
    {
        var item5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem5TextBox");
        var itemdscr5=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr5TextBox");
        if(item5.value=="")
            itemdscr5.value="";
    }
    function item6()
    {
        var item6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem6TextBox");
        var itemdscr6=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr6TextBox");
        if(item6.value=="")
            itemdscr6.value="";
    }
    function item7()
    {
        var item7=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem7TextBox");
        var itemdscr7=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr7TextBox");
        if(item7.value=="")
            itemdscr7.value="";
    }
    function item8()
    {
        var item8=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItem8TextBox");
        var itemdscr8=document.getElementById("ctl00_ContentPlaceHolder1_FormView_MiscSub_vItemDscr8TextBox");
        if(item8.value=="")
            itemdscr8.value="";
    }
</script>
    <asp:FormView ID="FormView_MiscSub" runat="server" DataSourceID="MiscSub_ObjectDataSource">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <fieldset class="shade">
                <legend>Reference Information</legend>
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Estimate:</b></td>
                            <td nowrap><b><asp:Label ID="vEstimateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstimate") %>'> </asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Est Date:</b></td>
                            <td nowrap><b><asp:Label ID="vEstDateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Frm:</b></td>
                            <td nowrap><b><asp:Label ID="vFormLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vForm") %>'></asp:Label>
                                    of
                                    <asp:Label ID="vFormQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vFormQty") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Blk:</b></td>
                            <td nowrap><b>
                                        <asp:Label ID="vBlkLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlk") %>'></asp:Label>
                                        of
                                        <asp:Label ID="vBlkQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlkQty") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Cust Part:</b></td>
                            <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                        </tr>
                    </table>
            </fieldset>
            <fieldset class="shade">
                <legend>Sub-Contract, Farm-Out and Misc Cost:</legend>
                    <table>
                        <tr>
                            <td nowrap><b>S</b></td>
                            <td nowrap><b>B</b></td>
                            <td nowrap><b>Misc. Cost</b></td>
                            <td nowrap><b>Mat'l/SU</b></td>
                            <td nowrap><b>Labour/SU</b></td>
                            <td nowrap><b>Mat's/M</b></td>
                            <td nowrap><b>Labour/M</b></td>
                            <td nowrap><b>Simon</b></td>
                            <td nowrap><b>Markup</b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS1Label" runat="server" Text='<%# Bind("vS1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB1TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB1") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator27" ControlToValidate="vB1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost1TextBox" onkeyup="cost1enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost1") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf1TextBox" onkeyup="matsu1()" MaxLength="8" runat="server" Text='<%# Bind("vMatf1","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator15" ControlToValidate="vMatf1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf1TextBox" onkeyup="labsu1()" MaxLength="8" runat="server" Text='<%# Bind("vLabf1","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator21" ControlToValidate="vLabf1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm1TextBox" MaxLength="9" onkeyup="matm1()" runat="server" Text='<%# Bind("vMatm1","{0:#####0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator33" ControlToValidate="vMatm1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm1TextBox" MaxLength="9" onkeyup="labm1()" runat="server" Text='<%# Bind("vLabm1","{0:#####0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator44" ControlToValidate="vLabm1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon1DropDownList" runat="server" SelectedValue='<%# Bind("vSimon1") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp1TextBox" onkeyup="markup1()" MaxLength="6" runat="server" Text='<%# Bind("vMarkUp1","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator9" ControlToValidate="vMarkUp1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS2Label" runat="server" Width="40px" Text='<%# Bind("vS2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB2TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB2") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator28" ControlToValidate="vB2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost2TextBox" onkeyup="cost2enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost2") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf2TextBox" onkeyup="matsu2()" MaxLength="8" runat="server" Text='<%# Bind("vMatf2","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator16" ControlToValidate="vMatf2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf2TextBox" onkeyup="labsu2()" MaxLength="8" runat="server" Text='<%# Bind("vLabf2","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator22" ControlToValidate="vLabf2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm2TextBox" MaxLength="9" onkeyup="matm2()" runat="server" Text='<%# Bind("vMatm2","{0:#####0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator34" ControlToValidate="vMatm2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm2TextBox" MaxLength="9" onkeyup="labm2()" runat="server" Text='<%# Bind("vLabm2","{0:#####0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator43" ControlToValidate="vLabm2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon2DropDownList" runat="server" SelectedValue='<%# Bind("vSimon2") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp2TextBox" onkeyup="markup2()" MaxLength="6" runat="server" Text='<%# Bind("vMarkUp2","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator10" ControlToValidate="vMarkUp2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS3Label" runat="server" Width="40px" Text='<%# Bind("vS3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB3TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB3") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator29" ControlToValidate="vB3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost3TextBox" onkeyup="cost3enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost3") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf3TextBox" onkeyup="matsu3()" MaxLength="8" runat="server" Text='<%# Bind("vMatf3","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator17" ControlToValidate="vMatf3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf3TextBox" onkeyup="labsu3()" MaxLength="8" runat="server" Text='<%# Bind("vLabf3","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator23" ControlToValidate="vLabf3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm3TextBox" MaxLength="9" onkeyup="matm3()" runat="server" Text='<%# Bind("vMatm3","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator35" ControlToValidate="vMatm3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm3TextBox" MaxLength="9" onkeyup="labm3()" runat="server" Text='<%# Bind("vLabm3","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator42" ControlToValidate="vLabm3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon3DropDownList" runat="server" SelectedValue='<%# Bind("vSimon3") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp3TextBox" onkeyup="markup3()" MaxLength="6" runat="server" Text='<%# Bind("vMarkUp3","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator11" ControlToValidate="vMarkUp3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS4Label" runat="server" Width="40px" Text='<%# Bind("vS4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB4TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB4") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator30" ControlToValidate="vB4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost4TextBox" onkeyup="cost4enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost4") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf4TextBox" onkeyup="matsu4()" MaxLength="8" runat="server" Text='<%# Bind("vMatf4","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator18" ControlToValidate="vMatf4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf4TextBox" onkeyup="labsu4()" MaxLength="8" runat="server" Text='<%# Bind("vLabf4","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator24" ControlToValidate="vLabf4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm4TextBox" MaxLength="9" onkeyup="matm4()" runat="server" Text='<%# Bind("vMatm4","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator36" ControlToValidate="vMatm4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm4TextBox" MaxLength="9" onkeyup="labm4()" runat="server" Text='<%# Bind("vLabm4","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator41" ControlToValidate="vLabm4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon4DropDownList" runat="server" SelectedValue='<%# Bind("vSimon4") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp4TextBox" onkeyup="markup4()" MaxLength="6" runat="server" Text='<%# Bind("vMarkUp4","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator12" ControlToValidate="vMarkUp4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS5Label" runat="server" Width="40px" Text='<%# Bind("vS5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB5TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB5") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator31" ControlToValidate="vB5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost5TextBox" onkeyup="cost5enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost5") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf5TextBox" onkeyup="matsu5()" MaxLength="8" runat="server" Text='<%# Bind("vMatf5","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator19" ControlToValidate="vMatf5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf5TextBox" onkeyup="labsu5()" MaxLength="8" runat="server" Text='<%# Bind("vLabf5","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator25" ControlToValidate="vLabf5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm5TextBox" MaxLength="9" onkeyup="matm5()" runat="server" Text='<%# Bind("vMatm5","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator37" ControlToValidate="vMatm5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm5TextBox" MaxLength="9" onkeyup="labm5()" runat="server" Text='<%# Bind("vLabm5","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator40" ControlToValidate="vLabm5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon5DropDownList" runat="server" SelectedValue='<%# Bind("vSimon5") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp5TextBox" onkeyup="markup5()" MaxLength="6" runat="server" Text='<%# Bind("vMarkUp5","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator13" ControlToValidate="vMarkUp5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS6Label" runat="server" Width="40px" Text='<%# Bind("vS6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:TextBox ID="vB6TextBox" MaxLength="2" runat="server" Width="40px" Text='<%# Bind("vB6") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator32" ControlToValidate="vB6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="integer" runat="server" ErrorMessage="Only Integer Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vCost6TextBox" onkeyup="cost6enable()" MaxLength="20" runat="server" Text='<%# Bind("vCost6") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vMatf6TextBox" onkeyup="matsu6()" MaxLength="8" runat="server" Text='<%# Bind("vMatf6","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator20" ControlToValidate="vMatf6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabf6TextBox" onkeyup="labsu6()" MaxLength="8" runat="server" Text='<%# Bind("vLabf6","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator26" ControlToValidate="vLabf6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vMatm6TextBox" MaxLength="9" onkeyup="matm6()" runat="server" Text='<%# Bind("vMatm6","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator38" ControlToValidate="vMatm6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vLabm6TextBox" MaxLength="9" onkeyup="labm6()" runat="server" Text='<%# Bind("vLabm6","{0:#####0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator39" ControlToValidate="vLabm6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:DropDownList ID="vSimon6DropDownList" runat="server" SelectedValue='<%# Bind("vSimon6") %>'>
                                            <asp:ListItem>S</asp:ListItem>
                                            <asp:ListItem>I</asp:ListItem>
                                            <asp:ListItem>M</asp:ListItem>
                                            <asp:ListItem>O</asp:ListItem>
                                            <asp:ListItem>N</asp:ListItem>
                                            <asp:ListItem></asp:ListItem>
                                          </asp:DropDownList></b></td>
                            <td nowrap><b><asp:TextBox ID="vMarkUp6TextBox" onkeyup="markup6()" runat="server" MaxLength="6" Text='<%# Bind("vMarkUp6","{0:#####0.00}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator14" ControlToValidate="vMarkUp6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                        </tr>
                    </table>
            </fieldset>
            <fieldset class="shade">
                <legend>Special Materials:</legend>
                    <table>
                        <tr>
                            <td nowrap><b>Item#</b></td>
                            <td nowrap><b>Description</b></td>
                            <td nowrap><b>Qty/FG (Set)</b></td>
                            <td nowrap style="width:80px;" align="center"><b>UOM</b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem1TextBox" onblur="item1()" runat="server" Text='<%# Bind("vItem1") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onclick="Itemlook1(); return false"><asp:Image ID="img_board" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr1TextBox" runat="server" Text='<%# Bind("vItemDscr1") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty1TextBox" onkeyup="qty1()" MaxLength="11" runat="server" Text='<%# Bind("vQty1","{0:########0.0}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator1" ControlToValidate="vQty1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:Label ID="vUom1Label" runat="server" Text='<%# Bind("vUom1") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem2TextBox" onblur="item2()" runat="server" Text='<%# Bind("vItem2") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr2TextBox" runat="server" Text='<%# Bind("vItemDscr2") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty2TextBox" onkeyup="qty2()" MaxLength="11" runat="server" Text='<%# Bind("vQty2","{0:########0.0}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator2" ControlToValidate="vQty2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:Label ID="vUom2Label" runat="server" Text='<%# Bind("vUom2") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem3TextBox" onblur="item3()" runat="server" Text='<%# Bind("vItem3") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook3(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr3TextBox" runat="server" Text='<%# Bind("vItemDscr3") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty3TextBox" onkeyup="qty3()" MaxLength="11" runat="server" Text='<%# Bind("vQty3","{0:########0.0}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator3" ControlToValidate="vQty3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:Label ID="vUom3Label" runat="server" Text='<%# Bind("vUom3") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem4TextBox" onblur="item4()" runat="server" Text='<%# Bind("vItem4") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook4(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr4TextBox" runat="server" Text='<%# Bind("vItemDscr4") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty4TextBox" onkeyup="qty4()" MaxLength="11" runat="server" Text='<%# Bind("vQty4","{0:########0.0}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator4" ControlToValidate="vQty4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:Label ID="vUom4Label" runat="server" Text='<%# Bind("vUom4") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem5TextBox" onblur="item5()" runat="server" Text='<%# Bind("vItem5") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onclick="Itemlook5(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr5TextBox" runat="server" Text='<%# Bind("vItemDscr5") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty5TextBox" onkeyup="qty5()" MaxLength="11" runat="server" Text='<%# Bind("vQty5","{0:########0.0}") %>'></asp:TextBox></b>
                                          <asp:CompareValidator ID="CompareValidator5" ControlToValidate="vQty5TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </td>
                            <td nowrap><b><asp:Label ID="vUom5Label" runat="server" Text='<%# Bind("vUom5") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem6TextBox" onblur="item6()" runat="server" Text='<%# Bind("vItem6") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook6(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr6TextBox" runat="server" Text='<%# Bind("vItemDscr6") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty6TextBox" onkeyup="qty6()" MaxLength="11" runat="server" Text='<%# Bind("vQty6","{0:########0.0}") %>'></asp:TextBox></b>
                                          <asp:CompareValidator ID="CompareValidator6" ControlToValidate="vQty6TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </td>
                            <td nowrap><b><asp:Label ID="vUom6Label" runat="server" Text='<%# Bind("vUom6") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem7TextBox" onblur="item7()" runat="server" Text='<%# Bind("vItem7") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook7(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr7TextBox" runat="server" Text='<%# Bind("vItemDscr7") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty7TextBox" onkeyup="qty7()" MaxLength="11" runat="server" Text='<%# Bind("vQty7","{0:########0.0}") %>'></asp:TextBox></b>
                                          <asp:CompareValidator ID="CompareValidator7" ControlToValidate="vQty7TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </td>
                            <td nowrap><b><asp:Label ID="vUom7Label" runat="server" Text='<%# Bind("vUom7") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:TextBox ID="vItem8TextBox" onblur="item8()" runat="server" Text='<%# Bind("vItem8") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Itemlook8(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </b></td>
                            <td nowrap><b><asp:TextBox ID="vItemDscr8TextBox" runat="server" Text='<%# Bind("vItemDscr8") %>'></asp:TextBox></b></td>
                            <td nowrap><b><asp:TextBox ID="vQty8TextBox" onkeyup="qty8()" MaxLength="11" runat="server" Text='<%# Bind("vQty8","{0:########0.0}") %>'></asp:TextBox>
                                          <asp:CompareValidator ID="CompareValidator8" ControlToValidate="vQty8TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="double" runat="server" ErrorMessage="Only Decimal Values"></asp:CompareValidator>  
                            </b></td>
                            <td nowrap><b><asp:Label ID="vUom8Label" runat="server" Text='<%# Bind("vUom8") %>'></asp:Label></b></td>
                        </tr>
                    </table>
            </fieldset>
          
            
            <%--<asp:TextBox ID="vTypeTextBox" runat="server" Text='<%# Bind("vType") %>'>
            </asp:TextBox><br />--%>
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="buttonM" OnClick="UpdateButton_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CssClass="buttonM" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </asp:Panel>
        </EditItemTemplate>
        <%--<InsertItemTemplate>
            vEstimate:
            <asp:TextBox ID="vEstimateTextBox" runat="server" Text='<%# Bind("vEstimate") %>'>
            </asp:TextBox><br />
            vEstDate:
            <asp:TextBox ID="vEstDateTextBox" runat="server" Text='<%# Bind("vEstDate") %>'>
            </asp:TextBox><br />
            vForm:
            <asp:TextBox ID="vFormTextBox" runat="server" Text='<%# Bind("vForm") %>'>
            </asp:TextBox><br />
            vFormQty:
            <asp:TextBox ID="vFormQtyTextBox" runat="server" Text='<%# Bind("vFormQty") %>'>
            </asp:TextBox><br />
            vBlk:
            <asp:TextBox ID="vBlkTextBox" runat="server" Text='<%# Bind("vBlk") %>'>
            </asp:TextBox><br />
            vBlkQty:
            <asp:TextBox ID="vBlkQtyTextBox" runat="server" Text='<%# Bind("vBlkQty") %>'>
            </asp:TextBox><br />
            vCustPart:
            <asp:TextBox ID="vCustPartTextBox" runat="server" Text='<%# Bind("vCustPart") %>'>
            </asp:TextBox><br />
            vS1:
            <asp:TextBox ID="vS1TextBox" runat="server" Text='<%# Bind("vS1") %>'>
            </asp:TextBox><br />
            vB1:
            <asp:TextBox ID="vB1TextBox" runat="server" Text='<%# Bind("vB1") %>'>
            </asp:TextBox><br />
            vCost1:
            <asp:TextBox ID="vCost1TextBox" runat="server" Text='<%# Bind("vCost1") %>'>
            </asp:TextBox><br />
            vMatf1:
            <asp:TextBox ID="vMatf1TextBox" runat="server" Text='<%# Bind("vMatf1") %>'>
            </asp:TextBox><br />
            vLabf1:
            <asp:TextBox ID="vLabf1TextBox" runat="server" Text='<%# Bind("vLabf1") %>'>
            </asp:TextBox><br />
            vMatm1:
            <asp:TextBox ID="vMatm1TextBox" runat="server" Text='<%# Bind("vMatm1") %>'>
            </asp:TextBox><br />
            vLabm1:
            <asp:TextBox ID="vLabm1TextBox" runat="server" Text='<%# Bind("vLabm1") %>'>
            </asp:TextBox><br />
            vSimon1:
            <asp:TextBox ID="vSimon1TextBox" runat="server" Text='<%# Bind("vSimon1") %>'>
            </asp:TextBox><br />
            vMarkUp1:
            <asp:TextBox ID="vMarkUp1TextBox" runat="server" Text='<%# Bind("vMarkUp1") %>'>
            </asp:TextBox><br />
            vS2:
            <asp:TextBox ID="vS2TextBox" runat="server" Text='<%# Bind("vS2") %>'>
            </asp:TextBox><br />
            vB2:
            <asp:TextBox ID="vB2TextBox" runat="server" Text='<%# Bind("vB2") %>'>
            </asp:TextBox><br />
            vCost2:
            <asp:TextBox ID="vCost2TextBox" runat="server" Text='<%# Bind("vCost2") %>'>
            </asp:TextBox><br />
            vMatf2:
            <asp:TextBox ID="vMatf2TextBox" runat="server" Text='<%# Bind("vMatf2") %>'>
            </asp:TextBox><br />
            vLabf2:
            <asp:TextBox ID="vLabf2TextBox" runat="server" Text='<%# Bind("vLabf2") %>'>
            </asp:TextBox><br />
            vMatm2:
            <asp:TextBox ID="vMatm2TextBox" runat="server" Text='<%# Bind("vMatm2") %>'>
            </asp:TextBox><br />
            vLabm2:
            <asp:TextBox ID="vLabm2TextBox" runat="server" Text='<%# Bind("vLabm2") %>'>
            </asp:TextBox><br />
            vSimon2:
            <asp:TextBox ID="vSimon2TextBox" runat="server" Text='<%# Bind("vSimon2") %>'>
            </asp:TextBox><br />
            vMarkUp2:
            <asp:TextBox ID="vMarkUp2TextBox" runat="server" Text='<%# Bind("vMarkUp2") %>'>
            </asp:TextBox><br />
            vS3:
            <asp:TextBox ID="vS3TextBox" runat="server" Text='<%# Bind("vS3") %>'>
            </asp:TextBox><br />
            vB3:
            <asp:TextBox ID="vB3TextBox" runat="server" Text='<%# Bind("vB3") %>'>
            </asp:TextBox><br />
            vCost3:
            <asp:TextBox ID="vCost3TextBox" runat="server" Text='<%# Bind("vCost3") %>'>
            </asp:TextBox><br />
            vMatf3:
            <asp:TextBox ID="vMatf3TextBox" runat="server" Text='<%# Bind("vMatf3") %>'>
            </asp:TextBox><br />
            vLabf3:
            <asp:TextBox ID="vLabf3TextBox" runat="server" Text='<%# Bind("vLabf3") %>'>
            </asp:TextBox><br />
            vMatm3:
            <asp:TextBox ID="vMatm3TextBox" runat="server" Text='<%# Bind("vMatm3") %>'>
            </asp:TextBox><br />
            vLabm3:
            <asp:TextBox ID="vLabm3TextBox" runat="server" Text='<%# Bind("vLabm3") %>'>
            </asp:TextBox><br />
            vSimon3:
            <asp:TextBox ID="vSimon3TextBox" runat="server" Text='<%# Bind("vSimon3") %>'>
            </asp:TextBox><br />
            vMarkUp3:
            <asp:TextBox ID="vMarkUp3TextBox" runat="server" Text='<%# Bind("vMarkUp3") %>'>
            </asp:TextBox><br />
            vS4:
            <asp:TextBox ID="vS4TextBox" runat="server" Text='<%# Bind("vS4") %>'>
            </asp:TextBox><br />
            vB4:
            <asp:TextBox ID="vB4TextBox" runat="server" Text='<%# Bind("vB4") %>'>
            </asp:TextBox><br />
            vCost4:
            <asp:TextBox ID="vCost4TextBox" runat="server" Text='<%# Bind("vCost4") %>'>
            </asp:TextBox><br />
            vMatf4:
            <asp:TextBox ID="vMatf4TextBox" runat="server" Text='<%# Bind("vMatf4") %>'>
            </asp:TextBox><br />
            vLabf4:
            <asp:TextBox ID="vLabf4TextBox" runat="server" Text='<%# Bind("vLabf4") %>'>
            </asp:TextBox><br />
            vMatm4:
            <asp:TextBox ID="vMatm4TextBox" runat="server" Text='<%# Bind("vMatm4") %>'>
            </asp:TextBox><br />
            vLabm4:
            <asp:TextBox ID="vLabm4TextBox" runat="server" Text='<%# Bind("vLabm4") %>'>
            </asp:TextBox><br />
            vSimon4:
            <asp:TextBox ID="vSimon4TextBox" runat="server" Text='<%# Bind("vSimon4") %>'>
            </asp:TextBox><br />
            vMarkUp4:
            <asp:TextBox ID="vMarkUp4TextBox" runat="server" Text='<%# Bind("vMarkUp4") %>'>
            </asp:TextBox><br />
            vS5:
            <asp:TextBox ID="vS5TextBox" runat="server" Text='<%# Bind("vS5") %>'>
            </asp:TextBox><br />
            vB5:
            <asp:TextBox ID="vB5TextBox" runat="server" Text='<%# Bind("vB5") %>'>
            </asp:TextBox><br />
            vCost5:
            <asp:TextBox ID="vCost5TextBox" runat="server" Text='<%# Bind("vCost5") %>'>
            </asp:TextBox><br />
            vMatf5:
            <asp:TextBox ID="vMatf5TextBox" runat="server" Text='<%# Bind("vMatf5") %>'>
            </asp:TextBox><br />
            vLabf5:
            <asp:TextBox ID="vLabf5TextBox" runat="server" Text='<%# Bind("vLabf5") %>'>
            </asp:TextBox><br />
            vMatm5:
            <asp:TextBox ID="vMatm5TextBox" runat="server" Text='<%# Bind("vMatm5") %>'>
            </asp:TextBox><br />
            vLabm5:
            <asp:TextBox ID="vLabm5TextBox" runat="server" Text='<%# Bind("vLabm5") %>'>
            </asp:TextBox><br />
            vSimon5:
            <asp:TextBox ID="vSimon5TextBox" runat="server" Text='<%# Bind("vSimon5") %>'>
            </asp:TextBox><br />
            vMarkUp5:
            <asp:TextBox ID="vMarkUp5TextBox" runat="server" Text='<%# Bind("vMarkUp5") %>'>
            </asp:TextBox><br />
            vS6:
            <asp:TextBox ID="vS6TextBox" runat="server" Text='<%# Bind("vS6") %>'>
            </asp:TextBox><br />
            vB6:
            <asp:TextBox ID="vB6TextBox" runat="server" Text='<%# Bind("vB6") %>'>
            </asp:TextBox><br />
            vCost6:
            <asp:TextBox ID="vCost6TextBox" runat="server" Text='<%# Bind("vCost6") %>'>
            </asp:TextBox><br />
            vMatf6:
            <asp:TextBox ID="vMatf6TextBox" runat="server" Text='<%# Bind("vMatf6") %>'>
            </asp:TextBox><br />
            vLabf6:
            <asp:TextBox ID="vLabf6TextBox" runat="server" Text='<%# Bind("vLabf6") %>'>
            </asp:TextBox><br />
            vMatm6:
            <asp:TextBox ID="vMatm6TextBox" runat="server" Text='<%# Bind("vMatm6") %>'>
            </asp:TextBox><br />
            vLabm6:
            <asp:TextBox ID="vLabm6TextBox" runat="server" Text='<%# Bind("vLabm6") %>'>
            </asp:TextBox><br />
            vSimon6:
            <asp:TextBox ID="vSimon6TextBox" runat="server" Text='<%# Bind("vSimon6") %>'>
            </asp:TextBox><br />
            vMarkUp6:
            <asp:TextBox ID="vMarkUp6TextBox" runat="server" Text='<%# Bind("vMarkUp6") %>'>
            </asp:TextBox><br />
            vItem1:
            <asp:TextBox ID="vItem1TextBox" runat="server" Text='<%# Bind("vItem1") %>'>
            </asp:TextBox><br />
            vItemDscr1:
            <asp:TextBox ID="vItemDscr1TextBox" runat="server" Text='<%# Bind("vItemDscr1") %>'>
            </asp:TextBox><br />
            vQty1:
            <asp:TextBox ID="vQty1TextBox" runat="server" Text='<%# Bind("vQty1") %>'>
            </asp:TextBox><br />
            vUom1:
            <asp:TextBox ID="vUom1TextBox" runat="server" Text='<%# Bind("vUom1") %>'>
            </asp:TextBox><br />
            vItem2:
            <asp:TextBox ID="vItem2TextBox" runat="server" Text='<%# Bind("vItem2") %>'>
            </asp:TextBox><br />
            vItemDscr2:
            <asp:TextBox ID="vItemDscr2TextBox" runat="server" Text='<%# Bind("vItemDscr2") %>'>
            </asp:TextBox><br />
            vQty2:
            <asp:TextBox ID="vQty2TextBox" runat="server" Text='<%# Bind("vQty2") %>'>
            </asp:TextBox><br />
            vUom2:
            <asp:TextBox ID="vUom2TextBox" runat="server" Text='<%# Bind("vUom2") %>'>
            </asp:TextBox><br />
            vItem3:
            <asp:TextBox ID="vItem3TextBox" runat="server" Text='<%# Bind("vItem3") %>'>
            </asp:TextBox><br />
            vItemDscr3:
            <asp:TextBox ID="vItemDscr3TextBox" runat="server" Text='<%# Bind("vItemDscr3") %>'>
            </asp:TextBox><br />
            vQty3:
            <asp:TextBox ID="vQty3TextBox" runat="server" Text='<%# Bind("vQty3") %>'>
            </asp:TextBox><br />
            vUom3:
            <asp:TextBox ID="vUom3TextBox" runat="server" Text='<%# Bind("vUom3") %>'>
            </asp:TextBox><br />
            vItem4:
            <asp:TextBox ID="vItem4TextBox" runat="server" Text='<%# Bind("vItem4") %>'>
            </asp:TextBox><br />
            vItemDscr4:
            <asp:TextBox ID="vItemDscr4TextBox" runat="server" Text='<%# Bind("vItemDscr4") %>'>
            </asp:TextBox><br />
            vQty4:
            <asp:TextBox ID="vQty4TextBox" runat="server" Text='<%# Bind("vQty4") %>'>
            </asp:TextBox><br />
            vUom4:
            <asp:TextBox ID="vUom4TextBox" runat="server" Text='<%# Bind("vUom4") %>'>
            </asp:TextBox><br />
            vItem5:
            <asp:TextBox ID="vItem5TextBox" runat="server" Text='<%# Bind("vItem5") %>'>
            </asp:TextBox><br />
            vItemDscr5:
            <asp:TextBox ID="vItemDscr5TextBox" runat="server" Text='<%# Bind("vItemDscr5") %>'>
            </asp:TextBox><br />
            vQty5:
            <asp:TextBox ID="vQty5TextBox" runat="server" Text='<%# Bind("vQty5") %>'>
            </asp:TextBox><br />
            vUom5:
            <asp:TextBox ID="vUom5TextBox" runat="server" Text='<%# Bind("vUom5") %>'>
            </asp:TextBox><br />
            vItem6:
            <asp:TextBox ID="vItem6TextBox" runat="server" Text='<%# Bind("vItem6") %>'>
            </asp:TextBox><br />
            vItemDscr6:
            <asp:TextBox ID="vItemDscr6TextBox" runat="server" Text='<%# Bind("vItemDscr6") %>'>
            </asp:TextBox><br />
            vQty6:
            <asp:TextBox ID="vQty6TextBox" runat="server" Text='<%# Bind("vQty6") %>'>
            </asp:TextBox><br />
            vUom6:
            <asp:TextBox ID="vUom6TextBox" runat="server" Text='<%# Bind("vUom6") %>'>
            </asp:TextBox><br />
            vItem7:
            <asp:TextBox ID="vItem7TextBox" runat="server" Text='<%# Bind("vItem7") %>'>
            </asp:TextBox><br />
            vItemDscr7:
            <asp:TextBox ID="vItemDscr7TextBox" runat="server" Text='<%# Bind("vItemDscr7") %>'>
            </asp:TextBox><br />
            vQty7:
            <asp:TextBox ID="vQty7TextBox" runat="server" Text='<%# Bind("vQty7") %>'>
            </asp:TextBox><br />
            vUom7:
            <asp:TextBox ID="vUom7TextBox" runat="server" Text='<%# Bind("vUom7") %>'>
            </asp:TextBox><br />
            vItem8:
            <asp:TextBox ID="vItem8TextBox" runat="server" Text='<%# Bind("vItem8") %>'>
            </asp:TextBox><br />
            vItemDscr8:
            <asp:TextBox ID="vItemDscr8TextBox" runat="server" Text='<%# Bind("vItemDscr8") %>'>
            </asp:TextBox><br />
            vQty8:
            <asp:TextBox ID="vQty8TextBox" runat="server" Text='<%# Bind("vQty8") %>'>
            </asp:TextBox><br />
            vUom8:
            <asp:TextBox ID="vUom8TextBox" runat="server" Text='<%# Bind("vUom8") %>'>
            </asp:TextBox><br />
            vType:
            <asp:TextBox ID="vTypeTextBox" runat="server" Text='<%# Bind("vType") %>'>
            </asp:TextBox><br />
            <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:LinkButton>
            <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </InsertItemTemplate>--%>
        <ItemTemplate>
            <fieldset class="shade">
                <legend>Reference Information:</legend>
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"><b>Estimate:</b></td>
                            <td nowrap><b><asp:Label ID="vEstimateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstimate") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Est Date:</b></td>
                            <td nowrap><b><asp:Label ID="vEstDateLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Frm:</b></td>
                            <td nowrap><b>
                                    <asp:Label ID="vFormLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vForm") %>'></asp:Label>
                                    of
                                    <asp:Label ID="vFormQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vFormQty") %>'></asp:Label>
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Blk</b></td>
                            <td nowrap><b>
                                  <asp:Label ID="vBlkLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlk") %>'></asp:Label>
                                  of
                                  <asp:Label ID="vBlkQtyLabel" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vBlkQty") %>'></asp:Label>  
                            </b></td>
                            <td nowrap align="right" style="padding-right:5px;"><b>Cust Part:</b></td>
                            <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                            
                        </tr>
                    </table>
            </fieldset>
            <fieldset class="shade">
                <legend>Sub-Contract,Farm-Out and Misc Cost:</legend>
                    <table>
                        <tr>
                            <td nowrap><b>S</b></td>
                            <td nowrap><b>B</b></td>
                            <td nowrap><b>Misc. Cost</b></td>
                            <td nowrap><b>Mat'l/SU</b></td>
                            <td nowrap><b>Labour/SU</b></td>
                            <td nowrap><b>Mat's/M</b></td>
                            <td nowrap><b>Labour/M</b></td>
                            <td nowrap><b>Simon</b></td>
                            <td nowrap><b>Markup</b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf1","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf1","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm1","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm1","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp1","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf2","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf2","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm2","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm2","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp2","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf3","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf3","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm3","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm3","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp3","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf4","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf4","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm4","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm4","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp4","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf5","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf5","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm5","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm5","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp5","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vS6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vS6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vB6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" Text='<%# Bind("vB6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vCost6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vCost6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatf6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatf6","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabf6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabf6","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMatm6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMatm6","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vLabm6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vLabm6","{0:###,##0.00}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vSimon6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vSimon6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vMarkUp6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vMarkUp6","{0:###,##0.00}") %>'></asp:Label></b></td>
                        </tr>
                    </table>
            </fieldset>           
            <fieldset class="shade">
                <legend>Special Materials</legend>
                    <table>
                        <tr>
                            <td nowrap><b>Item#</b></td>
                            <td nowrap><b>Description</b></td>
                            <td nowrap><b>Qty/FG (Set)</b></td>
                            <td nowrap><b>UOM</b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr1") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty1","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom1Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom1") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr2") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty2","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom2Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom2") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr3") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty3","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom3Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom3") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr4") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty4","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom4Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom4") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr5") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty5","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom5Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom5") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr6") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty6","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom6Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom6") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem7Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem7") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr7Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr7") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty7Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty7","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom7Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom7") %>'></asp:Label></b></td>
                        </tr>
                        <tr>
                            <td nowrap><b><asp:Label ID="vItem8Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vItem8") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vItemDscr8Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" Text='<%# Bind("vItemDscr8") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vQty8Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vQty8","{0:###,###,##0.0}") %>'></asp:Label></b></td>
                            <td nowrap><b><asp:Label ID="vUom8Label" runat="server" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" Text='<%# Bind("vUom8") %>'></asp:Label></b></td>
                        </tr>
                    </table>
            </fieldset> 
            <%--<asp:Label ID="vTypeLabel" runat="server" Text='<%# Bind("vType") %>'></asp:Label><br />--%>
            <asp:Button ID="btn_update" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" />
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="MiscSub_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectCorrMiscSub" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmForm" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmBlk" Type="Int32" />
            <asp:Parameter Name="prmBlkQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmS1" Type="Int32" />
            <asp:Parameter Name="prmB1" Type="Int32" />
            <asp:Parameter Name="prmCost1" Type="String" />
            <asp:Parameter Name="prmMatf1" Type="Decimal" />
            <asp:Parameter Name="prmMatm1" Type="Decimal" />
            <asp:Parameter Name="prmLabf1" Type="Decimal" />
            <asp:Parameter Name="prmLabm1" Type="Decimal" />
            <asp:Parameter Name="prmSimon1" Type="String" />
            <asp:Parameter Name="prmMarkup1" Type="Decimal" />
            <asp:Parameter Name="prmS2" Type="Int32" />
            <asp:Parameter Name="prmB2" Type="Int32" />
            <asp:Parameter Name="prmCost2" Type="String" />
            <asp:Parameter Name="prmMatf2" Type="Decimal" />
            <asp:Parameter Name="prmMatm2" Type="Decimal" />
            <asp:Parameter Name="prmLabf2" Type="Decimal" />
            <asp:Parameter Name="prmLabm2" Type="Decimal" />
            <asp:Parameter Name="prmSimon2" Type="String" />
            <asp:Parameter Name="prmMarkup2" Type="Decimal" />
            <asp:Parameter Name="prmS3" Type="Int32" />
            <asp:Parameter Name="prmB3" Type="Int32" />
            <asp:Parameter Name="prmCost3" Type="String" />
            <asp:Parameter Name="prmMatf3" Type="Decimal" />
            <asp:Parameter Name="prmMatm3" Type="Decimal" />
            <asp:Parameter Name="prmLabf3" Type="Decimal" />
            <asp:Parameter Name="prmLabm3" Type="Decimal" />
            <asp:Parameter Name="prmSimon3" Type="String" />
            <asp:Parameter Name="prmMarkup3" Type="Decimal" />
            <asp:Parameter Name="prmS4" Type="Int32" />
            <asp:Parameter Name="prmB4" Type="Int32" />
            <asp:Parameter Name="prmCost4" Type="String" />
            <asp:Parameter Name="prmMatf4" Type="Decimal" />
            <asp:Parameter Name="prmMatm4" Type="Decimal" />
            <asp:Parameter Name="prmLabf4" Type="Decimal" />
            <asp:Parameter Name="prmLabm4" Type="Decimal" />
            <asp:Parameter Name="prmSimon4" Type="String" />
            <asp:Parameter Name="prmMarkup4" Type="Decimal" />
            <asp:Parameter Name="prmS5" Type="Int32" />
            <asp:Parameter Name="prmB5" Type="Int32" />
            <asp:Parameter Name="prmCost5" Type="String" />
            <asp:Parameter Name="prmMatf5" Type="Decimal" />
            <asp:Parameter Name="prmMatm5" Type="Decimal" />
            <asp:Parameter Name="prmLabf5" Type="Decimal" />
            <asp:Parameter Name="prmLabm5" Type="Decimal" />
            <asp:Parameter Name="prmSimon5" Type="String" />
            <asp:Parameter Name="prmMarkup5" Type="Decimal" />
            <asp:Parameter Name="prmS6" Type="Int32" />
            <asp:Parameter Name="prmB6" Type="Int32" />
            <asp:Parameter Name="prmCost6" Type="String" />
            <asp:Parameter Name="prmMatf6" Type="Decimal" />
            <asp:Parameter Name="prmMatm6" Type="Decimal" />
            <asp:Parameter Name="prmLabf6" Type="Decimal" />
            <asp:Parameter Name="prmLabm6" Type="Decimal" />
            <asp:Parameter Name="prmSimon6" Type="String" />
            <asp:Parameter Name="prmMarkup6" Type="Decimal" />
            <asp:Parameter Name="prmItem1" Type="String" />
            <asp:Parameter Name="prmItemDesc1" Type="String" />
            <asp:Parameter Name="prmQty1" Type="Decimal" />
            <asp:Parameter Name="prmUom1" Type="String" />
            <asp:Parameter Name="prmItem2" Type="String" />
            <asp:Parameter Name="prmItemDesc2" Type="String" />
            <asp:Parameter Name="prmQty2" Type="Decimal" />
            <asp:Parameter Name="prmUom2" Type="String" />
            <asp:Parameter Name="prmItem3" Type="String" />
            <asp:Parameter Name="prmItemDesc3" Type="String" />
            <asp:Parameter Name="prmQty3" Type="Decimal" />
            <asp:Parameter Name="prmUom3" Type="String" />
            <asp:Parameter Name="prmItem4" Type="String" />
            <asp:Parameter Name="prmItemDesc4" Type="String" />
            <asp:Parameter Name="prmQty4" Type="Decimal" />
            <asp:Parameter Name="prmUom4" Type="String" />
            <asp:Parameter Name="prmItem5" Type="String" />
            <asp:Parameter Name="prmItemDesc5" Type="String" />
            <asp:Parameter Name="prmQty5" Type="Decimal" />
            <asp:Parameter Name="prmUom5" Type="String" />
            <asp:Parameter Name="prmItem6" Type="String" />
            <asp:Parameter Name="prmItemDesc6" Type="String" />
            <asp:Parameter Name="prmQty6" Type="Decimal" />
            <asp:Parameter Name="prmUom6" Type="String" />
            <asp:Parameter Name="prmItem7" Type="String" />
            <asp:Parameter Name="prmItemDesc7" Type="String" />
            <asp:Parameter Name="prmQty7" Type="Decimal" />
            <asp:Parameter Name="prmUom7" Type="String" />
            <asp:Parameter Name="prmItem8" Type="String" />
            <asp:Parameter Name="prmItemDesc8" Type="String" />
            <asp:Parameter Name="prmQty8" Type="Decimal" />
            <asp:Parameter Name="prmUom8" Type="String" />
            <asp:SessionParameter Name="prmBlank" SessionField="order_corrugated_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
</asp:Content>

