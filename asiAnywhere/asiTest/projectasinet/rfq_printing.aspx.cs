using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Text;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class rfqprinting : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        
        Session["my_new_rfq"] = null;

        //ImageButton rfqprinting = (ImageButton)Master.FindControl("rfq_printing");
        //rfqprinting.ImageUrl = "~/Images/rfqprinting1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Rfq Printing";
        FormView1.ChangeMode(FormViewMode.ReadOnly);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {


                string vUserId = UserLogin.UserName;
                string vPage = "rfq_printing.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;


                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                //lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    //lnk_brwsorder.Visible = true;
                    //brwsorder.Visible = true;

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
            //ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";

        }

        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode1TextBox.value=e[0];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode2TextBox.value=e[1];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode3TextBox.value=e[2];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode4TextBox.value=e[3];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode5TextBox.value=e[4];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode6TextBox.value=e[5];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode7TextBox.value=e[6];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode8TextBox.value=e[7];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode9TextBox.value=e[8];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode10TextBox.value=e[9];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr1TextBox.value=e[10];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr2TextBox.value=e[11];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr3TextBox.value=e[12];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr4TextBox.value=e[13];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr5TextBox.value=e[14];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr6TextBox.value=e[15];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr7TextBox.value=e[16];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr8TextBox.value=e[17];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr9TextBox.value=e[18];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr10TextBox.value=e[19];");

        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper1TextBox.value=e[20];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper2TextBox.value=e[21];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper3TextBox.value=e[22];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper4TextBox.value=e[23];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper5TextBox.value=e[24];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper6TextBox.value=e[25];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper7TextBox.value=e[26];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper8TextBox.value=e[27];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper9TextBox.value=e[28];");
        str.Append("document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper10TextBox.value=e[29];}");

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }


    }
    protected void UpdateButton_click(object sender, EventArgs e)
    {
        TextBox color = (TextBox)FormView1.FindControl("vPcolTextBox");
        TextBox passes = (TextBox)FormView1.FindControl("vPassTextBox");
        TextBox coating = (TextBox)FormView1.FindControl("vCoatTextBox");
        TextBox colordesc = (TextBox)FormView1.FindControl("vColdscrTextBox");
        TextBox pass1 = (TextBox)FormView1.FindControl("vIps1TextBox");
        TextBox pass2 = (TextBox)FormView1.FindControl("vIps2TextBox");
        TextBox pass3 = (TextBox)FormView1.FindControl("vIps3TextBox");
        TextBox pass4 = (TextBox)FormView1.FindControl("vIps4TextBox");
        TextBox pass5 = (TextBox)FormView1.FindControl("vIps5TextBox");
        TextBox pass6 = (TextBox)FormView1.FindControl("vIps6TextBox");
        TextBox pass7 = (TextBox)FormView1.FindControl("vIps7TextBox");
        TextBox pass8 = (TextBox)FormView1.FindControl("vIps8TextBox");
        TextBox pass9 = (TextBox)FormView1.FindControl("vIps9TextBox");
        TextBox pass10 = (TextBox)FormView1.FindControl("vIps10TextBox");
        TextBox code1 = (TextBox)FormView1.FindControl("vIcode1TextBox");
        TextBox code2 = (TextBox)FormView1.FindControl("vIcode2TextBox");
        TextBox code3 = (TextBox)FormView1.FindControl("vIcode3TextBox");
        TextBox code4 = (TextBox)FormView1.FindControl("vIcode4TextBox");
        TextBox code5 = (TextBox)FormView1.FindControl("vIcode5TextBox");
        TextBox code6 = (TextBox)FormView1.FindControl("vIcode6TextBox");
        TextBox code7 = (TextBox)FormView1.FindControl("vIcode7TextBox");
        TextBox code8 = (TextBox)FormView1.FindControl("vIcode8TextBox");
        TextBox code9 = (TextBox)FormView1.FindControl("vIcode9TextBox");
        TextBox code10 = (TextBox)FormView1.FindControl("vIcode10TextBox");
        TextBox description1 = (TextBox)FormView1.FindControl("vCdscr1TextBox");
        TextBox description2 = (TextBox)FormView1.FindControl("vCdscr2TextBox");
        TextBox description3 = (TextBox)FormView1.FindControl("vCdscr3TextBox");
        TextBox description4 = (TextBox)FormView1.FindControl("vCdscr4TextBox");
        TextBox description5 = (TextBox)FormView1.FindControl("vCdscr5TextBox");
        TextBox description6 = (TextBox)FormView1.FindControl("vCdscr6TextBox");
        TextBox description7 = (TextBox)FormView1.FindControl("vCdscr7TextBox");
        TextBox description8 = (TextBox)FormView1.FindControl("vCdscr8TextBox");
        TextBox description9 = (TextBox)FormView1.FindControl("vCdscr9TextBox");
        TextBox description10 = (TextBox)FormView1.FindControl("vCdscr10TextBox");
        TextBox perc1 = (TextBox)FormView1.FindControl("vIper1TextBox");
        TextBox perc2 = (TextBox)FormView1.FindControl("vIper2TextBox");
        TextBox perc3 = (TextBox)FormView1.FindControl("vIper3TextBox");
        TextBox perc4 = (TextBox)FormView1.FindControl("vIper4TextBox");
        TextBox perc5 = (TextBox)FormView1.FindControl("vIper5TextBox");
        TextBox perc6 = (TextBox)FormView1.FindControl("vIper6TextBox");
        TextBox perc7 = (TextBox)FormView1.FindControl("vIper7TextBox");
        TextBox perc8 = (TextBox)FormView1.FindControl("vIper8TextBox");
        TextBox perc9 = (TextBox)FormView1.FindControl("vIper9TextBox");
        TextBox perc10 = (TextBox)FormView1.FindControl("vIper10TextBox");

        
        //Response.Write(color.Text);
        //Response.Write(Session["col"]);

        
        int colval = Convert.ToInt32(color.Text);
        int coatval = Convert.ToInt32(coating.Text);
        int total = colval + coatval;
        //Response.Write(total);
        int count = 0;
        if (code1.Text.Trim() != "")
            count = count + 1;
        if (code2.Text.Trim() != "")
            count = count + 1;
        if (code3.Text.Trim() != "")
            count = count + 1;
        if (code4.Text.Trim() != "")
            count = count + 1;
        if (code5.Text.Trim() != "")
            count = count + 1;
        if (code6.Text.Trim() != "")
            count = count + 1;
        if (code7.Text.Trim() != "")
            count = count + 1;
        if (code8.Text.Trim() != "")
            count = count + 1;
        if (code9.Text.Trim() != "")
            count = count + 1;
        if (code10.Text.Trim() != "")
            count = count + 1;

        //Response.Write("count:"+count);
        if (total != count)
        {
            Response.Write("<script>alert('Invalid Number of Color and Coating')</script>");
            FormView1.ChangeMode(FormViewMode.Edit);
            //Response.Write("<script>history.back();</script>");
        }
        else
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRfqPrinting";
            ObjectDataSource1.SelectParameters["prmPcol"].DefaultValue = color.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPass"].DefaultValue = passes.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCoat"].DefaultValue = coating.Text.Trim();
            ObjectDataSource1.SelectParameters["prmColdscr"].DefaultValue = colordesc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps1"].DefaultValue = pass1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps2"].DefaultValue = pass2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps3"].DefaultValue = pass3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps4"].DefaultValue = pass4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps5"].DefaultValue = pass5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps6"].DefaultValue = pass6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps7"].DefaultValue = pass7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmvIps8"].DefaultValue = pass8.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps9"].DefaultValue = pass9.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIps10"].DefaultValue = pass10.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode1"].DefaultValue = code1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode2"].DefaultValue = code2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode3"].DefaultValue = code3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode4"].DefaultValue = code4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode5"].DefaultValue = code5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode6"].DefaultValue = code6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode7"].DefaultValue = code7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode8"].DefaultValue = code8.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode9"].DefaultValue = code9.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIcode10"].DefaultValue = code10.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr1"].DefaultValue = description1.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr2"].DefaultValue = description2.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr3"].DefaultValue = description3.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr4"].DefaultValue = description4.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr5"].DefaultValue = description5.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr6"].DefaultValue = description6.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr7"].DefaultValue = description7.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr8"].DefaultValue = description8.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr9"].DefaultValue = description9.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCdscr10"].DefaultValue = description10.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper1"].DefaultValue = perc1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper2"].DefaultValue = perc2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper3"].DefaultValue = perc3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper4"].DefaultValue = perc4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper5"].DefaultValue = perc5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper6"].DefaultValue = perc6.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper7"].DefaultValue = perc7.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper8"].DefaultValue = perc8.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper9"].DefaultValue = perc9.Text.Trim();
            ObjectDataSource1.SelectParameters["prmIper10"].DefaultValue = perc10.Text.Trim();

            //prmComp, prmUser, prmAction, PrmRfqNo, RfqPRowid, RfqSeq,

        }

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox col = (TextBox)FormView1.FindControl("vPcolTextBox");
            col.Focus();

        }
    }
}