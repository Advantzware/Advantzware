
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class salestax_viewlist : System.Web.UI.Page
{
    
    protected void Page_Load(object sender, System.EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "salestax_code_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        


        

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

            }

           
          
        }
        if (Convert.ToString(Session["sales_tax_add_new_code"]) == "addsaletax")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["sales_tax_add_new_code"] = null;
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox txgr = (TextBox)FormView1.FindControl("taxgrpTextBox");
            
            txgr.Focus();
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox fri1 = (TextBox)FormView1.FindControl("frt1TextBox");
            TextBox fri2 = (TextBox)FormView1.FindControl("frt2TextBox");
            TextBox fri3 = (TextBox)FormView1.FindControl("frt3TextBox");
            TextBox fri4 = (TextBox)FormView1.FindControl("frt4TextBox");
            TextBox fri5 = (TextBox)FormView1.FindControl("frt5TextBox");
            CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
            CheckBox edi2 = (CheckBox)FormView1.FindControl("CheckBox2");
            CheckBox edi3 = (CheckBox)FormView1.FindControl("CheckBox3");
            CheckBox edi4 = (CheckBox)FormView1.FindControl("CheckBox4");
            CheckBox edi5 = (CheckBox)FormView1.FindControl("CheckBox5");
            Label taxla = (Label)FormView1.FindControl("textlabel");
            CheckBox taxchk = (CheckBox)FormView1.FindControl("taxTextBox");
            TextBox code = (TextBox)FormView1.FindControl("code1TextBox");

            if (fri1.Text == "yes")
                edi.Checked = true;
            else
                edi.Checked = false;
            if (fri2.Text == "yes")
                edi2.Checked = true;
            else
                edi2.Checked = false;
            if (fri3.Text == "yes")
                edi3.Checked = true;
            else
                edi3.Checked = false;
            if (fri4.Text == "yes")
                edi4.Checked = true;
            else
                edi4.Checked = false;
            if (fri5.Text == "yes")
                edi5.Checked = true;
            else
                edi5.Checked = false;
            if (taxla.Text == "")
                taxchk.Checked = true;
            else
                taxchk.Checked = false;
            code.Focus();


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            
            try
            {
                Label fri1 = (Label)FormView1.FindControl("frt1Label");
                Label fri2 = (Label)FormView1.FindControl("frt2Label");
                Label fri3 = (Label)FormView1.FindControl("frt3Label");
                Label fri4 = (Label)FormView1.FindControl("frt4Label");
                Label fri5 = (Label)FormView1.FindControl("frt5Label");
                CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
                CheckBox edi2 = (CheckBox)FormView1.FindControl("CheckBox2");
                CheckBox edi3 = (CheckBox)FormView1.FindControl("CheckBox3");
                CheckBox edi4 = (CheckBox)FormView1.FindControl("CheckBox4");
                CheckBox edi5 = (CheckBox)FormView1.FindControl("CheckBox5");
                Label taxla = (Label)FormView1.FindControl("textlabel");
                CheckBox taxchk = (CheckBox)FormView1.FindControl("taxTextBox");

                if (fri1.Text == "yes")
                    edi.Checked = true;
                else
                    edi.Checked = false;
                if (fri2.Text == "yes")
                    edi2.Checked = true;
                else
                    edi2.Checked = false;
                if (fri3.Text == "yes")
                    edi3.Checked = true;
                else
                    edi3.Checked = false;
                if (fri4.Text == "yes")
                    edi4.Checked = true;
                else
                    edi4.Checked = false;
                if (fri5.Text == "yes")
                    edi5.Checked = true;
                else
                    edi5.Checked = false;
                if (taxla.Text == "")
                    taxchk.Checked = true;
                else
                    taxchk.Checked = false;
                Label reckey = (Label)FormView1.FindControl("reckeyLabel");
                Session["salestax_code_reckey"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
            }
            catch { }
        }
    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }


   
    
    protected void lnk_Listvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("salestax_code_list.aspx");
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("salestax_viewlist.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox taxgr = (TextBox)FormView1.FindControl("taxgrpTextBox");
        TextBox code1 = (TextBox)FormView1.FindControl("code1TextBox");
        TextBox dscr1 = (TextBox)FormView1.FindControl("dscr1TextBox");
        TextBox rate1 = (TextBox)FormView1.FindControl("rate1TextBox");
        TextBox acc1 = (TextBox)FormView1.FindControl("acclTextBox");
        TextBox frt1 = (TextBox)FormView1.FindControl("frt1TextBox");

        TextBox code2 = (TextBox)FormView1.FindControl("code2TextBox");
        TextBox dscr2 = (TextBox)FormView1.FindControl("dscr2TextBox");
        TextBox rate2 = (TextBox)FormView1.FindControl("rate2TextBox");
        TextBox acc2 = (TextBox)FormView1.FindControl("acc2TextBox");
        TextBox frt2 = (TextBox)FormView1.FindControl("frt2TextBox");

        TextBox code3 = (TextBox)FormView1.FindControl("code3TextBox");
        TextBox dscr3 = (TextBox)FormView1.FindControl("dscr3TextBox");
        TextBox rate3 = (TextBox)FormView1.FindControl("rate3TextBox");
        TextBox acc3 = (TextBox)FormView1.FindControl("acc3TextBox");
        TextBox frt3 = (TextBox)FormView1.FindControl("frt3TextBox");

        TextBox code4 = (TextBox)FormView1.FindControl("code4TextBox");
        TextBox dscr4 = (TextBox)FormView1.FindControl("dscr4TextBox");
        TextBox rate4 = (TextBox)FormView1.FindControl("rate4TextBox");
        TextBox acc4 = (TextBox)FormView1.FindControl("acc4TextBox");
        TextBox frt4 = (TextBox)FormView1.FindControl("frt4TextBox");

        TextBox code5 = (TextBox)FormView1.FindControl("code5TextBox");
        TextBox dscr5 = (TextBox)FormView1.FindControl("dscr5TextBox");
        TextBox rate5 = (TextBox)FormView1.FindControl("rate5TextBox");
        TextBox acc5 = (TextBox)FormView1.FindControl("acc5TextBox");
        TextBox frt5 = (TextBox)FormView1.FindControl("frt5TextBox");

        CheckBox tax = (CheckBox)FormView1.FindControl("taxTextBox");
        CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
        CheckBox edi2 = (CheckBox)FormView1.FindControl("CheckBox2");
        CheckBox edi3 = (CheckBox)FormView1.FindControl("CheckBox3");
        CheckBox edi4 = (CheckBox)FormView1.FindControl("CheckBox4");
        CheckBox edi5 = (CheckBox)FormView1.FindControl("CheckBox5");
        
       


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmtaxgrp"].DefaultValue = taxgr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcode1"].DefaultValue = code1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr1"].DefaultValue = dscr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate1"].DefaultValue = rate1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccl"].DefaultValue = acc1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt1"].DefaultValue = Convert.ToString(edi.Checked);

        ObjectDataSource1.SelectParameters["prmcode2"].DefaultValue = code2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr2"].DefaultValue = dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate2"].DefaultValue = rate2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc2"].DefaultValue = acc2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt2"].DefaultValue = Convert.ToString(edi2.Checked);

        ObjectDataSource1.SelectParameters["prmcode3"].DefaultValue = code3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr3"].DefaultValue = dscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate3"].DefaultValue = rate3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc3"].DefaultValue = acc3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt3"].DefaultValue = Convert.ToString(edi3.Checked);

        ObjectDataSource1.SelectParameters["prmcode4"].DefaultValue = code4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr4"].DefaultValue = dscr4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate4"].DefaultValue = rate4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc4"].DefaultValue = acc4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt4"].DefaultValue = Convert.ToString(edi4.Checked);

        ObjectDataSource1.SelectParameters["prmcode5"].DefaultValue = code5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr5"].DefaultValue = dscr5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate5"].DefaultValue = rate5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc5"].DefaultValue = acc5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt5"].DefaultValue = Convert.ToString(edi5.Checked);

        ObjectDataSource1.SelectParameters["prmtax"].DefaultValue = Convert.ToString(tax.Checked);

        

                //Response.Write("<script> window.location.href='vend_viewlist.aspx' </script>");
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        
        TextBox taxgr = (TextBox)FormView1.FindControl("taxgrpTextBox");
        TextBox code1 = (TextBox)FormView1.FindControl("code1TextBox");
        TextBox dscr1 = (TextBox)FormView1.FindControl("dscr1TextBox");
        TextBox rate1 = (TextBox)FormView1.FindControl("rate1TextBox");
        TextBox acc1 = (TextBox)FormView1.FindControl("acclTextBox");
        TextBox frt1 = (TextBox)FormView1.FindControl("frt1TextBox");

        TextBox code2 = (TextBox)FormView1.FindControl("code2TextBox");
        TextBox dscr2 = (TextBox)FormView1.FindControl("dscr2TextBox");
        TextBox rate2 = (TextBox)FormView1.FindControl("rate2TextBox");
        TextBox acc2 = (TextBox)FormView1.FindControl("acc2TextBox");
        TextBox frt2 = (TextBox)FormView1.FindControl("frt2TextBox");

        TextBox code3 = (TextBox)FormView1.FindControl("code3TextBox");
        TextBox dscr3 = (TextBox)FormView1.FindControl("dscr3TextBox");
        TextBox rate3 = (TextBox)FormView1.FindControl("rate3TextBox");
        TextBox acc3 = (TextBox)FormView1.FindControl("acc3TextBox");
        TextBox frt3 = (TextBox)FormView1.FindControl("frt3TextBox");

        TextBox code4 = (TextBox)FormView1.FindControl("code4TextBox");
        TextBox dscr4 = (TextBox)FormView1.FindControl("dscr4TextBox");
        TextBox rate4 = (TextBox)FormView1.FindControl("rate4TextBox");
        TextBox acc4 = (TextBox)FormView1.FindControl("acc4TextBox");
        TextBox frt4 = (TextBox)FormView1.FindControl("frt4TextBox");

        TextBox code5 = (TextBox)FormView1.FindControl("code5TextBox");
        TextBox dscr5 = (TextBox)FormView1.FindControl("dscr5TextBox");
        TextBox rate5 = (TextBox)FormView1.FindControl("rate5TextBox");
        TextBox acc5 = (TextBox)FormView1.FindControl("acc5TextBox");
        TextBox frt5 = (TextBox)FormView1.FindControl("frt5TextBox");

        CheckBox tax = (CheckBox)FormView1.FindControl("taxTextBox");
        CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
        CheckBox edi2 = (CheckBox)FormView1.FindControl("CheckBox2");
        CheckBox edi3 = (CheckBox)FormView1.FindControl("CheckBox3");
        CheckBox edi4 = (CheckBox)FormView1.FindControl("CheckBox4");
        CheckBox edi5 = (CheckBox)FormView1.FindControl("CheckBox5");




        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmtaxgrp"].DefaultValue = taxgr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcode1"].DefaultValue = code1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr1"].DefaultValue = dscr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate1"].DefaultValue = rate1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccl"].DefaultValue = acc1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt1"].DefaultValue = Convert.ToString(edi.Checked);

        ObjectDataSource1.SelectParameters["prmcode2"].DefaultValue = code2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr2"].DefaultValue = dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate2"].DefaultValue = rate2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc2"].DefaultValue = acc2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt2"].DefaultValue = Convert.ToString(edi2.Checked);

        ObjectDataSource1.SelectParameters["prmcode3"].DefaultValue = code3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr3"].DefaultValue = dscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate3"].DefaultValue = rate3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc3"].DefaultValue = acc3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt3"].DefaultValue = Convert.ToString(edi3.Checked);

        ObjectDataSource1.SelectParameters["prmcode4"].DefaultValue = code4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr4"].DefaultValue = dscr4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate4"].DefaultValue = rate4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc4"].DefaultValue = acc4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt4"].DefaultValue = Convert.ToString(edi4.Checked);

        ObjectDataSource1.SelectParameters["prmcode5"].DefaultValue = code5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdscr5"].DefaultValue = dscr5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmrate5"].DefaultValue = rate5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacc5"].DefaultValue = acc5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrt5"].DefaultValue = Convert.ToString(edi5.Checked);

        ObjectDataSource1.SelectParameters["prmtax"].DefaultValue = Convert.ToString(tax.Checked);

        FormView1.ChangeMode(FormViewMode.ReadOnly);
            
                //Response.Write("<script> window.location.href='cust_viewlist.aspx' </script>");
          
    }


    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        //Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");
    }
    
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    protected void Formview_Unload(object sender, EventArgs e)
    {

    }
    



}
