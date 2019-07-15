
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_gl_bank : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_gl_bank.aspx";
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

            //RadioButtonList1.SelectedIndex = 0;

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

                if (Convert.ToString(Session["gl_bank_list_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["gl_bank_list_add_button"] = null;
                }

            }

            //if (cyr1TextBox.text == "")
            //{
            //    cyr1TextBox.text = "0.00";
            //}
            
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {           


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            TextBox reckey = (TextBox)FormView1.FindControl("reckeyTextBox");
            Session["gl_bank_list_reckey"] = reckey.Text.Trim();
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


    

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox bnkcd = (TextBox)FormView1.FindControl("bank_codeTextBox");
        TextBox bnknm = (TextBox)FormView1.FindControl("bank_nameTextBox");

        TextBox add1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox stat = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox phn = (TextBox)FormView1.FindControl("phoneTextBox");
        TextBox cntat = (TextBox)FormView1.FindControl("contactTextBox");
        TextBox bkact = (TextBox)FormView1.FindControl("bk_actTextBox");
        TextBox act = (TextBox)FormView1.FindControl("actnumTextBox");
        Label actdsr = (Label)FormView1.FindControl("accdscrTextBox");
        TextBox lstchk = (TextBox)FormView1.FindControl("last_chkTextBox");
        TextBox bal = (TextBox)FormView1.FindControl("balTextBox");
        TextBox ochk = (TextBox)FormView1.FindControl("o_chkTextBox");
        TextBox dep = (TextBox)FormView1.FindControl("dep_trTextBox");
        TextBox serv = (TextBox)FormView1.FindControl("servTextBox");
        TextBox curr = (TextBox)FormView1.FindControl("curr_codeTextBox");       


        UserClass UserLogin = (UserClass)Session["User"];

        try
        {

            ledger val = new ledger();

            bool check = val.validbanklistbank("AddNewRec", UserLogin.UserName, "", bnkcd.Text.Trim(), bnknm.Text.Trim(), add1.Text.Trim(), add2.Text.Trim(), city.Text.Trim(), stat.Text.Trim(), zip.Text.Trim(), phn.Text.Trim(), cntat.Text.Trim() , bkact.Text.Trim(), act.Text.Trim(), lstchk.Text.Trim(), bal.Text.Trim(), ochk.Text.Trim(), dep.Text.Trim(), serv.Text.Trim(), curr.Text.Trim(), "", Convert.ToString(Session["gl_bank_list_reckey"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {               
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";                
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmBankcode"].DefaultValue = bnkcd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBankname"].DefaultValue = bnknm.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAddr1"].DefaultValue = add1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAddr2"].DefaultValue = add2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmState"].DefaultValue = stat.Text.Trim();
                ObjectDataSource1.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPhone"].DefaultValue = phn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = cntat.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBkact"].DefaultValue = bkact.Text.Trim();
                ObjectDataSource1.SelectParameters["prmActnum"].DefaultValue = act.Text.Trim();

                ObjectDataSource1.SelectParameters["prmLastchk"].DefaultValue = lstchk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBal"].DefaultValue = bal.Text.Trim();
                ObjectDataSource1.SelectParameters["prmOchk"].DefaultValue = ochk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDeptr"].DefaultValue = dep.Text.Trim();

                ObjectDataSource1.SelectParameters["prmServ"].DefaultValue = serv.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCurrcode"].DefaultValue = curr.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly); 

            }
        }
        catch { }
            

    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox bnkcd = (TextBox)FormView1.FindControl("bank_codeTextBox");
        TextBox bnknm = (TextBox)FormView1.FindControl("bank_nameTextBox");

        TextBox add1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox stat = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox phn = (TextBox)FormView1.FindControl("phoneTextBox");
        TextBox cntat = (TextBox)FormView1.FindControl("contactTextBox");
        TextBox bkact = (TextBox)FormView1.FindControl("bk_actTextBox");
        TextBox act = (TextBox)FormView1.FindControl("actnumTextBox");
        Label actdsr = (Label)FormView1.FindControl("accdscrTextBox");
        TextBox lstchk = (TextBox)FormView1.FindControl("last_chkTextBox");
        TextBox bal = (TextBox)FormView1.FindControl("balTextBox");
        TextBox ochk = (TextBox)FormView1.FindControl("o_chkTextBox");
        TextBox dep = (TextBox)FormView1.FindControl("dep_trTextBox");
        TextBox serv = (TextBox)FormView1.FindControl("servTextBox");
        TextBox curr = (TextBox)FormView1.FindControl("curr_codeTextBox");

        UserClass UserLogin = (UserClass)Session["User"];

        //try
        //{

        //    ledger val = new ledger();

        //    bool check = val.updatebanklistbank("UpdateRec", UserLogin.UserName, "", bnkcd.Text.Trim(), bnknm.Text.Trim(), add1.Text.Trim(), add2.Text.Trim(), city.Text.Trim(), stat.Text.Trim(), zip.Text.Trim(), phn.Text.Trim(), cntat.Text.Trim() , bkact.Text.Trim(), act.Text.Trim(), lstchk.Text.Trim(), bal.Text.Trim(), ochk.Text.Trim(), dep.Text.Trim(), serv.Text.Trim(), curr.Text.Trim(), "", Convert.ToString(Session["gl_bank_list_reckey"]));

        //    string value = Convert.ToString(check);
        //    if (value == "True")
        //    {         
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRec";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;                
                ObjectDataSource1.SelectParameters["prmBankcode"].DefaultValue = bnkcd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBankname"].DefaultValue = bnknm.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAddr1"].DefaultValue = add1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAddr2"].DefaultValue = add2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmState"].DefaultValue = stat.Text.Trim();
                ObjectDataSource1.SelectParameters["prmZip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPhone"].DefaultValue = phn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = cntat.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBkact"].DefaultValue = bkact.Text.Trim();
                ObjectDataSource1.SelectParameters["prmActnum"].DefaultValue = act.Text.Trim();

                ObjectDataSource1.SelectParameters["prmLastchk"].DefaultValue = lstchk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmBal"].DefaultValue = bal.Text.Trim();
                ObjectDataSource1.SelectParameters["prmOchk"].DefaultValue = ochk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDeptr"].DefaultValue = dep.Text.Trim();

                ObjectDataSource1.SelectParameters["prmServ"].DefaultValue = serv.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCurrcode"].DefaultValue = curr.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmBankno"].DefaultValue = .Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
               

        //    }
        //}
        //catch { }      
    }


    protected void delete_Button_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DeleteRec";
        Session["gl_bank_list_index"] = null;
        /* Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");*/
    }
    
    protected void lnk_listglaccount(object sender, EventArgs e)
    {
        Response.Redirect("gl_bank_list.aspx");
    }
    protected void lnk_viewglaccount_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_bank.aspx");
    }
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }
    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    
    



}